import itertools
import zipfile
from datetime import datetime, timedelta

import h5py
import numpy as np
import pytest

from srg import postprocess


NR, NAZ = 24, 20
DATES = [datetime(2025, 1, 1) + timedelta(days=12 * i) for i in range(6)]
VELOCITY = 0.05  # m/yr


def write_product(path, acq_time='T002200', x_first=-92.137, ref_locs=''):
    """Synthetic SRG SBAS product: linear LOS displacement at VELOCITY everywhere."""
    path.mkdir()
    (path / 'dem.rsc').write_text(
        f'WIDTH {NR}\nFILE_LENGTH {NAZ}\nX_FIRST {x_first}\nY_FIRST 15.155\n'
        'X_STEP 0.000277777776\nY_STEP -0.00027777778\nX_UNIT degrees\nY_UNIT degrees\n'
    )
    (path / 'parameters').write_text(f'{NR} {NAZ} {len(DATES)} {len(DATES) - 1}\n')
    names = [f'../S1A_IW_RAW__0SDV_{d:%Y%m%d}{acq_time}_{d:%Y%m%d}{acq_time}_000000_000000_0000.geo' for d in DATES]
    with open(path / 'sbas_list', 'w') as f:
        f.writelines(f'{names[i]} {names[i + 1]} 12.0 {10.0 * (-1) ** i}\n' for i in range(len(DATES) - 1))

    rng = np.random.default_rng(0)
    (rng.uniform(1000, 3000, (NAZ, NR))).astype(np.int16).tofile(path / 'dem')
    amp = rng.uniform(1, 2, (NAZ, NR)).astype(np.float32)
    # stack rate (rad/day) for a LOS velocity growing with column: VELOCITY * (1 + col / NR)
    stack_rate = -4 * np.pi / postprocess.WAVELENGTH_S1 * VELOCITY * (1 + np.arange(NR) / NR) / 365.25
    np.stack([amp, np.broadcast_to(stack_rate, amp.shape).astype(np.float32)], axis=1).tofile(path / 'stackmht')
    np.full((NAZ, NR), len(DATES) - 1, np.int32).tofile(path / 'npts')
    for d1, d2 in itertools.pairwise(DATES):
        np.stack([amp, np.full_like(amp, 0.7)], axis=1).tofile(path / f'{d1:%Y%m%d}_{d2:%Y%m%d}.cc')

    t = np.array([(d - DATES[0]).days for d in DATES]) / 365.25
    phase = -4 * np.pi / postprocess.WAVELENGTH_S1 * VELOCITY * t  # d = -phi * wavelength / (4 pi)
    disp = np.zeros((len(DATES) - 1, NAZ, 2, NR), np.float32)
    disp[:, :, 0, :] = amp
    disp[:, :, 1, :] = phase[1:, None, None]
    disp.tofile(path / 'displacement')
    rate = np.diff(phase) / 12.0
    np.broadcast_to(rate[:, None, None], (len(DATES) - 1, NAZ, NR)).astype(np.float32).tofile(path / 'velocity')
    (path / 'ref_locs').write_text(ref_locs)
    return path


def test_load_product_and_fit(tmp_path):
    prod = postprocess.load_product(write_product(tmp_path / 'prod'))
    assert prod.shape == (NAZ, NR)
    assert prod.dates == DATES
    assert prod.displacement.shape == (len(DATES), NAZ, NR)
    assert np.all(prod.displacement[0] == 0)
    np.testing.assert_allclose(prod.coherence, 0.7, rtol=1e-6)

    velocity, velocity_std = postprocess.linear_fit(prod.dates, prod.displacement)
    np.testing.assert_allclose(velocity, VELOCITY, rtol=1e-4)
    np.testing.assert_allclose(prod.stack_velocity, VELOCITY * (1 + np.arange(NR) / NR) * np.ones((NAZ, 1)), rtol=1e-5)
    assert np.all(velocity_std < 1e-5)


def test_load_product_epoch_mismatch(tmp_path):
    path = write_product(tmp_path / 'prod')
    (path / 'parameters').write_text(f'{NR} {NAZ} {len(DATES) + 1} {len(DATES) - 1}\n')
    with pytest.raises(ValueError, match='epochs in sbas_list'):
        postprocess.load_product(path)


def test_load_product_epoch_without_pairs(tmp_path):
    # drop the last pair: the last epoch is in no pair, so only geolist knows its date
    path = write_product(tmp_path / 'prod')
    lines = (path / 'sbas_list').read_text().splitlines()
    (path / 'sbas_list').write_text('\n'.join(lines[:-1]) + '\n')
    with pytest.raises(ValueError, match='put the job geolist'):
        postprocess.load_product(path)

    names = sorted({g for line in lines for g in line.split()[:2]})
    (path / 'geolist').write_text('\n'.join(names) + '\n')
    prod = postprocess.load_product(path)
    assert prod.dates == DATES
    assert prod.n_components == 2  # the isolated epoch is its own component
    assert len(prod.bperp) == len(DATES) and np.isfinite(prod.bperp[DATES[-1]])


def test_orbit_direction(tmp_path):
    # Tacana a136 (~00:15 UTC, lon -92) -> ~18:05 local: ascending
    assert postprocess.orbit_direction(postprocess.load_product(write_product(tmp_path / 'a'))) == 'ASCENDING'
    # ~12:05 UTC at lon -92 -> ~06:00 local: descending
    prod = postprocess.load_product(write_product(tmp_path / 'd', acq_time='T120500'))
    assert postprocess.orbit_direction(prod) == 'DESCENDING'


def test_export_hdf5(tmp_path):
    prod = postprocess.load_product(write_product(tmp_path / 'prod', ref_locs='5 4\n7 6\n9 8\n'))
    files = postprocess.export_hdf5(prod, tmp_path / 'hdf5', 'ASCENDING')
    assert [f.name for f in files] == [
        'timeseries.h5',
        'velocity.h5',
        'avgPhaseVelocity.h5',
        'avgSpatialCoh.h5',
        'avgAmplitude.h5',
        'geometryGeo.h5',
        'numInvIfgram.h5',
        'coherenceSpatialAvg.txt',
    ]
    pairs = [line.split() for line in (tmp_path / 'hdf5' / 'coherenceSpatialAvg.txt').read_text().splitlines()[1:]]
    assert len(pairs) == len(DATES) - 1 and pairs[0][0] == '20250101_20250113' and float(pairs[0][1]) == 0.7

    with h5py.File(tmp_path / 'hdf5' / 'timeseries.h5') as f:
        assert f['timeseries'].shape == (len(DATES), NAZ, NR)
        assert [d.decode() for d in f['date'][:]] == [f'{d:%Y%m%d}' for d in DATES]
        assert f['bperp'].shape == (len(DATES),)
        attrs = dict(f.attrs)
        assert attrs['FILE_TYPE'] == 'timeseries' and attrs['UNIT'] == 'm' and attrs['REF_DATE'] == '20250101'
        # middle ref_locs (7, 6) in 1-based (col, row) -> effective 0-based (row=6, col=6)
        assert (attrs['REF_Y'], attrs['REF_X']) == ('6', '6')
        assert np.all(f['timeseries'][:, 6, 6] == 0)
    with h5py.File(tmp_path / 'hdf5' / 'velocity.h5') as f:
        assert f.attrs['FILE_TYPE'] == 'velocity' and f.attrs['UNIT'] == 'm/year'
        np.testing.assert_allclose(f['velocity'][:], 0, atol=1e-6)  # uniform motion minus reference pixel
    with h5py.File(tmp_path / 'hdf5' / 'geometryGeo.h5') as f:
        assert f.attrs['FILE_TYPE'] == 'geometry' and f['height'].shape == (NAZ, NR)
    with h5py.File(tmp_path / 'hdf5' / 'avgPhaseVelocity.h5') as f:
        expected = VELOCITY * (np.arange(NR) - 6) / NR  # relative to the reference column 6
        np.testing.assert_allclose(f['velocity'][0], expected, atol=1e-6)
    with h5py.File(tmp_path / 'hdf5' / 'numInvIfgram.h5') as f:
        assert f.attrs['FILE_TYPE'] == 'mask' and np.all(f['mask'][:] == len(DATES) - 1)


def test_reference_pixel_without_ref_locs(tmp_path):
    prod = postprocess.load_product(write_product(tmp_path / 'prod'))
    prod.coherence[12, 15] = 0.99
    row, col, how = postprocess.reference_pixel(prod)
    assert 5 <= row < NAZ - 5 and 5 <= col < NR - 5
    assert abs(row - 12) <= 2 and abs(col - 15) <= 2
    assert 'ref_locs empty' in how


def test_make_browse(tmp_path):
    prod = postprocess.load_product(write_product(tmp_path / 'prod'))
    postprocess.export_hdf5(prod, tmp_path / 'hdf5', 'ASCENDING')
    files = postprocess.make_browse(tmp_path / 'hdf5', tmp_path / 'browse')
    names = {f.name for f in files}
    for name in (
        'velocity',
        'velocityStd',
        'avgPhaseVelocity',
        'avgCoherence',
        'avgAmplitude',
        'dispCumulative',
        'elevation',
    ):
        assert f'{name}.png' in names and f'{name}.kmz' in names
    assert {'network.png', 'dispPixel.png'} <= names
    assert all(f.exists() for f in files)
    with zipfile.ZipFile(tmp_path / 'browse' / 'velocity.kmz') as z:
        assert set(z.namelist()) == {'doc.kml', 'velocity_raster.png', 'velocity_legend.png'}
        assert '<west>-92.137</west>' in z.read('doc.kml').decode()


def test_postprocess(tmp_path):
    out = postprocess.postprocess(write_product(tmp_path / 'prod'))
    assert (tmp_path / 'prod' / 'browse' / 'velocity.png').exists()
    assert len(out['hdf5']) == 8


def write_orbtiming(path, lat, lon, inc=35.0, az=102.0, rng=800e3, speed=7000.0):
    """Straight-line orbit seen from (lat, lon, 0) with LOS incidence `inc`, azimuth `az` (MintPy conventions)."""
    la, lo = np.radians(lat), np.radians(lon)
    a, e2 = 6378137.0, 0.0066943799901499996
    n = a / np.sqrt(1 - e2 * np.sin(la) ** 2)
    ground = np.array([n * np.cos(la) * np.cos(lo), n * np.cos(la) * np.sin(lo), n * (1 - e2) * np.sin(la)])
    east = np.array([-np.sin(lo), np.cos(lo), 0])
    north = np.array([-np.sin(la) * np.cos(lo), -np.sin(la) * np.sin(lo), np.cos(la)])
    up = np.array([np.cos(la) * np.cos(lo), np.cos(la) * np.sin(lo), np.sin(la)])
    i, z = np.radians(inc), np.radians(az)
    los = np.sin(i) * (-np.sin(z) * east + np.cos(z) * north) + np.cos(i) * up
    heading = np.radians(az - 90)  # along-track azimuth, anti-clockwise from north
    vel = speed * (-np.sin(heading) * east + np.cos(heading) * north)
    with open(path, 'w') as f:
        f.write('0\n0\n0\n11\n')
        for t in range(-50, 51, 10):
            x = ground + rng * los + vel * t
            f.write(' '.join(str(v) for v in [t + 1000.0, *x, *vel, 0, 0, 0]) + '\n')


def test_los_geometry(tmp_path):
    path = write_product(tmp_path / 'prod')
    (path / 'dem').write_bytes(np.zeros((NAZ, NR), np.int16).tobytes())
    prod = postprocess.load_product(path)
    r = prod.rsc
    lat = r['Y_FIRST'] + (NAZ // 2 + 0.5) * r['Y_STEP']
    lon = r['X_FIRST'] + (NR // 2 + 0.5) * r['X_STEP']
    write_orbtiming(path / 'orbtiming', lat, lon)
    geom = postprocess.los_geometry(prod, path / 'orbtiming')
    assert geom['incidenceAngle'][NAZ // 2, NR // 2] == pytest.approx(35.0, abs=1e-3)
    assert geom['azimuthAngle'][NAZ // 2, NR // 2] == pytest.approx(102.0, abs=1e-3)
    assert geom['slantRangeDistance'][NAZ // 2, NR // 2] == pytest.approx(800e3, abs=1.0)
    # right-looking, ascending: incidence grows to the east (far range)
    assert geom['incidenceAngle'][NAZ // 2, -1] > geom['incidenceAngle'][NAZ // 2, 0]

    postprocess.export_hdf5(prod, tmp_path / 'hdf5', 'ASCENDING')
    with h5py.File(tmp_path / 'hdf5' / 'geometryGeo.h5') as f:
        assert {'height', 'incidenceAngle', 'azimuthAngle', 'slantRangeDistance'} <= set(f.keys())
        assert float(f.attrs['CENTER_INCIDENCE_ANGLE']) == pytest.approx(35.0, abs=0.1)

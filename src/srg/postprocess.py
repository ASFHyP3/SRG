"""MintPy-style HDF5, then browse images (PNG + KMZ), from an SRG SBAS product directory.

1. export_hdf5: reads the SRG binaries in the product (dem.rsc, parameters, dem, sbas_list, geolist, stackmht,
   ref_locs, *.cc, displacement, npts, orbtiming) and writes <out>/hdf5/*.h5
2. make_browse: reads only <out>/hdf5/ and writes <out>/browse/*.png, *.kmz

Runs at the end of the HyP3 job and on any downloaded, unzipped product:

    python postprocess.py sbas --out-dir .          # writes ./hdf5/ and ./browse/
"""

import argparse
import datetime as dt
import logging
import re
import zipfile
from collections import deque
from dataclasses import dataclass
from pathlib import Path

import h5py
import matplotlib


matplotlib.use('Agg')
import matplotlib.colors as mcolors
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import cm
from matplotlib.patches import Circle
from matplotlib.ticker import AutoMinorLocator, MaxNLocator
from mpl_toolkits.axes_grid1 import make_axes_locatable


log = logging.getLogger(__name__)

# Sentinel-1 C-band, c / 5.405 GHz; same value as proc_home sentinel/burstparams.f90
WAVELENGTH_S1 = 0.05546576


# ================================================================ 1. SRG binaries -> MintPy HDF5
@dataclass
class SrgProduct:
    """SBAS product read into memory; displacement in m, first epoch = 0."""

    product_dir: Path
    rsc: dict
    dates: list
    edges: list
    bperp: dict
    n_components: int
    dem: np.ndarray
    amplitude: np.ndarray
    stack_velocity: np.ndarray
    coherence: np.ndarray
    pair_coherence: dict
    displacement: np.ndarray
    ref_rc: np.ndarray
    wavelength: float

    @property
    def shape(self) -> tuple[int, int]:
        return self.rsc['FILE_LENGTH'], self.rsc['WIDTH']


def load_rsc(path: Path) -> dict:
    """Read a ROI_PAC-style .rsc key/value file."""
    rsc: dict = {}
    for line in Path(path).read_text().splitlines():
        parts = line.split()
        if len(parts) != 2:
            continue
        try:
            rsc[parts[0]] = float(parts[1]) if '.' in parts[1] or 'e' in parts[1].lower() else int(parts[1])
        except ValueError:
            rsc[parts[0]] = parts[1]
    return rsc


def read_bil(path: Path, nr: int, naz: int) -> tuple[np.ndarray, np.ndarray]:
    """Read a 2-band-per-line float32 file (.cc, stackmht): returns (band0, band1)."""
    data = np.fromfile(path, dtype=np.float32).reshape(naz, 2, nr)
    return data[:, 0, :], data[:, 1, :]


def parse_date(fname: str) -> dt.datetime:
    match = re.search(r'(\d{8})T', fname)
    if match is None:
        raise ValueError(f'no YYYYMMDDT date in {fname}')
    return dt.datetime.strptime(match.group(1), '%Y%m%d')


def epoch_bperp(edges: list, epochs: list) -> tuple[dict, int]:
    """Per-epoch Bperp by propagating pair Bperp through the network (each component starts at 0), demeaned."""
    adj: dict = {}
    for d1, d2, _, bp in edges:
        adj.setdefault(d1, []).append((d2, bp))
        adj.setdefault(d2, []).append((d1, -bp))
    bperp, n_comp = {}, 0
    for seed in epochs:
        if seed in bperp:
            continue
        n_comp += 1
        bperp[seed] = 0.0
        queue = deque([seed])
        while queue:
            cur = queue.popleft()
            for nxt, bp in adj.get(cur, []):
                if nxt not in bperp:
                    bperp[nxt] = bperp[cur] + bp
                    queue.append(nxt)
    mean_bp = np.mean(list(bperp.values()))
    return {k: v - mean_bp for k, v in bperp.items()}, n_comp


def load_product(product_dir: Path, wavelength: float = WAVELENGTH_S1) -> SrgProduct:
    """Read an SRG SBAS product directory.

    Args:
        product_dir: unzipped product (or the sbas work dir)
        wavelength: radar wavelength (m) to convert phase to LOS displacement, d = -phi * wavelength / (4 pi)

    Returns:
        the product in memory
    """
    product_dir = Path(product_dir)
    rsc = load_rsc(product_dir / 'dem.rsc')
    nr, naz = rsc['WIDTH'], rsc['FILE_LENGTH']
    _, _, nslc, _ = map(int, (product_dir / 'parameters').read_text().split())

    pairs = [line.split() for line in (product_dir / 'sbas_list').read_text().splitlines() if line.strip()]
    edges = [(parse_date(p[0]), parse_date(p[1]), float(p[2]), float(p[3])) for p in pairs]
    # geolist lists every SLC given to sbas, including epochs in no pair; sbas_list misses those
    geolist = product_dir / 'geolist'
    if geolist.exists():
        dates = sorted(parse_date(line) for line in geolist.read_text().splitlines() if line.strip())
    else:
        dates = sorted({d for e in edges for d in e[:2]})
    if len(dates) != nslc:
        raise ValueError(
            f'{len(dates)} epochs in {"geolist" if geolist.exists() else "sbas_list"} but parameters says {nslc} SLCs;'
            ' epochs in no pair are missing from sbas_list, put the job geolist in the product directory'
        )
    bperp, n_comp = epoch_bperp(edges, dates)

    amplitude, phase_rate = read_bil(product_dir / 'stackmht', nr, naz)  # phase_rate: sum(phi) / sum(dt), rad/day

    cc_files = sorted(product_dir.glob('*.cc'))
    cohsum, pair_coherence = np.zeros((naz, nr), dtype=np.float64), {}
    for f in cc_files:
        _, c = read_bil(f, nr, naz)
        cohsum += c
        pair_coherence[f.stem] = float(np.nanmean(c))
    coherence = (cohsum / max(len(cc_files), 1)).astype(np.float32)

    to_m = -wavelength / (4 * np.pi)
    stack_velocity = (phase_rate * to_m * 365.25).astype(np.float32)
    disp = np.fromfile(product_dir / 'displacement', dtype=np.float32).reshape(nslc - 1, naz, 2, nr)[:, :, 1, :]
    displacement = np.concatenate([np.zeros((1, naz, nr), np.float32), disp * to_m]).astype(np.float32)

    ref_file = product_dir / 'ref_locs'
    if ref_file.exists() and ref_file.stat().st_size > 0:
        ref_rc = np.loadtxt(ref_file, dtype=int).reshape(-1, 2)
    else:
        ref_rc = np.empty((0, 2), dtype=int)

    dem = np.fromfile(product_dir / 'dem', dtype=np.int16).reshape(naz, nr).astype(np.float32)
    return SrgProduct(
        product_dir,
        rsc,
        dates,
        edges,
        bperp,
        n_comp,
        dem,
        amplitude,
        stack_velocity,
        coherence,
        pair_coherence,
        displacement,
        ref_rc,
        wavelength,
    )


def linear_fit(dates: list, displacement: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """Per-pixel OLS line to a displacement time series: velocity and its standard error (m/yr)."""
    t = np.array([(d - dates[0]).days for d in dates], dtype=np.float64) / 365.25
    tc = t - t.mean()
    disp = displacement.astype(np.float64)
    slope = np.tensordot(tc, disp - disp.mean(axis=0), axes=1) / np.sum(tc**2)
    resid = disp - disp.mean(axis=0) - slope[None] * tc[:, None, None]
    std = np.sqrt(np.sum(resid**2, axis=0) / max(len(t) - 2, 1) / np.sum(tc**2))
    return slope.astype(np.float32), std.astype(np.float32)


def orbit_direction(prod: SrgProduct) -> str:
    """ASCENDING/DESCENDING from local solar time; Sentinel-1 is dawn-dusk, ascending node ~18:00 LST."""
    first = (prod.product_dir / 'sbas_list').read_text().splitlines()[0]
    match = re.search(r'(\d{8}T\d{6})', first)
    if match is None:
        raise ValueError(f'no YYYYMMDDTHHMMSS time in sbas_list line: {first}')
    utc = dt.datetime.strptime(match.group(1), '%Y%m%dT%H%M%S')
    lon = prod.rsc['X_FIRST'] + prod.rsc['WIDTH'] * prod.rsc['X_STEP'] / 2
    lst = (utc.hour + utc.minute / 60 + lon / 15) % 24
    return 'ASCENDING' if 12 <= lst < 24 else 'DESCENDING'


def mintpy_metadata(prod: SrgProduct, orbit: str) -> dict:
    """Root attributes shared by all MintPy files; only values known from the product."""
    r = prod.rsc
    return {
        'WIDTH': r['WIDTH'],
        'LENGTH': r['FILE_LENGTH'],
        'X_FIRST': r['X_FIRST'],
        'X_STEP': r['X_STEP'],
        'X_UNIT': 'degrees',
        'Y_FIRST': r['Y_FIRST'],
        'Y_STEP': r['Y_STEP'],
        'Y_UNIT': 'degrees',
        'EPSG': 4326,
        'DATA_TYPE': 'float32',
        'PROCESSOR': 'srg',
        'PLATFORM': 'Sentinel-1',
        'WAVELENGTH': prod.wavelength,
        'ANTENNA_SIDE': -1,
        'ORBIT_DIRECTION': orbit,
        'START_DATE': f'{prod.dates[0]:%Y%m%d}',
        'END_DATE': f'{prod.dates[-1]:%Y%m%d}',
        'SRG_N_REF_LOCS': len(prod.ref_rc),
        'SRG_PRODUCT': prod.product_dir.name,
    }


def reference_pixel(prod: SrgProduct) -> tuple[int, int, str]:
    """MintPy reference pixel (row, col), 0-based.

    With ref_locs: the ref_locs pixel nearest their centroid. sbas.f90 reads phase(nr+x, y, i) from a
    phase(nr, naz, :) array, i.e. pixel (x, y+1) in 1-based (col, row), so the effective 0-based pixel
    is (row=y, col=x-1). Without ref_locs (SRG did no re-referencing): highest 5x5 mean coherence, off the edges.
    """
    naz, nr = prod.shape
    if len(prod.ref_rc):
        rc = np.column_stack([np.clip(prod.ref_rc[:, 1], 0, naz - 1), np.clip(prod.ref_rc[:, 0] - 1, 0, nr - 1)])
        k = np.argmin(np.sum((rc - rc.mean(axis=0)) ** 2, axis=1))
        return int(rc[k, 0]), int(rc[k, 1]), f'ref_locs pixel nearest centroid of {len(rc)}'
    win, edge = 5, 5
    smooth = np.full(prod.shape, -np.inf)
    smooth[win // 2 : naz - win // 2, win // 2 : nr - win // 2] = np.lib.stride_tricks.sliding_window_view(
        prod.coherence, (win, win)
    ).mean(axis=(2, 3))
    smooth[:edge], smooth[-edge:], smooth[:, :edge], smooth[:, -edge:] = -np.inf, -np.inf, -np.inf, -np.inf
    row, col = np.unravel_index(np.argmax(smooth), prod.shape)
    return int(row), int(col), f'max {win}x{win} mean coherence, {edge}-px border excluded (ref_locs empty)'


def los_geometry(prod: SrgProduct, orbtiming: Path) -> dict[str, np.ndarray]:
    """Per-pixel LOS geometry from an .orbtiming state-vector file and the DEM, in MintPy conventions.

    LOS points from the ground to the satellite. incidenceAngle is from vertical; azimuthAngle is from north,
    anti-clockwise positive (S1 ascending ~ +102 deg); slantRangeDistance in m.
    """
    d = np.loadtxt(orbtiming, skiprows=4)
    t0 = d[:, 0].mean()
    coef = [np.polyfit(d[:, 0] - t0, d[:, j], 5) for j in range(1, 7)]  # x, y, z, vx, vy, vz

    def state(t):
        return (
            np.stack([np.polyval(coef[j], t) for j in range(3)], -1),
            np.stack([np.polyval(coef[j], t) for j in range(3, 6)], -1),
        )

    r = prod.rsc
    lon = np.radians(r['X_FIRST'] + (np.arange(r['WIDTH']) + 0.5) * r['X_STEP'])
    lat = np.radians(r['Y_FIRST'] + (np.arange(r['FILE_LENGTH']) + 0.5) * r['Y_STEP'])
    lon, lat = np.meshgrid(lon, lat)
    a, e2 = 6378137.0, 0.0066943799901499996
    n = a / np.sqrt(1 - e2 * np.sin(lat) ** 2)
    h = prod.dem.astype(np.float64)
    ground = np.stack(
        [(n + h) * np.cos(lat) * np.cos(lon), (n + h) * np.cos(lat) * np.sin(lon), (n * (1 - e2) + h) * np.sin(lat)], -1
    )

    t = np.zeros(prod.shape)
    for _ in range(10):  # Newton on zero Doppler: (ground - x) . v = 0
        x, v = state(t)
        t = t + np.einsum('...j,...j', ground - x, v) / np.einsum('...j,...j', v, v)
    x, _ = state(t)
    los = x - ground
    rng = np.linalg.norm(los, axis=-1)
    los = los / rng[..., None]
    east = np.stack([-np.sin(lon), np.cos(lon), np.zeros_like(lon)], -1)
    north = np.stack([-np.sin(lat) * np.cos(lon), -np.sin(lat) * np.sin(lon), np.cos(lat)], -1)
    up = np.stack([np.cos(lat) * np.cos(lon), np.cos(lat) * np.sin(lon), np.sin(lat)], -1)
    le, ln, lu = (np.einsum('...j,...j', los, b) for b in (east, north, up))
    return {
        'incidenceAngle': np.degrees(np.arccos(lu)).astype(np.float32),
        'azimuthAngle': np.degrees(np.arctan2(-le, ln)).astype(np.float32),
        'slantRangeDistance': rng.astype(np.float32),
    }


def write_h5(out_file: Path, datasets: dict, meta: dict) -> Path:
    """Write datasets + stringified root attributes (MintPy layout)."""
    with h5py.File(out_file, 'w') as f:
        for key, val in datasets.items():
            f.create_dataset(key, data=val, chunks=True if np.ndim(val) == 3 else None)
        for key, val in meta.items():
            f.attrs[key] = str(val)
    return out_file


def export_hdf5(prod: SrgProduct, out_dir: Path, orbit: str) -> list[Path]:
    """Write the MintPy-style HDF5 files (and coherenceSpatialAvg.txt) for a loaded product.

    Args:
        prod: loaded product
        out_dir: output directory
        orbit: 'ASCENDING' or 'DESCENDING'

    Returns:
        written files
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    meta = mintpy_metadata(prod, orbit)
    row, col, how = reference_pixel(prod)
    r = prod.rsc
    meta.update(
        {
            'REF_Y': row,
            'REF_X': col,
            'REF_LAT': r['Y_FIRST'] + (row + 0.5) * r['Y_STEP'],
            'REF_LON': r['X_FIRST'] + (col + 0.5) * r['X_STEP'],
            'SRG_REF_METHOD': how,
        }
    )
    ts = prod.displacement - prod.displacement[:, row : row + 1, col : col + 1]
    dates = np.array([f'{d:%Y%m%d}' for d in prod.dates], dtype='S8')
    date12 = f'{dates[0].decode()}_{dates[-1].decode()}'
    bperp = np.array([prod.bperp[d] for d in prod.dates], dtype=np.float32)
    velocity, velocity_std = linear_fit(prod.dates, ts)
    npts = np.fromfile(prod.product_dir / 'npts', dtype=np.int32).reshape(prod.shape).astype(np.float32)

    # incidence/azimuth angle and slant range need the orbit; products without 'orbtiming' get height only
    geometry, geometry_meta = {'height': prod.dem}, {}
    orbtiming = prod.product_dir / 'orbtiming'
    if orbtiming.exists():
        geometry.update(los_geometry(prod, orbtiming))
        geometry_meta['CENTER_INCIDENCE_ANGLE'] = float(np.nanmedian(geometry['incidenceAngle']))
    else:
        log.info('no orbtiming in the product: geometryGeo.h5 has height only')
    if prod.n_components > 1:
        log.warning(f'pair network has {prod.n_components} disconnected components')

    files = [
        write_h5(
            out_dir / 'timeseries.h5',
            {'timeseries': ts, 'date': dates, 'bperp': bperp},
            {**meta, 'FILE_TYPE': 'timeseries', 'UNIT': 'm', 'REF_DATE': dates[0].decode()},
        ),
        write_h5(
            out_dir / 'velocity.h5',
            {'velocity': velocity, 'velocityStd': velocity_std},
            {**meta, 'FILE_TYPE': 'velocity', 'UNIT': 'm/year', 'REF_DATE': dates[0].decode(), 'DATE12': date12},
        ),
        # interferogram stacking (sum phase / sum time, from sbas stackmht), referenced like the time series
        write_h5(
            out_dir / 'avgPhaseVelocity.h5',
            {'velocity': prod.stack_velocity - prod.stack_velocity[row, col]},
            {**meta, 'FILE_TYPE': 'velocity', 'UNIT': 'm/year', 'DATE12': date12},
        ),
        write_h5(
            out_dir / 'avgSpatialCoh.h5', {'coherence': prod.coherence}, {**meta, 'FILE_TYPE': 'coherence', 'UNIT': '1'}
        ),
        # mean SAR amplitude (sbas stackmht band 1); not a standard MintPy file, used for browse shading
        write_h5(
            out_dir / 'avgAmplitude.h5', {'amplitude': prod.amplitude}, {**meta, 'FILE_TYPE': 'amplitude', 'UNIT': '1'}
        ),
        write_h5(out_dir / 'geometryGeo.h5', geometry, {**meta, **geometry_meta, 'FILE_TYPE': 'geometry', 'UNIT': 'm'}),
        # number of interferograms with data at each pixel (sbas npts)
        write_h5(out_dir / 'numInvIfgram.h5', {'mask': npts}, {**meta, 'FILE_TYPE': 'mask', 'UNIT': '1'}),
    ]

    # pair list + mean coherence, MintPy coherenceSpatialAvg.txt format
    lines = ['#\tDATE12\t\tMean\tBtemp/days\tBperp/m\t\tNum']
    for i, (d1, d2, tbase, pbase) in enumerate(prod.edges):
        key = f'{d1:%Y%m%d}_{d2:%Y%m%d}'
        lines.append(f'{key}\t{prod.pair_coherence.get(key, np.nan):.4f}\t{tbase:8.0f}\t{pbase:8.1f}\t{i}')
    (out_dir / 'coherenceSpatialAvg.txt').write_text('\n'.join(lines) + '\n')
    return [*files, out_dir / 'coherenceSpatialAvg.txt']


# ================================================================ 2. MintPy HDF5 -> browse images
def read_h5(path: Path, dataset: str, index=()) -> np.ndarray:
    """Read one dataset (optionally a slice) from a MintPy HDF5 file."""
    with h5py.File(path) as f:
        return f[dataset][index]


def hillshade(dem: np.ndarray, vert_exag: float = 1, azdeg: float = 315, altdeg: float = 45) -> np.ndarray:
    """DEM -> grayscale illumination in [0,1]."""
    return mcolors.LightSource(azdeg=azdeg, altdeg=altdeg).hillshade(dem, vert_exag=vert_exag)


def normalize_amp(amp: np.ndarray, exponent: float = 0.3, denom_fac: float = 2.0) -> np.ndarray:
    """SAR amplitude -> [0,1] shade: power-law compress + mean-normalize."""
    bright = np.power(np.abs(amp), exponent)
    return np.clip(bright / (np.mean(bright[bright > 0]) * denom_fac), 0, 1)


def blend_multiply(data, cmap, vmin, vmax, shade) -> np.ndarray:
    """Colormap `data` then multiply by `shade` in [0,1] -> (naz, nr, 3) RGB."""
    rgb = plt.get_cmap(cmap)(plt.Normalize(vmin, vmax)(data))[..., :3]
    return rgb * shade[..., None]


def add_colorbar(im, ax, label='m', extend='both', size='4%', pad=0.1, cticks=None):
    cax = make_axes_locatable(ax).append_axes('right', size=size, pad=pad)
    cb = plt.colorbar(im, cax=cax, label=label, extend=extend)
    if cticks is not None:
        cb.locator = MaxNLocator(nbins=cticks)
        cb.update_ticks()
    return cb


def image_show(data, ax, extent, cmap='viridis', vlim=(None, None), clabel='', shade=None, anntext=None, cticks=None):
    """shade=None: plain imshow; shade=array in [0,1]: blend_multiply."""
    vmin = vlim[0] if vlim[0] is not None else np.nanmin(data)
    vmax = vlim[1] if vlim[1] is not None else np.nanmax(data)
    if shade is None:
        im = ax.imshow(data, extent=extent, cmap=cmap, vmin=vmin, vmax=vmax, interpolation='none')
    else:
        ax.imshow(blend_multiply(data, cmap, vmin, vmax, shade), extent=extent, interpolation='none')
        im = cm.ScalarMappable(norm=plt.Normalize(vmin, vmax), cmap=cmap)
    extend = ('min' if np.nanmin(data) < vmin else '') + ('max' if np.nanmax(data) > vmax else '')
    extend = 'both' if extend == 'minmax' else (extend or 'neither')
    add_colorbar(im, ax, label=clabel, extend=extend, cticks=cticks)
    ax.xaxis.set_major_locator(MaxNLocator(nbins=4))
    ax.yaxis.set_major_locator(MaxNLocator(nbins=4))
    if anntext is not None:
        ax.text(
            0.025,
            0.05,
            anntext,
            fontsize=11,
            transform=ax.transAxes,
            bbox={'boxstyle': 'round,pad=0.3', 'fc': 'whitesmoke', 'ec': 'k', 'lw': 0.8, 'alpha': 0.85},
        )
    return ax, im


def get_vlim(data, maskin=None, q=(0.002, 0.998)) -> tuple[float, float]:
    """Symmetric +/- limit from the quantile range, optionally restricted by maskin."""
    d = data[maskin] if maskin is not None and maskin.any() else data
    v = np.nanmax(np.abs(np.nanquantile(d, q)))
    return (-v, v)


def write_kmz(data, extent, cmap, vlim, label, out_path: Path, shade=None) -> Path:
    """Georeferenced KMZ: GroundOverlay raster + lower-left legend ScreenOverlay."""
    vmin, vmax = vlim
    name = out_path.stem
    img_name, leg_name = f'{name}_raster.png', f'{name}_legend.png'
    img_path, leg_path = out_path.with_name(img_name), out_path.with_name(leg_name)

    if shade is None:
        plt.imsave(img_path, data, cmap=cmap, vmin=vmin, vmax=vmax)
    else:
        plt.imsave(img_path, np.clip(blend_multiply(data, cmap, vmin, vmax, shade), 0, 1))

    fig = plt.figure(figsize=(0.8, 1.2))
    cax = fig.add_axes((0.3, 0.05, 0.18, 0.9))
    cb = plt.colorbar(cm.ScalarMappable(norm=plt.Normalize(vmin, vmax), cmap=cmap), cax=cax)
    cb.set_label(label, color='white', fontsize=8)
    cb.ax.tick_params(labelsize=7, color='white')
    plt.setp(plt.getp(cb.ax.axes, 'yticklabels'), color='white')
    fig.savefig(leg_path, dpi=150, transparent=True)
    plt.close(fig)

    west, east, south, north = extent
    kml = f"""<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
<Document>
  <name>{name}</name>
  <GroundOverlay>
    <name>{name}</name>
    <Icon><href>{img_name}</href></Icon>
    <LatLonBox><north>{north}</north><south>{south}</south><east>{east}</east><west>{west}</west></LatLonBox>
  </GroundOverlay>
  <ScreenOverlay>
    <name>legend</name>
    <Icon><href>{leg_name}</href></Icon>
    <overlayXY x="0" y="0" xunits="fraction" yunits="fraction"/>
    <screenXY x="0.02" y="0.02" xunits="fraction" yunits="fraction"/>
    <size x="0" y="0" xunits="fraction" yunits="fraction"/>
  </ScreenOverlay>
</Document>
</kml>"""
    with zipfile.ZipFile(out_path, 'w', zipfile.ZIP_DEFLATED) as z:
        z.writestr('doc.kml', kml)
        z.write(img_path, img_name)
        z.write(leg_path, leg_name)
    img_path.unlink()
    leg_path.unlink()
    return out_path


def plot_network(dates, bperp, pairs, coh_thresh: float, out_path: Path) -> None:
    """Pair network: epochs at their Bperp, pairs colored by mean coherence (from coherenceSpatialAvg.txt)."""
    n_bins = 256
    norm = plt.Normalize(0.2, 1.0)
    n_low = int(n_bins * norm(coh_thresh))
    colors = np.vstack(
        (
            plt.get_cmap('Reds_r', n_low)(np.linspace(0.0, 0.7, n_low)),
            plt.get_cmap('Blues', n_bins - n_low)(np.linspace(0.3, 1.0, n_bins - n_low)),
        )
    )
    cmap_net = mcolors.LinearSegmentedColormap.from_list('split_cmap', colors)
    bp = dict(zip(dates, bperp, strict=True))

    fig, ax = plt.subplots(figsize=[8, 4])
    ax.scatter(dates, bperp, ec='k', fc='orange', s=50, zorder=2)
    for d1, d2, coh in pairs:
        ax.plot([d1, d2], [bp[d1], bp[d2]], color=cmap_net(norm(coh)), lw=2, zorder=1, alpha=0.85)
    ax.set_ylabel('Perp Baseline (m)')
    ax.set_title('Interferogram Network')
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    ax.xaxis.set_minor_locator(mdates.MonthLocator())
    ax.yaxis.set_major_locator(MaxNLocator(nbins=8, steps=[1, 2, 2.5, 5, 10]))
    ax.yaxis.set_minor_locator(AutoMinorLocator(2))
    ax.tick_params(axis='both', which='major', direction='in', length=5, width=1.2, top=True, right=True)
    ax.tick_params(axis='both', which='minor', direction='in', length=3, width=1, top=True, right=True)
    add_colorbar(
        cm.ScalarMappable(norm=norm, cmap=cmap_net),
        ax,
        label='Avg Spatial Coherence',
        extend='neither',
        size='2.5%',
        pad=0.15,
    )
    ax.text(
        0.025,
        0.05,
        f'{len(dates)} epochs, {len(pairs)} pairs',
        fontsize=11,
        transform=ax.transAxes,
        bbox={'boxstyle': 'round,pad=0.3', 'fc': 'whitesmoke', 'ec': 'k', 'lw': 0.8, 'alpha': 0.85},
    )
    fig.savefig(out_path, dpi=150, bbox_inches='tight')
    plt.close(fig)


def make_browse(h5_dir: Path, out_dir: Path, coh_thresh: float = 0.5, lalo=None) -> list[Path]:
    """Write browse PNGs, plus a KMZ for every 2-D map, from the MintPy HDF5 files only.

    Args:
        h5_dir: directory written by export_hdf5
        out_dir: output directory
        coh_thresh: mean-coherence threshold for color limits, pixel search and network colors
        lalo: (lat, lon) of the time-series pixel; None = auto

    Returns:
        written files
    """
    h5_dir, out_dir = Path(h5_dir), Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    plt.rcParams.update({'font.size': 13})

    ts_file = h5_dir / 'timeseries.h5'
    with h5py.File(ts_file) as f:
        meta = {k: str(v) for k, v in f.attrs.items()}
        dates = [dt.datetime.strptime(d.decode(), '%Y%m%d') for d in f['date'][:]]
        bperp = f['bperp'][:]
        final = f['timeseries'][-1]
    naz, nr = int(meta['LENGTH']), int(meta['WIDTH'])
    x0, dx, y0, dy = (float(meta[k]) for k in ('X_FIRST', 'X_STEP', 'Y_FIRST', 'Y_STEP'))
    extent = [x0, x0 + nr * dx, y0 + naz * dy, y0]
    ref_y, ref_x = int(meta['REF_Y']), int(meta['REF_X'])
    ref_lonlat = (x0 + (ref_x + 0.5) * dx, y0 + (ref_y + 0.5) * dy)

    velocity = read_h5(h5_dir / 'velocity.h5', 'velocity')
    velocity_std = read_h5(h5_dir / 'velocity.h5', 'velocityStd')
    stack_velocity = read_h5(h5_dir / 'avgPhaseVelocity.h5', 'velocity')
    coherence = read_h5(h5_dir / 'avgSpatialCoh.h5', 'coherence')
    namp = normalize_amp(read_h5(h5_dir / 'avgAmplitude.h5', 'amplitude'))
    height = read_h5(h5_dir / 'geometryGeo.h5', 'height')
    demshade = hillshade(height)
    coh_mask = coherence > coh_thresh

    # name, title, data, cmap, vlim, colorbar label, shade (png), shade (kmz), mark reference pixel
    maps = [
        ('elevation', 'Elevation', height, 'terrain', (None, None), 'meter', demshade, demshade, False),
        ('avgCoherence', 'Mean spatial coherence', coherence, 'viridis', (0, 1), 'coherence', None, None, False),
        ('avgAmplitude', 'Mean amplitude', namp, 'gray', (0, 1), 'normalized (-)', None, None, False),
        (
            'avgPhaseVelocity',
            'LOS velocity (interferogram stacking)',
            stack_velocity,
            'RdYlBu_r',
            get_vlim(stack_velocity, coh_mask),
            'm/year',
            demshade,
            namp,
            True,
        ),
        (
            'velocity',
            'LOS velocity (linear fit to time series)',
            velocity,
            'RdYlBu_r',
            get_vlim(velocity, coh_mask),
            'm/year',
            demshade,
            namp,
            True,
        ),
        (
            'dispCumulative',
            'Final cumulative LOS displacement',
            final,
            'RdYlBu_r',
            get_vlim(final, coh_mask),
            'm',
            demshade,
            namp,
            True,
        ),
        (
            'velocityStd',
            'LOS velocity standard error',
            velocity_std,
            'magma_r',
            (0, float(np.nanquantile(velocity_std, 0.998))),
            'm/year',
            demshade,
            None,
            False,
        ),
    ]

    written = []
    for name, title, data, cmap, vlim, label, shade, kmz_shade, mark_ref in maps:
        vlim = (np.nanmin(data) if vlim[0] is None else vlim[0], np.nanmax(data) if vlim[1] is None else vlim[1])
        fig, ax = plt.subplots()
        ax.set_title(title)
        ann = f'{meta.get("SRG_N_REF_LOCS", "0")} ref_locs' if name == 'avgAmplitude' else None
        image_show(data, ax, extent, cmap=cmap, vlim=vlim, clabel=label, shade=shade, anntext=ann)
        if mark_ref:
            ax.plot(*ref_lonlat, 'ks', ms=5, mfc='none')
        fig.savefig(out_dir / f'{name}.png', dpi=150, bbox_inches='tight')
        plt.close(fig)
        written += [
            out_dir / f'{name}.png',
            write_kmz(data, extent, cmap, vlim, label, out_dir / f'{name}.kmz', shade=kmz_shade),
        ]

    # network: pairs + mean coherence from coherenceSpatialAvg.txt
    rows = [line.split() for line in (h5_dir / 'coherenceSpatialAvg.txt').read_text().splitlines()]
    pairs = [
        (*(dt.datetime.strptime(d, '%Y%m%d') for d in row[0].split('_')), float(row[1]))
        for row in rows
        if row and not row[0].startswith('#')
    ]
    plot_network(dates, bperp, pairs, coh_thresh, out_dir / 'network.png')

    # pixel time series: auto = highest |velocity| coherent pixel near the center
    r = nr // 3
    if lalo is None:
        r0, r1, c0, c1 = max(naz // 2 - r, 0), naz // 2 + r, max(nr // 2 - r, 0), nr // 2 + r
        box = np.where(coh_mask[r0:r1, c0:c1], np.abs(velocity[r0:r1, c0:c1]), -np.inf)
        by, bx = np.unravel_index(np.argmax(box), box.shape)
        iy, ix = r0 + int(by), c0 + int(bx)
    else:
        iy = int(np.clip(round((lalo[0] - y0) / dy), 1, naz - 2))
        ix = int(np.clip(round((lalo[1] - x0) / dx), 1, nr - 2))
    ts = read_h5(ts_file, 'timeseries', np.s_[:, max(iy - 1, 0) : iy + 2, max(ix - 1, 0) : ix + 2]).mean(axis=(1, 2))
    px_lon, px_lat = x0 + ix * dx, y0 + iy * dy

    fig, axs = plt.subplots(ncols=2, figsize=[12, 4], gridspec_kw={'width_ratios': [3.6, 6.4]})
    axs[0].set_title('Final LOS displacement')
    image_show(
        final, axs[0], extent, cmap='RdYlBu_r', vlim=get_vlim(final, coh_mask), clabel='m', shade=demshade, cticks=5
    )
    axs[0].scatter(px_lon, px_lat, marker='^', ec='k', fc='lime', s=50, zorder=3)
    axs[0].plot(*ref_lonlat, 'ks', ms=5, mfc='none')
    if lalo is None:
        center = (x0 + (nr // 2) * dx, y0 + (naz // 2) * dy)
        axs[0].add_patch(Circle(center, r * abs(dx), fill=False, ec='lightgray', lw=1.5, zorder=2))
    ax = axs[1]
    ax.plot(dates, ts, color='grey', lw=2, marker='o', ms=6, mfc='C0', mec='k', alpha=0.8, zorder=2)
    ax.set_xlabel('date')
    ax.set_ylabel('LOS displacement (m)')
    ax.set_title(f'{px_lat:.3f}°N, {px_lon:.3f}°E (velo={velocity[iy, ix]:.3f} m/yr)')
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    ax.xaxis.set_minor_locator(mdates.MonthLocator())
    ax.tick_params(axis='both', which='major', direction='in', length=5, width=1.2, top=True, right=True)
    ax.tick_params(axis='both', which='minor', direction='in', length=3, width=1, top=True, right=True)
    plt.tight_layout()
    fig.savefig(out_dir / 'dispPixel.png', dpi=150, bbox_inches='tight')
    plt.close(fig)
    return [*written, out_dir / 'network.png', out_dir / 'dispPixel.png']


# ================================================================ driver
def postprocess(
    product_dir: Path,
    out_dir: Path | None = None,
    coh_thresh: float = 0.5,
    lalo=None,
    wavelength: float = WAVELENGTH_S1,
) -> dict:
    """MintPy-style HDF5 into <out_dir>/hdf5, then browse images + KMZ from it into <out_dir>/browse.

    Args:
        product_dir: unzipped product directory
        out_dir: output root (default: product_dir)
        coh_thresh: mean-coherence threshold for color limits and pixel search
        lalo: (lat, lon) of the time-series pixel; None = auto
        wavelength: radar wavelength (m)

    Returns:
        {'hdf5': [...], 'browse': [...]}
    """
    product_dir = Path(product_dir)
    out_dir = Path(out_dir) if out_dir else product_dir
    prod = load_product(product_dir, wavelength)
    orbit = orbit_direction(prod)
    log.info(
        f'{product_dir.name}: {len(prod.dates)} epochs, {len(prod.edges)} pairs, {orbit}, {len(prod.ref_rc)} ref_locs'
    )
    h5_files = export_hdf5(prod, out_dir / 'hdf5', orbit)
    del prod
    return {'hdf5': h5_files, 'browse': make_browse(out_dir / 'hdf5', out_dir / 'browse', coh_thresh, lalo)}


def main():
    """Entrypoint: python postprocess.py PRODUCT_DIR [--out-dir DIR] [--coh-thresh 0.5] [--lalo LAT LON]."""
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('product_dir', type=Path, help='unzipped SRG SBAS product (or sbas work) directory')
    parser.add_argument('--out-dir', type=Path, default=None, help='output root (default: product_dir)')
    parser.add_argument('--coh-thresh', type=float, default=0.5, help='mean-coherence threshold (default: 0.5)')
    parser.add_argument(
        '--lalo',
        type=float,
        nargs=2,
        metavar=('LAT', 'LON'),
        default=None,
        help='time-series pixel (default: highest |velocity| coherent pixel near center)',
    )
    parser.add_argument('--wavelength', type=float, default=WAVELENGTH_S1, help='radar wavelength (m)')
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format='%(message)s')
    out = postprocess(args.product_dir, args.out_dir, args.coh_thresh, args.lalo, args.wavelength)
    for kind, files in out.items():
        print(f'{kind}: {len(files)} files in {files[0].parent}')


if __name__ == '__main__':
    main()

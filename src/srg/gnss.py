"""InSAR vs NGL (UNR) GNSS LOS time series at every GNSS site in the scene, after proc_home display/gps_compare.py.

Reads only <h5_dir>/timeseries.h5, velocity.h5 and geometryGeo.h5 (per-pixel incidence/azimuth angles) and
writes <out_dir>/gnss_compare.png (time series), gnss_velocity.png (velocity map) and gnss_rms.txt. GNSS and
InSAR are both differenced against one reference GNSS site, so the InSAR datum (a constant per epoch) and the
GNSS frame (IGS20, plate motion) cancel.
"""

import datetime as dt
import logging
import math
import urllib.request
from pathlib import Path

import h5py
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.lines import Line2D

from srg import postprocess


log = logging.getLogger(__name__)
NGL = 'https://geodesy.unr.edu'


def fetch_text(url: str) -> str:
    with urllib.request.urlopen(url, timeout=60) as r:
        return r.read().decode()


def find_sites(holdings: str, snwe, start: str, end: str) -> list[tuple[str, float, float]]:
    """NGL sites inside snwe whose record overlaps [start, end] (YYYY-MM-DD)."""
    s, n, w, e = snwe
    sites = []
    for line in holdings.splitlines()[1:]:
        p = line.split()
        lat, lon = float(p[1]), (float(p[2]) + 180) % 360 - 180
        if s <= lat <= n and w <= lon <= e and p[7] <= end and p[8] >= start:
            sites.append((p[0], lat, lon))
    return sites


def read_tenv3(text: str) -> tuple[np.ndarray, np.ndarray]:
    """NGL tenv3 text -> (dates datetime64[D], ENU displacement (n, 3) in m).

    Uses the fractional east/north/up columns only, as proc_home get_ngl_gps.py: the integer e0/n0/u0 are reset
    when a station is re-monumented.
    """
    rows = [line.split() for line in text.splitlines()[1:] if line.strip()]
    dates = np.array([dt.datetime.strptime(r[1], '%y%b%d') for r in rows], dtype='datetime64[D]')
    enu = np.array([[float(r[8]), float(r[10]), float(r[12])] for r in rows])
    return dates, enu


def compare(h5_dir: Path, out_dir: Path, ref_site: str | None = None, coh_thresh: float = 0.3) -> list[Path]:
    """Plot InSAR vs GNSS LOS time series relative to ref_site; [] if no geometry, network or sites.

    Args:
        h5_dir: directory written by postprocess.export_hdf5
        out_dir: output directory
        ref_site: reference GNSS site; None = the site nearest the InSAR reference pixel
        coh_thresh: mean-coherence threshold below which the velocity map is masked

    Returns:
        written files
    """
    h5_dir, out_dir = Path(h5_dir), Path(out_dir)
    with h5py.File(h5_dir / 'timeseries.h5') as f:
        meta = {k: str(v) for k, v in f.attrs.items()}
        dates = np.array([dt.datetime.strptime(d.decode(), '%Y%m%d') for d in f['date'][:]], dtype='datetime64[D]')
        ts = f['timeseries'][:]
    with h5py.File(h5_dir / 'geometryGeo.h5') as f:
        if 'incidenceAngle' not in f:
            log.warning('gnss: no incidence/azimuth angles in geometryGeo.h5 (no orbtiming), skipped')
            return []
        inc, az = np.deg2rad(f['incidenceAngle'][:]), np.deg2rad(f['azimuthAngle'][:])
    naz, nr = int(meta['LENGTH']), int(meta['WIDTH'])
    x0, dx, y0, dy = (float(meta[k]) for k in ('X_FIRST', 'X_STEP', 'Y_FIRST', 'Y_STEP'))
    snwe = (y0 + naz * dy, y0, x0, x0 + nr * dx)

    try:
        sites = find_sites(fetch_text(f'{NGL}/NGLStationPages/DataHoldings.txt'), snwe, str(dates[0]), str(dates[-1]))
    except OSError as e:
        log.warning(f'gnss: cannot reach {NGL} ({e}), skipped')
        return []

    # GNSS LOS displacement per site, ground-to-satellite unit vector at the site pixel (MintPy angle convention)
    gnss = {}
    for name, lat, lon in sites:
        iy = int(np.clip(round((lat - y0) / dy), 1, naz - 2))
        ix = int(np.clip(round((lon - x0) / dx), 1, nr - 2))
        try:
            g_dates, enu = read_tenv3(fetch_text(f'{NGL}/gps_timeseries/IGS20/tenv3/IGS20/{name}.tenv3'))
        except OSError as e:
            log.info(f'gnss: {name} skipped ({e})')
            continue
        keep = (g_dates >= dates[0]) & (g_dates <= dates[-1])
        i, a = inc[iy, ix], az[iy, ix]
        unit = np.array([-np.sin(i) * np.sin(a), np.sin(i) * np.cos(a), np.cos(i)])
        sar = np.nanmean(ts[:, iy - 1 : iy + 2, ix - 1 : ix + 2], axis=(1, 2))
        if keep.sum() >= 3 and np.isfinite(sar).any():
            gnss[name] = (lat, lon, g_dates[keep], enu[keep] @ unit, sar)
    if len(gnss) < 2:
        log.warning(f'gnss: {len(gnss)} usable sites in the scene, skipped')
        return []

    if ref_site is None:
        ref_lat, ref_lon = float(meta['REF_LAT']), float(meta['REF_LON'])
        ref_site = min(gnss, key=lambda k: math.hypot(gnss[k][0] - ref_lat, gnss[k][1] - ref_lon))
    _, _, ref_dates, ref_los, ref_sar = gnss[ref_site]

    results = []  # reference site first: flat by construction
    for name, (lat, lon, g_dates, g_los, sar) in sorted(gnss.items(), key=lambda kv: (kv[0] != ref_site, kv[0])):
        common, i1, i2 = np.intersect1d(g_dates, ref_dates, return_indices=True)
        g_rel, sar_rel = g_los[i1] - ref_los[i2], sar - ref_sar
        _, js, jg = np.intersect1d(dates, common, return_indices=True)
        if len(js) < 3:
            continue
        diff = g_rel[jg] - sar_rel[js]
        offset = np.nanmean(diff)
        rms = float(np.sqrt(np.nanmean((diff - offset) ** 2)))
        years = (common - common[0]).astype(float) / 365.25
        velocity = float(np.polyfit(years, g_rel, 1)[0]) if years[-1] > 0.5 else np.nan
        results.append((name, lat, lon, rms, common, g_rel - offset, sar_rel, velocity))

    out_dir.mkdir(parents=True, exist_ok=True)
    ncol = math.ceil(math.sqrt(len(results) * 16 / 9))
    nrow = math.ceil(len(results) / ncol)
    fig, axs = plt.subplots(nrow, ncol, figsize=(2.6 * ncol, 2.0 * nrow), sharex=True, squeeze=False)
    for ax, (name, _, _, rms, g_dates, g_rel, sar_rel, _) in zip(axs.flat, results):
        ax.plot(g_dates, g_rel, '.', color='k', ms=1)
        ax.plot(dates, sar_rel, 'o', color='r', ms=2.5)
        lo, hi = np.nanpercentile(g_rel, [1, 99])  # GNSS range, so one bad InSAR epoch does not squash the panel
        pad = max(0.2 * (hi - lo), 0.03)
        ax.set_ylim(lo - pad, hi + pad)
        ax.set_title(f'{name} (ref)' if name == ref_site else f'{name}  rms={rms * 100:.1f} cm', fontsize=9)
        ax.tick_params(labelsize=7, labelbottom=True)
        if ax.get_subplotspec().is_first_col():
            ax.set_ylabel('LOS (m)', fontsize=8)
        ax.xaxis.set_major_locator(mdates.YearLocator())
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    for ax in axs.flat[len(results) :]:
        ax.axis('off')
    handles = [Line2D([], [], ls='', marker='o', ms=4, color=c, label=k) for k, c in (('GNSS', 'k'), ('InSAR', 'r'))]
    axs.flat[0].legend(handles=handles, fontsize=7, loc='lower left')
    fig.suptitle(f'LOS displacement (m) relative to GNSS site {ref_site} (NGL IGS20)')
    fig.tight_layout()
    fig.savefig(out_dir / 'gnss_compare.png', dpi=150)
    plt.close(fig)

    # velocity map, styled as browse/velocity.png: InSAR and GNSS LOS velocity relative to the reference site
    vel = postprocess.read_h5(h5_dir / 'velocity.h5', 'velocity')
    coh_mask = postprocess.read_h5(h5_dir / 'avgSpatialCoh.h5', 'coherence') > coh_thresh
    shade = postprocess.hillshade(postprocess.read_h5(h5_dir / 'geometryGeo.h5', 'height'))
    ref_lat, ref_lon = gnss[ref_site][:2]
    ry, rx = int(np.clip(round((ref_lat - y0) / dy), 1, naz - 2)), int(np.clip(round((ref_lon - x0) / dx), 1, nr - 2))
    vel = vel - np.nanmean(vel[ry - 1 : ry + 2, rx - 1 : rx + 2])
    vlim = postprocess.get_vlim(vel, coh_mask)
    fig, ax = plt.subplots()
    ax.set_title(f'LOS velocity, GNSS ref {ref_site}')
    extent = [x0, x0 + nr * dx, y0 + naz * dy, y0]
    postprocess.image_show(np.where(coh_mask, vel, np.nan), ax, extent, 'RdYlBu_r', vlim, 'm/year', shade=shade)
    g_lon, g_lat, g_vel = (np.array([r[k] for r in results]) for k in (2, 1, 7))
    ax.scatter(g_lon, g_lat, c=g_vel, cmap='RdYlBu_r', vmin=vlim[0], vmax=vlim[1], s=40, ec='k', zorder=3)
    ax.plot(ref_lon, ref_lat, '*', color='w', mec='k', ms=14, zorder=4)
    for name, lat, lon, *_ in results:
        ax.annotate(name, (lon, lat), xytext=(3, 3), textcoords='offset points', fontsize=8)
    fig.savefig(out_dir / 'gnss_velocity.png', dpi=150, bbox_inches='tight')
    plt.close(fig)

    with open(out_dir / 'gnss_rms.txt', 'w') as f:
        f.write(f'# site lat lon rms_m  (InSAR and GNSS LOS relative to {ref_site}, mean offset removed)\n')
        f.writelines(f'{name} {lat:.4f} {lon:.4f} {rms:.4f}\n' for name, lat, lon, rms, *_ in results)
    median_rms = np.median([r[3] for r in results[1:]]) * 100
    log.info(f'gnss: {len(results) - 1} sites vs {ref_site}, median rms {median_rms:.1f} cm')
    return [out_dir / 'gnss_compare.png', out_dir / 'gnss_velocity.png', out_dir / 'gnss_rms.txt']

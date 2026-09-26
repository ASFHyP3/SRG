#!/usr/bin/env python3
"""PROC_HOME SBAS/PS outputs browse-image plotting script:

 + Make plots for DEM, coherence, velocity, displacement, and the interferogram network.
 + Generate kmz for google Earth visualization.

 Yuan-Kai Liu + claude code, Stanford, 2026

"""

import argparse
import os, re, glob, sys
import datetime as dt
import zipfile
from collections import deque

import numpy as np
import matplotlib
try:
    get_ipython()
except NameError:
    matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import matplotlib.colors as mcolors
import matplotlib.dates as mdates
from matplotlib.ticker import MaxNLocator
from matplotlib.patches import Circle
from mpl_toolkits.axes_grid1 import make_axes_locatable

WVL_S1 = 0.0554657  # meter, S1 C-band (use 0.24 for NISAR L-band)
# maybe there is a better way rather than harcoding it

#####################################################################################
EXAMPLE = """example:
  preview.py                                    # default runs if executed in a sbas output dir
  preview.py  ./sbas
  preview.py  ./sbas  --coh-thresh 0.4          # threshold of colorbar split for network plot
  preview.py  ./sbas  --lalo -0.83 -91.13       # lat/lon of the displacement time-series pixel
  preview.py  ./sbas  --pic-dir ./pics  --show  # also show plots interactively (default muted)
"""
#####################################################################################

# --------------------------
# I/O + plotting functions
#   Perhaps some of these functions can be put into a utility .py
#   and can re-used for other usecases in the future
#   But I just keep them all here for now. 
# --------------------------

def load_rsc(path):
    """Read a ROI_PAC-style .rsc key/value file."""
    rsc = {}
    for line in open(path):
        parts = line.split()
        if len(parts) != 2:
            continue
        try:
            rsc[parts[0]] = float(parts[1]) if '.' in parts[1] or 'e' in parts[1].lower() else int(parts[1])
        except ValueError:
            rsc[parts[0]] = parts[1]
    return rsc


def get_extent(rsc):
    """matplotlib imshow extent [xmin, xmax, ymin, ymax] in lon/lat."""
    xmin = rsc['X_FIRST']
    xmax = rsc['X_FIRST'] + rsc['WIDTH'] * rsc['X_STEP']
    ymin = rsc['Y_FIRST'] + rsc['FILE_LENGTH'] * rsc['Y_STEP']
    ymax = rsc['Y_FIRST']
    return [xmin, xmax, ymin, ymax]


def load_sar_data(file_path, nr, naz=None, mode='bsq'):
    """Universal binary reader. mode='complex': .int-style (amp,phase); mode='bil': .cc/.unw-
    style 2-band-per-line (band0,band1); mode='bsq': single or stacked full (naz,nr) slices."""
    # here is still a bit of arbitrary logic to read those binaries, hmmm... to be imroved in the future
    # or simply drop this func and do np.fromfile() manually everywhere, messy.
    # or just convert all output binaries to HDF5 for good, :)
    if not os.path.exists(file_path):
        return None, None
    if mode == 'complex':
        data = np.fromfile(file_path, dtype=np.complex64)
        rows = naz if naz is not None else data.size // nr
        data = data[:rows * nr].reshape(rows, nr)
        return np.abs(data), np.angle(data)
    elif mode == 'bil':
        data = np.fromfile(file_path, dtype=np.float32)
        rows = naz if naz is not None else data.size // (2 * nr)
        data = data[:rows * 2 * nr].reshape(rows, 2 * nr)
        return data[:, :nr], data[:, nr:]
    elif mode == 'bsq':
        data = np.fromfile(file_path, dtype=np.float32)
        if naz is None:
            naz = data.size // nr
            return None, data[:naz * nr].reshape(naz, nr)
        nslices = data.size // (naz * nr)
        if nslices <= 1:
            return None, data[:naz * nr].reshape(naz, nr)
        return None, data[:nslices * naz * nr].reshape(nslices, naz, nr)
    raise ValueError(f"unknown mode {mode!r}")


def get_cmy_cmap():
    """Howard's manual RGB triangular ramps, cyclic for wrapped phase."""
    up = np.linspace(100, 255, 120) / 255
    dn = np.linspace(255, 100, 120) / 255
    on = np.ones(120)
    r = np.concatenate([dn, up, on])
    g = np.concatenate([up, on, dn])
    b = np.concatenate([on, dn, up])
    return mcolors.ListedColormap(np.stack([r, g, b], axis=-1), name='cmy_cyclic')


def hillshade(dem, vert_exag=1, azdeg=315, altdeg=45):
    """DEM -> grayscale illumination in [0,1], via matplotlib's LightSource."""
    from matplotlib.colors import LightSource
    return LightSource(azdeg=azdeg, altdeg=altdeg).hillshade(dem, vert_exag=vert_exag)


def normalize_amp(amp, exponent=0.3, denom_fac=2.0):
    """SAR amplitude -> [0,1] shade: power-law compress + mean-normalize"""
    # this should be robust to bright outliers
    bright = np.power(np.abs(amp), exponent)
    bright = np.clip(bright / (np.mean(bright[bright > 0]) * denom_fac), 0, 1)
    return bright


def blend_multiply(data, cmap, vmin, vmax, shade):
    """Colormap `data` then multiply by `shade` (already in [0,1]) -> (naz,nr,3) RGB."""
    rgb = plt.get_cmap(cmap)(plt.Normalize(vmin, vmax)(data))[..., :3]
    return rgb * shade[..., None]


def image_show(data, ax, extent, cmap='viridis', vlim=(None, None), clabel='', extend='', 
               shade=None, anntext=None, nticks=4, cticks=None):
    """shade=None: plain imshow. shade=array in [0,1]: blend_multiply internally."""
    # I know this is a big function; just try to be convenient.

    vmin = vlim[0] if vlim[0] is not None else np.nanmin(data)
    vmax = vlim[1] if vlim[1] is not None else np.nanmax(data)
    if shade is None:
        im = ax.imshow(data, extent=extent, cmap=cmap, vmin=vmin, vmax=vmax, interpolation='none')
    else:
        rgb = blend_multiply(data, cmap, vmin, vmax, shade)
        ax.imshow(rgb, extent=extent, interpolation='none')
        im = cm.ScalarMappable(norm=plt.Normalize(vmin, vmax), cmap=cmap)  # colorbar only
    if np.min(data) < vmin:
        extend += 'min'
    if np.max(data) > vmax:
        extend += 'max'
    extend = 'both' if extend == 'minmax' else (extend or 'neither')
    add_colorbar(im, ax, label=clabel, extend=extend, cticks=cticks)
    if nticks is not None:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=nticks))
        ax.yaxis.set_major_locator(MaxNLocator(nbins=nticks))
    if anntext is not None:
        ax.text(0.025, 0.05, anntext, fontsize=11, transform=ax.transAxes,
                bbox=dict(boxstyle='round,pad=0.3', fc='whitesmoke', ec='k', lw=0.8, alpha=0.85))
    return ax, im


def add_colorbar(im, ax, label='m', extend='both', size='4%', pad=0.1, cticks=None):
    cax = make_axes_locatable(ax).append_axes("right", size=size, pad=pad)
    cb = plt.colorbar(im, cax=cax, label=label, extend=extend)
    if cticks is not None:
        cb.locator = MaxNLocator(nbins=cticks)
        cb.update_ticks()
    return cb


def get_vlim(data, maskin=None, q=(0.002, 0.998)):
    """Data-driven symmetric +/-vlim from the quantile range, optionally restricted by maskin."""
    d = data[maskin] if maskin is not None else data
    v = np.nanmax(np.abs(np.nanquantile(d, q)))
    return (-v, v)


def parse_date(fname):
    return dt.datetime.strptime(re.search(r'(\d{8})T', fname).group(1), '%Y%m%d')


def write_kmz(data, extent, cmap, vlim, label, out_path, shade=None):
    """Georeferenced KMZ. shade=array in [0,1] (e.g. namp): blend_multiply into the raster,
    same as image_show's shade path."""
    vmin, vmax = vlim
    out_dir    = os.path.dirname(out_path) or '.'
    name       = os.path.splitext(os.path.basename(out_path))[0]
    img_name, leg_name = f'{name}_raster.png', f'{name}_legend.png'
    img_path, leg_path = os.path.join(out_dir, img_name), os.path.join(out_dir, leg_name)

    if shade is None:
        plt.imsave(img_path, data, cmap=cmap, vmin=vmin, vmax=vmax)
    else:
        plt.imsave(img_path, blend_multiply(data, cmap, vmin, vmax, shade))

    # legend generation: can still be clipped in its width if label is long.
    fig = plt.figure(figsize=(0.8, 1.2))
    cax = fig.add_axes([0.3, 0.05, 0.18, 0.9])
    cb  = plt.colorbar(cm.ScalarMappable(norm=plt.Normalize(vmin, vmax), cmap=cmap), cax=cax)
    cb.set_label(label, color='white', fontsize=8)
    cb.ax.tick_params(labelsize=7)
    cb.ax.yaxis.set_tick_params(color='white')
    plt.setp(plt.getp(cb.ax.axes, 'yticklabels'), color='white')
    fig.savefig(leg_path, dpi=150, transparent=True)
    plt.close(fig)

    xmin, xmax, ymin, ymax = extent
    kml = f"""<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
<Document>
  <name>{name}</name>
  <GroundOverlay>
    <name>{name}</name>
    <Icon><href>{img_name}</href></Icon>
    <LatLonBox><north>{ymax}</north><south>{ymin}</south><east>{xmax}</east><west>{xmin}</west></LatLonBox>
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
    os.remove(img_path)
    os.remove(leg_path)


#####################################################################################
def run(inps):
    os.makedirs(inps.pic_dir, exist_ok=True)

    def savefig(fig, name):
        fig.savefig(os.path.join(inps.pic_dir, name), dpi=150, bbox_inches='tight')
        if inps.show:
            plt.show()
        plt.close(fig)

    cmy = get_cmy_cmap()

    # :: metadata
    print('reading metadata & DEM...')
    rsc = load_rsc(f'{inps.file_dir}/dem.rsc')
    nr, naz = rsc['WIDTH'], rsc['FILE_LENGTH']
    with open(f'{inps.file_dir}/parameters') as f:
        _, _, nslc, ncells = f.read().split()
    nslc, ncells = int(nslc), int(ncells)
    extent       = get_extent(rsc)
    print(f'  nr={nr} naz={naz} nslc={nslc} ncells(pairs)={ncells}')

    dem      = np.fromfile(f'{inps.file_dir}/dem', dtype=np.int16).reshape(naz, nr).astype(float)
    demshade = hillshade(dem, vert_exag=1)

    # :: sbas_list -> per-pair bperp (date1, date2, tbase days, bperp m)
    print('parsing sbas_list & propagating baseline network...')
    pairs  = [l.split() for l in open(f'{inps.file_dir}/sbas_list')]
    edges  = [(parse_date(p[0]), parse_date(p[1]), float(p[2]), float(p[3])) for p in pairs]
    epochs = sorted(set(d for e in edges for d in e[:2]))
    tbase  = [e[2] for e in edges]

    # :: bperp are treated as deterministic & redundant, no least-squares inversion
    adj = {}
    for d1, d2, _, bp in edges:
        adj.setdefault(d1, []).append((d2, bp))
        adj.setdefault(d2, []).append((d1, -bp))
    bperp = {epochs[0]: 0.0}
    queue = deque([epochs[0]])
    while queue:
        cur = queue.popleft()
        for nxt, bp in adj[cur]:
            if nxt not in bperp:
                bperp[nxt] = bperp[cur] + bp
                queue.append(nxt)
    mean_bp = np.mean(list(bperp.values()))
    bperp = {k: v - mean_bp for k, v in bperp.items()}

    # :: stackmht + coherence stack
    print('reading stackmht (mean amp/phase rate)...')
    amp, rate = load_sar_data(f'{inps.file_dir}/stackmht', nr, naz, mode='bil')
    namp = normalize_amp(amp)

    N = 20  # plot every 20 pts for faster plotting
    ref_rc   = np.loadtxt(f'{inps.file_dir}/ref_locs', dtype=int)
    ref_lons = rsc['X_FIRST'] + ref_rc[::N, 0] * rsc['X_STEP']
    ref_lats = rsc['Y_FIRST'] + ref_rc[::N, 1] * rsc['Y_STEP']

    cc_files = sorted(glob.glob(f'{inps.file_dir}/*.cc'))
    print(f'averaging coherence over {len(cc_files)} pairs...')
    cohsum, cc_pair_avg = np.zeros((naz, nr), dtype=np.float32), {}
    for i, f in enumerate(cc_files, 1):
        _, c = load_sar_data(f, nr, naz, mode='bil')
        cohsum += c
        cc_pair_avg[os.path.basename(f)[:-3]] = np.nanmean(c)  # key 'YYYYMMDD_YYYYMMDD'
        if i % 100 == 0:
            print(f'  ...{i}/{len(cc_files)}')
    cohavg   = cohsum / len(cc_files)
    coh_mask = cohavg > inps.coh_thresh

    # :: some overview plots
    print('plotting DEM/coherence/amplitude/rate...')
    fig, ax = plt.subplots()
    ax.set_title('Elevation')
    image_show(dem, ax, extent, cmap='terrain', clabel='meter', shade=demshade)
    savefig(fig, 'elevation.png')

    fig, ax = plt.subplots()
    ax.set_title(f'Mean coherence ({len(cc_files)} pairs)')
    image_show(cohavg, ax, extent, cmap='viridis', vlim=(0, 1), clabel='coherence')
    savefig(fig, 'avgCoherence.png')

    fig, ax = plt.subplots()
    ax.set_title('Mean amplitude')
    image_show(namp, ax, extent, cmap='gray', clabel='normalized (-)', anntext=f'{len(ref_rc)} ref_locs, every {N}$^{{th}}$ shown')
    ax.scatter(ref_lons, ref_lats, s=0.3, c='orange')
    savefig(fig, 'avgAmplitude.png')

    fig, ax = plt.subplots()
    ax.set_title('Mean stacked phase rate')
    image_show(rate, ax, extent, cmap=cmy, clabel='rad/day', shade=namp)
    savefig(fig, 'phaseVelocity.png')

    # :: velocity & displacement: (1) SBAS naive mean, (2) cumulative displacement, (3) per-pixel OLS fit
    print('velocity/displacement, per-pixel fit, network + time series...')
    _, vel  = load_sar_data(f'{inps.file_dir}/velocity', nr, naz, mode='bsq')
    vel     = vel.reshape(nslc - 1, naz, nr)
    vel_myr = np.nanmean(vel, axis=0) * -inps.wvl / (4 * np.pi) * 365.25

    disp         = np.fromfile(f'{inps.file_dir}/displacement', dtype=np.float32).reshape(nslc - 1, naz, 2, nr)
    disp_m       = disp[:, :, 1, :] * -inps.wvl / (4 * np.pi)
    disp_final_m = disp_m[-1]

    t = np.array([(e - epochs[0]).days for e in epochs[1:]])  # days; nslc-1 epochs
    t3 = t[:, None, None]
    slope = np.sum((t3 - t.mean()) * (disp_m - disp_m.mean(axis=0)), axis=0) / np.sum((t - t.mean())**2)
    intercept = disp_m.mean(axis=0) - slope * t.mean()
    resid = disp_m - (slope[None] * t3 + intercept[None])
    vel_fit_myr = slope * 365.25
    rmse = np.sqrt((resid**2).mean(axis=0))

    fig, ax = plt.subplots()
    ax.set_title('Naive mean LOS velocity')
    image_show(vel_myr, ax, extent, cmap='RdYlBu_r', vlim=get_vlim(vel_myr, coh_mask), clabel='m/year', shade=demshade)
    savefig(fig, 'velocityNaive.png')

    fig, ax = plt.subplots()
    ax.set_title('LOS velocity (linear fit to displacement)')
    image_show(vel_fit_myr, ax, extent, cmap='RdYlBu_r', vlim=get_vlim(vel_fit_myr, coh_mask), clabel='m/year', shade=demshade)
    savefig(fig, 'velocity.png')

    fig, ax = plt.subplots()
    ax.set_title('Final cumulative LOS displacement')
    image_show(disp_final_m, ax, extent, cmap='RdYlBu_r', vlim=get_vlim(disp_final_m, coh_mask), clabel='m', shade=demshade)
    savefig(fig, 'dispCumulative.png')

    fig, ax = plt.subplots()
    ax.set_title('Velocity RMSE')
    image_show(rmse, ax, extent, cmap='magma_r', clabel='m', shade=demshade)
    savefig(fig, 'velocityRMSE.png')

    # :: network plot
    print(f'  {len(pairs)} pairs, {len(epochs)} unique epochs, {epochs[0]:%Y%m%d}..{epochs[-1]:%Y%m%d}, '
          f'temporal baseline range {min(tbase)}-{max(tbase)} days')

    n_bins = 256
    norm = plt.Normalize(0.2, 1.0)
    n_low = int(n_bins * norm(inps.coh_thresh))
    n_high = n_bins - n_low
    new_colors = np.vstack((
        plt.get_cmap('Reds_r', n_low)(np.linspace(0.0, 0.7, n_low)),
        plt.get_cmap('Blues', n_high)(np.linspace(0.3, 1.0, n_high))
    ))
    cmap_net = mcolors.LinearSegmentedColormap.from_list('split_cmap', new_colors)
    norm = plt.Normalize(0.2, 1)

    fig, ax = plt.subplots(figsize=[8, 4])
    ax.scatter(epochs, [bperp[e] for e in epochs], ec='k', fc='orange', s=50, zorder=2)
    for d1, d2, _, _ in edges:
        key = f'{d1:%Y%m%d}_{d2:%Y%m%d}'
        ax.plot([d1, d2], [bperp[d1], bperp[d2]], color=cmap_net(norm(cc_pair_avg.get(key, np.nan))), lw=2, zorder=1, alpha=0.85)
    ax.set_ylabel('Perp Baseline (m)')
    ax.set_title('Interferogram Network')
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    ax.xaxis.set_minor_locator(mdates.MonthLocator())
    ax.yaxis.set_major_locator(plt.MultipleLocator(50))
    ax.yaxis.set_minor_locator(plt.MultipleLocator(25))
    ax.tick_params(axis='both', which='major', direction='in', length=5, width=1.2, top=True, right=True)
    ax.tick_params(axis='both', which='minor', direction='in', length=3, width=1, top=True, right=True)
    add_colorbar(cm.ScalarMappable(norm=norm, cmap=cmap_net), ax, label='Avg Spatial Coherence', extend='neither', size='2.5%', pad=0.15)
    ax.text(0.025, 0.05, f'{len(epochs)} epochs, {len(cc_pair_avg)} pairs', fontsize=11,
            transform=ax.transAxes, bbox=dict(boxstyle='round,pad=0.3', fc='whitesmoke', ec='k', lw=0.8, alpha=0.85))
    savefig(fig, 'network.png')

    # :: displacement time series: auto-pick highest-velocity coherent pixel, or user-given --lalo if specified
    draw_search_box = inps.lalo is None
    if draw_search_box:
        r = nr // 3
        r0, r1 = naz // 2 - r, naz // 2 + r
        c0, c1 = nr // 2 - r, nr // 2 + r
        box_mask = coh_mask[r0:r1, c0:c1]
        box_vel = np.where(box_mask, np.abs(vel_fit_myr[r0:r1, c0:c1]), -np.inf)
        iy, ix = np.unravel_index(np.argmax(box_vel), box_vel.shape)
        iy, ix = r0 + iy, c0 + ix
    else:
        lat, lon = inps.lalo
        iy = round((lat - rsc['Y_FIRST']) / rsc['Y_STEP'])
        ix = round((lon - rsc['X_FIRST']) / rsc['X_STEP'])
        iy, ix = np.clip(iy, 1, naz - 2), np.clip(ix, 1, nr - 2)

    ts = disp_m[:, iy-1:iy+2, ix-1:ix+2].mean(axis=(1, 2))
    ts = np.insert(ts, 0, 0.0)  # first epoch = reference, disp=0
    px_lon, px_lat = rsc['X_FIRST'] + ix * rsc['X_STEP'], rsc['Y_FIRST'] + iy * rsc['Y_STEP']

    fig, axs = plt.subplots(ncols=2, figsize=[12, 4], gridspec_kw={'width_ratios': [3.6, 6.4]})

    axs[0].set_title('Final LOS displacement')
    image_show(disp_final_m, axs[0], extent, cmap='RdYlBu_r', vlim=get_vlim(disp_final_m, coh_mask), clabel='m', shade=demshade, cticks=5)
    axs[0].scatter(px_lon, px_lat, marker='^', ec='k', fc='lime', s=50, zorder=3)
    if draw_search_box:
        center_lon = rsc['X_FIRST'] + (nr // 2) * rsc['X_STEP']
        center_lat = rsc['Y_FIRST'] + (naz // 2) * rsc['Y_STEP']
        radius_deg = r * abs(rsc['X_STEP'])
        axs[0].add_patch(Circle((center_lon, center_lat), radius_deg, fill=False, ec='lightgray', lw=1.5, zorder=2))

    ax = axs[1]
    ax.plot(epochs, ts, color='grey', lw=2, marker='o', ms=6, mfc='C0', mec='k', alpha=0.8, zorder=2)    
    ax.set_xlabel('date')
    ax.set_ylabel('LOS displacement (m)')
    ax.set_title(f'{px_lat:.3f}°N, {px_lon:.3f}°E (velo={vel_fit_myr[iy, ix]:.3f} m/yr)')
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    ax.xaxis.set_minor_locator(mdates.MonthLocator())
    ax.tick_params(axis='both', which='major', direction='in', length=5, width=1.2, top=True, right=True)
    ax.tick_params(axis='both', which='minor', direction='in', length=3, width=1, top=True, right=True)
    plt.tight_layout()
    savefig(fig, 'dispPixel.png')

    # :: kmz overlays (Google Earth): png raster + lower-left legend, no axes/ticks
    print('writing KMZ overlays...')
    write_kmz(cohavg, extent, 'viridis', (0, 1), 'coherence', os.path.join(inps.pic_dir, 'avgCoherence.kmz'))
    write_kmz(vel_fit_myr, extent, 'RdYlBu_r', get_vlim(vel_fit_myr, coh_mask), 'm/year', os.path.join(inps.pic_dir, 'velocity.kmz'), shade=namp)

    print(f'done. figures saved to {inps.pic_dir}')


#####################################################################################
def main(iargs=None):
    parser = argparse.ArgumentParser(description=__doc__, epilog=EXAMPLE, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('file_dir', nargs='?', default='./', help='SBAS output directory to read (default: cwd)')
    parser.add_argument('--pic-dir', dest='pic_dir', default=None, help="figure output dir (default: '<file_dir>/pics')")
    parser.add_argument('--show', action='store_true', help='call plt.show() interactively instead of only saving (default: off)')
    parser.add_argument('--coh-thresh', dest='coh_thresh', type=float, default=0.5,
                         help='coherence threshold: masks velocity/displacement maps + pixel search, '
                              'splits network-plot color (default: %(default)s)')
    parser.add_argument('--wvl', type=float, default=WVL_S1, help='radar wavelength (m); use 0.24 for NISAR L-band (default: %(default)s)')
    parser.add_argument('--lalo', type=float, nargs=2, metavar=('LAT', 'LON'), default=None,
                         help='lat/lon of the displacement time-series pixel '
                              '(default: auto-pick highest-velocity pixel in the center box)')
    inps = parser.parse_args(iargs)

    inps.file_dir = os.path.expanduser(inps.file_dir)
    inps.pic_dir = os.path.expanduser(inps.pic_dir) if inps.pic_dir else os.path.join(inps.file_dir, 'pics')

    plt.rcParams.update({'font.size': 13})
    run(inps)


#####################################################################################
if __name__ == '__main__':
    main(sys.argv[1:])


# end of file


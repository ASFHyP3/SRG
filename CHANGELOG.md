# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PEP 440](https://www.python.org/dev/peps/pep-0440/)
and uses [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.6.0]
### Added
- `src/srg/postprocess.py` (`python postprocess.py PRODUCT_DIR`), run by `time_series` before zipping:
  1. `hdf5/` (MintPy-style) from the SRG binaries: `timeseries.h5`, `velocity.h5`, `avgPhaseVelocity.h5`, `avgSpatialCoh.h5`,
     `avgAmplitude.h5`, `geometryGeo.h5` (height, incidenceAngle, azimuthAngle, slantRangeDistance),
     `numInvIfgram.h5`, `coherenceSpatialAvg.txt`; referenced to one pixel (REF_Y/REF_X).
  2. `browse/` from `hdf5/` only: PNG + KMZ for elevation, avgCoherence, avgAmplitude, avgPhaseVelocity, velocity,
     dispCumulative, velocityStd; plus network.png and dispPixel.png.
  - `<product>.png` (velocity) uploaded as the HyP3 browse image.
  - Reads only files in the product zip, so it also runs on downloaded products. Failures are logged; the product
    is still uploaded.
- `h5py` dependency.

### Changed
- `scripts/preview.py` moved into `src/srg/postprocess.py` so the HyP3 job can call it.
- The linear-fit velocity includes the first (zero-displacement) epoch, as MintPy `timeseries2velocity.py` does.
- The SBAS `velocity` binary (per-interval rates, averaged without time weights) is no longer used; the stacking
  velocity comes from `stackmht`.

## [0.5.0]
### Added
- Added hyp3-srg into SRG repo.

## [0.4.3]
### Fixed
- made the auxiliary script for PS processing executable.

## [0.4.2]
### Added
- Added auxiliary script for PS processing.

## [0.4.1]
### Changed
- Re-implemented Sentinel-1 orbit fetching using the `s1-orbits.asf.alaska.edu` API

### Added
- Instructions on how to update the repository based on a provided tarball

## [0.4.0]
### Changed
- Rename repository from Back-Projection to the Stanford Radar Group SAR Processor (SRG)

## [0.3.1]
### Fixed
- Double-slash in call to `sentinel_raw_process_cpu`.

## [0.3.0]
### Fixed
- Fixed createDEMcop.py to handle missing tiles, now fill missing tiles with zeros.
- Added createDEMcop_hz.py to keep original version accessible.
- `printf` formatting data types in azimuth_compress.cu to match input data.

### Changed
- File permissions for Python files to make them executable.

### Removed
- Vendored FFTW libary.
- Compiled C/C++ binaries

## [0.2.0]
### Added
- A new version of the processor was delivered by the Stanford team, which enables SBAS processing.

### Changed
- A new version of the processor was delivered by the Stanford team. All existing code was replaced.

## [0.1.0]
### Added
- Versioned releases are now published to GitHub and changes are tracked in CHANGELOG.md

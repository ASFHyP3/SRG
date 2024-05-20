# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PEP 440](https://www.python.org/dev/peps/pep-0440/)
and uses [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0]
### Fixed
- Fixed createDEMcop.py to handle missing tiles, now fill missing tiles with zeros
- Added createDEMcop_hz.py to keep original version accessible

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

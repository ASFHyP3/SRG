#!/bin/bash --login
set -e
conda activate srg
exec python -um srg "$@"

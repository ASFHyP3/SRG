# Stanford Radar Group (SRG) SAR Processor

This repository hosts the Stanford Radar Group (SRG) SAR Processor, which includes utilities for performing the deformation time series analysis of volcanic regions from space-geodetic InSAR and GNSS observations. The SRG processor is being used within the NASA-funded Live Analysis of Volcano Activity with SAR (LAVAS) project, which aims to provide crustal deformation data for every active volcano in the world.

THIS IS RESEARCH CODE PROVIDED TO YOU "AS IS" WITH NO WARRANTIES OF CORRECTNESS. USE AT YOUR OWN RISK.

## Updating this repo using provided source files
The Stanford Radar Group team does not use a software versioning tool to track the changes the make to the SRG processor. Instead, code changes are provide to ASF via the delivery of a tar file containing the full source code directory. To update this repository, follow these steps:

1. Un-tar and copy the provided source files into ./my_proc. Calling `ls ./my_proc/sentinel` should return a set of processing scripts.
1. Run `./update_with_srg_tar.sh`. This will copy over any files that exist in the SRG repository that are modified in the un-tarred `my_proc` directory.
1. Commit changes.
1. Manually copy over any new files to the appropriate location inside of the repository, and commit changes
1. Proceed as normal for a feature PR.

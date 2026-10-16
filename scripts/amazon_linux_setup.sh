#!/bin/bash

# GPU setup for the Amazon Linux 2023

# Install NVIDIA driver
DRIVER_VERSION=550.54.14
sudo dnf install -y kernel-devel-$(uname -r) kernel-headers-$(uname -r) kernel-modules-extra
curl -fSsl -O https://us.download.nvidia.com/tesla/$DRIVER_VERSION/NVIDIA-Linux-x86_64-$DRIVER_VERSION.run
chmod +x NVIDIA-Linux-x86_64-$DRIVER_VERSION.run
sudo ./NVIDIA-Linux-x86_64-$DRIVER_VERSION.run --tmpdir . --silent
rm ./NVIDIA-Linux-x86_64-$DRIVER_VERSION.run 

# Install and enable Docker
sudo dnf install -y docker git
sudo systemctl start docker
sudo systemctl enable docker
sudo usermod -aG docker ec2-user

# Install nvidia-container-toolkit
sudo dnf config-manager --add-repo https://nvidia.github.io/libnvidia-container/stable/rpm/nvidia-container-toolkit.repo
sudo dnf install -y nvidia-container-toolkit
sudo nvidia-ctk runtime configure --runtime=docker
sudo systemctl restart docker

# Install extra packages
sudo dnf install -y git

# Cleanup
dnf clean all && rm -rf /var/cache/dnf/*

# Reboot
sudo reboot

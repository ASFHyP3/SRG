FROM condaforge/mambaforge:latest as builder

ENV USEGPU=false
ENV FFTW_LIB=/usr/lib/x86_64-linux-gnu/libfftw3f.a
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends unzip vim curl git build-essential gfortran libfftw3-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

COPY ./scripts/build_proc.sh ./
RUN ./build_proc.sh && cd /

FROM condaforge/mambaforge:latest as runner

# For opencontainers label definitions, see:
#    https://github.com/opencontainers/image-spec/blob/master/annotations.md
LABEL org.opencontainers.image.title="HyP3 SRG"
LABEL org.opencontainers.image.description="HyP3 plugin for Stanford Radar Group SAR processing"
LABEL org.opencontainers.image.vendor="Alaska Satellite Facility"
LABEL org.opencontainers.image.authors="ASF Tools Team <UAF-asf-apd@alaska.edu>"
LABEL org.opencontainers.image.licenses="BSD-3-Clause"
LABEL org.opencontainers.image.url="https://github.com/ASFHyP3/hyp3-srg"
LABEL org.opencontainers.image.source="https://github.com/ASFHyP3/hyp3-srg"
LABEL org.opencontainers.image.documentation="https://hyp3-docs.asf.alaska.edu"

ARG CONDA_UID=1000
ARG CONDA_GID=1000

ENV PROC_HOME=/srg
ENV PYTHONDONTWRITEBYTECODE=true
ENV MYHOME=/home/conda
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends unzip vim curl gfortran && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN groupadd -g "${CONDA_GID}" --system conda && \
    useradd -l -u "${CONDA_UID}" -g "${CONDA_GID}" --system -d /home/conda -m  -s /bin/bash conda && \
    chown -R conda:conda /opt && \
    echo ". /opt/conda/etc/profile.d/conda.sh" >> /home/conda/.profile && \
    echo "conda activate base" >> /home/conda/.profile

SHELL ["/bin/bash", "-l", "-c"]

USER ${CONDA_UID}
WORKDIR /home/conda/

COPY --chown=${CONDA_UID}:${CONDA_GID} --from=builder /srg/snaphu_v2.0b0.0.0/bin/snaphu /srg/bin/snaphu
COPY --chown=${CONDA_UID}:${CONDA_GID} --from=builder /srg /srg

RUN mamba env create -f /srg/environment.yml && \
    conda clean -afy && \
    conda activate srg && \
    sed -i 's/conda activate base/conda activate srg/g' /home/conda/.profile && \
    python -m pip install --no-cache-dir /srg

ENTRYPOINT ["/srg/src/srg/etc/entrypoint.sh"]
CMD ["-h"]

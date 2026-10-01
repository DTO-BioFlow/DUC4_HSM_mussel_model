# Two stages: EDITO rejects images larger than 1 GB unpacked. The compilers,
# -dev headers and static libraries needed to build the R packages stay in the
# 'builder' stage; the final image gets only the compiled R library and the
# shared libraries it links against.

# --- builder: compiles the R packages and fuzzyfis ---------------------------
# Also the image to run tests/ in (docker build --target builder): the
# regression test compiles the old baseline evaluator with Rcpp, which needs
# g++. It holds the same compiled library the final image gets.
FROM rocker/r-ver:4.3.3 AS builder

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gdal-bin \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    make \
    g++ \
    awscli \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

RUN mkdir -p /app/output /app/input /app/scripts

# Pin to a dated CRAN snapshot (via Posit Package Manager) instead of the
# rolling 'latest' CRAN mirror, so a rebuild months from now doesn't silently
# pick up different package versions and change model output.
RUN R -q -e "install.packages(c('Rcpp','FuzzyR','raster','sp','paws'), repos='https://packagemanager.posit.co/cran/2024-03-15')"

# --- fuzzyfis: internal compiled FIS evaluator, built into the image -------
# Unlike the R scripts (synced from S3 at container startup), this package's
# C++ source is baked in at BUILD time. Changing pkg/fuzzyfis/src/evalfis2.cpp
# requires an image rebuild - there is no S3-based hot-update path for it.
COPY pkg/fuzzyfis /tmp/fuzzyfis
RUN R -q -e "Rcpp::compileAttributes('/tmp/fuzzyfis')" \
    && R CMD INSTALL /tmp/fuzzyfis \
    && rm -rf /tmp/fuzzyfis

# --- final: runtime only ----------------------------------------------------
# Plain Ubuntu 22.04 (the OS rocker/r-ver:4.3.3 is built on), not rocker
# itself: the rocker base alone is ~770 MB (Java, LLVM, gcc/gfortran, TeX
# fonts) and cannot be slimmed by a later layer. R and every compiled package
# are copied byte-for-byte from the builder, so the R build stays the same.
FROM ubuntu:jammy

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC
ENV LANG=en_US.UTF-8

# Runtime libraries, found with ldd in the builder:
#  - R itself (bin/exec/R, lib/, modules/, base packages). BLAS/LAPACK is
#    libopenblas0-pthread, the same provider rocker selects.
#  - the compiled R packages: only terra, curl, openssl and xml2 link system
#    libraries; libgdal30 pulls in the rest.
# Plus locales (rocker's LANG), tzdata, and awscli for the S3 sync in
# entrypoint.sh.
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    locales \
    tzdata \
    libopenblas0-pthread \
    libgfortran5 \
    libgomp1 \
    libquadmath0 \
    libreadline8 \
    libicu70 \
    libpcre2-8-0 \
    libpcre3 \
    libbz2-1.0 \
    liblzma5 \
    libdeflate0 \
    libcairo2 \
    libpango-1.0-0 \
    libpangocairo-1.0-0 \
    libjpeg-turbo8 \
    libpng16-16 \
    libtiff5 \
    libx11-6 \
    libxt6 \
    libgdal30 \
    libgeos-c1v5 \
    libproj22 \
    libcurl4 \
    libcurl3-gnutls \
    libssl3 \
    libxml2 \
    awscli \
    && sed -i 's/^# *en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
    && locale-gen \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local/lib/R /usr/local/lib/R
COPY --from=builder /usr/local/bin/R /usr/local/bin/Rscript /usr/local/bin/

WORKDIR /app

RUN mkdir -p /app/output /app/input /app/scripts

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

RUN groupadd --system app && useradd --system --gid app --home-dir /app app \
    && chown -R app:app /app
USER app

ENV SCRIPT_NAME=VSC_CB2_HSM_18.R \
    S3_SCRIPTS_PREFIX=scripts \
    S3_INPUT_PREFIX=input \
    S3_OUTPUT_PREFIX=output

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

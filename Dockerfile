FROM rocker/r-ver:4.3.3

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
RUN R -q -e "install.packages(c('Rcpp','FuzzyR','raster','terra','sp','paws'), repos='https://packagemanager.posit.co/cran/2024-03-15')"

# --- fuzzyfis: internal compiled FIS evaluator, built into the image -------
# Unlike the R scripts (synced from S3 at container startup), this package's
# C++ source is baked in at BUILD time. Changing pkg/fuzzyfis/src/evalfis2.cpp
# requires an image rebuild - there is no S3-based hot-update path for it.
COPY pkg/fuzzyfis /tmp/fuzzyfis
RUN R -q -e "Rcpp::compileAttributes('/tmp/fuzzyfis')" \
    && R CMD INSTALL /tmp/fuzzyfis \
    && rm -rf /tmp/fuzzyfis

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

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

RUN R -q -e "install.packages(c('Rcpp','FuzzyR','raster','terra','doSNOW','foreach','iterators','sp','paws'), repos='https://cloud.r-project.org')"

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENV SCRIPT_NAME=VSC_CB2_HSM_18.R \
    S3_SCRIPTS_PREFIX=scripts \
    S3_INPUT_PREFIX=input \
    S3_OUTPUT_PREFIX=output

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

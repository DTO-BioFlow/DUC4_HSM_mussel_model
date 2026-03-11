FROM rocker/r-ver:4.3.3

ENV DEBIAN_FRONTEND=noninteractive
ENV PROJECT_DIR=/app
ENV INPUT_FOLDER_NAME=input
ENV OUTPUT_FOLDER_NAME=output
ENV RC_LIST_PATH=/app/input/rc_list_year.rds
ENV BPNS_INPUT_DIR="/app/input/BPNS input layers median"
ENV OUTPUT_DIR=/output
ENV S3_INPUT_PREFIX=input
ENV S3_OUTPUT_PREFIX=output

RUN apt-get update && apt-get install -y --no-install-recommends \
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
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . /app

RUN mkdir -p /data /output

RUN R -q -e "install.packages(c('Rcpp','FuzzyR','raster','terra','doSNOW','foreach','iterators','sp','paws'), repos='https://cloud.r-project.org')"

CMD ["Rscript", "VSC_CB2_HSM_18_cpp.R"]

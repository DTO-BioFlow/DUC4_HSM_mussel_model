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
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . /app

RUN mkdir -p "/tmp/mussel-model/input/BPNS input layers median" "/tmp/mussel-model/output"

RUN R -q -e "install.packages(c('Rcpp','FuzzyR','raster','terra','doSNOW','foreach','iterators','sp','paws'), repos='https://cloud.r-project.org')"

ENTRYPOINT ["Rscript", "VSC_CB2_HSM_18.R"]

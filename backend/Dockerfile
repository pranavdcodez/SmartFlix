FROM rocker/r-ver:4.4.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev libcurl4-openssl-dev libsodium-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('plumber','dplyr','jsonlite'))"

WORKDIR /app
COPY api.R run.R movies_metadata.csv credits.csv ./

ENV PORT=8000
EXPOSE 8000
CMD ["Rscript", "run.R"]

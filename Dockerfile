# ==============================================================================
# PowerClimate Vision Explorer — Hugging Face Spaces Dockerfile
# ==============================================================================
# This Dockerfile is used ONLY by Hugging Face Spaces (port 7860).
# The server deployment uses docker/Dockerfile (port 3838) instead.
# Do NOT modify docker/Dockerfile or docker-compose.yml — those are for the
# ECMWF server deployment via SSH tunnel.
#
# DATA STRATEGY: The 1.5 GB Parquet data is stored in a separate HF Dataset
# repo (adumitrescu/powervision-data) because HF Space repos are limited to
# 1 GB. The data is downloaded during the Docker build step.
# ==============================================================================
FROM ghcr.io/rocker-org/r-ver:4.5.0

# --- System dependencies for R package compilation ---
# Identical to docker/Dockerfile — spatial stack, text rendering, Arrow C++ build
RUN apt-get update && apt-get install -y --no-install-recommends \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libsqlite3-dev \
  libudunits2-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff-dev \
  libjpeg-dev \
  libuv1-dev \
  libabsl-dev \
  gdal-bin \
  curl \
  cmake \
  make \
  gcc \
  g++ \
  python3 \
  python3-pip \
  && rm -rf /var/lib/apt/lists/*

# --- Install Inter font (used by the app's glassmorphism design) ---
RUN apt-get update && apt-get install -y --no-install-recommends \
  fonts-inter \
  && rm -rf /var/lib/apt/lists/* \
  && fc-cache -fv

# --- Set working directory ---
WORKDIR /app

# --- Copy renv bootstrap files first (Docker layer caching) ---
# This layer is cached — packages only rebuild when renv.lock changes
COPY app/renv.lock renv.lock
COPY app/.Rprofile .Rprofile
COPY app/renv/activate.R renv/activate.R
COPY app/renv/settings.json renv/settings.json

# --- Restore R packages from lockfile ---
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE
ENV ARROW_WITH_SNAPPY=ON
ENV NOT_CRAN=true
RUN R -e 'renv::restore()'

# --- Download Parquet data from HF Dataset repo ---
# The data is stored separately because HF Space repos are limited to 1 GB.
# Dataset repo: https://huggingface.co/datasets/adumitrescu/powervision-data
# Files are stored under pecd/ in the dataset repo, and snapshot_download
# preserves that structure, so local_dir='/app/www/data' creates /app/www/data/pecd/...
ARG CACHEBUST=1
RUN pip install --no-cache-dir --break-system-packages huggingface_hub && \
  python3 -c "from huggingface_hub import snapshot_download; snapshot_download(repo_id='adumitrescu/powervision-data', repo_type='dataset', local_dir='/app/www/data', allow_patterns=['pecd/**'])" && \
  pip uninstall -y --break-system-packages huggingface_hub && \
  echo '✅ Parquet data downloaded'

# --- Copy the full app source (code, GeoJSON, CSVs — NOT Parquet) ---
COPY app/ .

# --- Environment variables ---
ENV PECD_GEOJSON_VERSION=mixed

# --- HF Spaces requires port 7860 ---
EXPOSE 7860

# --- Run as non-root user (HF Spaces security requirement) ---
RUN useradd -m -u 1000 appuser && chown -R appuser:appuser /app
USER appuser

# --- Launch Shiny directly (no Shiny Server — single user on HF) ---
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=7860)"]

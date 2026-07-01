# ==============================================================================
# PowerClimate Vision Explorer — Docker Image
# Base: rocker/shiny (Ubuntu + R 4.5 + Shiny Server Open Source)
# ==============================================================================
FROM ghcr.io/rocker-org/shiny:4.5.0

# --- System dependencies for R package compilation ---
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

# --- Install Inter font ---
RUN apt-get update && apt-get install -y --no-install-recommends     fonts-inter   && rm -rf /var/lib/apt/lists/*   && fc-cache -fv

# --- Set working directory ---
WORKDIR /srv/shiny-server/powervision

# --- Copy renv bootstrap files first (Docker layer caching) ---
COPY app/renv.lock renv.lock
COPY app/.Rprofile .Rprofile
COPY app/renv/activate.R renv/activate.R
COPY app/renv/settings.json renv/settings.json

# --- Restore R packages from lockfile ---
# arrow and plotly are now included in renv.lock alongside all other dependencies,
# so a single renv::restore() handles everything.
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE
ENV ARROW_WITH_SNAPPY=ON
ENV NOT_CRAN=true
RUN R -e 'renv::restore()'

# --- Copy the full app source ---
COPY app/ .

# --- Download Parquet data from HF Dataset repo ---
# The data is ignored by git, so we download it from the HF dataset repo during build.
# Dataset repo: https://huggingface.co/datasets/adumitrescu/powervision-data
RUN pip install --no-cache-dir --break-system-packages huggingface_hub && \
    python3 -c "from huggingface_hub import snapshot_download; snapshot_download(repo_id='adumitrescu/powervision-data', repo_type='dataset', local_dir='/srv/shiny-server/powervision/www/data', allow_patterns=['pecd/**'])" && \
    pip uninstall -y --break-system-packages huggingface_hub && \
    echo '✅ Parquet data downloaded'

# --- Custom Shiny Server configuration ---
COPY docker/shiny-server.conf /etc/shiny-server/shiny-server.conf

# --- Environment variables ---
ENV PECD_GEOJSON_VERSION=mixed

# --- Expose port ---
EXPOSE 3838

# --- Run Shiny Server ---
CMD ["/usr/bin/shiny-server"]

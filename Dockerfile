# ==============================================================================
# PowerClimate Vision Explorer — Hugging Face Spaces Dockerfile
# ==============================================================================
# This Dockerfile is used ONLY by Hugging Face Spaces (port 7860).
# The server deployment uses docker/Dockerfile (port 3838) instead.
# Do NOT modify docker/Dockerfile or docker-compose.yml — those are for the
# ECMWF server deployment via SSH tunnel.
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

# --- Copy the full app source ---
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

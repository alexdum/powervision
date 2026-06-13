# Deploy PowerClimate Vision Explorer — Docker Edition

## Server Context

| Item | Value |
|------|-------|
| **Host** | `ch14-vm01.ecmwf-code4earth.f.ewcloud.host` / `136.156.139.243` |
| **OS** | Rocky Linux 9 (fresh — nothing installed) |
| **SSH** | `ssh -i ~/.ssh/powervision adumitrescu@136.156.139.243` |
| **Root disk** | 29 GB (24 GB free) — OS + Docker |
| **Data disk** | 984 GB at `/data` (934 GB free) — app image layers, CDS downloads |
| **RAM** | ~16 GB |
| **Users** | `adumitrescu`, `vamihaesei` |

---

## User Review Required

> [!IMPORTANT]
> **Shiny Server Open Source runs a single R process per app.** All concurrent users share that one process — if one triggers a heavy chart render, *everyone* blocks. For your current workload (map + Parquet charts, ~5–15 concurrent users during presentations), this is likely fine because most interactions are client-side (MapLibre GL) and the server-side R work is fast Parquet reads.
>
> However, if you expect heavier concurrent use, consider **Option B (ShinyProxy)** below, which spins up **one Docker container per user session** — fully isolated, no blocking. It's also open-source and free.

> [!IMPORTANT]
> **Which architecture do you want?**
>
> | | **Option A: Shiny Server OS** | **Option B: ShinyProxy** |
> |---|---|---|
> | **How it works** | Single container runs Shiny Server, all users share one R process | ShinyProxy launches a fresh container per user session |
> | **Concurrency** | Limited — users can block each other | Unlimited (bounded by RAM) |
> | **RAM per user** | Shared (~400 MB total) | ~300–400 MB per active session |
> | **Max concurrent (16 GB)** | ~20–30 users (if workload is light) | ~30–40 containers (with 400 MB each) |
> | **Complexity** | Simple — one Dockerfile, one compose service | Moderate — needs Docker socket access, `application.yml` |
> | **Auth** | None (open access) | Built-in (LDAP, simple users, OpenID) |
> | **Best for** | Internal tool, few users, light interactions | Public-facing, many users, heavy computation |
>
> **This plan documents Option A (Shiny Server OS) as you requested.** I've included a section at the end showing how to upgrade to Option B later if needed.

---

## Directory Layout on `/data`

```
/data/
├── powervision/                    # Shared project root (group: powervision)
│   ├── docker/                     # Docker build context
│   │   ├── Dockerfile
│   │   ├── shiny-server.conf       # Shiny Server configuration
│   │   └── .dockerignore
│   ├── app/                        # Shiny app source (mounted into container)
│   │   ├── global.R
│   │   ├── server.R
│   │   ├── ui.R
│   │   ├── renv.lock
│   │   ├── renv/                   # renv bootstrap files only (activate.R, settings.json)
│   │   └── www/
│   │       ├── styles.css
│   │       ├── app.js
│   │       ├── about.md
│   │       └── data/
│   │           ├── geo/            # GeoJSON boundary files (~15 MB)
│   │           └── pecd/           # Processed Parquet climate data (~17 MB)
│   ├── logs/                       # Shiny Server logs (volume-mounted from container)
│   └── docker-compose.yml
│
├── cds/                            # CDS data downloads (group: powervision)
│   ├── raw/
│   │   ├── pecd_csv/
│   │   └── shapefiles/
│   ├── processed/
│   └── scripts/                    # Download & processing scripts
```

> [!NOTE]
> The app source lives in `/data/powervision/app/` on the host and is **baked into the Docker image** at build time (not bind-mounted). This ensures reproducibility — every container has exactly the same code and data. Only `logs/` is volume-mounted for persistence.

---

## App Size Inventory

| Component | Size | Notes |
|-----------|------|-------|
| `global.R`, `server.R`, `ui.R` | ~60 KB | Core Shiny files |
| `www/styles.css`, `app.js`, `about.md` | ~34 KB | Frontend assets |
| `www/data/geo/*.geojson` (10 files) | ~15 MB | Spatial boundaries |
| `www/data/pecd/historical/**/*.parquet` (10 files) | ~17 MB | Climate data |
| `renv.lock` | 188 KB | Package manifest |
| **Total deployable** | **~33 MB** | Baked into image |
| **Docker image (estimated)** | **~2–3 GB** | R + system libs + compiled packages |

---

## Phase 1 — Server Bootstrap

### 1.1 Create shared group and fix permissions

```bash
# Create the shared group
sudo groupadd powervision

# Add both users to the group
sudo usermod -aG powervision adumitrescu
sudo usermod -aG powervision vamihaesei

# Create project directories
sudo mkdir -p /data/powervision/{docker,app,logs}
sudo mkdir -p /data/cds/{raw/pecd_csv,raw/shapefiles,processed,scripts}

# Set ownership: root:powervision, group-writable, sticky-group
sudo chown -R root:powervision /data/powervision /data/cds
sudo chmod -R 2775 /data/powervision /data/cds
```

> [!IMPORTANT]
> After adding users to the group, each user must **log out and log back in** (or run `newgrp powervision`) for the group membership to take effect.

### 1.2 Install Docker Engine on Rocky Linux 9

```bash
# Add Docker's official repo
sudo dnf config-manager --add-repo https://download.docker.com/linux/rhel/docker-ce.repo

# Install Docker Engine + Compose plugin
sudo dnf install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin

# Start and enable Docker
sudo systemctl enable --now docker

# Add both users to the docker group (so they can run docker commands without sudo)
sudo usermod -aG docker adumitrescu
sudo usermod -aG docker vamihaesei

# Verify
docker --version
docker compose version
```

> [!IMPORTANT]
> Log out and back in after adding users to the `docker` group. Verify with `docker ps`.

### 1.3 Configure Docker storage on `/data`

The root disk is only 29 GB — Docker images and layers should live on `/data`:

```bash
# Create Docker data directory on the large disk
sudo mkdir -p /data/docker

# Configure Docker to use /data/docker as its root
sudo mkdir -p /etc/docker
cat <<EOF | sudo tee /etc/docker/daemon.json
{
  "data-root": "/data/docker"
}
EOF

# Restart Docker to pick up the new data root
sudo systemctl restart docker

# Verify
docker info | grep "Docker Root Dir"
# Should show: /data/docker
```

### 1.4 Install Python + CDS API (host-level, for data pipeline)

```bash
sudo dnf install -y python3 python3-pip git htop tmux

# Each user configures their CDS API key
pip3 install --user cdsapi
cat > ~/.cdsapirc << 'EOF'
url: https://cds.climate.copernicus.eu/api
key: <YOUR-CDS-API-KEY>
EOF
chmod 600 ~/.cdsapirc
```

> [!NOTE]
> R, system libraries, renv — none of these need to be installed on the host. Everything R-related lives inside the Docker container.

---

## Phase 2 — Transfer App Files

### 2.1 rsync from your Mac

Run **locally on your Mac**:

```bash
rsync -avz --progress \
  -e "ssh -i ~/.ssh/powervision" \
  --exclude='.git/' \
  --exclude='renv/library/' \
  --exclude='renv/staging/' \
  --exclude='renv/sandbox/' \
  --exclude='data/' \
  --exclude='new_csv/' \
  --exclude='pipeline/' \
  --exclude='porposal/' \
  --exclude='scratch/' \
  --exclude='tests/' \
  --exclude='.DS_Store' \
  --exclude='*.log' \
  --exclude='download_log.txt' \
  --exclude='.posit/' \
  --exclude='.pytest_cache/' \
  --exclude='.Rhistory' \
  --exclude='.RData' \
  --exclude='AGENTS.md' \
  --exclude='DEVELOPMENT_GUIDELINES.md' \
  --exclude='.rsconnectignore' \
  --exclude='.gitignore' \
  /Users/alexandrudumitrescu/Documents/clima/2026/powervision/ \
  adumitrescu@136.156.139.243:/data/powervision/app/
```

> [!NOTE]
> We keep `renv/activate.R` and `renv/settings.json` (needed for bootstrap) but exclude `renv/library/`, `renv/staging/`, and `renv/sandbox/` (these are platform-specific compiled packages from macOS — useless on Linux).

### 2.2 Transfer pipeline scripts separately

```bash
rsync -avz --progress \
  -e "ssh -i ~/.ssh/powervision" \
  --exclude='__pycache__/' \
  --exclude='*.log' \
  /Users/alexandrudumitrescu/Documents/clima/2026/powervision/pipeline/ \
  adumitrescu@136.156.139.243:/data/cds/scripts/
```

---

## Phase 3 — Docker Configuration Files

### 3.1 Dockerfile

Create `/data/powervision/docker/Dockerfile`:

```dockerfile
# ==============================================================================
# PowerClimate Vision Explorer — Docker Image
# Base: rocker/shiny (Ubuntu + R 4.5 + Shiny Server Open Source)
# ==============================================================================
FROM ghcr.io/rocker-org/shiny:4.5.0

# --- System dependencies for R package compilation ---
# Spatial stack (sf, terra, mapgl), text rendering (systemfonts, ragg),
# networking (curl, ssl), and Arrow/Parquet C++ build
RUN apt-get update && apt-get install -y --no-install-recommends \
    # Spatial
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libudunits2-dev \
    # Networking & web
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    # Text rendering (systemfonts, textshaping, ragg)
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    # Build toolchain
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

# --- Set working directory to Shiny Server's app directory ---
WORKDIR /srv/shiny-server/powervision

# --- Copy renv bootstrap files first (Docker layer caching) ---
# This layer is cached — packages only rebuild when renv.lock changes
COPY app/renv.lock renv.lock
COPY app/.Rprofile .Rprofile
COPY app/renv/activate.R renv/activate.R
COPY app/renv/settings.json renv/settings.json

# --- Restore R packages from lockfile ---
# Disable renv sandbox (causes issues in containers)
# This step compiles ~50+ packages from source — takes 20-40 min on first build
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE
RUN R -e 'renv::restore()'

# --- Copy the full app source ---
COPY app/ .

# --- Custom Shiny Server configuration ---
COPY docker/shiny-server.conf /etc/shiny-server/shiny-server.conf

# --- Set environment variables for the app ---
ENV PECD_GEOJSON_VERSION=40

# --- Expose Shiny Server port ---
EXPOSE 3838

# --- Run Shiny Server ---
CMD ["/usr/bin/shiny-server"]
```

### 3.2 Shiny Server Configuration

Create `/data/powervision/docker/shiny-server.conf`:

```conf
# ==============================================================================
# Shiny Server Open Source Configuration
# ==============================================================================
# Shiny Server OS runs ONE R process per application.
# All concurrent users share that single process.
# For this app, most heavy lifting is client-side (MapLibre GL), so this is
# adequate for moderate concurrent use (~10-20 users).
# ==============================================================================

# Run as the shiny user (default in rocker/shiny images)
run_as shiny;

# Preserve logs for debugging
preserve_logs true;

# Define the server
server {
  # Listen on port 3838
  listen 3838;

  # Serve the PowerClimate app at the root path
  location / {
    # The app lives here inside the container
    app_dir /srv/shiny-server/powervision;

    # Log directory (will be volume-mounted for persistence)
    log_dir /var/log/shiny-server;

    # Timeout: how long to wait for the app's R process to start (seconds)
    # The app pre-loads ~33 MB of GeoJSON + Parquet at startup, so give it time
    app_init_timeout 120;

    # Idle timeout: shut down the R process after this many seconds of inactivity
    # Set high to avoid cold-start delays during presentations
    app_idle_timeout 3600;

    # Connection timeout: allow WebSocket connections to stay open
    # Important for Shiny's reactive communication
    reconnect true;
  }
}
```

### 3.3 Docker Compose

Create `/data/powervision/docker-compose.yml`:

```yaml
# ==============================================================================
# PowerClimate Vision Explorer — Docker Compose
# ==============================================================================
# Single-service setup using Shiny Server Open Source.
# Logs are persisted via a volume mount.
# The app code + data are baked into the image for reproducibility.
# ==============================================================================

services:
  powervision:
    build:
      context: .
      dockerfile: docker/Dockerfile
    container_name: powervision-app
    restart: unless-stopped
    ports:
      - "3838:3838"
    volumes:
      # Persist Shiny Server logs to the host for debugging
      - ./logs:/var/log/shiny-server
    environment:
      - PECD_GEOJSON_VERSION=40
    # Resource limits to protect the host
    deploy:
      resources:
        limits:
          memory: 4G
          cpus: "4.0"
        reservations:
          memory: 1G
          cpus: "1.0"
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:3838/"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 120s
```

### 3.4 .dockerignore

Create `/data/powervision/docker/.dockerignore`:

```
# Don't send unnecessary files to the Docker build context
.git
app/renv/library
app/renv/staging
app/renv/sandbox
app/data
app/new_csv
app/pipeline
app/porposal
app/scratch
app/tests
app/.posit
app/.pytest_cache
**/.DS_Store
**/*.log
**/download_log.txt
**/.Rhistory
**/.RData
**/AGENTS.md
**/DEVELOPMENT_GUIDELINES.md
logs/*
```

---

## Phase 4 — Build & Run

### 4.1 Build the Docker image

```bash
cd /data/powervision

# Build the image (first build takes 20-40 min for R package compilation)
# Run inside tmux in case SSH disconnects
tmux new -s build
docker compose build

# Ctrl+B, then D to detach. Reconnect with: tmux attach -t build
```

> [!WARNING]
> **First build time**: The `renv::restore()` step compiles ~50+ R packages from source inside the container. The heaviest are `arrow` (~10 min), `sf` (~3 min), `plotly` (~2 min). Subsequent builds are **fast** (~30 seconds) thanks to Docker layer caching — packages only recompile when `renv.lock` changes.

### 4.2 Start the app

```bash
cd /data/powervision

# Start in detached mode
docker compose up -d

# Check logs
docker compose logs -f powervision

# Verify it's running
docker compose ps
```

### 4.3 Test access

**Option 1: SSH tunnel (no firewall changes needed)**

From your Mac:
```bash
ssh -i ~/.ssh/powervision -L 3838:localhost:3838 adumitrescu@136.156.139.243
```
Then open `http://localhost:3838` in your browser.

**Option 2: Direct access (requires port 3838 open in firewall)**

Open `http://136.156.139.243:3838/` in your browser.

---

## Phase 5 — Deploy Script (Repeatable Updates)

Save as `/data/powervision/deploy.sh` (or run from your Mac):

### 5.1 Local deploy script (run from Mac)

Save as `deploy.sh` in your local project root:

```bash
#!/bin/bash
# deploy.sh — Build and deploy PowerClimate to Docker on remote server
set -euo pipefail

REMOTE="adumitrescu@136.156.139.243"
KEY="$HOME/.ssh/powervision"
APP_DIR="/data/powervision/app/"
SCRIPTS_DIR="/data/cds/scripts/"
LOCAL_DIR="$(cd "$(dirname "$0")" && pwd)/"

echo "🚀 Step 1: Syncing app files to server..."
rsync -avz --progress \
  -e "ssh -i $KEY" \
  --exclude='.git/' --exclude='renv/library/' --exclude='renv/staging/' \
  --exclude='renv/sandbox/' --exclude='data/' \
  --exclude='new_csv/' --exclude='pipeline/' --exclude='porposal/' \
  --exclude='scratch/' --exclude='tests/' --exclude='.DS_Store' \
  --exclude='*.log' --exclude='download_log.txt' \
  --exclude='.posit/' --exclude='.pytest_cache/' \
  --exclude='.Rhistory' --exclude='.RData' \
  --exclude='AGENTS.md' --exclude='DEVELOPMENT_GUIDELINES.md' \
  --exclude='.rsconnectignore' --exclude='.gitignore' \
  "$LOCAL_DIR" "$REMOTE:$APP_DIR"

echo "🔧 Step 2: Syncing pipeline scripts..."
rsync -avz --progress \
  -e "ssh -i $KEY" \
  --exclude='__pycache__/' --exclude='*.log' \
  "${LOCAL_DIR}pipeline/" "$REMOTE:$SCRIPTS_DIR"

echo "🐳 Step 3: Rebuilding and restarting Docker container..."
ssh -i "$KEY" "$REMOTE" "cd /data/powervision && docker compose build && docker compose up -d"

echo "✅ Deploy complete. App available at http://136.156.139.243:3838/"
```

```bash
chmod +x deploy.sh
```

### 5.2 Quick code-only update (no package changes)

If you only changed R code or CSS/JS (not `renv.lock`), the rebuild is fast because Docker caches the package layer:

```bash
# From Mac
./deploy.sh
# The docker compose build step will skip the renv::restore() layer
# and only re-copy the app source — takes ~30 seconds
```

---

## Phase 6 — Operations & Monitoring

### 6.1 Useful commands

```bash
# View live logs
docker compose logs -f powervision

# Check container health
docker compose ps

# Restart the app (e.g., after config change)
docker compose restart powervision

# Full rebuild (e.g., after renv.lock changes)
docker compose build --no-cache && docker compose up -d

# Stop everything
docker compose down

# Check disk usage by Docker
docker system df

# Clean up old images
docker image prune -f
```

### 6.2 Log rotation

Shiny Server logs are mounted to `/data/powervision/logs/`. Add a logrotate config:

```bash
sudo cat > /etc/logrotate.d/powervision << 'EOF'
/data/powervision/logs/*.log {
    weekly
    rotate 4
    compress
    missingok
    notifempty
    copytruncate
}
EOF
```

### 6.3 Auto-restart on reboot

The `restart: unless-stopped` in `docker-compose.yml` combined with Docker's systemd service ensures the container starts on boot:

```bash
# Verify Docker starts on boot
sudo systemctl is-enabled docker
# Should say: enabled
```

---

## Upgrading to ShinyProxy (Option B) — Future Reference

If you later find that Shiny Server OS is too limiting for concurrent users, here's how to switch to ShinyProxy. **You don't need to do this now** — it's here for reference.

### What changes

1. The Shiny app Dockerfile changes slightly: instead of bundling Shiny Server, it just runs `shiny::runApp()` directly
2. A new `application.yml` configures ShinyProxy
3. ShinyProxy runs as its own container and launches app containers on demand

### Modified Dockerfile (no Shiny Server)

```dockerfile
FROM ghcr.io/rocker-org/r-ver:4.5.0

# (same system dependencies as above)

WORKDIR /app
COPY app/renv.lock renv.lock
COPY app/.Rprofile .Rprofile
COPY app/renv/activate.R renv/activate.R
COPY app/renv/settings.json renv/settings.json

ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE
RUN R -e 'install.packages("renv", repos="https://cloud.r-project.org")' && \
    R -e 'renv::restore()'

COPY app/ .

ENV PECD_GEOJSON_VERSION=40
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
```

### ShinyProxy application.yml

```yaml
proxy:
  title: PowerClimate Vision Explorer
  port: 8080
  authentication: none    # or "simple" with users below
  docker:
    internal-networking: true
  specs:
    - id: powervision
      display-name: PowerClimate Vision Explorer
      container-image: powervision-app:latest
      container-network: powervision-net
      port: 3838
```

### ShinyProxy docker-compose.yml

```yaml
services:
  shinyproxy:
    image: openanalytics/shinyproxy:3.1.1
    container_name: shinyproxy
    restart: unless-stopped
    ports:
      - "8080:8080"
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
      - ./application.yml:/opt/shinyproxy/application.yml
    networks:
      - powervision-net

networks:
  powervision-net:
    name: powervision-net
```

---

## Open Questions

> [!IMPORTANT]
> **Docker access**: Do both users have `sudo`? Phase 1 requires it to install Docker and create groups. After setup, `docker` commands work without sudo for users in the `docker` group.

> [!IMPORTANT]
> **Firewall / port 3838**: Is the ECMWF cloud security group configured to allow inbound TCP on port 3838? If not, we'll use SSH tunneling and request the port opening from admin.

> [!NOTE]
> **Container resource limits**: The compose file sets 4 GB RAM / 4 CPUs per container. Your app pre-loads ~33 MB of data at startup and the main memory consumer is R + loaded packages (~300–400 MB). 4 GB is generous — adjust if you want to leave more room for CDS processing jobs.

> [!NOTE]
> **CDS download volume**: How large do you expect the raw CDS downloads to be? The `/data` disk has 934 GB free. Docker images/layers will use ~3–5 GB of that.

> [!NOTE]
> **SSH key for vamihaesei**: Does the second user already have SSH access to the server, or does that need to be set up?

> [!NOTE]
> **Shiny Server OS vs. ShinyProxy**: The plan implements Option A (Shiny Server OS) as requested. If you need per-user isolation or handle 20+ concurrent users doing heavy charting, Option B (ShinyProxy) is a drop-in upgrade documented above.

---

## Verification Plan

### Smoke Test
1. SSH in → `cd /data/powervision` → `docker compose up -d`
2. Check `docker compose logs -f powervision` for startup messages — all `[OK]` lines for GeoJSON and Parquet files
3. Open browser (direct or via SSH tunnel) → verify map loads, layers switch, click-to-select works, charts render

### Docker Health
- [ ] `docker compose ps` shows `healthy` status
- [ ] `docker compose logs` shows no errors
- [ ] Container restarts successfully after `docker compose restart`
- [ ] Container survives host reboot (`restart: unless-stopped`)

### File Integrity (inside container)
```bash
docker compose exec powervision ls -la /srv/shiny-server/powervision/www/data/geo/
# Should show 10 .geojson files

docker compose exec powervision ls -la /srv/shiny-server/powervision/www/data/pecd/historical/annual/
# Should show 5 .parquet files

docker compose exec powervision ls -la /srv/shiny-server/powervision/www/data/pecd/historical/seasonal/
# Should show 5 .parquet files
```

### Multi-User
- [ ] Both users can run `docker compose` commands (in the `docker` group)
- [ ] Both users can write to `/data/powervision/` and `/data/cds/`
- [ ] Both users can run `./deploy.sh` from their Mac
- [ ] CDS API works for both users on the host (`python3 -c "import cdsapi; print('OK')"`)

### Concurrency Test
- [ ] Open the app in 3 browser tabs simultaneously
- [ ] Click different regions in each tab — all respond independently
- [ ] Switch basemaps in one tab — others are unaffected

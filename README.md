![Workflow Status](https://github.com/PacificCommunity/ofp-sam-2026-BET/actions/workflows/test-and-build.yml/badge.svg)

# ofp-sam-2026-BET

This repository is a work in progress for the 2026 Bigeye assessment workflow, including the model run and plotting pipeline.

## Quick start

This setup is still a work in progress and has not yet been widely tested across platforms.

Workshop participants should have Docker available and running. On Windows and macOS, this usually means Docker Desktop. On Linux, Docker Engine or Docker Desktop should be installed, and the Docker service should be running. A Docker login is not usually required unless image access is restricted. R is recommended, and `make` is required for the shortcut commands below.

On Linux, if Docker is installed but not running, you may need to start it first:

```bash
sudo systemctl start docker
sudo systemctl status docker
```

Run the model in Docker:

```bash
make docker-run
```

Run the Shiny apps in Docker:

```bash
make docker-shiny_plot
make docker-shiny_launcher
```

Run both Shiny apps in the background:

```bash
make docker-shiny_bg
```

Docker Shiny apps read files from the local repository through the mounted workspace.

Then open:

- `http://127.0.0.1:3838` for `shiny_plot`
- `http://127.0.0.1:3839` for `shiny_launcher`

Useful commands:

```bash
make docker-shiny_status
make docker-shiny_stop
make shiny_stop
```

If port `3838` or `3839` is already in use, stop existing apps first or use a different port, for example:

```bash
make docker-shiny_plot SHINY_PLOT_PORT=38380
```

Windows users can run the same commands from Git Bash, WSL, or another shell that provides `make`, with Docker Desktop open and running. macOS users should also have Docker Desktop open and running. Linux users should run the commands from a shell with `make` after starting Docker.

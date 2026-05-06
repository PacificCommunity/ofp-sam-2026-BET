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

Run a one-replicate MFCL simulation-estimation self-test:

```bash
make selftest_build
make selftest SELFTEST_REPS="1" SELFTEST_REFIT_FEVALS=500
```

The self-test writes replicate simulation folders, pseudo-input folders, optional refits, and recovery CSVs under `selftest/`. By default it uses `mfcl/inputs/2023_4region` and the latest `.par` in that folder; override with `SELFTEST_BASE_DIR=...` and `SELFTEST_SOURCE_PAR=...`.

For a quick end-to-end smoke test, use a small refit budget such as `SELFTEST_REFIT_FEVALS=20`. In the Shiny launcher, the `Self-Test` job type expands replicate IDs into separate jobs: 100 replicates submit as 100 one-replicate simulate/refit jobs. In `shiny_plot`, load the self-test `refit` folder and open the `Self-Test` tab to compare truth against refit trajectories; depletion is the default, with truth in red and the refit median overlaid across replicates.

The pseudo-input builder replaces catch, length, weight, CPUE, tag, and age-length observations. Length/weight, catch, and CPUE are generated on the historical input so the ordering and sample sizes match the estimation likelihood. Age-length is regenerated from MFCL `agelengthresids.dat` predicted age-at-length probabilities while preserving each length-bin sample size and the input ESS. Historical tag data must come from MFCL `sim_realtag`: `parest_flags(241)=1` activates pseudo-observations, `parest_flags(242)=1` activates estimation-period real tag simulation, and the seed is read from `simseed`. The command-line `-tag_seed` option is for virtual projection tags (`sim_tag`). MFCL writes `report.realtag_#` inside the numbered stochastic simulation loop. With `age_flag(20)=0`, MFCL can still write size/catch/CPUE pseudo files, but it does not write `report.realtag_#`; with `age_flag(20)>0`, the stochastic projection input files `simulated_numbers_at_age`, `simulated_numbers_at_age_noeff`, and `simyears` must already be present. The runner therefore first creates a long projection input, runs MFCL stochastic setup and native `sim_realtag`, then reruns the historical pseudo-data simulation for the non-tag likelihood components. The projection horizon defaults to `max(30, age classes)` years so historical tag releases can age through MFCL's `sim_realtag` bookkeeping; override with `SELFTEST_NATIVE_TAG_PROJECTION_YEARS=...`. Tag data are only replaced from MFCL-native `report.realtag_#`; if MFCL does not produce that file the run stops by default rather than silently resampling tags.

If port `3838` or `3839` is already in use, stop existing apps first or use a different port, for example:

```bash
make docker-shiny_plot SHINY_PLOT_PORT=38380
```

Windows users can run the same commands from Git Bash, WSL, or another shell that provides `make`, with Docker Desktop open and running. macOS users should also have Docker Desktop open and running. Linux users should run the commands from a shell with `make` after starting Docker.

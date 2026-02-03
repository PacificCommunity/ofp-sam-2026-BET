# =========================================================================
# BET 2026 Assessment Workflow
#
# Primary workflow:
# 1. Edit config.R to set ACTIVE_MODELS and other parameters
# 2. Run 'make <target>' to submit jobs or fetch results
# =========================================================================

# Docker image and working directory
DOCKER_IMAGE=ghcr.io/pacificcommunity/bet-2026:v1.2
WORKDIR=/workspace

# -----------------------------------------------------------------------
# Job Submission (Condor)
# -----------------------------------------------------------------------
# These targets behave according to settings in config.R (e.g., ACTIVE_MODELS)
# Use 'make <target> local=true' to run locally instead of submitting

launch_args = $(if $(local),--local,)

model:
	Rscript launch.R model $(launch_args)

hessian:
	Rscript launch.R hessian $(launch_args)

prof:
	Rscript launch.R prof $(launch_args)

jitter:
	Rscript launch.R jitter $(launch_args)

all-jobs:
	Rscript launch.R all $(launch_args)

# -----------------------------------------------------------------------
# Results Management
# -----------------------------------------------------------------------

# Fetch results from Condor (requires remote_dir argument)
# Usage: make fetch remote_dir=develop/Feb_03_2026_model
fetch:
	@if [ -z "$(remote_dir)" ]; then \
		echo "Error: remote_dir not specified"; \
		echo "Usage: make fetch remote_dir=develop/Feb_03_2026_model"; \
		exit 1; \
	fi
	Rscript scripts/fetch_results.R $(remote_dir)

# Collate Hessian results (runs locally after fetch)
collate-hessian:
	Rscript scripts/collate_hessian.R
	Rscript scripts/collate_hessian_mfcl.R

# -----------------------------------------------------------------------
# Reporting & Visualization
# -----------------------------------------------------------------------

plot:
	Rscript -e "rmarkdown::render('plot/plots.rmd')"

report:
	quarto render report/bet-2026.qmd

presentation:
	quarto render presentation/prepaw/presentation.qmd

# -----------------------------------------------------------------------
# Docker Helpers
# -----------------------------------------------------------------------
# Run R scripts inside the docker container locally

docker-run:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript scripts/run_model.R

.PHONY: model hessian prof jitter all-jobs fetch collate-hessian plot report presentation docker-run

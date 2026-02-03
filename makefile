# Docker image and working directory
DOCKER_IMAGE=ghcr.io/pacificcommunity/bet-2026:v1.2
WORKDIR=/workspace

# Quick local tests using test_local.R
test-model:
	Rscript test_local.R model

test-hessian:
	Rscript test_local.R hessian

test-prof:
	Rscript test_local.R prof 100

test-jitter:
	Rscript test_local.R jitter

# Local runs using launch.R --local flag
local-model:
	Rscript launch.R model --local

local-hessian:
	Rscript launch.R hessian --local

local-prof:
	Rscript launch.R prof --local

local-jitter:
	Rscript launch.R jitter --local

# Fetch results from Condor (requires remote_dir argument)
# Usage: make fetch remote_dir=develop/Feb_03_2026_model
fetch:
	@if [ -z "$(remote_dir)" ]; then \
		echo "Error: remote_dir not specified"; \
		echo "Usage: make fetch remote_dir=develop/Feb_03_2026_model"; \
		exit 1; \
	fi
	Rscript fetch_results.R $(remote_dir)

# Regular targets
model:
	Rscript scripts/run_model.R

prof:
	Rscript scripts/run_prof.R

jitter:
	Rscript scripts/run_jitter.R

hessian:
	Rscript scripts/run_hessian.R

collate-hessian:
	Rscript collate_hessian.R

stitch-hessian:
	Rscript collate_hessian_mfcl.R

run: model prof
	
plot:
	Rscript -e "rmarkdown::render('plot/plots.rmd')"

prepaw:
	quarto render presentation/prepaw/presentation.qmd
	
report:
	quarto render report/bet-2026.qmd

docker-model:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript run_model.R

docker-prof:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) -e scaler=$(scaler) $(DOCKER_IMAGE) Rscript run_prof.R

docker-jitter:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) -e jitter_seed=$(jitter_seed) -e jitter_cv=$(jitter_cv) $(DOCKER_IMAGE) Rscript run_jitter.R

docker-hessian:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) -e hessian_part=$(hessian_part) -e nsplit=$(nsplit) $(DOCKER_IMAGE) Rscript run_hessian.R

docker-collate-hessian:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript collate_hessian.R

docker-stitch-hessian:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript collate_hessian_mfcl.R

docker-run: docker-model docker-prof
	
docker-plot:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "rmarkdown::render('plot/plots.rmd')"

docker-report:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(stitch-hessian docker-run docker-model docker-prof docker-jitter docker-hessian docker-collate-hessian docker-stitch

	
.PHONY: plot run model prof jitter hessian collate-hessian docker-run docker-model docker-prof docker-jitter docker-hessian docker-collate-hessian docker-plot prepaw report docker-report


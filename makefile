# Docker image and working directory
DOCKER_IMAGE=ghcr.io/pacificcommunity/bet-2026:v1.5
WORKDIR=/workspace

model:
	Rscript runners/run_model.R

prof:
	Rscript runners/run_prof.R

jitter:
	Rscript runners/run_jitter.R

hessian:
	Rscript runners/run_hessian.R

retro:
	Rscript runners/run_retro.R

collate-hessian:
	Rscript tools/collate_hessian.R

stitch-hessian:
	Rscript tools/collate_hessian_mfcl.R

run: model prof
	
plot:
	Rscript -e "rmarkdown::render('plot/plots.rmd')"

prepaw:
	quarto render presentation/prepaw/presentation.qmd
	
report:
	quarto render report/bet-2026.qmd

docker-model:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_model.R

docker-prof:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_prof.R

docker-jitter:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) -e jitter_seed=$(jitter_seed) -e jitter_cv=$(jitter_cv) $(DOCKER_IMAGE) Rscript runners/run_jitter.R

docker-hessian:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) -e hessian_part=$(hessian_part) -e nsplit=$(nsplit) $(DOCKER_IMAGE) Rscript runners/run_hessian.R

docker-retro:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) -e retro_peel=$(retro_peel) $(DOCKER_IMAGE) Rscript runners/run_retro.R

docker-collate-hessian:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript collate_hessian.R

docker-stitch-hessian:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript collate_hessian_mfcl.R

docker-run: docker-model docker-prof
	
docker-plot:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "rmarkdown::render('plot/plots.rmd')"

docker-report:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(stitch-hessian docker-run docker-model docker-prof docker-jitter docker-hessian docker-collate-hessian docker-stitch

	
.PHONY: plot run model prof jitter hessian retro collate-hessian docker-run docker-model docker-prof docker-jitter docker-hessian docker-retro docker-collate-hessian docker-plot prepaw report docker-report



# =============================================================================
# SHINY APP TARGETS
# =============================================================================

# Launch Shiny app locally
app:
	Rscript -e "shiny::runApp('shiny', launch.browser=TRUE)"

# Launch Shiny app in Docker
docker-app:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) -p 3838:3838 $(DOCKER_IMAGE) \
	Rscript -e "shiny::runApp('shiny', host='0.0.0.0', port=3838)"

# Launch Shiny app in background (detached)
app-bg:
	Rscript -e "shiny::runApp('shiny', launch.browser=TRUE)" &

# Stop background Shiny app
app-stop:
	pkill -f "shiny::runApp"

.PHONY: app docker-app app-bg app-stop


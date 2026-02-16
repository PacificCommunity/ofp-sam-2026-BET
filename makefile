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
	Rscript tools/collate_hessian_mfcl.R

stitch-hessian:
	Rscript tools/collate_hessian_mfcl.R

stitch-hessian-all:
	Rscript tools/collate_hessian_mfcl.R --all

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
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_jitter.R

docker-hessian:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_hessian.R

docker-retro:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_retro.R

docker-collate-hessian:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript tools/collate_hessian_mfcl.R

docker-stitch-hessian:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript tools/collate_hessian_mfcl.R

docker-stitch-hessian-all:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript tools/collate_hessian_mfcl.R --all

docker-run: docker-model docker-prof
	
docker-plot:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "rmarkdown::render('plot/plots.rmd')"

docker-report:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(stitch-hessian docker-run docker-model docker-prof docker-jitter docker-hessian docker-collate-hessian docker-stitch

	
.PHONY: plot run model prof jitter hessian retro collate-hessian stitch-hessian stitch-hessian-all docker-run docker-model docker-prof docker-jitter docker-hessian docker-retro docker-collate-hessian docker-stitch-hessian docker-stitch-hessian-all docker-plot prepaw report docker-report



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

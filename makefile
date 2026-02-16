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

shiny_plot:
	Rscript -e "shiny::runApp('plot/shiny_plot', launch.browser=TRUE)"

shiny_launcher:
	Rscript -e "shiny::runApp('launchers/shiny_launcher', launch.browser=TRUE)"

.PHONY: shiny_launcher shiny_plot

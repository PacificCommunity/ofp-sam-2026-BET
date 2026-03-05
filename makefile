# Docker image and working directory
DOCKER_IMAGE=ghcr.io/pacificcommunity/bet-2026:v1.5
WORKDIR=/workspace
DOCKER_USER=$(shell id -u):$(shell id -g)

model:
	Rscript runners/run_model.R

prof:
	Rscript runners/run_prof.R

jitter:
	Rscript runners/run_jitter.R

jitter_smoke:
	jitter_smoke_only=1 Rscript runners/run_jitter.R

jitter_smoke_hessian:
	jitter_smoke_only=1 jitter_hessian=1 jitter_smoke_hessian=1 Rscript runners/run_jitter.R

hessian:
	Rscript runners/run_hessian.R

retro:
	Rscript runners/run_retro.R
	
TagExclusion:
	Rscript sensitivities/TagExclusion.R

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
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_model.R

docker-prof:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_prof.R

docker-jitter:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_jitter.R

docker-jitter-smoke:
	docker run --rm --user "$(DOCKER_USER)" -e jitter_smoke_only=1 -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_jitter.R

docker-jitter-smoke-hessian:
	docker run --rm --user "$(DOCKER_USER)" -e jitter_smoke_only=1 -e jitter_hessian=1 -e jitter_smoke_hessian=1 -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_jitter.R

docker-hessian:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_hessian.R

docker-retro:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript runners/run_retro.R

docker-collate-hessian:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript tools/collate_hessian_mfcl.R

docker-stitch-hessian:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript tools/collate_hessian_mfcl.R

docker-stitch-hessian-all:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript tools/collate_hessian_mfcl.R --all

docker-run: docker-model docker-prof
	
docker-plot:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "rmarkdown::render('plot/plots.rmd')"

docker-report:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(stitch-hessian docker-run docker-model docker-prof docker-jitter docker-hessian docker-collate-hessian docker-stitch

	
.PHONY: plot run model prof jitter jitter_smoke jitter_smoke_hessian hessian retro collate-hessian stitch-hessian stitch-hessian-all docker-run docker-model docker-prof docker-jitter docker-jitter-smoke docker-jitter-smoke-hessian docker-hessian docker-retro docker-collate-hessian docker-stitch-hessian docker-stitch-hessian-all docker-plot prepaw report docker-report



# =============================================================================
# SHINY APP TARGETS
# =============================================================================

SHINY_PLOT_PORT ?= 3838
SHINY_LAUNCHER_PORT ?= 3839
SHINY_LOG_DIR ?= logs

shiny_plot:
	Rscript -e "shiny::runApp('plot/shiny_plot', launch.browser=TRUE)"

shiny_launcher:
	Rscript -e "shiny::runApp('launchers/shiny_launcher', launch.browser=TRUE)"

shiny_plot_bg:
	mkdir -p $(SHINY_LOG_DIR)
	nohup Rscript -e "shiny::runApp('plot/shiny_plot', host='127.0.0.1', port=$(SHINY_PLOT_PORT), launch.browser=FALSE)" > $(SHINY_LOG_DIR)/shiny_plot.log 2>&1 &
	@echo "shiny_plot running at http://127.0.0.1:$(SHINY_PLOT_PORT)"
	@echo "log: $(SHINY_LOG_DIR)/shiny_plot.log"

shiny_launcher_bg:
	mkdir -p $(SHINY_LOG_DIR)
	nohup Rscript -e "shiny::runApp('launchers/shiny_launcher', host='127.0.0.1', port=$(SHINY_LAUNCHER_PORT), launch.browser=FALSE)" > $(SHINY_LOG_DIR)/shiny_launcher.log 2>&1 &
	@echo "shiny_launcher running at http://127.0.0.1:$(SHINY_LAUNCHER_PORT)"
	@echo "log: $(SHINY_LOG_DIR)/shiny_launcher.log"

shiny_bg: shiny_plot_bg shiny_launcher_bg

shiny_plot_stop:
	-pkill -f "shiny::runApp\\('plot/shiny_plot'" || true
	@echo "stopped shiny_plot (if running)"

shiny_launcher_stop:
	-pkill -f "shiny::runApp\\('launchers/shiny_launcher'" || true
	@echo "stopped shiny_launcher (if running)"

shiny_stop: shiny_plot_stop shiny_launcher_stop

shiny_status:
	-ps -ef | grep "shiny::runApp" | grep -v grep || true

.PHONY: shiny_launcher shiny_plot shiny_plot_bg shiny_launcher_bg shiny_bg shiny_plot_stop shiny_launcher_stop shiny_stop shiny_status

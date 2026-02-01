# Docker image and working directory
DOCKER_IMAGE=ghcr.io/pacificcommunity/bet-2026:v1.2
WORKDIR=/workspace

model:
	Rscript run_model.R

prof:
	Rscript run_prof.R

jitter:
	Rscript run_jitter.R

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
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) -e jitter_seed=$(jitter_seed) -e jitter_amount=$(jitter_amount) $(DOCKER_IMAGE) Rscript run_jitter.R

docker-run: docker-model docker-prof
	
docker-plot:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "rmarkdown::render('plot/plots.rmd')"

docker-report:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) quarto render report/bet-2026.qmd

	
.PHONY: plot run model prof jitter docker-run docker-model docker-prof docker-jitter docker-plot prepaw report docker-report


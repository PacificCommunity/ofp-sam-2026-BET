# Docker image and working directory
DOCKER_IMAGE=ghcr.io/pacificcommunity/bet-2026:v1.1
WORKDIR=/workspace

run:
	Rscript run_mfcl.R
	
plot: plot/plots.rmd
	Rscript -e "rmarkdown::render('plot/plots.rmd')"

docker-run:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript run_mfcl.R

clean-mfcl:
	@echo "Cleaning up base/2023 folder, keeping only: doitall.sh mfcl.cfg mfclo64 bet.age_length bet.frq bet.ini bet.tag"
	@if [ -d "base/2023" ]; then \
		cd base/2023 && \
		find . -maxdepth 1 -type f ! -name "doitall.sh" ! -name "mfcl.cfg" ! -name "mfclo64" ! -name "bet.age_length" ! -name "bet.frq" ! -name "bet.ini" ! -name "bet.tag" ! -name "11.par" -delete; \
	else \
		echo "base/2023 directory not found"; \
	fi
	@echo "base/2023 cleanup completed"

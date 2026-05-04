# Docker image and working directory
DOCKER_IMAGE=ghcr.io/pacificcommunity/bet-2026:v1.9
WORKDIR=/workspace
DOCKER_USER=$(shell id -u):$(shell id -g)
INDEX_CSV ?= $(CURDIR)/mfcl/bet.2023.indices.4-region.csv
MFCL_EXE ?= $(CURDIR)/mfcl/exe/mfclo64_2026
MFCL_EXE_REL ?= $(patsubst $(CURDIR)/%,%,$(MFCL_EXE))
REGION4_INPUT_DIR ?= mfcl/inputs/2023_rep
REGION4_OUTPUT_DIR ?= mfcl/inputs/2023_4region
REGION4_MERGE_OUTPUT_DIR ?= mfcl/inputs/2023_4region_merge
REGION4_RELEASE_REGIONS_EXCLUDED ?= 9
REGION4_MODEL_DIR ?= model/2023R4
REGION4_MERGE_MODEL_DIR ?= model/2023R4_merge
REGION4_VARIANT_BASE_DIR ?= $(REGION4_OUTPUT_DIR)
REGION4_FIXVB_INPUT_DIR ?= mfcl/inputs/2023_fixVB
REGION4_FIXVB_OUTPUT_DIR ?= mfcl/inputs/2023_4region_fixVB
REGION4_FIXVB_M_INPUT_DIR ?= mfcl/inputs/2023_fixVB_M
REGION4_FIXVB_M_OUTPUT_DIR ?= mfcl/inputs/2023_4region_fixVB_M
REGION4_FIXM_INPUT_DIR ?= mfcl/inputs/2023_fixM
REGION4_FIXM_OUTPUT_DIR ?= mfcl/inputs/2023_4region_fixM
REGION4_FIXVB_M_MODEL_DIR ?= model/2023R4_fixVB_M
REGION4_FIXM_MODEL_DIR ?= model/2023R4_fixM
INPUT_CONFIG ?= configs/2023R4.R
INPUT_RECIPE_BASE ?= base
INPUT_RECIPE_OUTPUT_DIR ?=
INPUT_RECIPE_MOVEMENT_PAIRS ?=
INPUT_RECIPE_SEL_NODES ?=
INPUT_RECIPE_INDEX_CV_HALF ?= 0

build-4region:
	Rscript tools/collapse_regions_9to4.R \
		--input-dir $(REGION4_INPUT_DIR) \
		--output-dir $(REGION4_OUTPUT_DIR) \
		--index-csv $(INDEX_CSV) \
		--index-comp-mode representative \
		--overwrite
	tmpdir=$$(mktemp -d); \
	base_dir=$(REGION4_OUTPUT_DIR) \
	release_region_source_dir=$(REGION4_INPUT_DIR) \
	release_regions=$(REGION4_RELEASE_REGIONS_EXCLUDED) \
	out_dir=$$tmpdir \
	Rscript tools/apply_release_region_exclusion.R; \
	rm -rf $(REGION4_OUTPUT_DIR); \
	mkdir -p $(REGION4_OUTPUT_DIR); \
	cp -a $$tmpdir/. $(REGION4_OUTPUT_DIR)/; \
	rm -rf $$tmpdir

build-4region-merge:
	Rscript tools/collapse_regions_9to4.R \
		--input-dir $(REGION4_INPUT_DIR) \
		--output-dir $(REGION4_MERGE_OUTPUT_DIR) \
		--index-csv $(INDEX_CSV) \
		--index-comp-mode merge \
		--overwrite
	tmpdir=$$(mktemp -d); \
	base_dir=$(REGION4_MERGE_OUTPUT_DIR) \
	release_region_source_dir=$(REGION4_INPUT_DIR) \
	release_regions=$(REGION4_RELEASE_REGIONS_EXCLUDED) \
	out_dir=$$tmpdir \
	Rscript tools/apply_release_region_exclusion.R; \
	rm -rf $(REGION4_MERGE_OUTPUT_DIR); \
	mkdir -p $(REGION4_MERGE_OUTPUT_DIR); \
	cp -a $$tmpdir/. $(REGION4_MERGE_OUTPUT_DIR)/; \
	rm -rf $$tmpdir

build-4region-11par: build-4region
	Rscript tools/collapse_par_9to4_representative.R \
		--source-dir $(REGION4_INPUT_DIR) \
		--target-dir $(REGION4_OUTPUT_DIR) \
		--program-path $(MFCL_EXE) \
		--index-csv $(INDEX_CSV) \
		--overwrite

build-4region-variant: build-4region-11par
	@if [ -z "$(SOURCE_DIR)" ] || [ -z "$(OUTPUT_DIR)" ]; then echo "Usage: make build-4region-variant SOURCE_DIR=... OUTPUT_DIR=..."; exit 1; fi
	rm -rf $(OUTPUT_DIR)
	mkdir -p $(OUTPUT_DIR)
	cp -a $(REGION4_VARIANT_BASE_DIR)/. $(OUTPUT_DIR)/
	tmpdir=$$(mktemp -d); filtered_tmpdir=$$(mktemp -d); \
	Rscript tools/collapse_regions_9to4.R \
		--input-dir $(SOURCE_DIR) \
		--output-dir $$tmpdir \
		--index-csv $(INDEX_CSV) \
		--index-comp-mode representative \
		--overwrite; \
	base_dir=$$tmpdir \
	release_region_source_dir=$(SOURCE_DIR) \
	release_regions=$(REGION4_RELEASE_REGIONS_EXCLUDED) \
	out_dir=$$filtered_tmpdir \
	Rscript tools/apply_release_region_exclusion.R; \
	cp -a $$filtered_tmpdir/. $(OUTPUT_DIR)/; \
	rm -rf $$tmpdir $$filtered_tmpdir
	@for extra in program_exclusion_info.rds release_group_exclusion_info.rds; do \
		if [ -f "$(SOURCE_DIR)/$$extra" ]; then cp "$(SOURCE_DIR)/$$extra" "$(OUTPUT_DIR)/$$extra"; else rm -f "$(OUTPUT_DIR)/$$extra"; fi; \
	done

build-4region-variant-11par: build-4region-variant
	@if [ -z "$(SOURCE_DIR)" ] || [ -z "$(OUTPUT_DIR)" ]; then echo "Usage: make build-4region-variant-11par SOURCE_DIR=... OUTPUT_DIR=..."; exit 1; fi
	Rscript tools/collapse_par_9to4_representative.R \
		--source-dir $(SOURCE_DIR) \
		--target-dir $(OUTPUT_DIR) \
		--program-path $(MFCL_EXE) \
		--index-csv $(INDEX_CSV) \
		--overwrite

build-4region-fixVB:
	$(MAKE) build-4region-variant SOURCE_DIR=$(REGION4_FIXVB_INPUT_DIR) OUTPUT_DIR=$(REGION4_FIXVB_OUTPUT_DIR)

build-4region-fixVB-11par:
	$(MAKE) build-4region-variant-11par SOURCE_DIR=$(REGION4_FIXVB_INPUT_DIR) OUTPUT_DIR=$(REGION4_FIXVB_OUTPUT_DIR)

build-4region-fixVB_M:
	$(MAKE) build-4region-variant SOURCE_DIR=$(REGION4_FIXVB_M_INPUT_DIR) OUTPUT_DIR=$(REGION4_FIXVB_M_OUTPUT_DIR)

build-4region-fixVB_M-11par:
	$(MAKE) build-4region-variant-11par SOURCE_DIR=$(REGION4_FIXVB_M_INPUT_DIR) OUTPUT_DIR=$(REGION4_FIXVB_M_OUTPUT_DIR)

build-4region-fixM:
	$(MAKE) build-4region-variant SOURCE_DIR=$(REGION4_FIXM_INPUT_DIR) OUTPUT_DIR=$(REGION4_FIXM_OUTPUT_DIR)

build-4region-fixM-11par:
	$(MAKE) build-4region-variant-11par SOURCE_DIR=$(REGION4_FIXM_INPUT_DIR) OUTPUT_DIR=$(REGION4_FIXM_OUTPUT_DIR)

build-input-recipe:
	@if [ -z "$(INPUT_RECIPE_OUTPUT_DIR)" ]; then echo "Usage: make build-input-recipe INPUT_RECIPE_OUTPUT_DIR=mfcl/inputs/... [INPUT_RECIPE_BASE=base|fixM|fixVB|fixVB_M]"; exit 1; fi
	input_recipe_index_cv_half=$(INPUT_RECIPE_INDEX_CV_HALF) \
	Rscript tools/build_4region_input_recipe.R \
		--output-dir $(INPUT_RECIPE_OUTPUT_DIR) \
		--base $(INPUT_RECIPE_BASE) \
		--movement-pairs "$(INPUT_RECIPE_MOVEMENT_PAIRS)" \
		--sel-nodes "$(INPUT_RECIPE_SEL_NODES)" \
		--with-11par \
		--overwrite

build-config-inputs:
	Rscript tools/build_config_inputs.R --config $(INPUT_CONFIG) --overwrite

build-4region-all-inputs:
	Rscript tools/build_config_inputs.R --config configs/2023R4.R --overwrite

makepar-4region: build-4region
	cd $(REGION4_OUTPUT_DIR) && $(MFCL_EXE) bet.frq bet.ini 00.par -makepar

makepar-4region-merge: build-4region-merge
	cd $(REGION4_MERGE_OUTPUT_DIR) && $(MFCL_EXE) bet.frq bet.ini 00.par -makepar

run-4region: build-4region-11par
	program_path=$(MFCL_EXE_REL) \
	base_dir=$(REGION4_OUTPUT_DIR) \
	model_dir=$(REGION4_MODEL_DIR) \
	description="2023 4-region representative quick test" \
	config_summary="9->4 representative; 11.par collapsed from 2023_rep" \
	Rscript runners/run_model.R

run-4region-merge: build-4region-merge
	program_path=$(MFCL_EXE_REL) \
	base_dir=$(REGION4_MERGE_OUTPUT_DIR) \
	model_dir=$(REGION4_MERGE_MODEL_DIR) \
	mfcl_commands="./doitall.sh" \
	description="2023 4-region merge run" \
	config_summary="9->4 merge from 2023_rep" \
	Rscript runners/run_model.R

run-4region-fixM: build-4region-fixM-11par
	program_path=$(MFCL_EXE_REL) \
	base_dir=$(REGION4_FIXM_OUTPUT_DIR) \
	model_dir=$(REGION4_FIXM_MODEL_DIR) \
	description="2023 4-region fixM input" \
	config_summary="9->4 representative; 11.par collapsed from 2023_fixM" \
	Rscript runners/run_model.R

run-4region-fixVB_M: build-4region-fixVB_M-11par
	program_path=$(MFCL_EXE_REL) \
	base_dir=$(REGION4_FIXVB_M_OUTPUT_DIR) \
	model_dir=$(REGION4_FIXVB_M_MODEL_DIR) \
	description="2023 4-region fixVB_M input" \
	config_summary="9->4 representative; 11.par collapsed from 2023_fixVB_M" \
	Rscript runners/run_model.R

model:
	Rscript runners/run_model.R

prof:
	Rscript runners/run_prof.R

prof_chain:
	Rscript runners/run_prof_chain.R

prof_2d:
	Rscript runners/run_prof_2d.R

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

test:
	Rscript tests/run_tests.R
	
TagMovementSubset:
	Rscript sensitivities/TagMovementSubset.R

SelectivitySplineNodes:
	Rscript sensitivities/SelectivitySplineNodes.R

IndexCvHalf:
	Rscript sensitivities/IndexCvHalf.R

collate-hessian:
	Rscript tools/collate_hessian_mfcl.R

stitch-hessian:
	Rscript tools/collate_hessian_mfcl.R

stitch-hessian-all:
	Rscript tools/collate_hessian_mfcl.R --all

prof-init-map:
	Rscript tools/build_prof_init_map.R

prof-init-map-model:
	@if [ -z "$(MODEL)" ]; then echo "Usage: make prof-init-map-model MODEL=2023diag"; exit 1; fi
	model_name=$(MODEL) model_dir=model/$(MODEL) base_dir=$(BASE_DIR) prof_init_map_out=$(OUT) Rscript tools/build_prof_init_map.R

run: model prof
	
plot:
	Rscript -e "rmarkdown::render('plot/plots.rmd')"

shiny-plot-4region-shape:
	Rscript plot/shiny_plot/tools/4region_shape.R

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

docker-prof-init-map:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript tools/build_prof_init_map.R

docker-run: docker-model docker-prof
	
docker-plot:
	docker run --rm --user "$(DOCKER_USER)" -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "rmarkdown::render('plot/plots.rmd')"

docker-report:
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(stitch-hessian docker-run docker-model docker-prof docker-jitter docker-hessian docker-collate-hessian docker-stitch

	
.PHONY: build-4region build-4region-merge build-4region-11par build-4region-variant build-4region-variant-11par build-4region-fixVB build-4region-fixVB-11par build-4region-fixVB_M build-4region-fixVB_M-11par build-4region-fixM build-4region-fixM-11par build-input-recipe build-config-inputs build-4region-all-inputs makepar-4region makepar-4region-merge run-4region run-4region-merge run-4region-fixM run-4region-fixVB_M plot run model prof prof_chain prof_2d jitter jitter_smoke jitter_smoke_hessian hessian retro test TagMovementSubset SelectivitySplineNodes IndexCvHalf collate-hessian stitch-hessian stitch-hessian-all prof-init-map prof-init-map-model docker-run docker-model docker-prof docker-jitter docker-jitter-smoke docker-jitter-smoke-hessian docker-hessian docker-retro docker-collate-hessian docker-stitch-hessian docker-stitch-hessian-all docker-prof-init-map docker-plot prepaw report docker-report



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

docker-shiny_plot:
	docker run --rm -it --user "$(DOCKER_USER)" -p $(SHINY_PLOT_PORT):$(SHINY_PLOT_PORT) -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "shiny::runApp('plot/shiny_plot', host='0.0.0.0', port=$(SHINY_PLOT_PORT), launch.browser=FALSE)"

docker-shiny_launcher:
	docker run --rm -it --user "$(DOCKER_USER)" -p $(SHINY_LAUNCHER_PORT):$(SHINY_LAUNCHER_PORT) -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "shiny::runApp('launchers/shiny_launcher', host='0.0.0.0', port=$(SHINY_LAUNCHER_PORT), launch.browser=FALSE)"

docker-shiny_plot_bg:
	-docker rm -f bet2026_shiny_plot >/dev/null 2>&1
	docker run -d --name bet2026_shiny_plot --user "$(DOCKER_USER)" -p $(SHINY_PLOT_PORT):$(SHINY_PLOT_PORT) -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "shiny::runApp('plot/shiny_plot', host='0.0.0.0', port=$(SHINY_PLOT_PORT), launch.browser=FALSE)"
	@echo "docker shiny_plot running at http://127.0.0.1:$(SHINY_PLOT_PORT)"

docker-shiny_launcher_bg:
	-docker rm -f bet2026_shiny_launcher >/dev/null 2>&1
	docker run -d --name bet2026_shiny_launcher --user "$(DOCKER_USER)" -p $(SHINY_LAUNCHER_PORT):$(SHINY_LAUNCHER_PORT) -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript -e "shiny::runApp('launchers/shiny_launcher', host='0.0.0.0', port=$(SHINY_LAUNCHER_PORT), launch.browser=FALSE)"
	@echo "docker shiny_launcher running at http://127.0.0.1:$(SHINY_LAUNCHER_PORT)"

docker-shiny_bg: docker-shiny_plot_bg docker-shiny_launcher_bg

docker-shiny_stop:
	-docker rm -f bet2026_shiny_plot bet2026_shiny_launcher >/dev/null 2>&1
	@echo "stopped docker shiny apps (if running)"

docker-shiny_status:
	-docker ps --filter name=bet2026_shiny_plot --filter name=bet2026_shiny_launcher

shiny_plot_stop:
	-pkill -f "shiny::runApp\\('plot/shiny_plot'" || true
	@echo "stopped shiny_plot (if running)"

shiny_launcher_stop:
	-pkill -f "shiny::runApp\\('launchers/shiny_launcher'" || true
	@echo "stopped shiny_launcher (if running)"

shiny_stop: shiny_plot_stop shiny_launcher_stop

shiny_status:
	-ps -ef | grep "shiny::runApp" | grep -v grep || true

.PHONY: shiny_launcher shiny_plot shiny_plot_bg shiny_launcher_bg shiny_bg docker-shiny_plot docker-shiny_launcher docker-shiny_plot_bg docker-shiny_launcher_bg docker-shiny_bg docker-shiny_stop docker-shiny_status shiny_plot_stop shiny_launcher_stop shiny_stop shiny_status

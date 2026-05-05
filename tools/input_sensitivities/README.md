# Input Sensitivity Builders

This folder contains scripts that create MFCL input folders for sensitivity runs.
The framework is region-design agnostic: it starts from an explicit
`base_input_dir` input folder, then applies selected transformation steps.

- `build_input_recipe.R`: builds one input folder from a base plus selected sensitivity steps.
- `build_config_inputs.R`: builds all recipe-enabled input folders in a model config.
- `sensitivity_catalog.R`: one row per Shiny-visible sensitivity, including
  dropdown labels, compact suffixes, factorial grouping, and nested/exclusive
  levels.
- `recipe_registry.R`: the single list that wires recipe options to sensitivity steps.
- `steps/`: one script per input transformation.
- `diagnostics/`: sensitivity-related diagnostics that do not create model input folders.
- `helpers.R`: shared path, copy, env, and subprocess helpers.
- `check_setup.R`: quick validation that the catalog, compact names, and recipe
  steps are wired correctly.

Conceptually, every sensitivity input is:

```text
base_input_dir + sensitivity steps -> sensitivity_output_dir
```

Example:

```bash
Rscript tools/input_sensitivities/build_input_recipe.R \
  --base-input-dir mfcl/inputs/my_base \
  --output-dir mfcl/inputs/my_base_sel_spline4 \
  --sel-nodes 4 \
  --overwrite
```

## Adding a sensitivity

### Case 1: new level of an existing sensitivity type

If the new sensitivity can be expressed with existing recipe fields
(`movement_pairs`, `sel_nodes`, or `index_cv_half`), only add one
`input_sensitivity_row()` in `sensitivity_catalog.R`.

Example:

```r
input_sensitivity_row(
  id = "move_R1_R2",
  token = "m12",
  suffix = "_m12",
  input_suffix = "_movement_R1_R2",
  factor = "movement",
  label = "Tag movement R1-R2",
  nested = TRUE,
  movement_pairs = "1-2"
)
```

Important fields:

- `id`: internal stable id used by Shiny; do not reuse old ids.
- `token`: saved in `input_change_metadata.rds`; used to avoid applying the
  same sensitivity twice.
- `suffix`: compact launch/model-name suffix shown in Shiny.
- `input_suffix`: verbose suffix produced by the underlying input builder;
  this lets `compact_input_name()` shorten existing folders consistently.
- `factor`: factorial grouping name. Sensitivities in different factors can be
  combined in factorial mode.
- `nested`: set `TRUE` for mutually exclusive levels in the same factor, such
  as alternative movement subsets. Factorial mode will choose at most one level
  from that factor. If a selected base input already has another token from the
  same factor, Shiny treats the selected sensitivity as a replacement level: it
  removes the old factor suffix from the recipe source name and builds the new
  level, instead of stacking incompatible changes.

### Case 2: new transformation type

1. Add a script under `steps/` that accepts `base_dir` and `out_dir`
   environment variables.
2. In that script, copy the input folder, modify only the required files, and
   call `append_input_change_metadata()`.
3. Add one `input_sensitivity_row()` in `sensitivity_catalog.R`.
4. Add one `input_recipe_step_def()` entry in `recipe_registry.R`.
5. If the new step needs a new option beyond the current fields, add that
   option to `input_sensitivity_recipe_options()` and to the Shiny
   `input_recipe_plan()` pass-through.

Sensitivity scripts should infer targets from the selected `base_dir` wherever
possible. Avoid embedding a model name, region count, fishery range, or fixed
flag value in the script. For example, the current index-CV step halves only
negative fishery-specific flag 92 entries found in `doitall.sh` unless an
explicit `index_fisheries` environment variable is supplied; positive rows such
as `2 92 2` are MFCL options and are left unchanged. The selectivity step
updates global fish flag 61 when present and otherwise updates the
fishery-specific flag 61 entries it finds.

After editing, run:

```bash
Rscript tools/input_sensitivities/check_setup.R
Rscript -e "source('launchers/shiny_launcher/global.R'); source('launchers/shiny_launcher/ui.R'); source('launchers/shiny_launcher/server.R'); cat('app source ok\n')"
```

The canonical sensitivity code lives in this folder. Keep new input-building
sensitivities in `steps/` so recipe changes stay easy to review.

input_recipe_step_def <- function(name, enabled, subdir, script, env) {
  list(
    name = name,
    enabled = isTRUE(enabled),
    subdir = subdir,
    script = script,
    env = env
  )
}

build_input_recipe_steps <- function(options) {
  list(
    input_recipe_step_def(
      name = "movement_subset",
      enabled = nzchar(options$movement_pairs),
      subdir = "03_movement",
      script = input_sensitivity_step("tag_movement_subset.R"),
      env = function(base_dir, out_dir) c(
        paste0("base_dir=", base_dir),
        paste0("movement_pairs=", options$movement_pairs),
        paste0("out_dir=", out_dir)
      )
    ),
    input_recipe_step_def(
      name = "selectivity_spline_nodes",
      enabled = nzchar(options$sel_nodes),
      subdir = "04_sel",
      script = input_sensitivity_step("selectivity_spline_nodes.R"),
      env = function(base_dir, out_dir) c(
        paste0("base_dir=", base_dir),
        paste0("out_dir=", out_dir),
        paste0("node_count=", options$sel_nodes)
      )
    ),
    input_recipe_step_def(
      name = "index_cv_half",
      enabled = isTRUE(options$index_cv_half),
      subdir = "05_index_cv_half",
      script = input_sensitivity_step("index_cv_half.R"),
      env = function(base_dir, out_dir) c(
        paste0("base_dir=", base_dir),
        paste0("out_dir=", out_dir)
      )
    )
  )
}

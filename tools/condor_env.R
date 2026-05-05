condor_git_safe_env <- function(env = list()) {
  env <- as.list(env)
  git_env <- list(
    HOME = ".",
    XDG_CACHE_HOME = ".cache",
    GIT_CONFIG_NOSYSTEM = "1",
    GIT_CONFIG_GLOBAL = "/dev/null",
    GIT_TERMINAL_PROMPT = "0",
    GCM_INTERACTIVE = "Never"
  )
  utils::modifyList(env, git_env)
}

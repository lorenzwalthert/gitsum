init_gitsum <- function(path = ".", over_write = FALSE) {
  check_overwriting_clearance(gitsum_path(path), over_write, dir.exists)
  parse_log_detailed_full_run(path) %>%
    dump_parsed_log(path = ".", over_write)
}

#' Dump a parsed log
#'
#' @param log A parsed log as a tibble
#' @importFrom readr write_rds
dump_parsed_log <- function(log, path = ".", over_write = FALSE) {
  gitsum_path <- ensure_gitusm_repo(path)
  gitsum_path_log <- file.path(gitsum_path, "log.rds")
  check_overwriting_clearance(gitsum_path_log, over_write)
  write_rds(log, gitsum_path_log)
  message("\nLog created at ", gitsum_path_log)
  write_last_commit(log, gitsum_path)
}

write_last_commit <- function(log, gitsum_path) {
  last <- log %>%
    arrange(desc(date)) %>%
    slice(1)
  write_rds(last, file.path(gitsum_path, "last_commit.rds"))
}


ensure_gitusm_repo <- function(path = ".") {
  if (!is_gitsum_repo(path)) {
    dir.create(gitsum_path(path))
  }
  gitsum_path(path)
}

check_overwriting_clearance <- function(path, over_write, fun = file.exists) {
  if (fun(path)) {
   if (!over_write) stop(
     "Cannot overwrite file / path ", path,
     " since argument overwrite was set to FALSE"
    )
  }
}

read_parsed_log <- function(path = ".", log = "log.rds") {
  read_gitsum_data(path, log)
}

read_last_commit <- function(path = ".") {
  read_gitsum_data(path, "last_commit.rds")
}

read_last_hash <- function(path = ".") {
  last_commit <- read_last_commit(path)
  last_commit$hash
}

combine_logs_detailed <- function(path) {

}

parse_log_detailed <- function(path = ".") {
  last_hash <- read_last_hash(path)
  read_parsed_log(path) %>%
    bind_rows(
      parse_log_detailed_full_run(path, commit_range = paste0(last_hash, "..HEAD"))
    )
}
udpate_gistum_data <- function(path = ".") {
  parse_log_detailed(path) %>%
    write_rds(gitsum_path(path, "log.rds"))
  write_last_commit(log, gitsum_path(path))
}

read_gitsum_data <- function(path, ...) {
  assert_gitsum_repo(path)
  read_rds(gitsum_path(path, ...))
}

is_gitsum_repo <- function(path = ".") {
  dir.exists(gitsum_path(path))
}

gitsum_path <- function(path = ".", ...) {
  file.path(path, ".gitsum", ...)
}
assert_gitsum_repo <- function(path = ".") {
  if (!is_gitsum_repo(path)) stop(path, " is not a gistum repo")
}

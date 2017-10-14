#' Dump a parsed log
#'
#' @param log A parsed log as a tibble
#' @importFrom readr write_rds
dump_parsed_log <- function(log, path = ".", over_write = FALSE) {
  gitsum_path <- ensure_gitusm_repo(path)
  gitsum_path_log <- file.path(gitsum_path, "log.rds")
  check_overwriting_clearance(gitsum_path_log, over_write)
  write_rds(log, gitsum_path_log)
  message("Log created at ", gitsum_path_log)
  write_last_commit(log, gitsum_path)
}

write_last_commit <- function(log, gitsum_path) {
  write_rds(log[1, ], file.path(gitsum_path, "last_commit.rds"))
}


ensure_gitusm_repo <- function(path = ".") {
  if (!is_gitsum_repo(path)) {
    dir.create(gitsum_path(path))
  }
  gitsum_path(path)
}

check_overwriting_clearance <- function(path, over_write) {
  if (file.exists(path)) {
   if (!over_write) stop(
     "Canno't overwrite file ", path,
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

parse_log_detailed_quickly <- function() {

}
udpate_gistum_data <- function(path = ".") {
  last_hash <- read_last_hash(path)
  new_log <- read_parsed_log(path) %>%
    bind_rows(
  parse_log_detailed(path, commit_range = paste0(last_hash, "..HEAD"))
    ) %>%
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

#' Manage a gitsum repository
#'
#' A gitsum repository is a git repository with an additional .gitsum folder in
#' the root directory that is mainly used to store a parsed git log as a tibble.
#' @param path The path to the repository to initialize.
#' @param over_write Whether or not an existing directory should be overwritten.
#' @name manage_gitsum
#' @aliases init_gitsum remove_gitsum
NULL

#' @describeIn manage_gitsum Calling this function will parse the available git
#'   history and dump it into the directory .gitsum.
#' @export
init_gitsum <- function(path = ".", over_write = FALSE) {
  check_overwriting_clearance(gitsum_path(path), over_write, dir.exists)
  parse_log_detailed_full_run(path) %>%
    dump_parsed_log(path = ".", over_write)
}

#' @describeIn manage_gitsum Removes the gitsum repository, i.e just the folder
#' .gitsum and its contents.
#' @export
remove_gitsum <- function(path) {
  was_gitsum_repo <- is_gitsum_repo(path)
  unlink(gitsum_path(path), recursive = TRUE)
  if (was_gitsum_repo) message(".gitsum removed.")
}

#' Dump a parsed log into .gitsum
#'
#' Store a parsed git log in a gitsum repository.
#' @param log A parsed log as a tibble
#' @param path The path to the root directory of the gitsum repository.
#' @importFrom readr write_rds
dump_parsed_log <- function(log, path = ".", over_write = FALSE) {
  gitsum_path <- ensure_gitusm_repo(path)
  gitsum_path_log <- file.path(gitsum_path, "log.rds")
  check_overwriting_clearance(gitsum_path_log, over_write)
  write_rds(log, gitsum_path_log)
  message("\nLog created at ", gitsum_path_log)
  dump_last_commit(log, path)
}


#' @describeIn dump_parsed_log Dumps the last commit into .gitsum
#' @importFrom readr write_rds
dump_last_commit <- function(log, path) {
  last <- log %>%
    arrange(desc(date)) %>%
    slice(1)
  write_rds(last, gitsum_path(path, "last_commit.rds"))
}

#' Make sure the repository is a gitsum repository and create one if it is not
#' @inheritParams dump_parsed_log
ensure_gitusm_repo <- function(path = ".") {
  if (!is_gitsum_repo(path)) {
    dir.create(gitsum_path(path))
  }
  gitsum_path(path)
}

#' Check whether a file / path exists and return an error when it cannot be
#' overwritten.
#' @inheritParams dump_parsed_log
#' @param fun The function to apply to the path, either file.exists, or
#'   dir.exists.
check_overwriting_clearance <- function(path, over_write, fun = file.exists) {
  if (fun(path)) {
   if (!over_write) stop(
     "Cannot overwrite file / path ", path,
     " since argument overwrite was set to FALSE"
    )
  }
}

#' Read gitsum data into R
#' @inheritParams dump_parsed_log
#' @name read_gitsum
NULL

#' @describeIn read_gitsum Reads a parsed log.
read_log <- function(path = ".") {
  read_gitsum_data(".", "log.rds")
}

#' @describeIn read_gitsum Reads the last parsed commit.
read_last_commit <- function(path = ".") {
  read_gitsum_data(path, "last_commit.rds")
}

#' @describeIn read_gitsum Reads the last parsed hash.
read_last_hash <- function(path = ".") {
  last_commit <- read_last_commit(path)
  last_commit$hash
}



udpate_gistum_data <- function(path = ".") {
  parse_log_detailed(path) %>%
    write_rds(gitsum_path(path, "log.rds"))
  dump_last_commit(log, gitsum_path(path))
}

#' @importFrom readr write_rds
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

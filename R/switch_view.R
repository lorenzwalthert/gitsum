#' @export
switch_view <- function(log_table) {
  UseMethod("switch_view")
}

#' @export
switch_view.default <- function(log_table) {
  message("returning input since not of class commit_level_log or file_level_log")
  log_table
}


#' @importFrom tidyr unnest_
#' @export
switch_view.commit_level_log <- function(log_table) {
  message("switched to file level log")
  out <- unnest_(log_table, "nested")
  class(out) <- append("file_level_log", setdiff(class(out), "commit_level_log"))
  out
}

#' @importFrom tidyr nest_
#' @export
switch_view.file_level_log <- function(log_table) {
  message("switched to commit level log")
  out <- nest_(log_table, "nested", c("changed_file", "edits", "insertions", "deletions"))
  class(out) <- append("commit_level_log", setdiff(class(out), "file_level_log"))
  out
}



##  ............................................................................
##  merge                                                                   ####

#' @importFrom dplyr filter
#' @export
#' @keywords internal
no_merge <- function(log_table) {
  UseMethod("no_merge")
}

#' @export
no_merge.file_level_log <- function(log_table) {
  filter_(log_table, ~!is_merge)
}

#' @export
no_merge.commit_level_log <- function(log_table) {
  drop <- switch_view(log_table) %>%
    filter(is_merge)
  anti_join(log_table, drop, by = "hash")
}

#' @keywords internal
#' @export
only_merge <- function(log_table) {
  UseMethod("only_merge")
}

#' @importFrom dplyr filter_
#' @export
only_merge.commit_level_log <- function(log_table) {
  filter_(log_table, ~is_merge)
}

#' @importFrom dplyr filter_ inner_join
#' @export
only_merge.file_level_log <- function(log_table) {
  filter_(log_table, ~is_merge)
}



#' @export
add_changed_file_name <- function(unnested_log_table) {
  within(unnested_log_table, file_name <- changed_file_name(changed_file))
}

#' @importFrom stringi stri_locate_last
#' @export
changed_file_name <- function(changed_file) {
    start <- stri_locate_last(changed_file, fixed = "/")[, 2] + 1
    substring(changed_file, ifelse(is.na(start), 1, start))
}

#' @export
no_dot <- function(unnested_log_table) {
  changed <- changed_file_name(unnested_log_table$changed_file)
  unnested_log_table[!grepl("^\\.", changed),]
}

#' @export
only_dot <- function(unnested_log_table) {
  changed <- changed_file_name(unnested_log_table$changed_file)
  unnested_log_table[grepl("^\\.", changed),]
}

#' @export
has_description <- function(log_table) {
  log_table[!is.na(log_table$description),]
}

#' @export
weekday_commits <- function(log_table) {
  log_table[!(log_table$weekday %in% c("Sat", "Sun")),]
}

#' @export
weekend_commits <- function(log_table) {
  log_table[(log_table$weekday %in% c("Sat", "Sun")),]
}

#' @export
initial_commit <- function(log_table) {
  log_table[is.na(log_table$left_parent) & is.na(log_table$right_parent),]
}

#' @export
last_commit <- function(log_table) {
  log_table[which.max(log_table$date),]
}

#' @export
biggest_commit <- function(unnested_log_table, measure = "total_files_changed") {
  log <- nest_(unnested_log_table, "nested", c("changed_file", "edits", "insertions", "deletions"))
  index <- which.max(log[[measure]])
  log[index,]
}

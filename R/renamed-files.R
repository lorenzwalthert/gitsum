#' Set the column  `changed_files` of a `log` to the latest name of the file
#'
#' [parse_log_detailed()] parses a git log. However, changed file names
#' are not corrected retroactively. Hence, if one want's to process file-specific
#' commit data for one file accross different names for that file, the column
#' `changed_files` has to be unified. This is done with this function by
#' updating the file names recursively and setting them to the latest name used.
#' @param log An unnested detailed log.
#' @examples
#' library("magrittr")
#' gitsumlog %>%
#'   tidyr::unnest() %>%
#'   set_changed_file_to_latest_name() %>%
#'   add_line_history()
#' @export
set_changed_file_to_latest_name <- function(log) {
  is_renaming <- is_name_change(log$changed_file)
  contains_renaming <- log[is_renaming, ]
  contains_no_renaming <- log[!is_renaming,]
  transition_model <- parse_reassignment(
    contains_renaming$changed_file, which(is_renaming)
  )
  update_changed_file_sequentially(log, transition_model)
}


#' Detetct whether a `changed_file`-entry is a renaming
#'
#' @param changed_file The column `changed_file` from a parsed unnested
#'   detailed log.
#' @param reassignment_pattern The regular expression pattern that corresponds
#'   to renaming.
#' @importFrom stringr str_locate
#' @examples
#' gitsum:::is_name_change("R/{gitsum.R => gitsum-package.R}")
is_name_change <- function(changed_file,
                           reassignment_pattern = "\\{.+\\s\\=\\>.+\\}$") {
  regex_position_matrix <- str_locate(changed_file, reassignment_pattern)
  apply(regex_position_matrix, 1, function(x) !all(is.na(x)))
}

#' Parse a raw reassignment into a list
#'
#' Takes a vector of raw reassignments and turns them into a list of elements.
#' Every element contains the old path, the new path and the index of the
#' reassignment.
#' @param raw_reassignment Character vector with raw re-assignments.
#' @param reassignment_index Integer vector indicating the position of the
#'   re-assignment in the original unnested detailed log.
#' @examples
#' gitsum:::parse_reassignment(rep("R/{a => b}", 2), c(1, 2))
parse_reassignment <- function(raw_reassignment, reassignment_index) {
  separated <- separate_dir_and_reassignment(raw_reassignment)
  paths <- separated %>% map(~.x[1])
  base_names <- separate_old_and_new_name(separated)
  combine_dir_and_base(paths, base_names, reassignment_index)
}


#' Separate the directory from the reassignment
#'
#' @importFrom purrr map
#' @importFrom stringr str_split fixed str_sub
#' @inheritParams parse_reassignment
#' @examples
#' gitsum:::separate_dir_and_reassignment("R/{gitsum.R => gitsum-package.R}")
separate_dir_and_reassignment <- function(raw_reassignment) {
  str_split(raw_reassignment, fixed("{")) %>%
    map(~str_sub(.x, end = -2L))
}

#' @importFrom purrr map flatten_chr
#' @importFrom stringr fixed str_split
separate_old_and_new_name <- function(reassignment) {
  reassignment %>%
    map(~str_split(.x[2], fixed(" => ")) %>% flatten_chr())
}

#' Derive full paths
#'
#' @param dirs Character vector with directories.
#' @param base_names List of pairs containing the old base name as the first
#'   element and the new base name as the second.
#' @inheritParams parse_reassignment
#' @examples
#' gitsum:::combine_dir_and_base("R", list(c("a", "b")), 1)
#' @importFrom purrr pmap
combine_dir_and_base <- function(dirs, base_names, reassignment_index) {
  pmap(list(dirs, base_names, reassignment_index),
       combine_dir_and_base_one)
}

#' Combine dir with old and new basename
#'
#' @param dirname A directory name.
#' @param base_name The base name of a file.
#' @inheritParams parse_reassignment
#' @importFrom rlang ll !!!
combine_dir_and_base_one <- function(dirname, base_name, reassignment_index) {
  out <- ll(!!!as.list(file.path(dirname, base_name)), reassignment_index) %>%
    set_names(c("old_path", "new_path", "reassignment_index"))
  out
}


#' Apply all updates of file names sequentially to a log
#'
#' Recursively updates the column `changed_file` of `log` according to a
#' transition scheme provided in `transition_model`.
#' @param transition_model A transition model obtained from
#' [parse_reassignment()].
#' @inheritParams set_changed_file_to_latest_name
#' @importFrom purrr reduce
update_changed_file_sequentially <- function(log, transition_model) {
  reduce(transition_model, update_changed_file, .init = log)
}

#' Apply one file name update to a log
#' @inheritParams set_changed_file_to_latest_name
#' @param transition A named list with one entry from the transition model.
#' @importFrom rlang seq2
#' @importFrom dplyr bind_rows
update_changed_file <- function(log, transition) {
  seq_to_update <- group_reassignment(log, transition)
  grouped <- split(log, seq_to_update)
  bind_rows(
    update_changed_file_one(grouped$to_update, transition),
    grouped$not_to_update
  )
}

#' Apply one file name update to an unnested log that contains commits
#' up to that name change. Also needs to update the last entry in log, which
#' is the transition itself.
#' @inheritParams update_changed_file
update_changed_file_one <- function(log, transition) {
  to_update <- log$changed_file %in% transition$old_path
  log[c(which(to_update), nrow(log)), "changed_file"] <- transition$new_path
  log
}

#' Create indices for splitting `log` into a group to update and a group to
#' not update
#' @inheritParams update_changed_file_one
#' @importFrom forcats fct_inorder
group_reassignment <- function(log, transition) {
  fct_inorder(c(
    rep("to_update", transition$reassignment_index),
    rep("not_to_update", nrow(log) - transition$reassignment_index)
  ))
}

#' Use the latest name of a file in every log entry
#'
#' Set the column `changed_files` of a `log` to the latest name of the file
#' @details
#' [parse_log_detailed()] parses a git log. However, changed file names
#' are not corrected retroactively. Hence, if one wants to process file-specific
#' commit data for one file across different names for that file, the column
#' `changed_files` has to be unified. This is done with this function by
#' updating the file names recursively and setting them to the latest name used.
#' Filenames containing the sequence ` => ` are viewed as renamed files, so
#' there will likely be problems when you apply this function to a git
#' repository that contains (or contained) files with such files names. You
#' may have to update the column `changed_file` manually.
#' @param log An unnested detailed log.
#' @examples
#' library("magrittr")
#' gitsumlog %>%
#'   tidyr::unnest() %>%
#'   set_changed_file_to_latest_name() %>%
#'   add_line_history()
#' @importFrom purrr walk
#' @export
set_changed_file_to_latest_name <- function(log) {
  is_renaming <- is_name_change(log$changed_file)
  cat(paste0("The following name changes were identified (", sum(is_renaming), " in total):\n"))
  cli::cat_bullet(log$changed_file[is_renaming])

  contains_renaming <- log[is_renaming, ]
  contains_no_renaming <- log[!is_renaming,]
  transition_model <- parse_reassignment(
    contains_renaming$changed_file, which(is_renaming)
  )
  update_changed_file_sequentially(log, transition_model)
}


#' Detect whether a `changed_file`-entry is a renaming
#'
#' Filenames in `$ git log` containing the sequence ` => ` are viewed as renamed
#' files. There seems no other way around that, so this function will indicate
#' a file is a name change even if it may not be the case.
#' @param changed_file The column `changed_file` from a parsed unnested
#'   detailed log as a vector.
#' @param reassignment_pattern The regular expression pattern that corresponds
#'   to renaming.
#' @importFrom stringr str_locate
#' @examples
#' gitsum:::is_name_change(c(
#'   "R/{gitsum.R => gitsum-package.R}", "API => API2",
#'   "this is jus a file with => in it, it's not a name change"
#' ))
#' @keywords internal
is_name_change <- function(changed_file,
                           reassignment_pattern = ".+\\s\\=\\>.+$") {
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
#' gitsum:::parse_reassignment(
#'   c("R/{a => b}", "API => api2", "{src => inst/include}/dplyr_types.h"),
#'   c(1, 2, 33)
#' )
#' @keywords internal
parse_reassignment <- function(raw_reassignment, reassignment_index) {
  separated <- ensure_curly_enclosing(raw_reassignment) %>%
    ensure_dash_enclosing() %>%
    separate_dir_and_reassignment()
  paths <- separated %>% map(~.x[1])
  extensions <- separated %>% map(~.x[3])
  base_names <- separate_old_and_new_name(separated)
  combine_dir_and_base(paths, base_names, extensions, reassignment_index)
}

#' Ensure enclosing
#'
#' Make sure a string is enclosed with some values.
#' @name ensure_enclosing
NULL

#' @describeIn ensure_enclosing Ensures curly braces enclosing.
#' @param raw_reassignment A character vector containing text from
#'   raw assignemnts (see 'Examples').
#' @examples
#' gitsum:::ensure_curly_enclosing(
#'   c("API => api", "R/{a => b}")
#' )
#' @keywords internal
ensure_curly_enclosing <- function(raw_reassignment) {
  is_enclosed <- is_enclosed(raw_reassignment, c("{", "}"))
  raw_reassignment[!is_enclosed] <- enclose_string(
    raw_reassignment[!is_enclosed], c("{", "}")
  )
  raw_reassignment
}


#' @describeIn ensure_enclosing Ensures dash enclosing.
#' @examples
#' gitsum:::ensure_leading_dash(
#'   c("{API = api}", "R/{a => b}",
#'   "vignettes/{ => notes}/mysql-setup.Rmd")
#' )
#' @keywords internal
ensure_dash_enclosing <- function(raw_reassignment) {
  raw_reassignment %>%
    ensure_leading_dash() %>%
    ensure_trailing_dash()
}

#' @importFrom stringr str_locate fixed
#' @keywords internal
ensure_leading_dash <- function(string) {
  starts_with_brace <- str_locate(string, fixed("{"))[, 1] == 1L
  string[starts_with_brace] <- paste0("/", string[starts_with_brace])
  string
}

#' @importFrom stringr str_locate fixed str_length
#' @keywords internal
ensure_trailing_dash <- function(string) {
  ends_with_brace <- str_locate(string, fixed("}"))[, 2] == str_length(string)
  string[ends_with_brace] <- paste0(string[ends_with_brace], "/")
  string
}

#' @importFrom stringr str_locate fixed
#' @keywords internal
is_enclosed <- function(string, pattern) {
  is_enclosed <- str_locate(string, fixed(pattern[1]))[, 2] <=
    str_locate(string, fixed(pattern[2]))[, 1]
  ifelse(is.na(is_enclosed), FALSE, TRUE)
}

enclose_string <- function(string, enclosement) {
  paste0(enclosement[1], string, enclosement[2])
}

#' Separate the directory from the reassignment
#'
#' Splits according to regex "/\\{|\\}/".
#' @importFrom purrr map
#' @importFrom stringr str_split fixed str_sub
#' @inheritParams parse_reassignment
#' @examples
#' gitsum:::separate_dir_and_reassignment("R/{gitsum.R => gitsum-package.R}/")
#' @keywords internal
separate_dir_and_reassignment <- function(raw_reassignment) {
  str_split(raw_reassignment, "/\\{|\\}/")
}

correct_base_dir_for_toplevel_reassignments <- function(reassignments) {
  cleaned <- reassignments %>%
    map(complete_reassignment_if_necessary)
  cleaned
}

complete_reassignment_if_necessary <- function(reassignment) {
  if (length(reassignment) > 1L) return(reassignment)
  else c("", reassignment)
}

#' @importFrom purrr map flatten_chr
#' @importFrom stringr fixed str_split
separate_old_and_new_name <- function(reassignment) {
  reassignment %>%
    map(~str_split(.x[2], fixed(" => ")) %>% flatten_chr())
}

#' Derive full paths
#'
#' Derive full paths given directory paths, base_names, extensions and
#' reassignment index.
#' @param dirs Character vector with directories.
#' @param base_names List of pairs containing the old base name as the first
#'   element and the new base name as the second.
#' @param extensions The extensions that indicate the suffix of the name.
#' @inheritParams parse_reassignment
#' @examples
#' gitsum:::combine_dir_and_base(
#'   "R",
#'   list(c("a", "b"), c("API", "api2"), c("scr", "inst/include")),
#'   list("", "", "dplyr_types.h"), c(1, 42, 61)
#' )
#' @importFrom purrr pmap
#' @keywords internal
combine_dir_and_base <- function(dirs, base_names, extensions, reassignment_index) {
  pmap(list(dirs, base_names, extensions, reassignment_index),
       combine_dir_and_base_one)
}

#' Combining paths
#'
#' Combine dir with old and new basename and extension
#' @param dirname A directory name.
#' @param base_name The base name of a file.
#' @inheritParams parse_reassignment
#' @inheritParams combine_dir_and_base
#' @importFrom purrr pmap
#' @keywords internal
combine_dir_and_base_one <- function(dirname,
                                     base_name,
                                     extensions,
                                     reassignment_index) {
  old_new_reassignment <- pmap(list(dirname, base_name, extensions), file_path) %>%
    append(reassignment_index) %>%
    set_names(c("old_path", "new_path", "reassignment_index"))
  old_new_reassignment
}

#' Composing file paths
#'
#' Like [base::file.path()], but sorts our `character(1)` first.
#' @importFrom purrr compact flatten_chr
#' @importFrom rlang set_names
#' @inheritParams base::file.path
#' @keywords internal
file_path <- function(..., fsep = .Platform$file.sep) {
  compact_vars <- map(list(...), function(x) {
    if (x == "") {
      NULL
    } else {
      x
    }
  }) %>%
    compact() %>%
    flatten_chr() %>%
    append(c(fsep = fsep))
  do.call("file.path", as.list(compact_vars))
}


#' Apply all updates of file names sequentially to a log
#'
#' Recursively updates the column `changed_file` of `log` according to a
#' transition scheme provided in `transition_model`.
#' @param transition_model A transition model obtained from
#' [parse_reassignment()].
#' @inheritParams set_changed_file_to_latest_name
#' @importFrom purrr reduce
#' @keywords internal
update_changed_file_sequentially <- function(log, transition_model) {
  reduce(transition_model, update_changed_file, .init = log)
}


#' Apply one file name update to a log
#' @inheritParams set_changed_file_to_latest_name
#' @param transition A named list with one entry from the transition model.
#' @importFrom rlang seq2
#' @importFrom dplyr bind_rows
#' @keywords internal
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
#' @keywords internal
update_changed_file_one <- function(log, transition) {
  to_update <- log$changed_file %in% transition$old_path
  log[c(which(to_update), nrow(log)), "changed_file"] <- transition$new_path
  log
}

#' Create indices for splitting `log` into a group to update and a group to
#' not update
#' @inheritParams update_changed_file_one
#' @importFrom forcats fct_inorder
#' @keywords internal
group_reassignment <- function(log, transition) {
  fct_inorder(c(
    rep("to_update", transition$reassignment_index),
    rep("not_to_update", nrow(log) - transition$reassignment_index)
  ))
}

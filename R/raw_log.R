#' Obtain the log raw data
#' @param path The path to the git directory one wants to create summaries for.
#' @param file_name The name of the temporary file. If `NULL`, a file is
#'   created, otherwise, a file is read.
#' @param remove Whether a log should be deleted after read in.
#' @param commit_range A string of the form "hash1..hash2" indicating the commit
#'   range to parse. `NULL` means all commits.
#' @importFrom readr read_lines
get_raw_log <- function(path,
                        file_name = NULL,
                        remove = is.null(file_name),
                        commit_range = NULL) {
  file_name_progr <- ifelse(is.null(file_name), ".log.txt", file_name)

  if (is.null(file_name)) {
    sys_call <- paste(
      "export COLUMNS=999 &&",
      "cd", path, "&&",
      "git log ", commit_range, " --stat --parents ", " >", file_name_progr
    )
    if (Sys.info()[1] == "Windows") {
      error <- shell(sys_call)
    } else {
      error <- system(sys_call)
    }
    if (error == 128) stop(path, " is not a git repository")
  }

  # get list of commits
  path_to_file <- normalizePath(file.path(path, file_name_progr))
  temp <- read_lines(path_to_file, progress = TRUE)
  if (remove) unlink(path_to_file)

  data_frame(lines = temp)
}

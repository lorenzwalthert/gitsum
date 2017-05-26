#' Obtain the log raw data
#' @param path the path to the git directory one wants to create summaries for.
#' @param file_name the name of the temporary file. If `NULL`, a file is created,
#'   otherwise, a file is read.
#' @param remove whether a log should be deleted after read in.
#' @importFrom readr read_lines
get_raw_log <- function(path, file_name = NULL, remove = is.null(file_name)) {
  file_name_progr <- ifelse(is.null(file_name), ".log.txt", file_name)
  path_to_file <- file.path(path, file_name_progr)

  if (is.null(file_name)) {
    sys_call <- paste('cd', path, '&&', 'git log --stat --parents >', file_name_progr)
    if (Sys.info()[1] == "Windows") {
      error <- shell(sys_call)
    } else {
      error <- system(sys_call)
    }
    if (error == 128) stop(path, " is not a git repository")
  }

  # get list of commits
  temp <- read_lines(path_to_file, progress = TRUE)
  if (remove) unlink(path_to_file)

  data_frame(lines = temp)
}

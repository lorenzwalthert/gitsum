#' Summarize a git repo
#'
#' Producing a summary report of a repo
#' @param input_file the name of the temporary file that contains log data on
#'   which the report is base on. If `NULL`, a log file is created according to
#'   `path`, otherwise, a file is read.
#' @param .libpath_index index of the Library according to [base::libPaths] in
#'   which the template should be searched. The list with the available
#'   directories is returned invisibly for situations in which `.libPaths()`
#'   cannot be emulated.
#' @param template the template file to be used. Either one from a package
#'   (e.g. `gitsum::repo_summary_simple`), or a user defined template,
#'   which is then indicated with a path to this file including extension, i.e.
#'   `/users/xzy/a_template.Rmd`.
#' @param directory A directory to which the reports should be stored.
#' @param cached Whether or not the report should be cached with rmarkdown, so
#'   the graphs are written to a file.
#' @details Other packages can define templates. They must be store an .Rmd
#'   template in inst/report_templates. The data passed to the report template
#'   is log, the parsed log table.
#' @inheritParams get_raw_log
#' @inheritParams rmarkdown::render
#' @importFrom rmarkdown render
#' @import ggplot2
#' @export
git_report <- function(path = ".", output_file = NULL,
                       output_format = "html_document",
                       template = "gitsum::repo_summary_simple",
                       input_file = NULL,
                       directory = "gitsum", cached = TRUE,
                       .libpath_index = 1) {
  if (cached) {
    ensure_gitsum_repo(path)
    log <- parse_log_detailed(path = path)
  } else {
    log <- parse_log_detailed_full_run(path = path, file_name = input_file)
  }

  libpath <- .libPaths()[.libpath_index]

  template <- strsplit(template, "::", fixed = TRUE)[[1]]
  if (length(template) == 2) {
    path_in <- paste0(file.path(
      libpath, template[1], "report_templates",
      template[2]
    ), ".Rmd", collapse = "")
  } else {
    path_in <- path.expand(template)
  }

  if (!file.exists(path_in)) stop("template ", path_in, " does not exist.")

  if (!dir.exists(directory)) {
    dir.create(directory)
    message("created directory ", directory)
  }

  rmarkdown::render(
    input = path_in, # file 2
    output_format = output_format,
    output_file = output_file,
    output_dir = "./gitsum", quiet = TRUE
  )

  message("reports saved in ", directory)
  invisible(libpath)
}

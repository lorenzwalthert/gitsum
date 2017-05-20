#' Get a RMarkdown summary document of a repo
#' @param .libpath_index index of the Library according to [base:.libPaths] in
#'   which the template should be searched. The list with the available
#'   directories is returned invisibly for situations in which `.libPaths()``
#'   cannot be emulated.
#' @param template the template file to be used. Either one from a package
#'   (e.g. `gitsum::repo_summary_simple`), or a user defined template,
#'   which is then indicated with a path to this file including extension, i.e.
#'   `/users/xzy/a_template.Rmd`.
#' @param directory A directory to which the reports should be stored.
#' @param cache Whether or not the report should be cached with rmarkdown, so
#'   the graphs are writen to a file.
#' @details Other packages can define templates. They must be store an .Rmd
#'   template in inst/report_templates. The data passed to the report template
#'   is log, the parsed log table.
#' @inheritParams create_log
#' @inheritParams rmarkdown::render
#' @param file_out The path to the file the report should be created
#' @importFrom stringi stri_split
rmd_simple <- function(path = ".", output_file = NULL, output_format = "all",
                       template = "gitsum::repo_summary_simple",
                       directory = "gitsum", cached = FALSE,
                       .libpath_index = 1) {
  log <- get_log_regex(path = path)
  libpath <- .libPaths()[.libpath_index]

  template <- stri_split(template, fixed = "::")[[1]]
  if (length(template) == 2) {
    path_in <- concentrate(file.path(libpath, template[1], "report_templates",
                                     template[2]), ".Rmd")
  } else {
    path_in <- path.expand(template)
  }

  if (!file.exists(path_in)) stop("template ", path_in, " does not exist.")

  if(!dir.exists(directory)) {
    dir.create(directory)
    message("created directory ", directory)
  }

  rmarkdown::render(input = path_in,  # file 2
                    output_format = output_format,
                    output_file =  output_file,
                    output_dir = "./gitsum", quiet = TRUE)

  message("reports saved in ", directory)
  invisible(libpath)
}

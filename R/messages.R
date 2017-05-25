#' extract message or description from a log
#'
#' For message and description extraction, a different approach is used and
#'   one cannot rely on the approach with the simple extraction factory.
#' @param raw A character vector corresponding to one commit.
#' @param target either "message" or "description
find_message_and_desc <- function(raw, target) {
  ind <- which(raw == "    ")
  if (length(ind) == 1) {
    # if one line has just four spaces, we have both message and description
    c(substring(raw[ind - 1], 5), substring(raw[ind + 1], 5))
  } else {
    ind <- which(raw == "")[1]
    c(substring(raw[ind + 1], 5), NA)
  }
}

#' extract message or description from a log
#'
#' For message and description extraction, a different approach is used and
#'   one cannot rely on the approach with the simple extraction factory.
#' @param raw A character vector corresponding to one commit.
#' @param target either "message" or "description
#' @keywords internal
find_message_and_desc <- function(raw, target) {
  message_and_desc_sep <- which(raw == "")
  message_start <- message_and_desc_sep[1] + 1
  message_end <- message_and_desc_sep[2] - 1
  desc_sep <- which(raw == "    ")[1]
  if (is.na(desc_sep)) {
    c(paste(substring(raw[message_start:message_end], 5), collapse = "\n"), NA)
  } else {
    # if one line has just four spaces, we have both message and description
    c(
      paste(substring(raw[message_start:(desc_sep - 1)], 5), collapse = "\n"),
      paste(substring(raw[(desc_sep + 1):message_end], 5), collapse = "\n")
    )
  }
}

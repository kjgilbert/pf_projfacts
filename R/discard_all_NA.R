#' Discard dataframes without any data (all NA)
#'
#' @param .x dataframe
#'
#' @return data
#' @export
#' @import purrr
#' @importFrom magrittr %>%
#' 
#' 
#' @examples
#' x <- discard_all_NA(data.frame(a = 1, b = NA, c = 9))

discard_all_NA <- function(.x) {
  purrr::discard(.x, ~all(is.na(.)))
}
#' Set encoding of character variables to \code{latin-1}
#'
#' @param tabs e.g. all_tabs
#'
#' @return data
#' 
#' @import dplyr tidyselect
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' all_tabs <- getPFData() 
#' x <- setEncoding(all_tabs)
setEncoding <- function(tabs){
  purrr::map(tabs, function(x){
    x %>% 
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.character), 
                      stringr::str_conv, 
                      encoding = "latin-1"))
  })
}




#' Add custom fields to projects data
#'
#' @param project e.g. \code{all_tabs$project}
#' @param customfields e.g. \code{all_tabs$customfields}
#'
#' @return
#' @export
#'
#' @examples
#' project <- all_tabs$project
#' customfields <- all_tabs$customfields
#' project2 <- decodeCustomFields(project, customfields)
#' @importFrom magrittr %>%
decodeCustomFields <- function(project, customfields){
  
  tags <- customfields %>% 
    dplyr::mutate(
      storekey = as.character(STOREKEY),
      storekey = stringr::str_replace(storekey, "\\.", "\\\\."),
      regex = stringr::str_c("(?<=<", storekey, ">)(.|[[:space:]])+(?=</", storekey, ">)"),
      # regex = stringr::str_c("(?<=<", storekey, ">).{1,}(?=</", storekey, ">)"),
      name = janitor::make_clean_names(NAME),
      name = stringr::str_c("cf_", name)
    ) %>% 
    dplyr::select(PK_CUSTOMFIELD, NAME, AKTIV, STOREKEY, storekey, AREA, regex, name) 
    
  tags <- tags %>% 
    dplyr::filter(.data$STOREKEY %in% names(which(sapply(tags$STOREKEY, function(x) any(grepl(x, project$CUSTOMFIELDVALUES))))))
  
  out <- project
  
  if(nrow(tags) > 0){
    # res <- lapply(1:nrow(tags), function(x){
    #   name <- tags$name[x]
    #   regex <- tags$regex[x]
    #   z <- data.frame(z = stringr::str_extract(project$CUSTOMFIELDVALUES, regex))
    #   names(z) <- name
    #   z
    #   }) 
    cf <- project$CUSTOMFIELDVALUES
    for(x in 1:nrow(tags)){
      name <- tags$name[x]
      regex <- tags$regex[x]
      storekey <- tags$storekey[x]
      out[, name] <- NA
      tag_in <- !is.na(cf) & stringi::stri_detect_regex(cf, storekey)
      out[tag_in, name] <- stringi::stri_extract_first_regex(out$CUSTOMFIELDVALUES[tag_in], regex)
    }
    
    # out <- out %>% 
    #   dplyr::bind_cols(res)
  }
  
  return(out)
  
}

# microbenchmark(
#   decodeCustomFields(all_tabs$ticket, all_tabs$customfields),
#   decodeCustomFields2(all_tabs$ticket, all_tabs$customfields),
#   decodeCustomFields3(all_tabs$ticket, all_tabs$customfields),
#   times = 10
# )
# 
# microbenchmark(
# all_tabs$project$CUSTOMFIELDVALUES |> stringi::stri_detect_regex(tags$regex[1]),
# all_tabs$project$CUSTOMFIELDVALUES |> stringi::stri_extract_first_regex(tags$regex[1])
# , times = 10)

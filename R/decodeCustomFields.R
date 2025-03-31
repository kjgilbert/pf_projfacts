#' Add custom fields to projects data
#'
#' @param dat e.g. \code{all_tabs$project}
#' @param dat_ID_col the column name containing the unique ID field for that table, e.g. \code{"PK_Project"}
#' @param customfields e.g. \code{all_tabs$customfields}
#' @param customfieldvalue e.g. \code{all_tabs$customfieldvalue}
#' @param trace progress bar written to console, default = FALSE
#'
#' @return data
#' @import dplyr stringr stringi
#' @importFrom janitor make_clean_names
#' @export
#'
#' @examples
#' all_tabs <- getPFData()
#' project <- all_tabs$project
#' customfields <- all_tabs$customfields
#' customfieldvalue <- all_tabs$customfieldvalue
#' project2 <- decodeCustomFields(project, customfields)
#' @importFrom magrittr %>%
 


decodeCustomFields <- function(dat, dat_ID_col, customfields, customfieldvalue, trace = FALSE){
  
  STOREKEY <- NAME <- PK_CUSTOMFIELD <- AKTIV <- AREA <- NULL
  
  tags <- customfields %>%
    dplyr::mutate(
      storekey = as.character(STOREKEY),
      storekey = stringr::str_replace(storekey, "\\.", "\\\\."),
      name = janitor::make_clean_names(NAME),
      name = stringr::str_c("cf_", name)
    ) %>%
    dplyr::select(PK_CUSTOMFIELD, NAME, AKTIV, STOREKEY, AREA, name)
  
  # just take the ID number I need
  tags$storekey_trimmed <- sapply(strsplit(tags$STOREKEY, split = "\\."), function(x) x[2])
  
  # merge with customfieldvalue
  new_cfs <- merge(x=customfieldvalue, y=tags, by.x="FK_CUSTOMFIELD", by.y="storekey_trimmed")
  
  # just get the custom field values and ignore the NAs
  cf_values <- new_cfs[c("NUMBERVALUE", "DATEVALUE", "TEXTVALUE", "IDENTITYVALUE", "BOOLEANVALUE")]
  
  # can double check that there is only one value per row, but it is the case, and there is no empty row as also checked here
  ##num_nas_if_four_empty <- 4*dim(cf_values)[1]
  ##na_count <- sum(is.na(cf_values))
  
  cf_singlecolumn_value <- apply(cf_values, 1, function(x){ na.omit(x) })
  new_cfs$cf_singlecolumn_value <- cf_singlecolumn_value
  
  # take only the columns we need 
  new_cfs <- new_cfs %>%
    select(FK_ITEM, name, cf_singlecolumn_value)

  # reshape the single columns into their own columns for each cf
  cfs_reshaped <- new_cfs %>%
    pivot_wider(
      names_from = name,    # New column names come from the 'name' column
      values_from = cf_singlecolumn_value
    )
  
  # now merge with the dataframe of choice, e.g. tickets, projects, etc using FK_ITEM
  out <- dat %>%
    left_join(cfs_reshaped, by = setNames("FK_ITEM", dat_ID_col))
  
  return(out)
}




#' use the above decodeCustomFields function across all four instances of tables from pf
#' list = all_tabs
#' the direct backend download from projectfacts
#' @param list  \code{all_tabs}
#' 
#' @export


decodeAllCustomFields <- function(list){
  cfs <- list$customfields
  cfvs <- list$customfieldvalue
  list$customer <- decodeCustomFields(dat=list$customer, dat_ID_col="PK_CUSTOMER", customfields=cfs, customfieldvalue=cfvs)
  list$project <- decodeCustomFields(dat=list$project, dat_ID_col="PK_Project", customfields=cfs, customfieldvalue=cfvs)
  list$ticket <- decodeCustomFields(dat=list$ticket, dat_ID_col="PK_TICKET", customfields=cfs, customfieldvalue=cfvs)
  list$worker <- decodeCustomFields(dat=list$worker, dat_ID_col="PK_Worker", customfields=cfs, customfieldvalue=cfvs)

  return(list)
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


# z <- lapply(1:nrow(tags), function(x){
#   name <- tags$name[x]
#   regex <- tags$regex[x]
#   storekey <- tags$storekey[x]
#   rm <- regmatches(xcf, regexpr(regex, xcf, perl = TRUE))
#   list(regex = regex,
#        storekey = stringi::stri_detect_regex(xcf, storekey),
#        full_regex = grepl(regex, xcf, perl = TRUE),
#        regmatch = ifelse(length(rm) > 0, rm, NA)
#        )
# })
# 
# do.call(rbind, z) |> View()



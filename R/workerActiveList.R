#' List of workers with their active status
#'
#' @param all_tabs e.g. the result from \code{getPFData()}
#'
#' @return data
#' @import dplyr
#' @importFrom stringr str_detect
#' @export
#' 
#' @examples
#' all_tabs <- getPFData()
#' x <- setEncoding(all_tabs) 
#' y <- workerActiveList(x)
#' 

workerActiveList <- function(all_tabs){
  
  Vorname <- Nachname <- Path <- Path <- PK_CUSTOMER <- FK_CRMKONTAKT <- FK_CUSTOMER <- EMail <- workerAktiv <- NULL
  
  dplyr::left_join(all_tabs$worker %>% 
                    dplyr::select(-c(Vorname, Nachname)), 
                  all_tabs$crmkontakt %>% 
                    discard_all_NA(), 
                  by = c("FK_CRMKONTAKT" = "PK_CRMKONTAKT")) %>% 
    dplyr::left_join(all_tabs$customer %>% 
                dplyr::filter(stringr::str_detect(Path, "CTU Bern")) %>% 
                dplyr::select(PK_CUSTOMER, Path),
              by = c("FK_CUSTOMER" = "PK_CUSTOMER")) %>% 
    dplyr::select(FK_CRMKONTAKT, FK_CUSTOMER, Vorname, 
                  Nachname, EMail, Path, workerAktiv) %>% 
    dplyr::rename(Customer = Path,
           'First name' = Vorname,
           'Last name' = Nachname,
           Email = EMail)
}




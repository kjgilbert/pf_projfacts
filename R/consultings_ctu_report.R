#' Function to get the booked hours for consultings in total and in each year
#' 
#' This code has been adapted for the purpose of producing the graphs for the QMR Yearly Report and CTU Annual Report.
#' The code gets (among others) the createYear of the ticket, customer (top_CustomerName, CustomerName, CustomerName2, Organization), default project of customer,
#' extracts whether it is a grant application consulting, runs prepTime to get total time bookings and time bookings for each year.
#' 
#' @param all_tabs result from e.g. getPFData
#' 
#' @import dplyr
#' @importFrom lubridate year
#' @return consultings
#' @export
#' 
#' @examples 
#' all_tabs <-  getPFData()
#' x <- consultings_ctu_report(all_tabs)



consultings_ctu_report <- function(all_tabs){
  
  CreateDate <- TICKETID <- PK_Project <- Name <- PK_CUSTOMER <- CustomerName <- CustomerName4 <- default_project <- FK_TICKET <- Timespent <- total_time <- BookedDate <- BookedYear <- time_per_year <- SUBJECT <- Categories <- cf_grant_application_or_submission_to_snf_eu_charity <- CreateYear <- hrs_per_year <- NULL
  
  consultings <- all_tabs$ticket |> 
    dplyr::mutate(CreateYear = lubridate::year(CreateDate)
    ) |> 
    dplyr::filter(grepl("C-", TICKETID)) |> 
    
    # join with table 'customer'
    dplyr::left_join(
      all_tabs$customer |> 
        dplyr::left_join(pf::constructCustomerParents(all_tabs$customer)) |> 
        # join with table 'projects' to get consulting project of customer
        dplyr::left_join(all_tabs$project |> 
                           dplyr::select(PK_Project, default_project = Name),
                         by = c("FK_DEFAULTPROJECT" = "PK_Project")) |> 
        dplyr::select(PK_CUSTOMER, CustomerName:CustomerName4,default_project),
      by = c(FK_CUSTOMER = "PK_CUSTOMER")
    ) |> 
      
    # join with total time bookings (see prepTime.R)
    dplyr::left_join(
      pf::prepTime(all_tabs) |> 
        dplyr::filter(!is.na(FK_TICKET)) |> 
        dplyr::group_by(FK_TICKET) |>
        dplyr::summarise(total_time = sum(Timespent, na.rm = TRUE)) |>
        dplyr::mutate(total_hrs = total_time/60),
      by = c(PK_TICKET = "FK_TICKET")
    ) |> 
    
    # join with time bookings per year (see prepTime.R)  
    dplyr::right_join(
      pf::prepTime(all_tabs) |> 
        dplyr::filter(!is.na(FK_TICKET)) |> 
        dplyr::mutate(BookedYear = lubridate::year(BookedDate)) |> 
        dplyr::group_by(FK_TICKET,BookedYear) |> 
        dplyr::summarise(time_per_year = sum(Timespent, na.rm = TRUE)) |> 
        dplyr::mutate(hrs_per_year = time_per_year/60),
      by = c(PK_TICKET = "FK_TICKET"),
      multiple = "all") |> 
    
    # wrap-up
    dplyr::select(TICKETID,
                  SUBJECT,
                  Categories,
                  GrantApplication = cf_grant_application_or_submission_to_snf_eu_charity,
                  CreateYear:hrs_per_year)
  
  return(consultings)
}

#' Function to get the booked hours for projects in total and in each year
#' 
#' This code has been adapted for the purpose of producing the graphs for the QMR Yearly Report and CTU Annual Report. 
#' The code gets (among others) customer info (top_CustomerName, CustomerName, CustomerName2, Organization), default project of customer,
#' center info (mono/multi), ordinance info, archiving date of the project, runs prepTime to get total time bookings and time bookings for each year, 
#' categorizes sum of time bookings (< 8h, 9-20h,...).
#' 
#' @param all_tabs result from e.g. getPFData
#' 
#' @return projects
#' 
#' @import dplyr lubridate stringr
#' @export
#' 
#' @examples 
#' all_tabs <-  getPFData()
#' x <- projects_ctu_report(all_tabs)



projects_ctu_report <- function(all_tabs){

  CreateDate <- CaseId <- Path <- PK_CUSTOMER <- FK_DEFAULTPROJECT <- PK_Project <- Name <- FK_PROJECT <- FK_PROJECTSTATEDEFINITION <- ClosedDate <- NAME <- ArchivingDate <- Centers <- Ordinance <- Ordinance_sub <- CustomerName <- CustomerName4 <- default_project <- CreateYear <- ArchivingYear <- BookedDate <- top_project <- ctu_division <- Timespent <- total_time <- projnum <- total_divisions <- total_hrs <- total_hrs_cat_500 <- total_hrs_cat_100 <- BookedYear <- ctu_projectName <- proj <- projecttype <- time_per_year <- divisions_per_year <- hrs_per_year <- hrs_per_year_cat_500 <- hrs_per_year_cat_100 <- NULL

  projects <- all_tabs$project  |> 
    dplyr::mutate(CreateYear = lubridate::year(CreateDate)) |> 
                    
    dplyr::filter(
      # only select projects
      grepl("P-|FTE", CaseId),
      # remove test projects
      !grepl("\\bTest\\b", Path,ignore.case = TRUE),
      # remove Uni REDCap Project
      CaseId != "FTE-1939",
      # remove TO support, Uni REDCap
      CaseId != "P-1444",
      # remove SCTO/SERI project
      CaseId != "P-0733" 
      )  |> 
    
    
    # get customer info
    dplyr::left_join(constructCustomerParents(all_tabs$customer),
              by = c("FK_CUSTOMER"="PK_CUSTOMER")) |> 
    
    # get default (=consulting) project
    dplyr::left_join(all_tabs$customer  |>  
                dplyr::select(PK_CUSTOMER, FK_DEFAULTPROJECT) |>
                dplyr::left_join(all_tabs$project |> 
                                   dplyr::select(PK_Project,
                                                 default_project = Name),
                                 by=c("FK_DEFAULTPROJECT"="PK_Project")),
              by=c("FK_CUSTOMER"="PK_CUSTOMER"))  |> 
    
    # get archiving date
  dplyr::left_join(all_tabs$projectactivity |> 
                     dplyr::select(FK_PROJECT,FK_PROJECTSTATEDEFINITION,
                                   ArchivingDate=ClosedDate) |> 
                     dplyr::left_join(all_tabs$projectstatedefinition |> 
                                        dplyr::select("PK_PROJECTSTATEDEFINITION","NAME"),
                                      by = c("FK_PROJECTSTATEDEFINITION" = "PK_PROJECTSTATEDEFINITION")
                     ) |> 
                     dplyr::filter(NAME == "Study: Archived") |>   
                     dplyr::arrange(FK_PROJECT, ArchivingDate) |> 
                     # some projects have multiple archiving-dates (e.g. if somebody changed the project state again at a later point)
                     # take only the older one:
                     dplyr::distinct(FK_PROJECT,.keep_all = TRUE) |> 
                     dplyr::mutate(ArchivingYear = lubridate::year(ArchivingDate)), 
                   by = c("PK_Project" = "FK_PROJECT"))  |> 
    
  # get center info
  dplyr::mutate(
    Centers = dplyr::case_when(
    grepl("Single", cf_setup_number_of_sites) ~ "monocenter",
    grepl("Multi", cf_setup_number_of_sites) & cf_location == "Swiss" ~ "multicenter (national only)",
    grepl("Multi", cf_setup_number_of_sites) & cf_location == "International" ~ "multicenter (international)"
  )) |> 
    
    # get ordinance info
    dplyr::mutate(
      Ordinance = dplyr::case_when(
        stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("ClinO", ignore_case = TRUE)) ~ "ClinO",    
        stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("HRO", ignore_case = TRUE)) ~ "HRO",
        stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("HRA", ignore_case = TRUE)) ~ "Non-HRA"),
      Ordinance_sub = dplyr::case_when(
        stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("IMP", ignore_case = TRUE)) ~ "IMP",
        stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Medical Device", ignore_case = TRUE)) ~ "MD",
        stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Other health", ignore_case = TRUE)) ~ "Other",
        stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Further", ignore_case = TRUE)) ~ "Further Use",
        stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Observational", ignore_case = TRUE)) ~ "Sample Data Collection")
    ) |> 
    dplyr::mutate(dplyr::across(c(Centers, Ordinance, Ordinance_sub), 
                                as.factor),
                  Centers = factor(Centers, 
                                   levels(Centers)[c(1,3,2)])) |> 
  
  # clean-up
  dplyr::select(PK_Project,
                CaseId,Name,
                CustomerName:CustomerName4, 
                default_project, 
                Centers, 
                Ordinance,Ordinance_sub,
                CreateYear,ArchivingYear
  )  |> 
    
    
    # join with total time bookings (see prepTime.R)
    dplyr::left_join(pf::prepTime(all_tabs)  |>   
                       dplyr::mutate(BookedYear = lubridate::year(BookedDate)) |> 
                       dplyr::group_by(top_project, ctu_division) |> 
                       dplyr::summarise(total_time = sum(Timespent, na.rm = TRUE))  |> 
                       dplyr::group_by(top_project) |>
                       dplyr::summarise(total_divisions = paste0(ctu_division, collapse = ","),
                                        total_time = sum(total_time, na.rm = TRUE)),
                     by = c("PK_Project" = "top_project"))  |> 
    
    # merge matching FTE and P projects:
    tidyr::separate(CaseId, into = c("projtype","projnum"), sep = "-",remove = FALSE) |>
    dplyr::group_by(projnum) |> 
    dplyr::mutate(
      # add up total time
      total_time = sum(total_time),
      # merge divisions (only if they are unique)
      total_divisions = paste(unique(unlist(strsplit(total_divisions, ","))), collapse = ",")
      ) |> 
    dplyr::ungroup() |> 
    
  # categorize total time bookings
    dplyr::mutate(total_time = dplyr::if_else(is.na(total_time),0,total_time),
                  total_hrs = round(total_time/60,digits=2),
                  total_hrs_cat_500 = cut(total_hrs, 
                                          breaks = c(0,7.9999,20.49,40.49,60.49,100.49,500.49,Inf),
                                          labels = c("< 8h", "8-20h", "21-40h", "41-60h", "61-100h", "101-500h", "> 500h")),
                  total_hrs_cat_100 = cut(total_hrs, 
                                          breaks = c(0,7.9999,20.49,40.49,60.49,100.49,Inf),
                                          labels = c("< 8h", "8-20h", "21-40h", "41-60h", "61-100h", "> 100h")),
                  dplyr::across(c(total_hrs_cat_500,total_hrs_cat_100),as.factor)
    )  |> 
    
    # join with time bookings per year (see prepTime.R)
    dplyr::left_join(pf::prepTime(all_tabs) |> 
                       dplyr::mutate(BookedYear = lubridate::year(BookedDate)) |> 
                       dplyr::select(top_project,BookedDate, BookedYear, Timespent, ctu_projectName, ctu_division, proj, projnum, projecttype )  |> 
                       
                       dplyr::group_by(top_project, ctu_division, BookedYear) |>
                       dplyr::summarise(time_per_year = sum(Timespent, na.rm = TRUE)) |> 
                       dplyr::group_by(top_project,BookedYear) |>
                       dplyr::summarise(divisions_per_year = paste0(ctu_division, collapse = ","),
                                        time_per_year = sum(time_per_year, na.rm = TRUE)),
                     by=c("PK_Project" = "top_project")) |> 
    
    dplyr::select(-PK_Project)  |> 
    
    # merge matching FTE and P projects:
    dplyr::group_by(projnum,BookedYear) |> 
    dplyr::mutate(
      # add up time per year
      time_per_year = sum(time_per_year),
      # merge divisions (only if they are unique)
      new = paste(unique(unlist(strsplit(divisions_per_year, ","))), collapse = ",")
    )  |> 
    dplyr::ungroup() |> 
  
  # categorize time bookings per year
  dplyr::mutate(hrs_per_year = time_per_year/60,
                time_per_year = dplyr::if_else(is.na(time_per_year),0,time_per_year),
                hrs_per_year_cat_500 = cut(hrs_per_year, 
                                               breaks = c(0,7.9999,20.49,40.49,60.49,100.49,500.49,Inf),
                                               labels = c("< 8h", "8-20h", "21-40h", "41-60h", "61-100h", "101-500h", "> 500h")),
                hrs_per_year_cat_100 = cut(hrs_per_year, 
                                               breaks = c(0,7.9999,20.49,40.49,60.49,100.49,Inf),
                                               labels = c("< 8h", "8-20h", "21-40h", "41-60h", "61-100h", "> 100h")),
                dplyr::across(c(hrs_per_year_cat_500,hrs_per_year_cat_100),as.factor)
    )  |> 
    
    # remove FTE-projects with matching P-project
    dplyr::distinct(projnum,BookedYear,.keep_all = TRUE)
    
    
    
      
      
return(projects)
}


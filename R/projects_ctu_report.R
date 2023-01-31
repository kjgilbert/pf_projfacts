#' function to get the projects in a specific year
#' 
#' This code has been adapted for the purpose of producing the graphs for the CTU Yearly report
#' 
#' @param all_tabs result from e.g. getPFData
#' @param selected_year year for which projects should be summarized
#' 
#' @return projects_year
#' @export
#' 
#' @examples 
#' all_tabs <-  getPFData()
#' x <- projects_ctu_report(all_tabs,selected_year)



projects_ctu_report <- function(all_tabs,selected_year){


# TABLES ----
### Needed tables with information for the CTU Report: activitydata, project, customer 
tmp1 <- pf::constructCustomerParents(all_tabs$customer) %>% 
  dplyr::left_join(all_tabs$customer %>% 
                     dplyr::select(PK_CUSTOMER, FK_DEFAULTPRICELIST) %>%
                     dplyr::left_join(all_tabs$pricerecord %>%
                                        dplyr::select(PK_PRICERECORD, NAME), 
                                      by=c("FK_DEFAULTPRICELIST"="PK_PRICERECORD")),
                   by="PK_CUSTOMER") %>% 
  dplyr::rename(Organization = NAME) %>%
  dplyr::left_join(all_tabs$project %>% 
              dplyr::select(PK_Project, FK_CUSTOMER, Path, CaseId, cf_applicable_ordinance_2, cf_study_category,
                     cf_snf, cf_eu, cf_charity_non_profit, cf_industry, cf_other_funding, cf_swiss_ctu, 
                     cf_other_ctu_cro, cf_location, cf_setup_number_of_sites), 
            by=c("PK_CUSTOMER"="FK_CUSTOMER")) %>%
  dplyr::filter(grepl("P-|FTE", CaseId)) %>%
  dplyr::select(top_CustomerName, CustomerName, Organization, Path, CaseId, cf_applicable_ordinance_2, cf_study_category,
                cf_snf, cf_eu, cf_charity_non_profit, cf_industry, cf_other_funding, cf_swiss_ctu, 
                cf_other_ctu_cro, cf_location, cf_setup_number_of_sites) %>%
  dplyr::mutate(projnum = readr::parse_number(gsub('[-]', ' ', CaseId))) %>%
  dplyr::arrange(projnum) %>%
  dplyr::distinct(projnum, Path, .keep_all=TRUE) %>%
  dplyr::mutate(across(projnum, as.character)) %>%
  dplyr::mutate(projnum = stringr::str_pad(projnum, 4, pad = "0")) %>%
  dplyr::rename(Project.name = Path) %>%
  dplyr::relocate(projnum, Project.name, .before = top_CustomerName)

### Select projects and FTE necessary. 
tmp2 <- pf::prepTime(all_tabs) %>%
  filter(lubridate::year(BookedDate) == selected_year) %>%
  dplyr::filter(grepl("P-|FTE", proj)) %>%
  select(BookedDate, Timespent, ctu_projectName, ctu_division, proj, projnum, projecttype) %>%
  group_by(projnum, ctu_division) %>% 
  summarise(total_time = sum(Timespent, na.rm = TRUE),
  ProjectName = first(ctu_projectName)) %>%
  group_by(projnum) %>%
  summarise( total_time = sum(total_time, na.rm = TRUE),
             divisions = paste0(ctu_division, collapse = ",")) %>%
  dplyr::mutate(Data.management = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("DM", ignore_case = TRUE))  ~ "yes", 
    TRUE ~ "no") ) %>%
  dplyr::mutate(Project.management = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("CSM|QM", ignore_case = TRUE))  ~ "yes", 
    TRUE ~ "no") ) %>%
  dplyr::mutate(Monitoring = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("MON", ignore_case = TRUE))  ~ "yes",
    TRUE ~ "no") ) %>%
  dplyr::mutate(Statistics = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("STA", ignore_case = TRUE))  ~ "yes",
    TRUE ~ "no") ) %>%
  dplyr::mutate(Clinical.investigation = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("CI", ignore_case = TRUE))  ~ "yes",
    TRUE ~ "no") )

### merge the two dataframes 
tmp1_2 <- merge(tmp1, tmp2, by="projnum", all.x = FALSE) 

### create variable centers, ordinance, customer, institution, cat for reports
projects_year <- tmp1_2%>% 
  dplyr::mutate(Centers = dplyr::case_when(
    stringr::str_detect(cf_location, stringr::regex("International", ignore_case = TRUE)) ~ "multicenter (international)",
    stringr::str_detect(cf_setup_number_of_sites, stringr::regex("Single center", ignore_case = TRUE)) ~ "monocenter",
    cf_location=="Swiss" & cf_setup_number_of_sites=="Multicenter" ~ "multicenter (national only)" ) ) %>% 
  dplyr::mutate(Ordinance_scto = dplyr::case_when(
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("IMP", ignore_case = TRUE)) ~ "ClinO_IMP",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Medical Device", ignore_case = TRUE)) ~ "ClinO_MD",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Other health", ignore_case = TRUE)) ~ "ClinO_Other",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Further", ignore_case = TRUE)) ~ "HRO_FurtherUse",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Observational", ignore_case = TRUE)) ~ "HRO_sampleDataCollection",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("HRA", ignore_case = TRUE)) ~ "Non-HRA",
  ))  %>% 
  dplyr::mutate(dplyr::across(c(cf_snf,cf_eu,cf_charity_non_profit, cf_industry, cf_other_funding, cf_swiss_ctu, cf_other_ctu_cro), 
                tolower) ) %>%
  dplyr::filter(projnum != 9999) %>%
  dplyr::mutate(dplyr::across(c(Centers, Ordinance_scto), 
                as.factor)) %>%
  dplyr::mutate(Centers = factor(Centers, 
                          levels(Centers)[c(1,3,2)]))  %>% 
  dplyr::mutate(Customer = dplyr::case_when(top_CustomerName == "Insel Gruppe AG" ~ "Insel",
                                            # medical faculty 
                                            # retrieved from https://www.medizin.unibe.ch/ueber_uns/fakultaet/index_ger.html 2023-01-27
                                            # names compared with projectfacts
                                            CustomerName == "Dekanat Medizinische Fakultät"  ~ "UniBe Medical Faculty",
                                            CustomerName == "Forensisch-Psychiatrischer Dienst (FPD)" ~"UniBe Medical Faculty",
                                            CustomerName == "Institut für Infektionskrankheiten" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institut für Pathologie" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institut für Rechtsmedizin" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institut für Anatomie" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institut für Biochemie und Molekulare Medizin" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institute of Biochemistry  and Molecular Medicine" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institut für Medizingeschichte" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institut für Pharmakologie" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institut für Physiologie" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institute of Social and Preventive Medicine (ISPM)" ~ "UniBe Medical Faculty",
                                            CustomerName == "Theodor Kocher Institut (TKI)" ~ "UniBe Medical Faculty",
                                            CustomerName == "Department for BioMedical Research" ~ "UniBe Medical Faculty",
                                            CustomerName == "Zahnmedizinische Kliniken" ~ "UniBe Medical Faculty",
                                            CustomerName == "ARTORG Center for Biomedical Engineering Research" ~ "UniBe Medical Faculty",
                                            CustomerName == "Bern Center for Precision Medicine (BCPM)" ~ "UniBe Medical Faculty", # not yet in PF
                                            CustomerName == "sitem Center for Translational Medicine and Biomedical Entrepreneurship" ~ "UniBe Medical Faculty",
                                            CustomerName == "Zentrum für Künstliche Intelligenz in der Medizin" ~ "UniBe Medical Faculty", # not yet in PF
                                            CustomerName == "Berner Institut für Hausarztmedizin (BIHAM)" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institut für Komplementäre und Integrative Medizin" ~ "UniBe Medical Faculty",
                                            CustomerName == "Institut für Medizinische Lehre" ~ "UniBe Medical Faculty",
                                            top_CustomerName == "Universitäre Psychiatrische Dienste" ~ "UniBe Medical Faculty",
                                            # UNIBE other (all other institutes)
                                            #top_CustomerName == "Universität Bern" ~ "UniBe Other",
                                            # external non-profit
                                            top_CustomerName == "External Non-Profit" ~ "External Non-Profit",
                                            Organization == "External non-profit" ~ "External Non-Profit",
                                            top_CustomerName == "Lindenhofgruppe" ~ "External Non-Profit",
                                            # external for-profit
                                            top_CustomerName == "External For-Profit" ~ "External For-Profit",
                                            Organization == "External for-profit" ~ "External For-Profit"),
                Institution = dplyr::case_when(Customer == "Insel" ~ CustomerName,
                                               Customer == "UniBe Medical Faculty" ~ CustomerName,
                                               Customer == "UniBe Other" ~ CustomerName,
                                               Customer == "External Non-Profit" ~ Customer,
                                               Customer == "External For-Profit" ~ Customer),
                Institution = stringr::str_remove(Institution,"Universitätsklinik für "),
                Category = dplyr::case_when(Customer == "Insel" ~ "Insel",
                                            Customer == "UniBe Medical Faculty" ~ "UniBe",
                                            Customer == "UniBe Other" ~ "UniBe",
                                            Customer == "External For-Profit" ~ "External For-Profit",
                                            Customer == "External Non-Profit" ~ "External Non-Profit")
  ) 

return(projects_year)
}


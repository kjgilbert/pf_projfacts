#' Function to get sponsoring institution and organization for projects/consultings
#' 
#' This code has been adapted for the purpose of producing the graphs for the QMR Yearly Report and CTU Annual Report.
#' The code generates the variable SponsorOrganization (Insel, MedFac, UniBe Other, External Non/For Profit),
#' SponsorInstitution (e.g., Department at Inselspital), and SponsorInst_short (abbreviated institution-name to be used for plotting)

#' 
#' @param input_data consultings/project data table with minimum variables CustomerName, CustomerName2, Organization, default_project
#' 
#' @return output_data
#' 
#' @import dplyr
#' @export
#' 
#' @examples 
#' all_tabs <-  getPFData()
#' input_data <-  consultings_ctu_report(all_tabs)
#' x <- get_sponsor(input_data)


get_sponsor <- function(input_data){
  
  SponsorInstitution <- SponsorOrganization <- SponsorInst_short <- SponsorUniBe <- NULL
  
  output_data <- input_data %>% 
    dplyr::mutate(
      
      # SponsorOrganization: Insel, UniBe MedFak, UniBe Other, External Non or For Profit
      SponsorOrganization = dplyr::case_when(CustomerName1 == "Insel Gruppe AG" ~ "Insel",
                                            # medical faculty 
                                            CustomerName1 == "Universit\u00E4t Bern" &
                                              CustomerName2 == "Medizinische Fakult\u00E4t" ~ "UniBe Medical Faculty",
                                            # UNIBE other
                                            # some have no faculty/instute specified
                                            CustomerName == "Universit\u00E4t Bern"  | 
                                              CustomerName1 == "Universit\u00E4t Bern" &
                                              CustomerName2 != "Medizinische Fakult\u00E4t" ~ "UniBe Other",
                                            # external for-profit
                                            default_project == "Consulting External For-Profit" ~ "External For-Profit",
                                            # external non-profit
                                            CustomerName1 == "Lindenhofgruppe" |
                                              CustomerName == "Lindenhofgruppe" ~ "External Non-Profit",
                                            grepl("External",default_project) &
                                              default_project != "Consulting External For-Profit" ~ "External Non-Profit"
                                            ),
      
      # SponsorInstitution: Insel Clinic or Insitute of UniBe
      SponsorInstitution = dplyr::case_when(SponsorOrganization == "Insel" ~ CustomerName,
                                            SponsorOrganization == "UniBe Medical Faculty" ~ CustomerName,
                                            SponsorOrganization == "UniBe Other" &
                                              !is.na(CustomerName1) ~ CustomerName,
                                            SponsorOrganization == "UniBe Other" &
                                              is.na(CustomerName1) ~ SponsorOrganization, # if no faculty/institute provided
                                            SponsorOrganization == "External Non-Profit" ~ SponsorOrganization,
                                            SponsorOrganization == "External For-Profit" ~ SponsorOrganization),
      SponsorInstitution = stringr::str_remove(SponsorInstitution,"^Universit\u00E4tsklinik f\u00FCr "),
      SponsorInstitution = stringr::str_remove(SponsorInstitution, "^Universit\u00E4tsinstitut f\u00FCr "),
      
      # SponsorInst_short: shorten names for plotting
      SponsorInst_short = dplyr::case_when(SponsorInstitution == "An\u00E4sthesiologie und Schmerztherapie" ~ "An\u00E4sth. und Schmerztherapie",
                                    SponsorInstitution == "ARTORG Center for Biomedical Engineering Research" ~ "ARTORG",
                                    SponsorInstitution == "Berner Institut f\u00FCr Hausarztmedizin (BIHAM)" ~ "BIHAM",
                                    SponsorInstitution == "Center for the Study of Language and Society (CSLS)" ~ "CSLS",
                                    SponsorInstitution == "Centre for Development and Environment (CDE)" ~ "CDE",
                                    SponsorInstitution == "Department for BioMedical Research" ~ "Dep. for BioMedical Research",
                                    SponsorInstitution == "Diagnostische, Interventionelle und P\u00E4diatrische Radiologie" ~ "Diagn., Interv. und P\u00E4d. Radiologie",
                                    SponsorInstitution == "Diagnostische und Interventionelle Neuroradiologie" ~ "Diagn. und Interv. Neuroradiologie",
                                    SponsorInstitution == "Endokrinologie, Diabetologie und Klinische Ern\u00E4hrung" ~ "Endokr., Diabet. und Klin. Ern\u00E4hrung",
                                    SponsorInstitution == "Forensisch-Psychiatrischer Dienst (FPD)" ~ "FPD",
                                    SponsorInstitution == "Hals, Nasen- und Ohrenkrankheiten (HNO), Kopf- und Halschirurgie" ~ "HNO, Kopf- und Halschirurgie",
                                    SponsorInstitution == "H\u00E4matologie und H\u00E4matologisches Zentrallabor" ~ "H\u00E4matologie und H\u00E4mat. Zentrallabor",
                                    SponsorInstitution == "Institute of Biochemistry  and Molecular Medicine" ~ "Inst. of Biochem. and Mol. Med.", 
                                    SponsorInstitution == "Institute of Social and Preventive Medicine (ISPM)" ~ "ISPM",
                                    SponsorInstitution == "Institut f\u00FCr Infektionskrankheiten" ~ "Inst. f\u00FCr Infektionskrankheiten",
                                    SponsorInstitution == "Institut f\u00FCr Komplement\u00E4re und Integrative Medizin" ~ "Inst. f\u00FCr Kompl. und Integ. Medizin",
                                    SponsorInstitution == "Klinische Psychologie und Psychotherapie" ~ "Klin. Psychologie und Psychoth.",
                                    SponsorInstitution == "Philosophisch-humanwissenschaftliche Fakult\u00E4t" ~ "Phil.-humanw. Fakult\u00E4t",
                                    SponsorInstitution == "Rheumatologie und Immunologie" ~ "Rheumato- und Immunologie",
                                    SponsorInstitution == "Sch\u00E4del-, Kiefer- und Gesichtschirurgie" ~ "Sch\u00E4.-, Kief.- und Ges.-chirurgie",
                                    SponsorInstitution == "sitem Center for Translational Medicine and Biomedical Entrepreneurship" ~ "sitem Center for Transl. Medicine...",
                                    SponsorInstitution == "Universit\u00E4res Zentrum f\u00FCr Palliative Care" ~ "Univ. Zentrum f\u00FCr Palliative Care",
                                    SponsorInstitution == "Universit\u00E4re Psychiatrische Dienste" ~ "UPD",
                                    TRUE ~ SponsorInstitution
      ),
      SponsorOrganization = as.factor(SponsorOrganization),
      SponsorInstitution = as.factor(SponsorInstitution),
      SponsorInst_short = as.factor(SponsorInst_short),
      
      # SponsorUniBe: mark all UniBe organizations
      SponsorUniBe = dplyr::if_else(grepl("UniBe", SponsorOrganization), "UniBe", SponsorOrganization),
      SponsorUniBe = as.factor(SponsorUniBe)
    )
    
  return(output_data)
}

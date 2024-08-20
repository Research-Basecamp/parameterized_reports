##Read in
library(tidyverse)
library(openxlsx)
library(janitor)


#JAN-2024

#FEB-2024
#url <- "https://ohss.dhs.gov/sites/default/files/2024-06/24-0607_ohss_immigration-enforcement-and-legal-processes-tables-february-2024.xlsx"

#MAR-2024
#url <- "https://ohss.dhs.gov/sites/default/files/2024-07/24-0705_ohss_immigration-enforcement-and-legal-processes-tables-march-2024.xlsx"

#APR-2024
url <- "24-0809_ohss_immigration-enforcement-and-legal-processes-tables-april-2024.xlsx"


enc_region <-openxlsx::read.xlsx(url, sheet=2)
enc_ctzn <- openxlsx::read.xlsx(url, sheet=3)
enc_family <- openxlsx::read.xlsx(url, sheet = 4)
enc_office <- openxlsx::read.xlsx(url, sheet = 5)
tot_enc_ctzn_100 <- openxlsx::read.xlsx(url, sheet = 6)
tot_ofo_ctzn_100 <- openxlsx::read.xlsx(url, sheet = 7)
swborder_enc_ctzn_100 <- openxlsx::read.xlsx(url, sheet = 8)
cbp1_apt_ctzn <- openxlsx::read.xlsx(url, sheet = 9)
swborder_bookouts_agency <- openxlsx::read.xlsx(url, sheet = 10)
swborder_bookouts_family <- openxlsx::read.xlsx(url, sheet = 11)
swborder_bookouts_ctzn <- openxlsx::read.xlsx(url, sheet = 12)
ero_arrests_ctzn <- openxlsx::read.xlsx(url, sheet = 13)
ero_arrests_crim <- openxlsx::read.xlsx(url, sheet = 14)
ice_bookins_ctzn <- openxlsx::read.xlsx(url, sheet = 15)
ice_bookins_crim <- openxlsx::read.xlsx(url, sheet = 16)
ice_bookins_agency <- openxlsx::read.xlsx(url, sheet = 17)
ice_bookouts_crim <- openxlsx::read.xlsx(url, sheet = 18)
ice_bookouts_agency <- openxlsx::read.xlsx(url, sheet = 19)
ice_ero_RR_ctzn <- openxlsx::read.xlsx(url, sheet = 20)
ice_ero_RR_crim <- openxlsx::read.xlsx(url, sheet = 21)
ice_ero_RR_agency <- openxlsx::read.xlsx(url, sheet = 22)
dhs_repats_type <- openxlsx::read.xlsx(url, sheet = 23)
dhs_removals_crim <- openxlsx::read.xlsx(url, sheet = 24)
dhs_removals_agency <- openxlsx::read.xlsx(url, sheet = 25)
dhs_removals_ctzn <- openxlsx::read.xlsx(url, sheet = 26)
dhs_enfreturns_ctzn <- openxlsx::read.xlsx(url, sheet = 27)
dhs_adminreturns_ctzn <- openxlsx::read.xlsx(url, sheet = 28)
swborder_credfear_ctzn <- openxlsx::read.xlsx(url, sheet = 29)
swborder_credfear_result <- openxlsx::read.xlsx(url, sheet = 30)
tot_credfear_uscis_ctzn <- openxlsx::read.xlsx(url, sheet = 31)
tot_credfear_result <- openxlsx::read.xlsx(url, sheet = 32)
chnv_beneficiaries <- openxlsx::read.xlsx(url, sheet = 33)
chnv_auth <- openxlsx::read.xlsx(url, sheet = 34)
chnv_paroles <- openxlsx::read.xlsx(url, sheet = 35)

nested_cleaner <- function(df){
  headings <- df |>
    filter(row_number()<4) |>
    filter(row_number()>1)
  
  headings2 <- as.data.frame(t(headings))|>
    fill(everything(),.direction = "down")|>
    mutate(final_names = paste0(V1, "-", V2)) |>
    mutate(across("final_names", str_replace, "NA-", ""))
  final_names <- headings2$final_name
  
  lastrow <- nrow(df)
  lastrow_heading <- nrow(df)+1
  
  clean <- df |>
    rbind(final_names) |>
    slice(lastrow_heading, 2:lastrow) |>
    row_to_names(row_number = 1) |>
    clean_names() |>
    filter(!is.na(month))|>
    filter(month != "Month") |>
    fill(fiscal_year, .direction = "down")
  
}

cleaner <- function(df){
  clean <- df |>
    row_to_names(row_number = 2) |>
    clean_names() |>
    fill(fiscal_year, .direction = "down") |>
    filter(!is.na(month))
}

enc_region_clean <- nested_cleaner(enc_region)
enc_ctzn_clean <- nested_cleaner(enc_ctzn)
enc_family_clean <- nested_cleaner(enc_family)
enc_office_clean <- nested_cleaner(enc_office)
tot_enc_ctzn_100_clean <- cleaner(tot_enc_ctzn_100) 
tot_ofo_ctzn_100_clean <- cleaner(tot_ofo_ctzn_100) 
swborder_enc_ctzn_100_clean <- cleaner(swborder_enc_ctzn_100)
cbp1_apt_ctzn_clean <- cleaner(cbp1_apt_ctzn)
swborder_bookouts_agency_clean <- nested_cleaner(swborder_bookouts_agency)
swborder_bookouts_family_clean <- nested_cleaner(swborder_bookouts_family)
swborder_bookouts_ctzn_clean <- nested_cleaner(swborder_bookouts_ctzn)
ero_arrests_ctzn_clean <- cleaner(ero_arrests_ctzn)
ero_arrests_crim_clean <- nested_cleaner(ero_arrests_crim) 
ice_bookins_ctzn_clean <- cleaner(ice_bookins_ctzn)
ice_bookins_crim_clean <- nested_cleaner(ice_bookins_crim)
ice_bookins_agency_clean <- cleaner(ice_bookins_agency)
ice_bookouts_crim_clean <- nested_cleaner(ice_bookouts_crim)
ice_bookouts_agency_clean <- nested_cleaner(ice_bookouts_agency)
ice_ero_RR_ctzn_clean <- cleaner(ice_ero_RR_ctzn)
ice_ero_RR_crim_clean <- nested_cleaner(ice_ero_RR_crim)
ice_ero_RR_agency_clean <- cleaner(ice_ero_RR_agency)
dhs_repats_type_clean <- nested_cleaner(dhs_repats_type)
dhs_removals_crim_clean <- cleaner(dhs_removals_crim)
dhs_removals_agency_clean <- cleaner(dhs_removals_agency)
dhs_removals_ctzn_clean <- cleaner(dhs_removals_ctzn)
dhs_enfreturns_ctzn_clean <- cleaner(dhs_enfreturns_ctzn)
dhs_adminreturns_ctzn_clean <- cleaner(dhs_adminreturns_ctzn)
swborder_credfear_ctzn_clean <- cleaner(swborder_credfear_ctzn)
swborder_credfear_result_clean <- cleaner(swborder_credfear_result)
tot_credfear_uscis_ctzn_clean <- cleaner(tot_credfear_uscis_ctzn)
tot_credfear_result_clean <- cleaner(tot_credfear_result)
chnv_beneficiaries_clean <- cleaner(chnv_beneficiaries)
chnv_auth_clean <- cleaner(chnv_auth)
chnv_paroles_clean <- cleaner(chnv_paroles)

names <- list("CBP_encounter_type_region" = enc_region_clean,
                   "SWB_encounter_ctzn" = enc_ctzn_clean,
                   "SWB_encounter_family_status" = enc_family_clean,
                   "SWB_encounter_sector_field_ofc" = enc_office_clean,
                   "Nationwide_USBP" = tot_enc_ctzn_100_clean,
                   "Nationwide_OFO" = tot_ofo_ctzn_100_clean,
                   "SWB_USBP" = swborder_enc_ctzn_100_clean,
                   "CBP_One_Apt" = cbp1_apt_ctzn_clean,
                   "CBP_SWB_bookouts_agency" = swborder_bookouts_agency_clean,
                   "CBP_SWB_bookouts_family" = swborder_bookouts_family_clean,
                   "CBP_SWB_bookouts_ctzn" = swborder_bookouts_ctzn_clean,
                   "ERO_arrests_ctzn" = ero_arrests_ctzn_clean,
                   "ERO_arrests_crim" = ero_arrests_crim_clean,
                   "ICE_bookins_ctzn" = ice_bookins_ctzn_clean,
                   "ICE_bookins_crim" = ice_bookins_crim_clean,
                   "ICE_bookins_agency" = ice_bookins_agency_clean,
                   "ICE_bookouts_crim" = ice_bookouts_crim_clean,
                   "ICE_bookouts_agency" = ice_bookouts_agency_clean,
                   "ICE_ERO_RR_ctzn" = ice_ero_RR_ctzn_clean,
                   "ICE_ERO_RR_crim" = ice_ero_RR_crim_clean,
                   "ICE_ERO_RR_agency" = ice_ero_RR_agency_clean,
                   "DHS_repats_type" = dhs_repats_type_clean,
                   "DHS_removals_crim" = dhs_removals_crim_clean,
                   "DHS_removals_agency" = dhs_removals_agency_clean,
                   "DHS_removals_ctzn" = dhs_removals_ctzn_clean,
                   "DHS_enf_returns_ctzn" = dhs_enfreturns_ctzn_clean,
                   "DHS_adminreturns_ctzn" = dhs_adminreturns_ctzn_clean,
                   "SWB_CredFear_ctzn" = swborder_credfear_ctzn_clean,
                   "SWB_CredFear_result" = swborder_credfear_result_clean,
                   "Nationwide_CredFear_ctzn" = tot_credfear_uscis_ctzn_clean,
                   "Nationwide_CredFear_result" = tot_credfear_result_clean,
                   "CHNV_beneficiaries" = chnv_beneficiaries_clean,
                   "CHNV_authorization" = chnv_auth_clean,
                   "CHNV_paroles" = chnv_paroles_clean)

##TOUCHPOINT FILENAME
write.xlsx(names, file="2024-04_ohss_tables.xlsx")

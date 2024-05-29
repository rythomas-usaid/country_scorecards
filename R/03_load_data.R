library(ggplot2)
library(ggstar)
library(tidyverse)
library(zoo)
library(readxl)
library(extrafont)
library(extrafontdb)
library(DBI)
library(RSQLite)
library(disR)
library(googlesheets4)


# Load data ------------
# EG.3 budget
# budget <- read_sheet("1xlHsKEhzznJI0ZBH5_9hHhdeXPpamSRD1wc0wtilRIk", sheet = "budget")
# write_csv(budget, "01_data/budget.csv")
budget <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c"
                     , sheet = "Final Budget", na = "-") %>%
  pivot_longer(-ou, names_to = "year") %>%
  rename(budget = value) %>%
  separate(year , into = c("type", "year"), sep = " - ", fill = "left") %>%
  mutate(year = as.numeric(year)
         , type = ifelse(ou == "International Food Assistance Division (IFA) (USDA/IFA)", "Enacted Appropriation"
                         , ifelse(is.na(type), "653(a) Control", type))
         , budget = as.numeric(budget)) %>%
  drop_na()

ftf_budget  <- read_excel("data/FY 2023 GFSS Implementation Report Budget Table .xlsx"
                          , sheet = "Table for Input", na = c("footnote", "N/A", "NA"), skip = 4) %>%
  pivot_longer(-c(ro, program), names_to = "year")

ati <- read_excel("data/IFPRI ATI Database (02-26-2024).xlsx", skip = 6) %>%
  pivot_longer(-c(country, iso, income_group), names_to = "year")

targets <- read_excel("data/target_setting_data.xlsx"
                      , sheet = "targets_long"
                      , col_types = c("text", "text", "numeric", "text", "numeric"))

kin <- read_excel("data/kin_section4_narratives.xlsx")

map_files <- read_csv("data/map_files.csv")
sales_ous <- unique(targets$ou[targets$name == "PT1: Sales"])
gf_ous <- unique(targets$ou[targets$name == "PT2: Gender financing ratio"])
ht_ous <- unique(targets$ou[targets$name == "PT3: Climate hectares"])
psi_ous <- unique(targets$ou[targets$name == "PT4: Private sector investment (3-yr avg)"])

ou_target_countries <-c("FTF Initiative",
                        "Afghanistan (OIG/AFG)",
                        "USAID Bangladesh (BANGLADESH)",
                        "Bureau for Resilience and Food Security (RFS)",
                        "Georgia Program (GEORGIA)",
                        "USAID Burma (BURMA)",
                        "USAID Cambodia (CAMBODIA)",
                        "USAID Colombia (COLOMBIA)",
                        "USAID Dem Rep Congo (DROC)",
                        "USAID Ethiopia (ETHIOPIA)",
                        "USAID Egypt (EGYPT)",
                        "USAID Ghana (GHANA)",
                        "Group Target",
                        "USAID Guatemala (GUATEMALA)",
                        "USAID Haiti (HAITI)",
                        "USAID Honduras (HONDURAS)",
                        "International Food Assistance Division (IFA) (USDA/IFA)",
                        "USAID Kenya (KENYA)",
                        "USAID Liberia (LIBERIA)",
                        "USAID Madagascar (MADAGASCAR)",
                        "USAID Malawi (MALAWI)",
                        "USAID Mali (MALI)",
                        # "USAID Mali (MALI) S",
                        "USAID Mozambique (MOZAMBIQUE)",
                        "USAID Nepal (NEPAL)",
                        "USAID Niger (NIGER)",
                        "USAID Nigeria (NIGERIA)",
                        "USAID Pakistan (PAKISTAN)",
                        "East Africa (EAST AFRICA)",
                        "USAID Rwanda (RWANDA)",
                        "Sahel Regional Program (SAHEL)",
                        "USAID Senegal (SENEGAL)",
                        "USAID South Sudan (SOUTH SUDAN)",
                        "Regional Center for South Africa (S_AFR_REG)",
                        "Sri Lanka",
                        "USAID Tajikistan (TAJIKISTAN)",
                        "USAID Tanzania (TANZANIA)",
                        "USAID Uganda (UGANDA)",
                        "West Africa Regional Program (WARP)",
                        "USAID Zambia (ZAMBIA)",
                        "USAID Zimbabwe (ZIMBABWE)")

con <- DBI::dbConnect(RSQLite::SQLite(), "../../data/2024-04-30/dis_extract.db")
# dbWriteTable(con, "pt_udns", pt_udns, overwrite = TRUE)
# con <- DBI::dbDisconnect(con)

# copy_to(con, data)
extract <- tbl(con, "extract") %>%
  filter(str_detect(a_name, stringr::fixed("_HLI_"), negate = TRUE) # Is not HLI
         & str_detect(ou, stringr::fixed("test"), negate = TRUE) # not a test bilateral OU DIS Training Activity - To Be Deleted
         & str_detect(a_name, stringr::fixed("test"), negate = TRUE) # not a test activity name
         & str_detect(a_name, stringr::fixed("DIS Training Activity - To Be Deleted"), negate = TRUE) # not the Training Activity from Ghana
         &( # Is an FTF indicator or activity
           indicator_origin == "FTF" | is_ftf == "Y")) %>%
  janitor::clean_names() %>% as_tibble()

pt_udns <- tbl(con, "pt_udns") %>% as_tibble()
# dbWriteTable(con, "pt_udns", as.data.frame(pt_udns))

## Define active activities -----------
# Active activities are defined on page 3 in this document:
# https://docs.google.com/document/d/1sE11RQUUf8Je3LyoWVq2c2Rvlkq-AFwEk34XGPSXBa4/edit
# Have any FY-2023 reporting on an FTF indicator (meaning, FY-2023 actuals,
# targets, or deviation narratives for an FTF indicator)
# —OR—
# Have any FY-24 Targets for any FTF indicator

## These criteria are excluded -->
## —OR—
## Have an activity ‘initiative association’ of FTF, have an “Ongoing”
## activity status, and have dates that seem applicable, i.e. end dates after October 1, 2022
## or if they don’t have an end date in the system, have start dates after October 1, 2017
## <--
active_activities_dat <- extract %>%
  filter(str_detect(a_name, stringr::fixed("_HLI_"), negate = TRUE) # Is not HLI
         & str_detect(ou, stringr::fixed("test"), negate = TRUE) # not a test bilateral OU DIS Training Activity - To Be Deleted
         & str_detect(a_name, stringr::fixed("test"), negate = TRUE) # not a test activity name
         & str_detect(a_name, stringr::fixed("DIS Training Activity - To Be Deleted"), negate = TRUE) # not the Training Activity from Ghana
         &( # Is an FTF indicator or activity
           indicator_origin == "FTF" | is_ftf == "Y"
         )
         &(# Reported actuals, targets, or deviation narratives in FY23
           (
             year == 2023  & if_any(c(actual, target, deviation_narrative), ~ !is.na(.))
           )|( # OR reported FY24 targets
             year == 2024 & !is.na(target)
           )
         )
  ) %>% as_tibble()
active_activities_unique <- active_activities_dat %>%  group_by(ro, ou, a_code, a_name) %>%
  summarize(unique_indicator_count = length((unique(ic)))
            , indicators = paste0(unique(ic), collapse = "; ")
            , .groups = "drop")
# active_usaid_activities_unique %>%
#   googlesheets4::sheet_write("1qFVbBDLM_8F5tFqMFK_eRsRWWOs0TJy3VXn9WI7vlDI", sheet = "Active FTF Activities")


input_dat <- inner_join(extract, pt_udns) %>%
  as_tibble()
input_dat <- input_dat %>%
  filter(a_code != 2339) %>%
  mutate(actual = ifelse(ic == "EG.3.2-26" & year == 2023 & udn == "3" & a_code == 1612
                         , 119106690
                  , ifelse(ic == "EG.3.2-26" & year == 2023 & udn == "3" & a_code == 4553
                          , 395800000, actual)))
# DBI::dbDisconnect(con)

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
gs4_auth(email = "rythomas@usaid.gov")


# Load data ------------
# EG.3 budget
# budget <- read_sheet("1xlHsKEhzznJI0ZBH5_9hHhdeXPpamSRD1wc0wtilRIk", sheet = "budget")
# write_csv(budget, "01_data/budget.csv")
budget <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "Final Budget", na = "-") %>%
  pivot_longer(-ou, names_to = "year") %>% rename(budget = value) %>%
  separate(year , into = c("type", "year"), sep = " - ", fill = "left") %>%
  mutate(year = as.numeric(year)
         , type = ifelse(ou == "International Food Assistance Division (IFA) (USDA/IFA)", "Enacted Appropriation"
                         , ifelse(is.na(type), "653(a) Control", type))
         , budget = as.numeric(budget)) %>% drop_na()
top5_disbursements <- googlesheets4::read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "top5_disbursements")
top5_results <- googlesheets4::read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "top5_results")
ftf_budget  <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "ftf_budget", na = c("footnote", "N/A", "NA")) %>%
  pivot_longer(-c(ro, program), names_to = "year")
ati <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "ati") %>%
  pivot_longer(-c(country, iso, income_group), names_to = "year")
targets <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "targets_long")
kin <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "kin")

map_files <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "map_files")

ftf_programs <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "ftf_programs")
target_country_ous <- ftf_programs$ou[!is.na(ftf_programs$scorecard_program)]
usaid_ous <- ftf_programs$ou[!is.na(ftf_programs$scorecard_program) & ftf_programs$ro == "USAID"]

ou_to_program <- function(x = targets, name, indicator=NA) {
  ous <- unique(targets$ou[targets$name == name & !is.na(targets$value)])
  country_programs <- ftf_programs$country_program[ftf_programs$ou %in% ous]
  country_program_ous <- ftf_programs$ou[ftf_programs$country_program %in% country_programs]
  otp <- list("ous" = ous, "programs" = country_programs, "program_ous" = country_program_ous)
  return(otp)
}
sales_ous_to_program <- ou_to_program(name = "PT1: Sales")
gf_ous_to_program <- ou_to_program(name = "PT2: Gender financing ratio")
ht_ous_to_program <- ou_to_program(name = "PT3: Climate hectares")
psi_ous_to_program <- ou_to_program(name = "PT4: Private sector investment (3-yr avg)")

con <- DBI::dbConnect(RSQLite::SQLite(), "~/R_scripts/data/2024-05-30/dis_extract.db")
extract <- tbl(con, "extract") %>%
  filter(str_detect(a_name, stringr::fixed("_HLI_"), negate = TRUE) # Is not HLI
         & str_detect(ou, stringr::fixed("test"), negate = TRUE) # not a test bilateral OU DIS Training Activity - To Be Deleted
         & str_detect(a_name, stringr::fixed("test"), negate = TRUE) # not a test activity name
         & str_detect(a_name, stringr::fixed("DIS Training Activity - To Be Deleted"), negate = TRUE) # not the Training Activity from Ghana
         & ( # Is an FTF indicator or activity
           indicator_origin == "FTF" | is_ftf == "Y")) %>%
  filter(a_code != "00002339") %>%
  # these figures were corrected previously; for 1612, in a post-script from the
  # DIS export, and for 4553, the corrections are in DIS
  # mutate(actual = ifelse(ic == "EG.3.2-26" & year == 2023 & udn == "3" & a_code == "00001612"
  #                        , 119106690
  #                        , ifelse(ic == "EG.3.2-26" & year == 2023 & udn == "3" & a_code == "00004553"
  #                                 , 395800000, actual))) %>%
  janitor::clean_names() %>% as_tibble() %>%
  left_join(ftf_programs)

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
  filter(# Reported actuals, targets, or deviation narratives in FY23
           (
             year == 2023  & if_any(c(actual, target, deviation_narrative), ~ !is.na(.))
           )|( # OR reported FY24 targets
             year == 2024 & !is.na(target)
           )
         ) %>% as_tibble()
active_activities_unique <- active_activities_dat %>% inner_join(pt_udns) %>%  group_by(ro, ou, a_code, a_name) %>%
  summarize(unique_indicator_count = length((unique(ic)))
            , indicators = paste0(unique(ic), collapse = "; ")
            , .groups = "drop")
# active_usaid_activities_unique %>%
#   googlesheets4::sheet_write("1qFVbBDLM_8F5tFqMFK_eRsRWWOs0TJy3VXn9WI7vlDI", sheet = "Active FTF Activities")


input_dat <- inner_join(extract, pt_udns)
# DBI::dbDisconnect(con)


## Define params -----------
### Sales ------------------
sales <- input_dat %>%
  filter(year %in% 2022:2023) %>%
  sales_() %>% filter(name == "actual") %>%
  left_join(ftf_programs) %>%
  group_by(scorecard_program, year, type = name) %>%
  summarise(sales_ous = paste0(unique(ou), collapse = "; ")
            , `PT1: Sales` = sum_(value)) %>%
# # Add group of contributing countries
# sales_group <- input_dat %>%
#   filter(year %in% 2022:2023 & ! ou %in% sales_ous_to_program$program_ous) %>%
#   sales_() %>% filter(name == "actual") %>%
#   select(ro, ou, year, type = name, `PT1: Sales` = value) %>%
#   group_by(type, year) %>%
#   summarise(ro = "USAID", ous = paste0(unique(ou), collapse = "; ")
#             , ou = "Group Target", `PT1: Sales` = sum_(`PT1: Sales`))
# add initiative level

  bind_rows(input_dat %>%
  filter(year %in% 2022:2023) %>%
  sales_() %>% filter(name == "actual") %>%
  select(ro, ou, year, type = name, `PT1: Sales` = value) %>% group_by(year, type) %>%
  summarize(scorecard_program = "FTF Initiative"
            , sales_ous =  paste0(unique(ou), collapse = "; ")
            , `PT1: Sales` = sum_(`PT1: Sales`))
  )
# sales <- bind_rows(sales_initiative, sales, sales_group) %>% select(-ous)

### GF ratio  ------------------
gf <- input_dat %>%
  filter(year %in% 2022:2023) %>%
  gender_financing_(level = "im") %>% filter(name == "actual") %>%
  left_join(ftf_programs) %>%
  group_by(scorecard_program, type = name, year) %>%
  disR:::summarize_financing_ratio(level = "ou") %>%
  rename(`PT2: Gender financing ratio` = value, gf_ous = ous) %>%
  mutate(`PT2: Gender financing ratio` = case_when(
    is.na(`PT2: Gender financing ratio`) & scorecard_program != "Group Target" ~ 0
    , .default = `PT2: Gender financing ratio`)) %>%

# gf_group <- input_dat %>%
#   filter(year %in% 2022:2023 & ! ou %in% gf_ous_to_program$program_ous) %>%
#   gender_financing_(level = "initiative") %>%
#   filter(name == "actual") %>%
#   select(type = name, year, `PT2: Gender financing ratio` = value)  %>%
#   mutate(ro = "USAID", ou = "Group Target", .before = everything()) %>%
#   mutate(`PT2: Gender financing ratio` = case_when(
#     is.na(`PT2: Gender financing ratio`) & ou %in% financing_ous$ou ~ 0
#     , .default = `PT2: Gender financing ratio`))
# Add initiative level
  bind_rows(
    input_dat %>%
      filter(year %in% 2022:2023) %>%
      gender_financing_(level = "im") %>% filter(name == "actual") %>%
      left_join(ftf_programs) %>%
      group_by(year, type = name) %>% disR:::summarize_financing_ratio(level = "ou") %>%
      rename(`PT2: Gender financing ratio` = value, gf_ous = ous) %>%
      mutate(scorecard_program = "FTF Initiative")
  )
    # gf <- bind_rows(gf_initiative, gf, gf_group)

### HT ------------------
ht <- input_dat %>%
  filter( year %in% 2022:2023) %>%
  hectares_() %>%  filter(name == "actual") %>%
  left_join(ftf_programs) %>%
  group_by(scorecard_program, year, type = name) %>%
  summarise(ht_ous = paste0(unique(ou), collapse = "; ")
            , `PT3: Climate hectares`= sum_(value)) %>%
  mutate(`PT3: Climate hectares` = case_when(
    is.na(`PT3: Climate hectares`) & scorecard_program != "Group Target" ~ 0
    , .default = `PT3: Climate hectares`)) %>%
# ht_group <- input_dat %>%
#   filter( year %in% 2022:2023) %>%
#   hectares_() %>% filter(name == "actual" & ! ou %in% ht_ous_to_program$program_ous) %>%
#   select(ro, ou, type = name, year, `PT3: Climate hectares` = value) %>%
#   mutate(`PT3: Climate hectares` = case_when(
#     is.na(`PT3: Climate hectares`) & ou %in% hectares_ous$ou ~ 0
#     , .default = `PT3: Climate hectares`)) %>%
#   group_by(type, year) %>%
#   summarise(ro = "USAID", ous = paste0(unique(ou), collapse = "; ")
#             , ou = "Group Target"
#             , `PT3: Climate hectares` = sum_(`PT3: Climate hectares`))

  bind_rows(
    input_dat %>%
      filter( year %in% 2022:2023) %>%
      hectares_() %>%  filter(name == "actual") %>%
      left_join(ftf_programs) %>%
      group_by(year, type = name) %>%
      summarise(ht_ous = paste0(unique(ou), collapse = "; ")
                , `PT3: Climate hectares`= sum_(value)) %>%
      mutate(scorecard_program = "FTF Initiative")
  )
# ht <- bind_rows(ht_initiative, ht, ht_group) %>% select(-ous)

### PSI ------------------
psi_initiative <- input_dat %>% psi_(level = "initiative") %>%
  filter(name %in% c("target", "actual") & year %in% 2022:2023 & !is.na(value)) %>%
  rename(type = name, `PT4: Private sector investment` = value) %>%
  select(-c(ic, a_codes)) %>% mutate(ro = "USAID", ou = "FTF Initiative", .before=everything())
psi <- input_dat %>% psi_(level = "ou") %>%
  filter(str_detect(name, "_3y") & year %in% 2022:2023
         & !is.na(value) & ou %in% psi_ous_to_program$program_ous) %>%
  rename(type = name, `PT4: Private sector investment (3-yr avg)` = value) %>%
  # pivot_wider() %>%
  mutate(type = str_remove(type, "_3y")) %>%
  select(-c(ic, a_codes))
psi_group <- input_dat %>% psi_(level = "ou") %>%
  filter(str_detect(name, "_3y") & year %in% 2022:2023
         & !is.na(value) & !ou %in% psi_ous_to_program$program_ous) %>%
  rename(type = name, `PT4: Private sector investment (3-yr avg)` = value) %>%
  # pivot_wider() %>%
  mutate(type = str_remove(type, "_3y")) %>%
  select(-c(ic, a_codes)) %>% group_by(type, year) %>%
  summarise(ro = "USAID", ous = paste0(unique(ou), collapse = "; ")
            , ou = "Group Target"
            , `PT4: Private sector investment (3-yr avg)` = sum_(`PT4: Private sector investment (3-yr avg)`))
psi_baseline <- bind_rows(psi_group, psi, filter(psi_initiative, type == "actual" & year == 2022)) %>%
  select(ro, ou, type, year, starts_with("PT4")) %>%
  filter(year==2022 & type == "actual") %>% mutate(type = "baseline")
psi <- bind_rows(filter(psi_initiative, !(type == "actual" & year == 2022)), psi_group, psi) %>% select(-ous)

### MDDW ------------------
mali_pbs <- c("USAID Mali (MALI) SOUTH","USAID Mali (MALI) RFZ")
# copied from https://docs.google.com/spreadsheets/d/1ua_Qp995kNlNnlIBDfYH1fmaNUBOgygWodAZg7fXz5U/edit#gid=1662466342
mddw <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "mddw_scorecard") %>%
  pivot_longer(value) %>%
  mutate(name = case_when(ou == "USAID Mali (MALI) SOUTH" ~ "PT5: MDD-W (South)"
                          , ou == "USAID Mali (MALI) RFZ" ~ "PT5: MDD-W (RFZ)"
                          , .default = "PT5: MDD-W")
         ,  year = case_when(type == "performance target" ~ 2030, .default = year(date))
         , ro = "USAID"
         , ou = case_when(ou %in% mali_pbs ~ "USAID Mali (MALI)", .default = ou)) %>%
  pivot_wider()

# Only 6 have actual values for in this data set. Use Anne's sheet above.
# mddw <- input_dat %>%
#   filter(ro == "USAID") %>%
#   mddw_() %>% filter(name == "actual") %>%
#   select(ro, ou, year, mddw = value) %>% mutate(mddw = mddw/100)

psi_names <- c("PT4: Private sector investment (3-yr avg)", "PT4: Private sector investment")
required_pts <- c("PT1: Sales", "PT3: Climate hectares"
                  , "PT4: Private sector investment (3-yr avg)"
                  , "PT4: Private sector investment")
deviation_pts <- c("PT1: Sales", "PT3: Climate hectares")
odd_balls <- c("PT2: Gender financing ratio")

# Make PT Data -------------
pt_dat <- sales %>% full_join(gf) %>% full_join(ht) %>% full_join(psi) %>%
  full_join(select(mddw,  -c(date)) %>%  filter(type != "outyear")) %>%
  bind_rows(targets  %>%
              filter(! name == "PT5: MDD-W") %>%
              pivot_wider(id_cols=c(ou, year, type)) %>%
              mutate(ro = case_when(ou == "International Food Assistance Division (IFA) (USDA/IFA)"~ "USDA"
                                    , .default="USAID"))
  ) %>%
  bind_rows(psi_baseline) %>%
  pivot_longer(-c(ro, ou, year, type)) %>% drop_na(value) %>%
  arrange(ro, ou, name, type) %>% ungroup()

googlesheets4::write_sheet(pt_dat, "1ynvPDs5RqmfUbSFlcTlCry7z4dRnFnMBR37S_GtvNLw"
                           , sheet = "pt_dat")
# Add back OUs with no PSI baseline so they show up in the Summary Chart on page 1.
psiNA <- setdiff(target_country_ous ,psi_baseline$ou)
addtopsi <- tibble(ro =  "USAID", ou = psiNA,  year = 2022, type = "baseline", name = "PT4: Private sector investment (3-yr avg)", value = NA)

pt_upload <- pt_dat %>%
  filter(!(year == 2022 & type != "baseline" & name != "PT5: MDD-W") & type != "target") %>%
  bind_rows(addtopsi) %>%
  select(-year) %>% pivot_wider(names_from = type, values_from = value) %>%
  filter(name != "PT4: Private sector investment" | (name == "PT4: Private sector investment" & ou == "FTF Initiative")) %>%
  dplyr::rowwise() %>%
  mutate(interim_target = ifelse(
    !is.na(baseline) & !is.na(`performance target`) &  name %in% deviation_pts
    , approx(x = c(2022,2030), c(baseline, `performance target`), n=9)$y[2]

    , ifelse(!is.na(baseline) & !is.na(`performance target`) &  name == "PT2: Gender financing ratio" & baseline < 1
             , approx(x = c(2022,2030), c(baseline, `performance target`), n=9)$y[2]
             , ifelse(!is.na(baseline) & !is.na(`performance target`) &  name == "PT2: Gender financing ratio" & baseline >= 1, 1

                      , ifelse(name %in% psi_names & ou != "FTF Initiative", baseline * 1.075
                               , ifelse(name %in% psi_names & ou == "FTF Initiative", baseline * 1.0288

                                        , ifelse(!is.na(baseline) & !is.na(`performance target`) &  str_starts(name, "PT5")
                                                 , approx(x = c(2022,2030), c(baseline, `performance target`), n=9)$y[2]
                                                 , NA))))))) %>%

  mutate(across(c(baseline, actual, interim_target), ~ . / interim_target, .names = "{.col}_norm")
         , across(-c(ro, ou, name), ~ ifelse(is.finite(.), ., NA))
         , absolute_difference = actual - interim_target

         , ou_track = ifelse(
           ou == "USAID Nigeria (NIGERIA)" & name == "PT1: Sales", "Not Available"
           # If actual is NA
           , ifelse(is.na(`performance target`) & !str_starts(name, "PT4"), "Not applicable"
                    , ifelse(is.na(`performance target`) & str_starts(name, "PT4"), "Not available"
                             # For PT1-PT4, any OU with a performance target with a 0 or NA for FY23 is not on track
                             , ifelse(is.na(actual) & name %in% c(required_pts, odd_balls), "Not on track"
                                      , ifelse(is.na(actual) & str_starts(name, "PT5"), "Not available"

                                               # For PT PT1-3 and 5, multiply the interim target by .9
                                               , ifelse((name %in% c(deviation_pts, odd_balls) | str_starts(name, "PT5")) # PT1-3, PT5
                                                        & (!is.na(baseline) & actual < interim_target*.9 & actual < `performance target`*.9), "Not on track"
                                                        , ifelse((!is.na(baseline) & name %in% c(deviation_pts, odd_balls) | str_starts(name, "PT5"))
                                                                 & (actual >= interim_target*.9 | actual >= `performance target`*.9), "On track"

                                                                 , ifelse(str_starts(name, "PT4") & !is.na(baseline)
                                                                          & (actual < interim_target*.9 & actual < `performance target`), "Not on track"
                                                                          , ifelse(str_starts(name, "PT4") & !is.na(baseline)
                                                                                   & (actual >= interim_target*.9 | actual >= `performance target`) , "On track"
                                                                                   ,  "Not available")))))))))
  ) %>%  filter(if_any(c(baseline, `performance target`)
                       , ~ !is.na(.)) | str_detect(name, "PT5") | str_detect(name, "PT4")) %>%
  rowwise() %>% mutate(across(ends_with("norm"), ~ ifelse( !is.finite(.), NA, . ))) %>%
  mutate(group_target = ! ou %in% target_country_ous)

finance_total <- extract %>% gender_financing_(level = "ou") %>%
  filter(name == "actual", year ==2023) %>%
  group_by(ro, ou, year) %>%
  summarise(financing_total = sum_(`Female Value`, `Male Value`), name = "PT2: Gender financing ratio")


# pt_upload %>% left_join(finance_total)

googlesheets4::write_sheet(pt_upload %>% left_join(finance_total), "1ynvPDs5RqmfUbSFlcTlCry7z4dRnFnMBR37S_GtvNLw"
                           , sheet = "Performance target data")

# bring in national dat
# create a cleaning function:
wb_clean <- function(df,var){
  df |>
    select(ou = country, iso, 5:67) |>
    pivot_longer(cols = c(3:65), values_to = var, names_to = "year") |>
    mutate(year = as.numeric(year)) |>
    filter(year>=2011)

}

# merge into a dat frame and pull down/up existing values to replace missings
national_df <- wb_clean(read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "hunger"), "hunger") |>
  left_join(wb_clean(read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "poverty"), "poverty")) |>
  left_join(wb_clean(read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "stunting"), "stunting")) |>
  left_join(wb_clean(read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "wasting"), "wasting")) %>%
  mutate(ou = ifelse(ou == "Myanmar", "Burma",
                     ifelse(ou == "Congo, Dem. Rep.", "Democratic Republic of the Congo",
                            ifelse(ou == "Egypt, Arab Rep.", "Egypt", ou))))
# group_by(ou) #|>
#fill(c(3:6), .direction = "downup")

mddw_dat <- mddw %>%
  pivot_longer(c(`PT5: MDD-W`, `PT5: MDD-W (South)`, `PT5: MDD-W (RFZ)`)) %>%
  # drop_na() %>%
  mutate(value = ifelse(value == -9999, NA, value)
         , year = ifelse(type == "performance target", 2030, year))

# ou_name <- "USAID Dem Rep Congo (DROC)"
# ou_names <- unique(active_activities_unique$ou)

map_files <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c", sheet = "map_files")


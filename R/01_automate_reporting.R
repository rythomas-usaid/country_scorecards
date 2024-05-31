
# Setup -------------

library(usaidplot)
# library(cowplot)
library(ggplot2)
library(ggrepel)
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

gs4_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  subject = NULL,
  scopes = "spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

source("R/05_helpers.R")
source("R/02_make_scorecard_plots.R")
source("R/03_load_data.R") # includes relevant phoenix module data
# source("_phoenix_module.R")

## Define params -----------
### Sales ------------------
ou_target_countries <- unique(ftf_programs$ou[ftf_programs$target_program==TRUE])
sales <- input_dat %>%
  filter(year %in% 2022:2023 & ou %in% sales_ous) %>%
  sales_() %>% filter(name == "actual") %>%
  select(ro, ou, year, type = name, `PT1: Sales` = value)
# Add group of contributing countries
sales_group <- input_dat %>%
  filter(year %in% 2022:2023 & ! ou %in% sales_ous) %>%
  sales_() %>% filter(name == "actual") %>%
  select(ro, ou, year, type = name, `PT1: Sales` = value) %>%
  group_by(type, year) %>%
  summarise(ro = "USAID", ous = paste0(unique(ou), collapse = "; ")
            , ou = "Group Target", `PT1: Sales` = sum_(`PT1: Sales`))
# add initiative level
sales_initiative <- input_dat %>%
  filter(year %in% 2022:2023) %>%
  sales_() %>% filter(name == "actual") %>%
  select(ro, ou, year, type = name, `PT1: Sales` = value) %>% group_by(year, type) %>%
  summarize(ro = "USAID", ou="FTF Initiative", `PT1: Sales` = sum_(`PT1: Sales`))
sales <- bind_rows(sales_initiative, sales, sales_group) %>%
  filter(ou %in% ou_target_countries) %>% select(-ous)

### GF ratio  ------------------
financing_ous <- extract %>%
  filter(ic == "EG.3.2-27" & udn == "3" & year %in% 2021:2022 & !is.na(actual)) %>%
  distinct(ro, ou)
gf <- input_dat %>%
  filter(year %in% 2022:2023) %>%
  gender_financing_(level = "ou") %>% filter(name == "actual" & ou %in% gf_ous) %>%
  select(ro, ou, type = name, year, `PT2: Gender financing ratio` = value) %>%
  mutate(`PT2: Gender financing ratio` = case_when(
          is.na(`PT2: Gender financing ratio`) & ou %in% financing_ous$ou ~ 0
          , .default = `PT2: Gender financing ratio`))
gf_group_ous <- input_dat %>%
  filter(year %in% 2022:2023 & ! ou %in% gf_ous) %>%
  gender_financing_(level = "ou") %>% distinct(ou)
gf_group <- input_dat %>%
  filter(year %in% 2022:2023 & ! ou %in% gf_ous) %>%
  gender_financing_(level = "initiative") %>%
  filter(name == "actual") %>%
  select(type = name, year, `PT2: Gender financing ratio` = value)  %>%
  mutate(ro = "USAID", ou = "Group Target", .before = everything()) %>%
  mutate(`PT2: Gender financing ratio` = case_when(
    is.na(`PT2: Gender financing ratio`) & ou %in% financing_ous$ou ~ 0
    , .default = `PT2: Gender financing ratio`))
# Add initiative level
gf_initiative <- input_dat %>%
  filter(year %in% 2022:2023) %>%
  gender_financing_(level = "initiative") %>%
  select(type = name, year, `PT2: Gender financing ratio` = value) %>%
  mutate(ro = "USAID", ou = "FTF Initiative", .before = everything())
gf <- bind_rows(gf_initiative, gf, gf_group) %>%
  filter(ou %in% ou_target_countries)

### HT ------------------
hectares_ous <- extract %>%
  filter(ic == "EG.3.2-25" & udn == "3" & year %in% 2021:2022 & !is.na(actual)) %>%
  distinct(ro, ou)
ht <- input_dat %>%
  filter( year %in% 2022:2023) %>%
  hectares_() %>% filter(name == "actual" & ou %in% ht_ous) %>%
  select(ro, ou, type = name, year, `PT3: Climate hectares` = value) %>%
  mutate(`PT3: Climate hectares` = case_when(
           is.na(`PT3: Climate hectares`) & ou %in% hectares_ous$ou ~ 0
           , .default = `PT3: Climate hectares`))
ht_group <- input_dat %>%
  filter( year %in% 2022:2023) %>%
  hectares_() %>% filter(name == "actual" & ! ou %in% ht_ous) %>%
  select(ro, ou, type = name, year, `PT3: Climate hectares` = value) %>%
  mutate(`PT3: Climate hectares` = case_when(
    is.na(`PT3: Climate hectares`) & ou %in% hectares_ous$ou ~ 0
    , .default = `PT3: Climate hectares`)) %>%
  group_by(type, year) %>%
  summarise(ro = "USAID", ous = paste0(unique(ou), collapse = "; ")
            , ou = "Group Target"
            , `PT3: Climate hectares` = sum_(`PT3: Climate hectares`))
ht_initiative <- input_dat %>%
  filter( year %in% 2022:2023) %>%
  hectares_() %>% filter(name == "actual") %>%
  select(ro, ou, type = name, year, `PT3: Climate hectares` = value) %>%
  mutate(`PT3: Climate hectares` = case_when(
    is.na(`PT3: Climate hectares`) & ou %in% hectares_ous$ou ~ 0
    , .default = `PT3: Climate hectares`)) %>%
  group_by(type, year) %>%
  summarise(ro = "USAID", ou = "FTF Initiative"
            , `PT3: Climate hectares` = sum_(`PT3: Climate hectares`)
            , .groups = "drop")
ht <- bind_rows(ht_initiative, ht, ht_group) %>%
  filter(ou %in% ou_target_countries) %>% select(-ous)

### PSI ------------------
psi_initiative <- input_dat %>% psi_(level = "initiative") %>%
  filter(name %in% c("target", "actual") & year %in% 2022:2023 & !is.na(value)) %>%
  rename(type = name, `PT4: Private sector investment` = value) %>%
  select(-c(ic, a_codes)) %>% mutate(ro = "USAID", ou = "FTF Initiative", .before=everything())
psi <- input_dat %>% psi_(level = "ou") %>%
  filter(str_detect(name, "_3y") & year %in% 2022:2023
         & !is.na(value) & ou %in% psi_ous) %>%
  rename(type = name, `PT4: Private sector investment (3-yr avg)` = value) %>%
  # pivot_wider() %>%
  mutate(type = str_remove(type, "_3y")) %>%
  select(-c(ic, a_codes))
psi_group <- input_dat %>% psi_(level = "ou") %>%
  filter(str_detect(name, "_3y") & year %in% 2022:2023
         & !is.na(value) & !ou %in% psi_ous) %>%
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
psi <- bind_rows(filter(psi_initiative, !(type == "actual" & year == 2022)), psi_group, psi) %>%
  filter(ou %in% ou_target_countries) %>% select(-ous)

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
psiNA <- setdiff(ou_target_countries ,psi_baseline$ou)
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
  mutate(group_target = ! ou %in% ou_target_countries)

 finance_total <- extract %>% gender_financing_(level = "ou") %>%
   filter(name == "actual", year ==2023) %>%
   group_by(ro, ou, year) %>%
   summarise(financing_total = sum_(`Female Value`, `Male Value`), name = "PT2: Gender financing ratio")


pt_upload %>% left_join(finance_total)

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
national_df <- wb_clean(read_csv("data/hunger.csv"), "hunger") |>
  left_join(wb_clean(read_csv("data/poverty.csv"), "poverty")) |>
  left_join(wb_clean(read_csv("data/stunting.csv"), "stunting")) |>
  left_join(wb_clean(read_csv("data/wasting.csv"), "wasting")) |>
  left_join(wb_clean(read_csv("data/value_add.csv"), "ag_value_add")) |>
  left_join(wb_clean(read_csv("data/fdi.csv"), "fdi")) |>
  mutate(ou = ifelse(ou == "Myanmar", "Burma",
                     ifelse(ou == "Congo, Dem. Rep.", "Dem Rep Congo",
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

map_files <- read_csv("data/map_files.csv")

# Auto scorecard -----------------
for(program in unique(c(ftf_programs$program[ftf_programs==TRUE], "Group Target"))) {

  print(paste0("Working on ... ", program))
  scores <- data.frame(
    pt = c("PT1", "PT2", "PT3", "PT4", "PT5")
    , name = c("sales", "financing", "hectares", "psi", "mddw")
    , y = rep(1,5)
    , score = factor(rep("Not applicable", 5)
                     , levels = c("On track", "Not on track", "Not applicable", "Not available"))
  )
  mapfile <- paste0("data/maps/", map_files$file[map_files$country == program])


  tryCatch({
    output_file <- paste0(gsub("/", "", program), " FTF Performance Scorecard.docx")
    ou_names <- ftf_programs$ou[ftf_programs$program==program]

    # overall
    print(paste0("saving to ", output_file))
    rmarkdown::render(
      input = "templates/Scorecard.Rmd",
      output_format = "word_document",
      output_file = output_file,
      output_dir = "output/",
      params = list(
        ou_names = ou_names,
        program = program,
        input_dat = input_dat,
        extract = extract,
        pt_dat = pt_dat,
        pt_upload = pt_upload,
        active_activities_dat = active_activities_dat,
        active_activities_unique = active_activities_unique,
        kin = kin,
        mddw_dat = mddw_dat,
        scores = scores,
        national_df = national_df,
        ati = ati,
        top5_disbursements = top5_disbursements,
        top5_results = top5_results,
        budget = budget,
        ftf_budget = ftf_budget,
        mapfile = mapfile
      )
    )
  })
  }




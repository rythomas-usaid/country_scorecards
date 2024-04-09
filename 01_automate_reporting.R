
# Setup -------------
library(usaidplot)
library(cowplot)
library(ggplot2)
library(ggstar)
library(tidyverse)
library(readxl)
library(extrafont)
library(extrafontdb)
library(DBI)
library(RSQLite)
library(disR)
source("10_helpers.R")
source("02_make_scorecard_plots.R")

# Load data ------------
# EG.3 budget
# budget <- read_sheet("1xlHsKEhzznJI0ZBH5_9hHhdeXPpamSRD1wc0wtilRIk", sheet = "budget")
# write_csv(budget, "01_data/budget.csv")
budget <- read_excel("data/target_setting_data.xlsx"
                   , sheet = "Budget"
                   , col_types = c("text", "numeric", "numeric"))

targets <- read_excel("data/target_setting_data.xlsx"
                      , sheet = "FINAL OU TARGETS"
                      , col_types = c("text", "numeric", "numeric", "numeric", "text", "numeric", "date", "date"))  %>%
  rename("EG.3.2-26" = "PT1 Value of annual sales of producers and firms receiving USG assistance"
         , "EG.3.2-27" = "PT2 Value of financing accessed by female FTF participants per $1 of financing accessed by male FTF participants"
         , "EG.3.2-25" =  "PT3 The number of cultivated hectares under climate adaptation/climate risk management practices and technologies with USG assistance"
         , "EG.3.1-15" = "PT4 Value of private sector investment leveraged by the USG to support food security and nutrition"
         , "HL.9.1-d" = "PT5 Percent of women consuming a diet of minimum diversity (MDD-W)"
  )  %>%  janitor::clean_names()

kin <- read_excel("data/kin_section4_narratives.xlsx")

con <- DBI::dbConnect(RSQLite::SQLite(), "../../data/20240804/dis_extract.db")
dbWriteTable(con, "pt_udns", pt_udns, overwrite = TRUE)
# con <- DBI::dbDisconnect(con)

# copy_to(con, data)
extract <- tbl(con, "extract") %>% janitor::clean_names()
pt_udns <- tbl(con, "pt_udns")

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
active_activities <- extract %>%
  filter(str_detect(a_name, stringr::fixed("_HLI_"), negate = TRUE) # Is not HLI
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
         ) %>%
  as_tibble()

# mddw_targets

input_data <- inner_join(extract, pt_udns) %>% as_tibble()
# DBI::dbDisconnect(con)
## Define params -----------

ou_names <- c("Bangladesh", "Malawi", "Ethiopia")
 scores <- data.frame(pt = c("PT1", "PT2", "PT3", "PT4", "PT5")
                       , name = c("sales", "financing", "hectares", "psi", "mddw")
                       , y = rep(1,5)
                       , score = factor(rep("Not applicable", 5)
                                        , levels = c("On track", "Falling behind", "Not applicable"))
                       )

for ( ou_name in ou_names) {
  ou_name = "Malawi"
  output_file <- paste0(ou_name, ".docx")
  n_activities <- active_activities %>%
    filter(ro == "USAID" & str_detect(ou, ou_name)) %>%
    distinct(a_code) %>% arrange(a_code) %>% nrow()

  ou_kin <- kin %>% filter(str_detect(ou, ou_name))

  rmarkdown::render(
    input = "Scorecard.Rmd",
    output_format = "word_document",
    output_file = output_file,
    output_dir = "output",
    params = list(
      ou_name = ou_name,
      data = input_data,
      targets = targets,
      ou_kin = ou_kin,
      n_activities = n_activities,
      scores = scores
      )
    )
  }






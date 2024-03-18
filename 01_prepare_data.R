

library(usaidplot)
library(tidyverse)
library(readxl)
library(extrafont)
library(extrafontdb)

reload_disR()
source("10_helpers.R")


# EG.3 budget
# budget <- read_sheet("1xlHsKEhzznJI0ZBH5_9hHhdeXPpamSRD1wc0wtilRIk", sheet = "budget")
# write_csv(budget, "01_data/budget.csv")
budget <- read_csv("data/budget.csv")

targets <- read_excel("data/OU proportion of aggregate for FTF Target Setting Exercise_20230731.xlsx"
                      , sheet = "FINAL RO TARGETS"
                      , col_types = c("text", "numeric", "numeric", "numeric", "text", "numeric")
                      , skip = 1)  %>%
  rename("EG.3.2-26" = "PT1 Value of annual sales of producers and firms receiving USG assistance"
         , "EG.3.2-27" = "PT2 Value of financing accessed by female FTF participants per $1 of financing accessed by male FTF participants"
         , "EG.3.2-25" =  "PT3 The number of cultivated hectares under climate adaptation/climate risk management practices and technologies with USG assistance"
         , "EG.3.1-15" = "PT4 Value of private sector investment leveraged by the USG to support food security and nutrition"
         , "HL.9.1-d" = "PT5 Percent of women consuming a diet of minimum diversity (MDD-W)"
  )  %>%  janitor::clean_names()


con <- DBI::dbConnect(RSQLite::SQLite(), "dis_extract.db")
# copy_to(con, data)
extract <- tbl(con, "extract")
pt_udns <- tbl(con, "pt_udns")

data <- inner_join(extract, pt_udns) %>% as_tibble()


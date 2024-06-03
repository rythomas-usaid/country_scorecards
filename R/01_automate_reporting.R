
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

# Auto scorecard -----------------
for(ou_name in ou_target_countries) {
  if(ou_name == "International Food Assistance Division (IFA) (USDA/IFA)") ro <- "IFAD" else ro <- "USAID"
  print(paste0("Working on ... ", ou_name))

  mapfile <- paste0("data/maps/",map_files$file[map_files$ou == ou_name])

  tryCatch({
    output_file <- paste0(gsub("/", "", ou_name), ".docx")
    # overall
    print(paste0("saving to ", output_file))
    rmarkdown::render(
      input = if(ou_name %in% c("FTF Initiative", "Group Target")) "templates/Scorecard-basic.rmd" else "templates/Scorecard.Rmd",
      output_format = "word_document",
      output_file = output_file,
      output_dir = "output/",
      params = list(
        ou_name = ou_name,
        input_dat = input_dat,
        extract = extract,
        pt_dat = pt_dat,
        pt_upload = pt_upload,
        active_activities_dat = active_activities_dat,
        active_activities_unique = active_activities_unique,
        kin = kin,
        mddw_dat = mddw_dat,
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




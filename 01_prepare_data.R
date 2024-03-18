

library(usaidplot)
library(tidyverse)
library(readxl)
library(extrafont)
library(extrafontdb)

reload_disR()
source("02_ugly_helpers.R")


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



# FINANCING -------------
gender_finance <- gender_financing_(data, level="ou") %>%
  filter(ro == "USAID" & str_detect(ou, "Bangladesh"))

abline <- gender_finance %>%
  filter(name == "actual") %>%
  drop_na() %>%
  # slice(which.max(year)) %>%
  filter(year == 2022) %>%
  select(year, value) %>%
  bind_rows(tibble(year = 2030L, value = 1)) %>%
  mutate(name = "Centrally set Target")

on_track <- approxfun(x = abline$year, abline$value)
this_year <- gender_finance %>%
  filter(name == "actual") %>%
  drop_na() %>% slice(which.max(year)) %>%
  # filter(year == 2023) %>%
  select(year, value)
this_year_value <- this_year$value
lower <- on_track(this_year$year) - .07
upper <- on_track(this_year$year) + .07

title <- make_plot_title(this_year_value = this_year_value, lower = lower, upper = upper)
subtitle <- make_plot_subtitle(gender_finance)

gender_finance_plot <- gender_finance  %>%
  filter(str_detect(ou, "Bangladesh")) %>%
  bind_rows(abline) %>%
  mutate(name = case_when(name == "actual" ~ "Actual", name == "target" ~ "OU Target"
                          , .default = name)
         , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
  rename(`Value Type` = name) %>%
  ggplot(aes(x = `Fiscal Year`, y = value , color = `Value Type`)) +
  geom_point(size = 2.3) + geom_line(size = 1) +
  ylim(c(0, NA)) + ylab("PT1: Female financing ratio (per person)") +
  # theme_settings +
  usaid_colors() +
  common_theme() +
  guides(color = guide_legend(position = "inside")) + usaid_colors() + common_theme() +
  ggtitle(label = title, subtitle = subtitle)
# gender_finance_plot


# SALES ------------
sales <- sales_(data) %>% filter(ro == "USAID" & str_detect(ou, "Bangladesh"))
target <- targets %>% filter(operating_unit == "Bangladesh") %>%
  select(value = eg_3_2_26) %>% mutate(year = 2030L)
sales_abline <- sales %>% filter(name == "actual") %>%
  drop_na() %>% # slice(which.max(year)) %>%
  filter(year == 2022) %>% select(year, value) %>% bind_rows(target) %>%
  mutate(name = "Centrally set Target")

on_track <- approxfun(x = sales_abline$year, sales_abline$value)
this_year <- sales %>% filter(str_detect(ou, "Bangladesh") & name == "actual") %>%
  drop_na() %>% slice(which.max(year)) %>%# filter(year == 2023) %>%
  select(year,value)
this_year_value <- this_year$value
upper <- on_track(this_year$year) + on_track(this_year$year)/20
lower <- on_track(this_year$year) - on_track(this_year$year)/20
# ----------- Prep data
title <- make_plot_title(this_year_value = this_year_value, lower = lower, upper = upper)
subtitle <- make_plot_subtitle(sales)
sales_plot <- sales  %>% filter(str_detect(ou, "Bangladesh")) %>%
  bind_rows(sales_abline) %>%
  mutate(name = case_when(
    name == "actual" ~ "Actual", name == "target" ~ "OU Target", .default = name)
    , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
  rename(`Value Type` = name, "PT2: Sales" = value) %>%
  # ------------ ggplot
  ggplot(aes(x = `Fiscal Year`, y = `PT2: Sales`, color = `Value Type`, linetype = `Value Type`)) +
  geom_point(size = 2.3) + geom_line(size = 1) +
  # ------------ Set themes
  ylim(c(0, NA)) +
  # theme_settings +
  usaid_colors() +
  common_theme() +
  guides(color = guide_legend(position = "inside")) + usaid_colors() + common_theme() +
  ggtitle(label = title, subtitle = subtitle)
sales_plot


# Hectares -------------------
hectares <- hectares_(data) %>% filter(ro == "USAID" & str_detect(ou, "Bangladesh"))
target <- targets %>% filter(operating_unit == "Bangladesh") %>%
  select(value = eg_3_2_25) %>% mutate(year = 2030L)
hectares_abline <- hectares %>% filter(name == "actual") %>%
  drop_na() %>% # slice(which.max(year)) %>%
  filter(year == 2022) %>% select(year, value) %>% bind_rows(target) %>%
  mutate(name = "Centrally set Target")

on_track <- approxfun(x = hectares_abline$year, hectares_abline$value)
this_year <- sales %>% filter(name == "actual") %>%
  drop_na() %>% slice(which.max(year)) %>%# filter(year == 2023) %>%
  select(year,value)
this_year_value <- this_year$value
upper <- on_track(this_year$year) + on_track(this_year$year)/20
lower <- on_track(this_year$year) - on_track(this_year$year)/20

# ----------- Prep data
title <- make_plot_title(this_year_value = this_year_value, lower = lower, upper = upper)
subtitle <- make_plot_subtitle(hectares)
hectares_plot <- hectares  %>%
  bind_rows(hectares_abline) %>%
  mutate(name = case_when(
    name == "actual" ~ "Actual", name == "target" ~ "OU Target", .default = name)
    , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
  rename(`Value Type` = name, "PT3: Climate Hectares" = value) %>%
  # ------------ ggplot
  ggplot(aes(x = `Fiscal Year`, y = `PT3: Climate Hectares`, linetype = `Value Type`, color = `Value Type`)
         ) +
  geom_point(size = 2.3) + geom_line(size = 1) +
  # ------------ Set themes
  ylim(c(0, NA)) +
  # theme_settings +
  # scale_linetype_manual(values = c("Actual" = "solid", "OU Target" = "dotted", "Centrally set Target" = "dashed")) +
  guides(color = guide_legend(position = "inside")) + usaid_colors() + common_theme() +
  ggtitle(label = title, subtitle = subtitle)
hectares_plot


# PSI -------------
psi <- psi_(data) %>% filter(ro == "USAID" & str_detect(ou, "Bangladesh"))

subtitle <- make_plot_subtitle(psi)

# ----------- Prep data
psi_plot <- psi  %>%
  # bind_rows(psi_abline) %>%
  mutate(name = case_when(
    name == "actual" ~ "Actual", name == "target" ~ "OU Target", .default = name)
    , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
  rename(`Value Type` = name, "PT4: PSI" = value) %>%
  # ------------ ggplot
  ggplot(aes(x = `Fiscal Year`, y = `PT4: PSI` , color = `Value Type`)) +
  geom_point(size = 2.3) + geom_line(size = 1) +
  # ------------ Set themes
  # xlim(c(2022, 2030)) +
  guides(color = guide_legend(position = "inside")) + usaid_colors() + common_theme() +
  ylim(c(0, NA)) +
  ggtitle(label = "Centrally set Target not Evaluated", subtitle = subtitle)
psi_plot

# MDD-W -------------
mddw <- mddw_(data) %>% filter(ro == "USAID" & str_detect(ou, "Bangladesh"))
target <- targets %>% filter(operating_unit == "Bangladesh") %>%
  select(value = hl_9_1_d) %>% mutate(value = value *100, year = 2030L)
mddw_abline <- mddw %>% filter(name == "actual") %>%
  drop_na() %>% # slice(which.max(year)) %>%
  filter(year == 2022) %>% select(year, value) %>% bind_rows(target) %>%
  mutate(name = "Centrally set Target")

on_track <- approxfun(x = mddw_abline$year, mddw_abline$value)
this_year <- mddw %>% filter(name == "actual") %>%
  drop_na() %>% slice(which.max(year)) %>%# filter(year == 2023) %>%
  select(year,value)
this_year_value <- this_year$value
upper <- on_track(this_year$year) + on_track(this_year$year)/20
lower <- on_track(this_year$year) - on_track(this_year$year)/20

# ----------- Prep data
title <- make_plot_title(this_year_value = this_year_value, lower = lower, upper = upper)
subtitle <- make_plot_subtitle(mddw)
mddw_plot <- mddw  %>%
  bind_rows(mddw_abline) %>%
  mutate(name = case_when(
    name == "actual" ~ "Actual", name == "target" ~ "OU Target", .default = name)
    , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
  rename(`Value Type` = name, "PT5: MDD-W" = value) %>%
  # ------------ ggplot
  ggplot(aes(x = `Fiscal Year`, y = `PT5: MDD-W`/100
             , linetype = `Value Type`, color = `Value Type`)) +
  geom_point() +
  geom_line( size = 1) +
  # ------------ Set themes
  # theme_settings +
  guides(color = guide_legend(position = "inside")) + usaid_colors() + common_theme() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  ylab("MDD-W (% in ZOI)")
  ggtitle(label = title, subtitle = subtitle)
# mddw_plot

# Budget --------
budget <- budget %>% filter(year %in% 2020:2023 & str_detect(OU, "Bangladesh")) %>%
  mutate(budget = budget * 1000)

budget_hline <- budget %>% filter(year == 2023) %>% select(budget) %>% as.numeric


# ----------- Prep data
budget_plot <- budget  %>%
  rename(`Fiscal Year` = year, `Budget` = budget) %>%
  # ------------ ggplot
  ggplot(aes(x = `Fiscal Year`, y = `Budget`)) +
  geom_point(size = 2.3) + geom_line(size = 1) +
  geom_hline(color = "#BA0C2F", yintercept = budget_hline) +
  # ------------ Set themes

  # theme_settings +
  guides(color = guide_legend(position = "inside")) + usaid_colors() + common_theme() +
  scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
  ggtitle(label = "EG3 Budget Trend", subtitle = "Horizontal line is baseline")

plot_grid(gender_finance_plot, sales_plot, hectares_plot, psi_plot, mddw_plot
          , budget_plot, nrow = 3)
ggsave("./output/combined_plots.png", device = "png", width = 7, height = 9.5, units = "in")

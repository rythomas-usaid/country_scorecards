

scorecard_plot <- function(country_name) {

  # FINANCING -------------
  gender_finance <- gender_financing_(data, level="ou") %>%
    filter(ro == "USAID" & str_detect(ou, country_name))

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
    bind_rows(abline) %>%
    mutate(name = case_when(name == "actual" ~ "Actual", name == "target" ~ "OU Target"
                            , .default = name)
           , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
    rename(`Value Type` = name) %>%
    ggplot(aes(x = `Fiscal Year`, y = value , color = `Value Type`, linetype = `Value Type`)) +
    geom_point(size = 2.3) + geom_line(size = 1) +
    ylim(c(0, NA)) + ylab("PT1: Gender financing") +
    usaid_colors() + common_theme() + theme(legend.justification = "top") +
    ggtitle(label = title, subtitle = subtitle)
  gender_finance_plota <- gender_finance_plot + theme(legend.position = "none")


  # SALES ------------
  sales <- sales_(data) %>% filter(ro == "USAID" & str_detect(ou, country_name))
  target <- targets %>% filter(operating_unit == country_name) %>%
    select(value = eg_3_2_26) %>% mutate(year = 2030L)
  sales_abline <- sales %>% filter(name == "actual") %>%
    drop_na() %>% # slice(which.max(year)) %>%
    filter(year == 2022) %>% select(year, value) %>% bind_rows(target) %>%
    mutate(name = "Centrally set Target")

  on_track <- approxfun(x = sales_abline$year, sales_abline$value)
  this_year <- sales %>% filter(name == "actual") %>%
    drop_na() %>% slice(which.max(year)) %>%# filter(year == 2023) %>%
    select(year,value)
  this_year_value <- this_year$value
  upper <- on_track(this_year$year) + on_track(this_year$year)/20
  lower <- on_track(this_year$year) - on_track(this_year$year)/20
  # ----------- Prep data
  title <- make_plot_title(this_year_value = this_year_value, lower = lower, upper = upper)
  subtitle <- make_plot_subtitle(sales)
  sales_plot <- sales  %>%
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
    usaid_colors() + common_theme() + theme(legend.position="none") +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    guides(color = guide_legend(position = "inside")) + usaid_colors() + common_theme() +
    ggtitle(label = title, subtitle = subtitle)
  # sales_plot


  # Hectares -------------------
  hectares <- hectares_(data) %>% filter(ro == "USAID" & str_detect(ou, "Bangladesh"))
  target <- targets %>% filter(operating_unit == country_name) %>%
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
    scale_y_continuous( labels = scales::comma, limits = c(0, NA)) +
    guides(color = guide_legend(position = "inside")) +
    usaid_colors() + common_theme() + theme(legend.position="none") +
    ggtitle(label = title, subtitle = subtitle)
  # hectares_plot


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
    ggplot(aes(x = `Fiscal Year`, y = `PT4: PSI` , color = `Value Type`, linetype = `Value Type`)) +
    geom_point(size = 2.3) + geom_line(size = 1) +
    # ------------ Set themes
    xlim(c(as.Date("2022-01-01"), as.Date("2030-01-01"))) +
    guides(color = guide_legend(position = "inside")) +

    common_theme() + theme(legend.position="none") +
    # Then override the following theme parameters.
    scale_linetype_manual(values = c("Actual" = 1, "OU Target" = 3)) +
    scale_color_manual(values = c( "#002F6C", "#0067B9")) +

    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    ggtitle(label = "Centrally set Target not Evaluated", subtitle = subtitle)
  # psi_plot

  # MDD-W -------------
  mddw <- mddw_(data) %>% filter(ro == "USAID" & str_detect(ou, country_name))
  target <- targets %>% filter(operating_unit == country_name) %>%
    select(value = hl_9_1_d) %>% mutate(value = value *100, year = 2030L)
  mddw_abline <- mddw %>% filter(name == "actual") %>%
    drop_na() %>% # slice(which.max(year)) %>%
    filter(year == 2022) %>% select(year, value) %>% bind_rows(target) %>%
    mutate(name = "Centrally set Target")
  this_year <- mddw %>% filter(name == "actual") %>%
    drop_na() %>% slice(which.max(year)) %>%
    select(year,value)
  this_year_value <- this_year$value


  if(all(!is.na(mddw_abline$value))) {
    on_track <- approxfun(x = mddw_abline$year, mddw_abline$value)
    upper <- on_track(this_year$year) + on_track(this_year$year)/20
    lower <- on_track(this_year$year) - on_track(this_year$year)/20
    title <- make_plot_title(this_year_value = this_year_value, lower = lower, upper = upper)
  } else {
    title <- "MDD-W not evaluated pending PBS"
    }

  # ----------- Prep data
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
    guides(color = guide_legend(position = "inside")) +
    usaid_colors() + common_theme() + theme(legend.position="none") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ylab("P5: MDD-W (% in ZOI)") +
    ggtitle(label = title, subtitle = subtitle)
  # mddw_plot

  # Budget --------
  budget <- budget %>% filter(year %in% 2020:2023 & str_detect(OU, country_name)) %>%
    mutate(budget = budget)

  budget_hline <- budget %>% filter(year == 2023) %>% select(budget) %>% as.numeric


  # ----------- Prep data
  budget_plot <- budget  %>%
    rename(`Fiscal Year` = year, `EG3 Budget (000)` = budget) %>%
    # ------------ ggplot
    ggplot(aes(x = `Fiscal Year`, y = `EG3 Budget (000)`)) +
    geom_point(size = 2.3) + geom_line(size = 1) +
    geom_hline(color = "#BA0C2F", yintercept = budget_hline) +
    # ------------ Set themes

    # theme_settings +
    guides(color = guide_legend(position = "inside")) +
    usaid_colors() + common_theme() + theme(legend.position="none") +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    ggtitle(label = "EG3 Budget Trend", subtitle = "Horizontal line is baseline")

  # extract the legend from one of the plots
  legend <- get_legend(
    gender_finance_plot + theme(legend.box.margin = margin(0, 0, 0, 12))
  )

  plots <- plot_grid(gender_finance_plota, sales_plot, hectares_plot, psi_plot
                     , ncol = 2)

  bottom_row <- plot_grid(mddw_plot, budget_plot, legend
                          , rel_widths = c(1,.7, .3), nrow = 1)

  # add the legend to the row we made earlier. Give it one-third of
  # the width of one plot (via rel_widths).
  p1 <- plot_grid(plots, bottom_row, nrow = 2
                  , rel_heights = c(2/3, 1/3))
  return(p1)
}

scorecard_plot("Zambia")

ggsave("./output/combined_plots.png", device = "png", width = 7, height = 9.5, units = "in")


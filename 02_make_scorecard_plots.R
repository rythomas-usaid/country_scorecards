


s1_plot <- function(data, ou_name, targets, ou_kin, n_activities, scores = scores, years = 2022:2025) {
# data <- input_data
  rh <- c(.2, 1)
  deviation = .1

  # SALES ------------
  pt_data <- sales_(data) %>%
    filter(ro == "USAID" & str_detect(ou, ou_name) & year %in% years)
  target <- targets %>% filter(operating_unit == ou_name) %>%
    select(value = eg_3_2_26) %>% mutate(year = 2030L)
  abline <- make_abline(pt_data, target = target)

  this_year <- pt_data %>% filter(name == "actual") %>%
    drop_na() %>% slice(which.max(year)) %>%
    select(year,value)
  # ----------- Prep data
  plot_params <- make_plot_params(this_year = this_year, abline = abline, deviation = deviation)
  # print(plot_params["title"])
  scores$score[scores$pt == "PT1"] <- as.factor(plot_params["title"])
  sales_plot <- pt_data  %>%  bind_rows(abline) %>%
    mutate(name = case_when(
      name == "actual" ~ "Actual", name == "target" ~ "OU Target"
      , .default = name)
      , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
    rename(`Value Type` = name, "PT2: Sales" = value) %>%
    # ------------ ggplot
    ggplot(aes(x = `Fiscal Year`, y = `PT2: Sales`
               , color = `Value Type`, linetype = `Value Type`)) +
    geom_point(size = 2.3) + geom_line(linewidth = 1) +
    # ------------ Set themes
    ylim(c(0, NA)) + ylab("") + xlab("") +
    theme(legend.position = "bottom") +

    geom_ribbon(aes(ymin = lower, ymax = upper, fill = paste0(deviation*100, "% Deviation"))
                , alpha = 0.1, colour = NA ) +

    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    usaid_colors() + #usaid_fill_manual() +
    guides(color=guide_legend(override.aes=list(fill=NA))) +
    common_theme() +
    ggtitle(label = "PT1: Sales")

  # extract the legend from one of the plots
  legend <- cowplot::get_plot_component(
    sales_plot + ggplot2::theme(
      legend.direction = "horizontal",
      legend.justification="left",
      legend.box.just = "bottom")
    , 'guide-box-bottom'
    )
  sales_plota <- sales_plot + theme(legend.position = "none")

  ## TEXT ---------------
  # Get text data
  pt_text <- data %>% filter(year == 2023 & ro == "USAID" & str_detect(ou, ou_name)) %>%
    select(ro, ou, ic, udn, year,a_code, a_name, d_name, actual, target
           , deviation_percentage, deviation_narrative)
  pt_text <- filter(pt_text, ic == "EG.3.2-26")
  n_aligned <- length(pt_text$actual)
  n_contributing <- sum(!is.na(pt_text$actual))
  # Write Text
  txt <- paste0("Of "
                , n_activities," reporting activities for USAID ", ou_name, " in FY23, "
                , n_aligned, " aligned the sales indicator, 'Value of annual sales of producers and firms receiving USG assistance,’ and "
                , n_contributing, " contributed to the OU total in FY2023.")
  sales_text <- ggplot() +
    geom_text(aes(x=0, y=0, label = plot_params["title"])
                  , family = "Gill Sans MT", color = plot_params["color"]
                  , fontface = "bold", size = 14 /.pt) +
    theme_void()
    sales_subtext <- ggplot() +
      geom_textbox(aes(x= 0, y = 0, label = txt)
                 , size = 8 / .pt
                 , width = unit(3, "inches")) +
    theme_void()

   sales_text <- plot_grid(sales_text, sales_subtext
                           , ncol = 1, align = "top"
                           , rel_heights = rh)
  # sales_plot

  # FINANCING -------------
  pt_data <- gender_financing_(data, level="ou") %>%
    filter(ro == "USAID" & str_detect(ou, ou_name) & year %in% years)
  abline <- make_abline(pt_data, target = tibble(year = 2030L, value = 1))
  this_year <- pt_data %>%
    filter(name == "actual") %>%
    drop_na() %>% slice(which.max(year)) %>%
    select(year, value)
  plot_params <- make_plot_params(this_year = this_year, abline = abline, deviation = deviation)
  scores$score[scores$pt == "PT2"] <- as.factor(plot_params["title"])
  gender_finance_plot <- pt_data  %>%
    bind_rows(abline) %>%
    mutate(name = case_when(name == "actual" ~ "Actual"
                            , name == "target" ~ "Out Year Targets"
                            , .default = name)
           , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
    rename(`Value Type` = name) %>%
    # ------------ ggplot
    ggplot(aes(x = `Fiscal Year`, y = value , color = `Value Type`
               , linetype = `Value Type`)) +
    geom_point(size = 2.3) + geom_line(linewidth = 1) +
    ylim(c(0, NA)) + ylab("") + xlab("") +
    theme(legend.position = "none") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = paste0(deviation*100, "% Deviation"))
                , alpha = 0.1, colour = NA ) +
    usaid_colors() + #usaid_fill_manual() +
    guides(color=guide_legend(override.aes=list(fill=NA))) +
    common_theme() +
    ggtitle(label = "PT2: Gender financing ratio ($ per female/$ per male)")

  ## TEXT ---------------
  pt_text <- gender_financing_(data, level="im") %>%
    filter(year == 2023 & ro == "USAID" & str_detect(ou, ou_name)) %>%
    select(ro, ou, year, a_code, a_name, name, `Female Per Person`
           , `Male Per Person`) %>%
    mutate(finance_gap = `Female Per Person` / `Male Per Person`)
  n_aligned <- length(unique(pt_text$a_code))
  n_contributing <- sum(!is.na(pt_text$actual))
  txt <- paste0(
    length(pt_text$a_code[pt_text$name == "actual"])
    ," of those activities aligned the financing indicator, and "
    , sum(!is.na(pt_text$finance_gap[pt_text$name == "actual"]))
    , " reported gender-disaggregated data for the count and value for males and females receiving financing in FY2023.")
  # deviation_narratives <- extract %>%
  #   filter(year == 2023 & ic == "EG.3.2-27" & udn == "3" & a_code %in% pt_text$a_code) %>%
  #   select(ou, ic, udn, year, deviation_narrative)
  gf_text <- ggplot() +
    geom_text(aes(x=0, y=0, label = plot_params["title"])
              , family = "Gill Sans MT", color = plot_params["color"]
              , fontface = "bold", size = 14 /.pt) +
    theme_void()
  gf_subtext <- ggplot() +
    geom_textbox(aes(x= 0, y = 0, label = txt)
                 , size = 8 / .pt
                 , width = unit(3, "inches")) +
    theme_void()
  gf_text <- plot_grid(gf_text, gf_subtext
                          , ncol = 1, align = "top"
                       , rel_heights = rh)

  # Hectares -------------------
  pt_data <- hectares_(data) %>%
    filter(ro == "USAID" & str_detect(ou, ou_name) & year %in% years)
  target <- targets %>% filter(operating_unit == ou_name) %>%
    select(value = eg_3_2_25) %>% mutate(year = 2030L)
  abline <- make_abline(pt_data, target = target)
  this_year <- pt_data %>% filter(name == "actual") %>%
    drop_na() %>% slice(which.max(year)) %>%
    select(year,value)
  # ----------- Prep data
  plot_params <- make_plot_params(this_year = this_year, abline = abline, deviation = deviation)
  scores$score[scores$pt == "PT3"] <- as.factor(plot_params["title"])
  hectares_plot <- pt_data  %>%
    bind_rows(abline) %>%
    mutate(name = case_when(
      name == "actual" ~ "Actual", name == "target" ~ "OU Target", .default = name)
      , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
    rename(`Value Type` = name, "PT3: Climate Hectares" = value) %>%
    # ------------ ggplot
    ggplot(aes(x = `Fiscal Year`, y = `PT3: Climate Hectares`
               , linetype = `Value Type`, color = `Value Type`)) +
    geom_point(size = 2.3) + geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper
                    , fill = paste0(deviation*100, "% Deviation"))
              , alpha = 0.1, colour = NA ) +
    # ------------ Set themes
    ylim(c(0, NA)) + ylab("") + xlab("") +
    scale_y_continuous( labels = scales::comma, limits = c(0, NA)) +
    guides(color=guide_legend(override.aes=list(fill=NA))) +
    usaid_colors() + common_theme() + theme(legend.position="none") +
    ggtitle(label = "PT3: Climate Hectares")
  # hectares_plot

  ## TEXT ----------------
  pt_text <- data %>%
    filter(year == 2023 & ro == "USAID" & str_detect(ou, ou_name)) %>%
    select(ro, ou, ic, udn, year,a_code, a_name, d_name, actual, target
           , deviation_percentage, deviation_narrative)
  pt_text <- filter(pt_text, ic == "EG.3.2-25")

  n_aligned <- length(pt_text$actual)
  n_contributing <- sum(!is.na(pt_text$actual))
  # Write Text
  txt <- paste0(n_activities, " of those activities aligned the hectares indicator, '"
                , unique(pt_text$d_name), "', and "
                , n_contributing, " contributed to the OU total in FY2023.")
  ht_text <- ggplot() +
    geom_text(aes(x=0, y=0, label = plot_params["title"])
              , family = "Gill Sans MT", color = plot_params["color"]
              , fontface = "bold", size = 14 /.pt) +
    theme_void()
  ht_subtext <- ggplot() +
    geom_textbox(aes(x= 0, y = 0, label = txt)
                 , size = 8 / .pt
                 , width = unit(3, "inches")) +
    theme_void()
  ht_text <- plot_grid(ht_text, ht_subtext
                       , ncol = 1, align = "top"
                       , rel_heights = rh)

  # PSI -------------
  psi <- psi_(data) %>%
    filter(ro == "USAID" & str_detect(ou, ou_name))
  this_year <- psi %>% filter(name == "actual" & year %in% 2021:2023) %>%
    summarize(three_yr_avg = mean(value, na.rm=T), value = last(value))

  target <- targets %>% filter(operating_unit == ou_name) %>%
    select(value = eg_3_1_15) %>% mutate(year = 2030L)
  # abline <- make_abline(pt_data = pt_data, target = target)
  # this_year <- psi %>% filter(name == "actual") %>%
  #   drop_na() %>% slice(which.max(year)) %>%
  #   select(year,value)
  # ----------- Prep data
  plot_params <- make_plot_params(this_year = this_year, psi = TRUE)
  scores$score[scores$pt == "PT4"] <- as.factor(plot_params["title"])
    # ----------- Prep data
  psi_plot <- psi  %>%
    mutate(name = case_when(
      name == "actual" ~ "Actual", name == "target" ~ "OU Target"
      , .default = name)
      , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
    rename(`Value Type` = name, "PT4: PSI" = value) %>%
    # ------------ ggplot
    ggplot(aes(x = `Fiscal Year`, y = `PT4: PSI` , color = `Value Type`
               , linetype = `Value Type`)) +
    geom_point(size = 2.3) + geom_line(linewidth = 1) +
    # ------------ Set themes
    xlim(c(as.Date("2022-01-01"), as.Date("2030-01-01"))) + ylab("") + xlab("") +
    common_theme() + theme(legend.position="none") +
    # Then override the following theme parameters.
    scale_linetype_manual(values = c("Actual" = 1, "OU Target" = 3)) +
    usaid_colors() +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    ggtitle(label = "PT4: PSI")
  # psi_plot
  ## TEXT ----------------
  pt_text <- data %>%
    filter(year == 2023 & ro == "USAID" & str_detect(ou, ou_name)) %>%
    select(ro, ou, ic, udn, year,a_code, a_name, d_name, actual, target
           , deviation_percentage, deviation_narrative)
  pt_text <- filter(pt_text, ic %in% c("EG.3.1-14", "EG.3.1-15"))

  n_aligned <- length(pt_text$actual)
  n_contributing <- sum(!is.na(pt_text$actual))
  # Write Text
  txt <- paste0(n_activities, " of those activities aligned the private sector investment indicators (EG.3.1-15/-14), '"
                , paste(unique(pt_text$d_name), collapse = ", "), "', and "
                , n_contributing, " contributed to the OU total in FY2023.")
  psi_text <- ggplot() +
    geom_text(aes(x=0, y=0, label = plot_params["title"])
              , family = "Gill Sans MT", color = plot_params["color"]
              , fontface = "bold", size = 14 /.pt) +
    theme_void()
  psi_subtext <- ggplot() +
    geom_textbox(aes(x= 0, y = 0, label = txt)
                 , size = 8 / .pt
                 , width = unit(3, "inches")) +
    theme_void()
  psi_text <- plot_grid(psi_text, psi_subtext
                       , ncol = 1, align = "top"
                       , rel_heights = rh)

  # MDD-W -------------
  mddw <- mddw_(data) %>% filter(ro == "USAID" & str_detect(ou, ou_name))
  if(nrow(mddw) == 0L) {
    plot_params <- list(title = "Not available", color = "#8C8985")
    mddw_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = "No available PT5: MDD-W data")
                , family = "Gill Sans MT", color = plot_params["color"]
                , fontface = "bold", size = 14 /.pt) +
      theme_void()
  } else if(nrow(mddw) > 0L) {
    target <- targets %>% filter(operating_unit == ou_name) %>%
      select(value = hl_9_1_d, last_pbs, next_pbs) %>%
      mutate(value = value *100, year = 2030L)
    abline <- make_abline(mddw, target = target)
    this_year <- mddw %>% filter(name == "actual") %>%
      drop_na() %>% slice(which.max(year)) %>%
      select(year,value)
    plot_params <- make_plot_params(this_year = this_year, abline = abline, deviation = deviation)

    mddw_plot <- mddw  %>% filter(year %in% 2022:2030) %>%
      bind_rows(abline) %>%
      mutate(name = case_when(
        name == "actual" ~ "Actual", name == "target" ~ "OU Target", .default = name)
        , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
      rename(`Value Type` = name, "PT5: MDD-W" = value) %>%

      # ------------ ggplot
      ggplot(aes(x = `Fiscal Year`, y = `PT5: MDD-W`/100
                 , linetype = `Value Type`, color = `Value Type`)) +
      geom_point(size = 2.3) + geom_line(linewidth = 1) +
      # ------------ Set themes
      ylab("") + xlab("") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      usaid_colors() + common_theme() + theme(legend.position="none") +
      ggtitle(label = "PT5: MDD-W (% in ZOI)")
  }

  mddw_text <- NULL
  scores$score[scores$pt == "PT5"] <- as.factor(plot_params["title"])

    # Stars ------
  stars_plot <- scores %>%
    mutate(score = fct_relevel(score, c("On track", "Falling behind", "Not applicable"))) %>%
    arrange(score) %>%  mutate(x = row_number()) %>%
    ggplot(aes(x, y, color = score, fill = score)) +
    geom_star(starshape = 1, size = 7) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = fills) +
    xlab("") + ylab("") + theme(plot.margin = unit(c(0,0,0,0), "lines")) +
    theme_void() + theme(legend.position = "none") +
    coord_fixed(ratio = 3)
    overall_score <- paste0(sum_(scores$score == "On track"), "/"
                            , sum_(!is.na(scores$score )), " Performance Targets")
    overall_score_text <- ggplot() +
      geom_text(aes(x=0, y=0, label = overall_score)
                , family = "Gill Sans MT", color = "#6C6463"
                , fontface = "bold", size = 14 /.pt) +
      theme_void() + theme(plot.margin = unit(c(0,0,0,0), "lines"))

  s1p <- plot_grid(
    plot_grid(legend,
              plot_grid(overall_score_text, stars_plot+ theme(plot.margin = unit(c(0,0,0,0), "lines"))
                        , ncol = 1, hjust = 0, rel_heights = c(1,1))
              , nrow = 1
              ),
    plot_grid(sales_plota, sales_text
              , gender_finance_plot, gf_text
              , hectares_plot, ht_text
              , psi_plot, psi_text
              , mddw_plot, mddw_text
              , ncol = 2, rel_widths = c(.6, 1))
    , ncol = 1, rel_heights = c(.15, .85), hjust = 0
    )
  # add the legend to the row we made earlier. Give it one-third of
  # the width of one plot (via rel_widths).
  return(list(plot = s1p, scores = scores))
}

s2_plot <- function(pov=NA, agti=NA, budget=NA) {
  # Budget --------
  budget <- budget %>% filter(year %in% 2020:2024 & str_detect(ou, ou_name)) %>%
    mutate(budget = budget)
  budget_hline <- budget %>% filter(year == 2023) %>%
    select(budget) %>% as.numeric()
  # # ----------- Prep data
  budget_plot <- budget  %>%
    rename(`Fiscal Year` = year, `EG3 Budget (000)` = budget) %>%
    # ------------ ggplot
    ggplot(aes(x = `Fiscal Year`, y = `EG3 Budget (000)`)) +
    geom_point(size = 2.3) + geom_line(linewidth = 1) +
    geom_hline(color = "#BA0C2F", yintercept = budget_hline, linewidth = 1) +
    # ------------ Set themes
    guides(color = guide_legend(position = "inside")) +
    usaid_colors() + common_theme() + theme(legend.position="none") +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    ylab("") + xlab("") +
    ggtitle(label = "EG3 Budget Trend", subtitle = "Horizontal line is baseline")
}


# Narrative

# ggsave("./output/combined_plots.png"
#        , device = "png", width = 7, height = 8, units = "in"
#        , bg='transparent')


# source("01_prepare_data.R")
# ou_name <- "Zambia"
# rmarkdown::render("Scorecard.Rmd"
#                   , output_file = paste0("output/", ou_name, ".pdf")
#                   , params = list(ou_name = ou_name, data = data))





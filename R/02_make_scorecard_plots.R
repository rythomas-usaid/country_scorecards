
library(patchwork)
s1_plot <- function(dat, program, ou_names, targets, ou_dat, mddw, n_activities, finance_sex_dna
                    , total_financing, finance_sex_mixed, scores = scores
                    , start_year = 2022, out_year = 2024, deviation = .9) {
  ou_label <- program
# deviation = .9
# dat <- input_dat
# years <- 2022:2024
# start_year <- 2022
# out_year <- 2024
  rh <- c(.2, 1)
  new_target_name <- "FY23 Actual to FY24 Out year target"
  pt_label <- "Trajectory from Baseline* to FY30 Performance Target"

  # SALES  ------------
  if("PT1: Sales" %in% ou_dat$name){
    sales_actual_to_outyear <- if(program == "Group Target"){
     sales_(dat) %>%
        filter( ! ou %in% sales_ous
                & !is.na(value)
                & ((name == "target" & year == out_year) | (name == "actual" & year == 2023))) %>%
        group_by(name, year) %>%
        summarise(program = program, value = sum_(value), .groups = "drop")
    } else if(program == "FTF Initiative") {
      sales_(dat) %>%
        filter(!is.na(value)
               & ((name == "target" & year == out_year) | (name == "actual" & year == 2023))) %>%
        select(program, year, name, value) %>%
        group_by(year, name) %>%
        summarize(ro = NA, program="FTF Initiative", value = sum_(value))
    }
    sales_actual_to_outyear <- sales_actual_to_outyear %>%
      select(program, type = name, year, value) %>%
      mutate(name = "PT1: Sales"
             , type = new_target_name)
    sales_target <- ou_dat %>%
      filter(name == "PT1: Sales" & (type == "baseline" | type == "performance target")) %>%
      mutate(type = pt_label
             , lower = value*deviation)
    this_year <- sales_actual_to_outyear %>%
      slice(which(year == 2023)) %>%
      select(year,value)
    # ----------- Prep data
    sales_plot_params <- make_plot_params(this_year = this_year, pt_abline = sales_target, deviation = deviation)
    # print(sales_plot_params["title"])
    scores$score[scores$pt == "PT1"] <- as.factor(sales_plot_params["title"])

    ## PLOT  ------------
    sales_dat <- sales_actual_to_outyear  %>% bind_rows(sales_target) %>%
      bind_rows(sales_actual_to_outyear %>%
                  filter(year == 2023) %>%
                  mutate(type = "FY23 Reported Value")) %>%
      mutate(`Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
      rename(`Value Type` = type, "PT1: Sales" = value)

    sales_labels <- sales_dat %>% filter(! (`Value Type` != "FY23 Reported Value" & year == 2023))

    # sales_labels <- sales_dat %>%
    #   filter(!(`Value Type` == new_target_name & year == 2023))
    sales_plot <- sales_dat %>% #filter(`Value Type` != "FY23 Reported Value") %>%
      # ------------ ggplot layers
      ggplot(aes(x = `Fiscal Year`, y = `PT1: Sales`, color = `Value Type`
                 , linetype = `Value Type`)) +
      geom_line(linewidth = 1, linetype = 2) + geom_point(size = 2.3) +
      # ------------ Set themes
      theme(legend.position = "bottom") +
      ylim(c(0, NA)) + ylab("") + xlab("") +
      common_theme() +  usaid_color_manual() +
      geom_label_repel(
        aes(label = paste0(scales::dollar_format()(round(`PT1: Sales`, -6)/1000000), "M"))
        , data = sales_labels, show.legend = F, segment.color = NA) +
      # guides(color=guide_legend(override.aes=list(fill=NA))) +
      scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
      scale_x_date(date_breaks = "2 years"
                   , date_labels = paste0("FY", "%y")) +
      scale_color_manual(values = plot_colors) +
      guides(color = guide_legend(nrow = 3)) +
      labs(caption = "*For this PT, the baseline is the FY20-FY22 average.")

  # SALES
  ## TEXT -----------------
  sales_text <- if (program == "Group Target") {
    dat %>%
      filter(year %in% 2023:2024 & ro == ro & !ou %in% sales_ous) %>%
      select(program, ic, udn, year, a_code, a_name, d_name, actual, target
             , deviation_percentage, deviation_narrative) %>%
      filter(ic == "EG.3.2-26")
  } else if (program == "FTF Initiative") {
    dat %>% filter(year %in% 2023:2024 ) %>%
      select(program, ic, udn, year, a_code, a_name, d_name, actual, target
             , deviation_percentage, deviation_narrative) %>%
      filter(ic == "EG.3.2-26")
  } else  {
    dat %>%
    filter(year %in% 2023:2024 & ro == ro & ou == ou_name) %>%
    select(program, ic, udn, year, a_code, a_name, d_name, actual, target
           , deviation_percentage, deviation_narrative) %>%
    filter(ic == "EG.3.2-26")
  }
  sales_n_aligned <- length(unique(sales_text$a_code[sales_text$year == 2023]))
  sales_n_contributing <- length(unique(sales_text$a_code[!is.na(sales_text$actual) & sales_text$year == 2023]))
  sales_total <- sum_(sales_text$actual[sales_text$year == 2023])
  sales_fy24target <- sum_(sales_text$target[sales_text$year == 2024])
  # Write Text
  # sales_score <- sales_plot_params["title"]
  # sales_score_color <- sales_plot_params["color"]
  sales_txt <- paste0("In FY 2023, ", ou_label, " had "
                      , sales_n_aligned," activities that aligned the 'Value of annual sales from producers and firms receiving USG assistance' (EG.3.2-26), and "
                      , sales_n_contributing, " contributed to the sales total of "
                      , scales::dollar_format()(round(sales_total, -4)), ".")
  if(!is.na(sales_fy24target) &
     !is.null(sales_plot_params$trajectory.y[sales_plot_params$trajectory.x==2024])) {
    if(sales_plot_params["title"] == "Not on track"
       & sales_fy24target > sales_plot_params$trajectory.y[sales_plot_params$trajectory.x==2024]) {
    sales_txt <- paste0(sales_txt
                        , "///nNote that while ", ou_label
                        ," was 'Not on track' in FY23, it expects an increase in FY24. "
                        , "Such an increase would make it 'On track' for this performance indicator.")
    } else if(sales_plot_params["title"] == "On track"
              & sales_fy24target < sales_plot_params$trajectory.y[sales_plot_params$trajectory.x==2024]) {
      sales_txt <- paste0(sales_txt
                          , "///nNote that while ", ou_label
                          ," was 'On track' in FY23, it expects a substantial decrease in FY24. "
                          , "Such an increase would make it 'Not on track' for this performance indicator.")
    }
    }
    sales_text_plot <- ggplot() +
    geom_text(aes(x=0, y=0, label = sales_plot_params["title"])
              , family = "Gill Sans MT", color = sales_plot_params["color"]
              , fontface = "bold", size = 16 /.pt) +
    theme_void()
    # sales_subtext <- ggplot() +
    # geom_textbox(aes(x= 0, y = 0, label = sales_txt)
    #              , size = 8 / .pt
    #              , width = unit(3, "inches")) +
    # theme_void()

  } else if (! "PT1: Sales" %in% ou_dat$name) {
    sales_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = "No available PT1 data")
                , family = "Gill Sans MT", color = "#C0C0C0"
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    sales_text_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = "Not applicable")
                , family = "Gill Sans MT", color = "#C0C0C0"
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    sales_txt <- paste0("USAID ", ou_label
                        ," did not have a PT1 target.")
  }

  # FINANCING -------------
  if(program == "Group Target") {
    gf_actual_to_outyear <- dat %>%
      filter(! ou %in% gf_ous) %>%
      gender_financing_(level = "initiative") %>%
      filter(!is.na(value)
             & ((name == "target" & year == out_year) |
                  (name == "actual" & year == 2023))) %>%
      mutate(program = "Group Target", .before = everything()) %>%
      rename(type = name) %>% select(-c(a_codes)) %>%
      mutate(name = "PT2: Gender financing ratio", type = new_target_name) %>%
      select(program, year, name, type, value)
    gf_target <- ou_dat %>%
      filter(name == "PT2: Gender financing ratio" & (type == "baseline" | type == "performance target")) %>%
      mutate(type = pt_label
             , lower = value*deviation)
    this_year <- gf_actual_to_outyear %>% slice(which(year == 2023)) %>%
      select(year,value)
    # ----------- Prep data
    gf_plot_params <- make_plot_params(this_year = this_year, pt_abline = gf_target, deviation = deviation, gf = TRUE)
    # print(sales_plot_params["title"])
    scores$score[scores$pt == "PT2"] <- as.factor(gf_plot_params["title"])

    ## PLOT group data ------------
    gf_dat <- gf_actual_to_outyear  %>% bind_rows(gf_target) %>%
      bind_rows(gf_actual_to_outyear %>%
                  filter(year == 2023) %>%
                  mutate(type = "FY23 Reported Value")) %>%
      rename(`Value Type` = type, "PT2: Gender financing ratio" = value)

    } else if(program == "FTF Initiative" ) {
    gf_actual_to_outyear <- gender_financing_(dat, level = "initiative") %>%
      filter(!is.na(value)
             & ((name == "target" & year == out_year) |
                  (name == "actual" & year == 2023))) %>%
      mutate(program ="FTF Initiative", .before = everything()) %>%
        rename(type = name) %>% select(-c(a_codes)) %>%
        mutate(name = "PT2: Gender financing ratio", type = new_target_name) %>%
        select(program, year, name, type, value)
    gf_target <- ou_dat %>%
      filter(name == "PT2: Gender financing ratio" & (type == "baseline" | type == "performance target")) %>%
      mutate(type = pt_label
             , lower = value*deviation)
    this_year <- gf_actual_to_outyear %>% slice(which(year == 2023)) %>%
      select(year,value)
    # ----------- Prep data
    gf_plot_params <- make_plot_params(this_year = this_year, pt_abline = gf_target, deviation = deviation, gf = TRUE)
    # print(sales_plot_params["title"])
    scores$score[scores$pt == "PT2"] <- as.factor(gf_plot_params["title"])

    ## PLOT  ------------
    gf_dat <- gf_actual_to_outyear  %>% bind_rows(gf_target) %>%
      bind_rows(gf_actual_to_outyear %>%
                  filter(year == 2023) %>%
                  mutate(type = "FY23 Reported Value")) %>%
      rename(`Value Type` = type, "PT2: Gender financing ratio" = value)
    } else {
   gf_actual_to_outyear <- gender_financing_(dat, level = "ou") %>%
    filter(ou == ou_name
           & !is.na(value)
           & ((name == "target" & year == out_year) |
                (name == "actual" & year == 2023)))  %>%
    rename(type = name) %>% select(-c(a_codes)) %>%
    mutate(name = "PT2: Gender financing ratio", type = new_target_name) %>%
    select(program, year, name, type, value)
  gf_target <- ou_dat %>%
    filter(name == "PT2: Gender financing ratio" & (type == "baseline" | type == "performance target")) %>%
    mutate(type = pt_label
           , lower = value*deviation)
  this_year <- gf_actual_to_outyear %>% slice(which(year == 2023)) %>%
    select(year,value)
  # ----------- Prep data
  gf_plot_params <- make_plot_params(this_year = this_year, pt_abline = gf_target, deviation = deviation, gf = TRUE)
  # print(sales_plot_params["title"])
  scores$score[scores$pt == "PT2"] <- as.factor(gf_plot_params["title"])

  ## PLOT  ------------
  gf_dat <- gf_actual_to_outyear  %>% bind_rows(gf_target) %>%
    bind_rows(gf_actual_to_outyear %>%
                filter(year == 2023) %>%
                mutate(type = "FY23 Reported Value")) %>%
    rename(`Value Type` = type, "PT2: Gender financing ratio" = value)
  }

  if(nrow(gf_dat) <= 1L) {
    gender_finance_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = "No available PT2: Gender Financing Ratio data")
                , family = "Gill Sans MT", color = gf_plot_params["color"]
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    gf_text_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = gf_plot_params["title"])
                , family = "Gill Sans MT", color = "#C0C0C0"
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
  } else if(nrow(gf_dat) > 1) {
    gf_dat <- gf_dat %>%
      mutate(`Fiscal Year` = as.Date(paste0(year, "-01-01")))
    gf_labels <- gf_dat %>%
      filter(! (`Value Type` != "FY23 Reported Value" & year == 2023))
    gender_finance_plot <- gf_dat %>%
      # ------------ ggplot
      ggplot(aes(x = `Fiscal Year`, y = `PT2: Gender financing ratio` , color = `Value Type`
                 , linetype = `Value Type`)) +
      geom_line(linewidth = 1, linetype = 2) + geom_point(size = 2.3) +
      theme(legend.position = "bottom") +
      common_theme() +  usaid_color_manual() +
      geom_label_repel(aes(label = scales::dollar_format()(`PT2: Gender financing ratio`))
                       , data = gf_labels, show.legend = F, direction = "x", nudge_x = 1) +
      scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)
                         , expand = expansion(mult = .5)) +
      ylim(c(0, NA)) + ylab("") + xlab("") +
      scale_x_date(date_labels = paste0("FY", "%y")) +
      scale_color_manual(values = plot_colors) +
      guides(color = guide_legend(nrow = 3)) +
      labs(caption = "*For this PT the baseline includes reporting from FY 2021-2022")
    gf_text_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = gf_plot_params["title"])
                , family = "Gill Sans MT", color = gf_plot_params["color"]
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
  }

  ## TEXT ---------------
  gf_text <- if(program == "Group Target") {
    gender_financing_(dat, level="im") %>%
      filter(year %in% 2023 & ro == ro & ! ou %in% gf_ous & name =="actual") %>%
      select(program, year, a_code, a_name, name, `Male Value`, `Female Value`
             , `Female Per Person`, `Male Per Person`) %>%
      mutate(finance_gap = `Female Per Person` / `Male Per Person`)
    } else if(program == "FTF Initiative"){
      gender_financing_(dat, level="im") %>%
        filter(year %in% 2023 & name =="actual") %>%
        select(program, year, a_code, a_name, name, `Male Value`, `Female Value`
               , `Female Per Person`, `Male Per Person`) %>%
        mutate(finance_gap = `Female Per Person` / `Male Per Person`)
    } else  {
    gender_financing_(dat, level="im") %>%
        filter(year %in% 2023 & ro == ro & ou == ou_name & name =="actual") %>%
        select(program, year, a_code, a_name, name, `Male Value`, `Female Value`
               , `Female Per Person`, `Male Per Person`) %>%
        mutate(finance_gap = `Female Per Person` / `Male Per Person`)
  }

  gf_n_aligned <- length(unique(gf_text$a_code))
  gf_n_contributing <- sum(!is.na(gf_text$finance_gap))
  gf_total_financing <- sum_(gf_text$`Male Value` , gf_text$`Female Value`)
  gf_fy24_financing_target <-  gender_financing_(dat, level="ou") %>%
    filter(year == 2024 & ro == ro & ou == ou_name & name == "target") %>%
    select(value)
  gf_txt <- paste0("In FY 2023, "
                , ou_label
                ," had ", gf_n_contributing
                , " IMs/Activities that reported all four sex disaggregates needed to contribute to PT2."
                , " The total value of financing among all activities, regardless of disaggregates, was "
                , scales::dollar_format()(round(gf_total_financing, -4))
                ," (EG.3.2-27 total), with "
                , scales::dollar_format()(round(gf_total_financing, -4))
                , " that was disaggregated by females and males, and an additional "
                , if(!is.na(finance_sex_mixed$mixed)) {
                  scales::dollar_format()(round(finance_sex_mixed$mixed, -4))
                  } else {"0"}
                , " to 'mixed' firms that have both males and females, and "
                , scales::dollar_format()(round(finance_sex_dna$dna, -4))
                , " that was not sex disaggregated.\\\n\\\n")
  if(nrow(gf_fy24_financing_target) >0L){
    if(!is.na(gf_fy24_financing_target)) {
    if(gf_plot_params["title"] == "Not on track"  & gf_fy24_financing_target > gf_plot_params$trajectory.y[gf_plot_params$trajectory.x==2024]) {
    gf_txt <- paste0(gf_txt
                        , "Note that while ", ou_label
                        ," was 'Not on track' in FY23, it expects an increase in FY24. "
                        , "Such an increase would make it 'On track' for this performance indicator.")
  } else if(gf_plot_params["title"] == "On track"
            & gf_fy24_financing_target < gf_plot_params$trajectory.y[gf_plot_params$trajectory.x==2024]) {
    gf_txt <- paste0(gf_txt
                        , "Note that while ", ou_label
                        ," was 'On track' in FY23, it expects a substantial decrease in FY24. "
                        , "Such an increase would make it 'Not on track' for this performance indicator.")
  }
    }
  }

  # Hectares -------------------
  if("PT3: Climate hectares" %in% ou_dat$name) {
    ht_actual_to_outyear <- if(program == "Group Target") {
      hectares_(dat) %>%
        filter(!ou %in% ht_ous
               # & !is.na(value)
               & ((name == "target" & year == out_year) |
                    (name == "actual" & year == 2023)))  %>%
        mutate(value = replace_na(value, 0)) %>%
        rename(type = name) %>% select(-c(ic, a_codes)) %>%
        mutate(name = "PT3: Climate hectares", type = new_target_name) %>%
        group_by(year, type, name) %>% summarize(value = sum_(value))
      } else if(program == "FTF Initiative") {
        hectares_(dat) %>%
          filter(((name == "target" & year == out_year) |
                      (name == "actual" & year == 2023)))  %>%
          mutate(value = replace_na(value, 0)) %>%
          rename(type = name) %>% select(-c(ic, a_codes)) %>%
          mutate(name = "PT3: Climate hectares", type = new_target_name) %>%
          group_by(year, type, name) %>% summarize(value = sum_(value))
      } else  {
      hectares_(dat) %>%
          filter(ou == ou_name
                 # & !is.na(value)
                 & ((name == "target" & year == out_year) |
                      (name == "actual" & year == 2023)))  %>%
          mutate(value = replace_na(value, 0)) %>%
          rename(type = name) %>% select(-c(ic, a_codes)) %>%
          mutate(name = "PT3: Climate hectares", type = new_target_name)
        }

    ht_target <- ou_dat %>%
      filter(name == "PT3: Climate hectares" & (type == "baseline" | type == "performance target")) %>%
      mutate(type = pt_label
             , lower = value*deviation)
    this_year <- ht_actual_to_outyear %>%
      filter(year == 2023) %>%
      group_by(year) %>% summarize(value = sum_(value))
    # ----------- Prep data
    ht_plot_params <- make_plot_params(this_year = this_year, pt_abline = ht_target, deviation = deviation)
    # print(sales_plot_params["title"])
    scores$score[scores$pt == "PT3"] <- as.factor(ht_plot_params["title"])
    ## PLOT  ------------
    ht_dat <- ht_actual_to_outyear  %>% bind_rows(ht_target) %>%
      bind_rows(ht_actual_to_outyear %>%
                  filter(year == 2023) %>%
                  mutate(type = "FY23 Reported Value")) %>%
      mutate(`Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
      rename(`Value Type` = type, "PT3: Climate hectares" = value)
    ht_labels <- ht_dat %>%
      filter(! (`Value Type` != "FY23 Reported Value" & year == 2023))

    hectares_plot <- ht_dat  %>%
      # ------------ ggplot
      ggplot(aes(x = `Fiscal Year`, y = `PT3: Climate hectares`
                 , linetype = `Value Type`, color = `Value Type`)) +
      geom_line(linewidth = 1) + geom_point(size = 2.3) +
      geom_label_repel(aes(label = scales::comma_format()(round(`PT3: Climate hectares`, -1)))
                       , data = ht_labels, show.legend = F, direction = "x", nudge_x = 1) +
      theme(legend.position = "bottom") + xlab("") + ylab("") +
      common_theme() + usaid_color_manual() +
      scale_color_manual(values = plot_colors) +
      scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
      scale_x_date(date_breaks = "2 years", date_labels=paste0("FY", "%y")) +

      guides(color=guide_legend(nrow=3)) +
      labs(caption = "*For this PT, the baseline is the average of FY 2021 and FY 2022.")
    ht_text_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = ht_plot_params["title"])
                , family = "Gill Sans MT", color = ht_plot_params["color"]
                , fontface = "bold", size = 16 /.pt) +
      theme_void()

    ## TEXT ----------------
    ht_text <- if(program == "Group Target") {
      dat %>%
        filter(year == 2023 & ro == ro & !ou %in% ht_ous) %>%
        select(program, ic, udn, year,a_code, a_name, d_name, actual, target
               , deviation_percentage, deviation_narrative)
    } else if (program == "FTF Initiative") {
      dat %>%
        filter(year == 2023) %>%
        select(program, ic, udn, year,a_code, a_name, d_name, actual, target
               , deviation_percentage, deviation_narrative)
    } else {
      dat %>%
        filter(year == 2023 & ro == ro & ou == ou_name) %>%
        select(program, ic, udn, year,a_code, a_name, d_name, actual, target
               , deviation_percentage, deviation_narrative)
      }
    ht_text <- filter(ht_text, ic == "EG.3.2-25")

    n_aligned <- length(unique(ht_text$a_code))
    n_contributing <- sum(!is.na(ht_text$actual))
    # Write Text
    ht_txt <- paste0(
      "In FY 2023, ", ou_label
      , " accounted for ", n_activities, " activities that aligned the "
      ,"indicator for hectares under improved management practices (EG.3.2-25). "
      , "Of those activities, ", n_contributing, " reported on the '"
      , unique(ht_text$d_name), "' disaggregate and contributed to the OU total for this PT.")

  } else if (! "PT3: Climate hectares" %in% ou_dat$name) {
    hectares_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = "No available PT3 data")
                , family = "Gill Sans MT", color = "#C0C0C0"
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    ht_text_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = "Not applicable")
                , family = "Gill Sans MT", color = "#C0C0C0"
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    ht_txt <- paste0("USAID ", ou_label
                      ," did not have a PT3 target.")
  }

  # PSI -------------
  if(program == "Group Target") {
    psi_actual_to_outyear <-  dat %>% filter(!ou %in% psi_ous) %>%
      psi_(level = "ou") %>%
      mutate(name = str_remove(name, "_3y")) %>%
      filter(!is.na(value)
             & ((name == "target_3y" & year == out_year) |
                  (name == "actual_3y" & year == 2023)))  %>%
      select(-c(ic, a_codes)) %>%
      mutate(name = "PT4: Private sector investment", type = new_target_name)

    psi_target <- ou_dat %>%
      filter(name == "PT4: Private sector investment (3-yr avg)" &
               (type == "baseline" | type == "performance target")) %>%
      mutate(type = pt_label
             , lower = value*deviation)
    this_year <- psi_actual_to_outyear %>% slice(which(year == 2023)) %>%
      select(year,value)
    # ----------- Prep data
    psi_plot_params <- make_plot_params(this_year = this_year, pt_abline = psi_target, deviation = deviation)
    scores$score[scores$pt == "PT4"] <- as.factor(psi_plot_params["title"])
    ## PLOT -----------
    psi_plot_dat <- bind_rows(psi_actual_to_outyear, psi_target)

    } else if(program == "FTF Initiative") {
    psi_actual_to_outyear <-  psi_(dat, level = "initiative") %>%
      mutate(program ="FTF Initiative", .before=everything()) %>%
      select(-c(ic, a_codes)) %>%
      filter(!is.na(value)
             & ((name == "target" & year == out_year) |
                  (name == "actual" & year == 2023)))  %>%
      rename(type = name) %>%
      mutate(name = "PT4: Private sector investment", type = new_target_name)

    psi_target <- ou_dat %>%
      filter(name == "PT4: Private sector investment" &
               (type == "baseline" | type == "performance target")) %>%
      mutate(type = pt_label
             , lower = value*deviation)
    this_year <- psi_actual_to_outyear %>% slice(which(year == 2023)) %>%
      select(year,value)
    # ----------- Prep data
    psi_plot_params <- make_plot_params(this_year = this_year, pt_abline = psi_target, deviation = deviation)
    scores$score[scores$pt == "PT4"] <- as.factor(psi_plot_params["title"])
    ## PLOT -----------
    psi_plot_dat <- bind_rows(psi_actual_to_outyear, psi_target)
  } else {
    psi_actual_to_outyear <-  psi_(dat) %>%
      filter(ou == ou_name & !is.na(value)
             & ((name == "target_3y" & year == out_year) |
                  (name == "actual_3y" & year == 2023)))  %>%
      rename(type = name) %>% select(-c(ic, a_codes)) %>%
      mutate(name = "PT4: Private sector investment", type = new_target_name)

    psi_target <- ou_dat %>%
      filter(name == "PT4: Private sector investment (3-yr avg)" &
               (type == "baseline" | type == "performance target")) %>%
      mutate(type = pt_label
             , lower = value*deviation)
    this_year <- psi_actual_to_outyear %>% slice(which(year == 2023)) %>%
      select(year,value)
    # ----------- Prep data
    psi_plot_params <- make_plot_params(this_year = this_year
                                        , pt_abline = psi_target, deviation = deviation)
    scores$score[scores$pt == "PT4"] <- as.factor(psi_plot_params["title"])


    ## PLOT -----------
    psi_plot_dat <- bind_rows(psi_actual_to_outyear, psi_target)
  }

  if(nrow(psi_plot_dat) == 0L) {
    psi_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = "No available PT4 data")
                , family = "Gill Sans MT", color = "#C0C0C0"
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    psi_text <- ggplot() +
      geom_text(aes(x=0, y=0, label = psi_plot_params["title"])
                , family = "Gill Sans MT", color = "#C0C0C0"
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    psi_txt <- paste0(ou_label
                      ," did not report on EG.3.1-15 in FY 2023.")
  } else if(nrow(psi_plot_dat) > 0L) {
    psi_plot_dat <- psi_plot_dat %>%
      bind_rows(psi_actual_to_outyear %>%
                  filter(year == 2023) %>%
                  mutate(type = "FY23 Reported Value")) %>%
      mutate(`Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
      rename(`Value Type` = type, "PT4: Private sector investment" = value) %>%
      arrange(`Value Type`)
    psi_labels <- psi_plot_dat %>%
      filter(! (`Value Type` == "FY23 Actual to FY24 Out year target" & year == 2023))
    psi_plot <- psi_plot_dat  %>%
      # ------------ ggplot
      ggplot(aes(x = `Fiscal Year`, y = `PT4: Private sector investment`
                 , color = `Value Type`, linetype = `Value Type`)) +
      geom_line(linewidth = 1, linetype = 2) + geom_point(size = 2.3) +
      # ------------ Set themes
      ylab("") + xlab("") +
      common_theme() + usaid_color_manual() + theme(legend.position="bottom") +
      geom_label_repel(aes(label = paste0(scales::label_dollar()(`PT4: Private sector investment`/1000000),"M"))
                       , psi_labels, show.legend = FALSE, direction = "both") +
      scale_color_manual(values = plot_colors) +
      scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
      scale_x_date(date_breaks="2 years", date_label=paste0("FY", "%y")) +
      # scale_linetype_manual(values = c("FY23 Actual to FY24 Out year target" = 2
      #   , "Trajectory from Baseline* to FY30 Performance Target" = 2, "FY23 Reported value" = NA)) +
      ggtitle(label = "PT4: Private sector investment (3-yr avg)") +
      guides(color=guide_legend(nrow=3, byrow=TRUE), linetype=guide_legend(nrow=2, byrow=TRUE)) +
      labs(caption = "*For this PT, the baseline is the average of FY 2020 to FY 2022.\nAll values are three year averages.")
  ## TEXT ----------------
  psi_text <- if(program == "FTF Initiative") {
    dat %>%
      filter(year == 2023) %>%
      select(program, ic, udn, year,a_code, a_name, d_name, actual, target
             , deviation_percentage, deviation_narrative)
  } else if(program == "Group Target") {
    dat %>%
      filter(year == 2023 & ro == ro & ! ou %in% psi_ous) %>%
      select(program, ic, udn, year,a_code, a_name, d_name, actual, target
             , deviation_percentage, deviation_narrative)

  } else {
    dat %>%
      filter(year == 2023 & ro == ro & ou == ou_name) %>%
      select(program, ic, udn, year,a_code, a_name, d_name, actual, target
             , deviation_percentage, deviation_narrative)
  }
    psi_text <- filter(psi_text, ic %in% c("EG.3.1-14", "EG.3.1-15"))
    psi_n_aligned <- length(psi_text$actual)
    n_contributing <- sum(!is.na(psi_text$actual))
    # Write Text
    activity <- if(psi_n_aligned > 1 | psi_n_aligned == 0L) " activities" else  " activity"
    psi_txt <- paste0("In FY 2023, ", ou_label
                      , " had ", psi_n_aligned, activity
                      , " that aligned the private sector investment indicator (EG.3.1-15/-14), '"
                      , paste(unique(psi_text$d_name), collapse = ", "), "', and "
                      , n_contributing, " contributed to the OU total in FY 2023.")
    psi_text <- ggplot() +
      geom_text(aes(x=0, y=0, label = psi_plot_params["title"])
                , family = "Gill Sans MT", color = psi_plot_params["color"]
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
  }

  # MDD-W PLOT -------------
  if(ou_name != "USAID Mali (MALI)") {
    mddw_dat <- mddw_dat %>%
      filter(ou == ou_name & name == "PT5: MDD-W")
    has_target <-  if(nrow(mddw_dat[mddw_dat$type=="performance target",]) > 0L) {
      any(!is.na(mddw_dat$value[mddw_dat$type == "performance target"]))
    } else FALSE
    has_baseline <- if(nrow(mddw_dat[mddw_dat$type=="baseline",]) > 0L) {
      any(!is.na(mddw_dat$value[mddw_dat$type == "baseline"]))
    } else FALSE

    if(!any(has_target, has_baseline)) { # has no target or baseline
      mddw_plot_params <- list(title = "Not available", color = "#C0C0C0")
      mddw_plot <- ggplot() +
        geom_text(aes(x=0, y=0, label = "No available PT5: MDD-W data")
                  , family = "Gill Sans MT", color = mddw_plot_params["color"]
                  , fontface = "bold", size = 16 /.pt) +
        theme_void()
      mddw_text <- ggplot() +
        geom_text(aes(x=0, y=0, label = mddw_plot_params["title"])
                  , family = "Gill Sans MT", color = "#C0C0C0"
                  , fontface = "bold", size = 16 /.pt) +
        theme_void()
      mddw_txt <- paste0("PT5 is not available for "
                         , ou_label,".")
    } else if(has_baseline & !has_target) {
      mddw_plot_params <- list(title = "TBD after next PBS", color = "#C0C0C0")
      mddw_plot <- mddw_dat  %>% filter(type != "actual") %>%
        mutate(type = str_to_sentence(name)
               , `Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
        rename(`Value Type` = name
               , `PT5: Minimum dietary diversity for women` = value) %>%

        # ------------ ggplot
        ggplot(aes(x = `Fiscal Year`, y = `PT5: Minimum dietary diversity for women`
                   , linetype = `Value Type`, color = `Value Type`)) +
        geom_point(size = 2.3) + geom_line(linewidth = 1) +
        # ------------ Set themes
        ylab("") + xlab("") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        usaid_color_manual() + common_theme() + theme(legend.position="bottom") +
        ggtitle(label = "PT5: MDD-W (% in ZOI)")

      mddw_text <- ggplot() +
        geom_text(aes(x=0, y=0, label = mddw_plot_params["title"])
                  , family = "Gill Sans MT", color = "#C0C0C0"
                  , fontface = "bold", size = 16 /.pt) +
        theme_void()
      mddw_txt <- paste0("The most recent population-based survey (PBS) in "
                         , ou_label," was in "
                         , format(mddw_dat$date[mddw_dat$type == "baseline"], "%B %Y")," and showed that "
                         , mddw_dat$value[mddw_dat$type=="baseline"]
                         , "percent of women in the ZOI had consumed a minimally-diverse diet in the previous day or night."
                         , "The FY30 target is ", target$value," percent."
                         , " The next PBS is scheduled for "
                         ,  if(!is.na(mddw_dat$date[mddw_dat$type == "actual"])) {
                           format(mddw_dat$date[mddw_dat$type == "actual"], "%B %Y")
                         } else {mddw_dat$date[mddw_dat$type == "outyear"]},". ")

    } else if(has_baseline & has_target) {
      # target <- targets %>% filter(operating_unit == ou_name) %>%
      #   select(value = hl_9_1_d, last_pbs, next_pbs) %>%
      #   mutate(value = value *100, year = 2030L)
      pt_abline <- mddw_dat %>%
        filter(type %in% c("baseline", "performance target")) %>% drop_na() %>%
        mutate(type = pt_label
               , lower = value*deviation)
      this_year <- mddw_dat %>% filter(type == "actual") %>%
        drop_na() %>% slice(which.max(year)) %>%
        select(year,name, value)
      mddw_plot_params <- make_plot_params(
        this_year = this_year, pt_abline = pt_abline, deviation = deviation
        , mddw = TRUE)

      mddw_plot_dat <- mddw_dat  %>%
        mutate(type = case_when(
          type == "performance target" |
            type == "baseline" ~ pt_label
          , type == "actual" ~ "Midline")
          ,`Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
        drop_na() %>%
        rename(`Value Type` = type) %>% arrange(year) %>%
        mutate(`Value Type` = ifelse(ou == "USAID Mali (MALI)"
                                     , paste(name, `Value Type`
                                             , sep = " - ")
                                     ,`Value Type`))
      mddw_plot <- mddw_plot_dat  %>%
        # ------------ ggplot
        ggplot(aes(x = `Fiscal Year`, y = value
                   , color = `Value Type`)) +
        geom_line(linewidth = 1, linetype = 2) +
        geom_point(size = ifelse(mddw_plot_dat$`Value Type` == "Midline", 3, 2.3)) +
        # geom_point(data = mddw_dat %>% filter(`Value Type` == "Midline")
        #            , color = "#002F6C"
        #            , size = 3) +
        geom_label_repel(aes(label = scales::percent(value)), show.legend = F
                         , direction = "x", nudge_x = 1) +
        # ------------ Set themes
        ylab("") + xlab("") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        scale_color_manual(values = mddw_fills, drop=TRUE) +
        # scale_color_manual(values = c("#002F6C", "#0067B9", "#0067B9")) +
        common_theme() + theme(legend.position="bottom") +
        guides(color = guide_legend(nrow = 3)) +
        ggtitle(label = "PT5: MDD-W (%)")

      mddw_text <- ggplot() +
        geom_text(aes(x=0, y=0, label = mddw_plot_params["title"])
                  , family = "Gill Sans MT", color =  mddw_plot_params["color"]
                  , fontface = "bold", size = 16 /.pt) +
        theme_void()

      mddw_txt <- paste0("The baseline population-based survey (PBS) for "
                         , ou_label," was in "
                         , mddw_dat$year[mddw_dat$type == "baseline"]," and showed that "
                         , scales::percent_format()(mddw_dat$value[mddw_dat$type == "baseline"])
                         , " of women in the ZOI had consumed a minimally-diverse diet in the previous day or night."
                         , " The FY30 target is ", scales::percent_format()(mddw_dat$value[mddw_dat$type == "performance target"])," percent."
                         , if("actual" %in% mddw_dat$type) {
                           if(!is.na(mddw_dat$date[mddw_dat$type == "actual"]) & is.na(mddw_dat$value[mddw_dat$type == "actual"])) {
                             paste0("The most recent PBS was in ", mddw_dat$year[mddw_dat$type == "actual"],", but results are not yet available.")
                             } else if(!is.na(mddw_dat$date[mddw_dat$type == "actual"]) & !is.na(mddw_dat$value[mddw_dat$type == "actual"])) {
                               paste0("The most recent PBS was in ", mddw_dat$year[mddw_dat$type == "actual"],", and it showed that the updated value is ", mddw_dat$value[mddw_dat$type == "actual"], ".")
                             }
                         }
                         , if("outyear" %in% mddw_dat$type) " The following PBS is scheduled for ", mddw_dat$year[mddw_dat$type == "outyear"],".")
    }

    mddw_text <- mddw_text
    mddw_txt <- mddw_txt
    scores$score[scores$pt == "PT5"] <- as.factor(mddw_plot_params["title"])
  } else if(program == "USAID Mali (MALI)") {
    mddw_datS <- mddw_dat %>%
      filter(ou == ou_name & name == "PT5: MDD-W (South)")

    pt_ablineS <- mddw_datS %>% filter("actual" %in% type, .by=name) %>%
      filter(type %in% c("baseline", "performance target")) %>% drop_na() %>%
      mutate(type = pt_label
             , lower = value*deviation)
    this_yearS <- mddw_datS %>% filter(type == "actual") %>%
      drop_na() %>% slice(which.max(year)) %>%
      select(year,name, value)
    mddw_plot_paramsS <- make_plot_params(
      this_year = this_yearS, pt_abline = pt_ablineS, deviation = deviation
      , mddw = TRUE)

    mddw_datRFZ <- mddw_dat %>%
      filter(ou == ou_name & name == "PT5: MDD-W (RFZ)")
    pt_ablineRFZ <- mddw_datRFZ %>% filter("actual" %in% type, .by=name) %>%
      filter(type %in% c("baseline", "performance target")) %>% drop_na() %>%
      mutate(type = pt_label
             , lower = value*deviation)
    this_yearRFZ <- mddw_datRFZ %>% filter(type == "actual") %>%
      drop_na() %>% slice(which.max(year)) %>%
      select(year,name, value)
    mddw_plot_paramsRFZ <- make_plot_params(
      this_year = this_yearRFZ, pt_abline = pt_ablineRFZ, deviation = deviation
      , mddw = TRUE)

    mddw_plot_dat <- mddw_dat %>%
      filter(ou == ou_name)  %>%
      mutate(type = case_when(
        type == "performance target" |
          type == "baseline" ~ pt_label
        , type == "actual" ~ "Midline")
        ,`Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
      drop_na() %>%
      rename(`Value Type` = type) %>% arrange(year)
    mddw_plot <- mddw_plot_dat  %>%
      # ------------ ggplot
      ggplot(aes(x = `Fiscal Year`, y = value
                 , color = `Value Type`)) +
      geom_line(linewidth = 1, linetype = 2) +
      geom_point(size = ifelse(mddw_plot_dat$`Value Type` == "Midline", 3, 2.3)) +
      # geom_point(data = mddw_dat %>% filter(`Value Type` == "Midline")
      #            , color = "#002F6C"
      #            , size = 3) +
      geom_label_repel(aes(label = scales::percent(value)), show.legend = F) +
      # ------------ Set themes
      ylab("") + xlab("") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      scale_color_manual(values = mddw_fills, drop=TRUE) +
      # scale_color_manual(values = c("#002F6C", "#0067B9", "#0067B9")) +
      common_theme() + theme(legend.position="bottom") +
      facet_wrap(.~name) +
      guides(color = guide_legend(nrow = 3)) +
      ggtitle(label = "PT5: MDD-W (%)")

    mddw_textS <- ggplot() +
      geom_text(aes(x=0, y=0, label = mddw_plot_paramsS["title"])
                , family = "Gill Sans MT", color =  mddw_plot_paramsS["color"]
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    mddw_textRFZ <- ggplot() +
      geom_text(aes(x=0, y=0, label = mddw_plot_paramsRFZ["title"])
                , family = "Gill Sans MT", color =  mddw_plot_paramsRFZ["color"]
                , fontface = "bold", size = 16 /.pt) +
      theme_void()

    mddw_text <- mddw_textRFZ + mddw_textS

    mddw_txt <- "Write manually."
    scores$score[scores$pt == "PT5"] <- as.factor(mddw_plot_paramsS["title"])
  }

  scores$score[is.na(scores$score)] <- "Not applicable"

  # Stars ------
  stars_plot <- scores %>%
    mutate(score = fct_relevel(score, c("On track", "Not on track", "Not applicable", "Not available"))) %>%
    arrange(score) %>% mutate(x = row_number()/1) %>%
    ggplot(aes(x, y, color = score, fill = score)) +
    geom_star(starshape = 1, size = 6) +
    # facet_wrap(.~pt, nrow = 1) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = fills) +
    xlim(c(-1, 6)) +
    xlab("") + ylab("") + theme(plot.margin = unit(c(0,0,0,0), "lines")) +
    theme_void() + theme(legend.position = "none") +
    coord_fixed(ratio = 15)

  score <- scores$score
  on_track <- sum_(score == "On track") > 0L & sum_(score == "On track") >= sum_(! score  %in% c("Not available", "Not applicable" )) -1
  overall_score <- if(on_track == TRUE) {
    "On track"
  } else if(on_track == FALSE) {
    "Not on track"
  }

  overall_score_ratio <- paste0(
    sum_(score == "On track"), "/"
    ,  sum_(!scores$score  %in% c("Not available", "Not applicable" )), " Performance Targets")

  overall_score_text <- ggplot() +
    geom_text(aes(x=0, y=1, label = paste0(overall_score, ",\nmeeting ", overall_score_ratio))
              , family = "Gill Sans MT"
              , color = if(on_track >= .5) "#0067B9" else "#651D32"
              , fontface = "bold"
              , size = 16 /.pt) +
    theme_void()

    return(list("overall_score_text" = overall_score_text
              , "stars_plot" = stars_plot
              , "sales_plot" = sales_plot
              , "sales_text"= sales_text_plot
              , "sales_txt" = sales_txt
              , "gender_finance_plot" = gender_finance_plot
              , "gf_text" = gf_text_plot
              , "gf_txt" = gf_txt
              , "hectares_plot" = hectares_plot
              , "ht_text" = ht_text_plot
              , "ht_txt" = ht_txt
              , "psi_plot" = psi_plot
              , "psi_text" = psi_text
              , "psi_txt" = psi_txt
              , "mddw_plot" = mddw_plot
              , "mddw_text" = mddw_text
              , "mddw_txt" = mddw_txt
              , "scores" = scores)
    )
}


# Budget -------------
make_budget_plot <- function(budget, ou_name) {
  # Budget --------
  ou_budget <- if(length(ou_name) == 1) {
    budget %>% filter(year %in% 2020:2024 & ou == ou_name) %>%
    mutate(budget = ifelse(ou == "Bureau for Resilience and Food Security (RFS)", budget/2,
                           ifelse(budget == 0, NA, budget)))
  } else if (length(ou_name) > 1) {
     budget %>% filter(year %in% 2020:2024 & ! ou %in% ou_target_countries) %>%
        mutate(budget = ifelse(ou == "Bureau for Resilience and Food Security (RFS)", budget/2,
                               ifelse(budget == 0, NA, budget)))
  }

  budget_hline <- ou_budget %>% slice_max(year, n=1) %>%
    select(budget) %>% as.numeric()
  # # ----------- Prep data
  budget_plot <- ou_budget  %>%
    rename(`Fiscal Year` = year, `EG3 Budget (000)` = budget) %>%
    mutate(type = fct_relevel(type, c("Ukraine Supplemental","653(a) Control"))) %>%
    # ------------ ggplot
    ggplot(aes(x = paste0("FY ",`Fiscal Year`), y = `EG3 Budget (000)`, fill = type)) +
    geom_bar(stat = "identity") +
    geom_label(aes(label = scales::dollar_format()(`EG3 Budget (000)`))
               , color = "#FFFFFF", fontface = "bold", position = position_stack(vjust = 0.5)
               , show.legend = FALSE) +
    geom_label_repel(aes(label = paste0("Baseline: ", scales::dollar_format()(budget_hline)))
                     , x = "FY 2023", y = budget_hline
                   , nudge_y = 1, show.legend = FALSE) +
    geom_hline(color = "#BA0C2F", yintercept = budget_hline
               , linewidth = 1, linetype = 2) +

    # ------------ Set themes
    # guides(color = guide_legend(position = "inside")) +
    common_theme() +
    theme(legend.position="bottom", plot.title = element_text(size = 16)) +
    scale_fill_manual(values = budget_fills, breaks = c("653(a) Control", "Ukraine Supplemental", "Request")) +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    scale_x_discrete(expand = expansion(mult = .25)) +
    ylab("") + xlab("") +
    ggtitle(label = "EG.3 Budget Trend ($000s)"
            , subtitle = "Horizontal line is the FY 2024 baseline that was used to make OU-level targets")
  return(budget_plot)
}

make_ftf_budget_plot <- function(budget, ou_name) {
  # Budget --------
  budget_hline <- budget %>% slice_max(year, n=1) %>%
    summarize(value = sum_(value)) %>% as.numeric()
  # # ----------- Prep data
  ftf_budget_plot_df <- budget  %>%
    filter(value > 0) %>%
    rename(`Fiscal Year` = year, `Enacted Budget (000)` = value) %>%
    mutate(ro = fct_reorder(ro, `Enacted Budget (000)`))
  budget_plot <- ftf_budget_plot_df %>%
    # ------------ ggplot
    ggplot(aes(x = as.Date(paste0(`Fiscal Year`, "-01-01"))
               , y = `Enacted Budget (000)`, fill = ro)) +
    geom_bar(stat = "identity") +

    # ------------ Set themes
    common_theme() + usaid_plot() +
    scale_fill_manual(values = rev(
      c("#002F6C",  "#BA0C2F", "#0067B9", "#6C6463", "#651D32", "#A7C6ED"
        , "#8C8985", "#CFCDC9"))) +
    theme(legend.position="bottom", plot.title = element_text(size = 16)) +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    scale_x_date(date_labels = paste0("FY", "%y")) +
    ylab("") + xlab("") + guides(fill = guide_legend(nrow = 2)) +
    labs(caption = "*The FY22 figure includes Ukraine Supplemental.") + theme(plot.caption = element_text(size = 10))+
    ggtitle(label = "Enacted Appropriation* ($000s)")

  return(budget_plot)
}


# National context -------------
national_plot <- function(x, ou_name) {
  # plot_captions <- x %>%
  #   distinct(name, txt)
  n_plot <- x %>%
      ggplot(aes(x = year, y = value, color = name)) +
      geom_line(linetype = 1, size = 1) +
      geom_point(aes(fill = name, col = name), size = 2) +
      geom_text_repel(data = slice_max(plot_df, order_by = year, by = name)
                     , aes(label = scales::label_percent()(value/100))
                     , direction = "x", nudge_x = 1) +
    # geom_text(data=plot_captions, y=0, x=2012,
    #           mapping=aes(label=txt), hjust=0,
    #           fontface="italic") +
      scale_x_date(date_breaks = "3 years", date_labels = "%Y"
                   , expand = expansion(mult = .4)) +
      facet_wrap(~txt,
               labeller = label_wrap_gen(width = 40),
               nrow = 2) +
      ylim(c(0, 100)) +
      usaid_plot() + common_theme() + theme(panel.spacing = unit(2, "lines")) +
      labs(x = "", y = ""
           #, caption = "*Values have been carried over to most recent year."
           )
  return(n_plot)
}

ati_plot <- function(x, ou_name) {
  ou_ati <- x %>%
    filter(country == trimws(gsub("\\(.*$", "",
                                  gsub("USAID ", ""
                                       ,  gsub("Program ", "",  ou_name ) )))) %>%
    mutate(year = as.Date(paste0("01-01-", year), format = "%m-%d-%Y")) %>%
    unique()
  p_ati <- if(nrow (ou_ati) > 0L) {
    ou_ati %>%
      #pivot_longer(cols = c(3:8)) %>%
      ggplot(aes(x = year, y = value), color = "#0067B9") +
      geom_line(alpha = 0.6, linetype = 2, size = 1) +
      geom_point(size = 2, color = "#002F6C") +
      geom_hline(yintercept = 1, color = "#0067B9", linetype = 4 , size = 1) +
      geom_label(label = "The goal for all countries is an index value of 1.", y = 1, x = min(ou_ati$year), color = "#0067B9"
                 , hjust=0) +
      geom_text_repel(data = slice_max(ou_ati, order_by = year, by = country)
                       , aes(label = round(value, 2))
                       , color = "#002F6C", direction = "x", nudge_x = 1) +
    scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = expansion(.2)) +
    ylim(c(0, 1)) +
    usaid_plot() + common_theme() +
    labs(x = "", y = "")

  } else p_ati <- knitr::asis_output(
    paste0("There is no Ag Transformation Index available for ",  trimws(gsub("\\(.*$", "", gsub("USAID", "", ou_name ))), ".")
  )
  return(p_ati)
}




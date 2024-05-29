
library(patchwork)
s1_plot <- function(dat, ou_name, targets, ou_dat, mddw, n_activities, finance_sex_dna
                    , total_financing, finance_sex_mixed, scores = scores
                    , start_year = 2022, out_year = 2024, deviation = .9) {
  # deviation = .9
  # dat <- input_dat
  # years <- 2022:2024
  # start_year <- 2022
  # out_year <- 2024
  rh <- c(.2, 1)
  new_target_name <- "FY23 Actual to FY24 Out year target"
  pt_label <- "Trajectory from Baseline* to FY30 Performance Target"

  # SALES  ------------
  sales_actual_to_outyear <- sales_(dat) %>%
    filter(ou == ou_name
           & !is.na(value)
           & ((name == "target" & year == out_year) |
                (name == "actual" & year == 2023)))  %>%
    rename(type = name) %>% select(-c(ic, a_codes)) %>%
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
      , data = sales_labels, show.legend = F, direction = "x") +
    # guides(color=guide_legend(override.aes=list(fill=NA))) +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    scale_x_date(date_breaks = "2 years"
                 , date_labels = paste0("FY", "%y")) +
    scale_color_manual(values = plot_colors) +
    guides(color = guide_legend(nrow = 3)) +
    labs(caption = "*Baseline is FY20-FY22 average")

  # SALES
  ## TEXT -----------------
  sales_text <- dat %>%
    filter(year == 2023 & ro == "USAID" & ou == ou_name) %>%
    select(ro, ou, ic, udn, year, a_code, a_name, d_name, actual, target
           , deviation_percentage, deviation_narrative) %>%
    filter(ic == "EG.3.2-26")
  sales_n_aligned <- length(unique(sales_text$a_code))
  sales_n_contributing <- length(unique(sales_text$a_code[!is.na(sales_text$actual)]))
  sales_total <- sum_(sales_text$actual)
  # Write Text
  # sales_score <- sales_plot_params["title"]
  # sales_score_color <- sales_plot_params["color"]
  sales_txt <- paste0("In FY 2023, USAID ", trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name ))), " had "
                      , sales_n_aligned," activities that aligned the 'Value of annual sales from producers and firms receiving USG assistance' (EG.3.2-26), and "
                      , sales_n_contributing, " contributed to the sales total of "
                      , scales::dollar_format()(round(sales_total, -4)), ".")
  sales_text <- ggplot() +
    geom_text(aes(x=0, y=0, label = sales_plot_params["title"])
              , family = "Gill Sans MT", color = sales_plot_params["color"]
              , fontface = "bold", size = 16 /.pt) +
    theme_void()
  sales_subtext <- ggplot() +
    geom_textbox(aes(x= 0, y = 0, label = sales_txt)
                 , size = 8 / .pt
                 , width = unit(3, "inches")) +
    theme_void()

  # FINANCING -------------
  gf_actual_to_outyear <- gender_financing_(dat, level = "ou") %>%
    filter(ou == ou_name
           & !is.na(value)
           & ((name == "target" & year == out_year) |
                (name == "actual" & year == 2023)))  %>%
    rename(type = name) %>% select(-c(a_codes)) %>%
    mutate(name = "PT2: Gender financing ratio", type = new_target_name) %>%
    select(ro, ou, year, name, type, value)
  gf_target <- ou_dat %>%
    filter(name == "PT2: Gender financing ratio" & (type == "baseline" | type == "performance target")) %>%
    mutate(type = pt_label
           , lower = value*deviation)
  this_year <- gf_actual_to_outyear %>% slice(which(year == 2023)) %>%
    select(year,value)
  # ----------- Prep data
  gf_plot_params <- make_plot_params(this_year = this_year, pt_abline = gf_target, deviation = deviation)
  # print(sales_plot_params["title"])
  scores$score[scores$pt == "PT2"] <- as.factor(gf_plot_params["title"])

  ## PLOT  ------------
  gf_dat <- gf_actual_to_outyear  %>% bind_rows(gf_target) %>%
    bind_rows(gf_actual_to_outyear %>%
                filter(year == 2023) %>%
                mutate(type = "FY23 Reported Value")) %>%
    mutate(`Fiscal Year` = as.Date(paste0(year, "-01-01"))) %>%
    rename(`Value Type` = type, "PT2: Gender financing ratio" = value)
  gf_labels <- gf_dat %>%
    filter(! (`Value Type` != "FY23 Reported Value" & year == 2023))

  if(nrow(gf_dat) == 1L) {
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
    gender_finance_plot <- gf_dat %>%
      # ------------ ggplot
      ggplot(aes(x = `Fiscal Year`, y = `PT2: Gender financing ratio` , color = `Value Type`
                 , linetype = `Value Type`)) +
      geom_line(linewidth = 1, linetype = 2) + geom_point(size = 2.3) +
      theme(legend.position = "bottom") +
      ylim(c(0, NA)) + ylab("") + xlab("") +
      common_theme() +  usaid_color_manual() +
      geom_label_repel(aes(label = scales::dollar_format()(`PT2: Gender financing ratio`))
                       , data = gf_labels, show.legend = F, direction = "y") +
      scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
      scale_x_date(date_breaks = "2 years"
                   , date_labels = paste0("FY", "%y")) +
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
  gf_text <- gender_financing_(dat, level="im") %>%
    filter(year == 2023 & ro == "USAID" & ou == ou_name & name == "actual") %>%
    select(ro, ou, year, a_code, a_name, name, `Male Value`, `Female Value`
           , `Female Per Person`, `Male Per Person`) %>%
    mutate(finance_gap = `Female Per Person` / `Male Per Person`)

  gf_n_aligned <- length(unique(gf_text$a_code))
  gf_n_contributing <- sum(!is.na(gf_text$finance_gap))
  gf_total_financing <- sum_(gf_text$`Male Value` , gf_text$`Female Value`)

  gf_txt <- paste0("In FY 2023, USAID "
                   , trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name )))
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
                   , " that was not sex disaggregated.")

  # Hectares -------------------
  ht_actual_to_outyear <- hectares_(dat) %>%
    filter(ou == ou_name
           & !is.na(value)
           & ((name == "target" & year == out_year) |
                (name == "actual" & year == 2023)))  %>%
    rename(type = name) %>% select(-c(ic, a_codes)) %>%
    mutate(name = "PT3: Climate hectares", type = new_target_name)

  ht_target <- ou_dat %>%
    filter(name == "PT3: Climate hectares" & (type == "baseline" | type == "performance target")) %>%
    mutate(type = pt_label
           , lower = value*deviation)
  this_year <- ht_actual_to_outyear %>%
    filter(year == 2023) %>%
    select(year,value)
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
    geom_label_repel(aes(label = scales::comma_format()(round(`PT3: Climate hectares`, -2)))
                     , data = ht_labels, show.legend = F) +
    theme(legend.position = "bottom") + xlab("") + ylab("") +
    usaid_color_manual() + common_theme() +
    # scale_color_manual(values = plot_colors) +
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
  ht_text <- dat %>%
    filter(year == 2023 & ro == "USAID" & ou == ou_name) %>%
    select(ro, ou, ic, udn, year,a_code, a_name, d_name, actual, target
           , deviation_percentage, deviation_narrative)
  ht_text <- filter(ht_text, ic == "EG.3.2-25")

  n_aligned <- length(unique(ht_text$a_code))
  n_contributing <- sum(!is.na(ht_text$actual))
  # Write Text
  ht_txt <- paste0(
    "In FY 2023, ", trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name )))
    , " had ", n_activities, " activities that aligned the "
    ,"indicator for hectares under improved management practices (EG.3.2-25). "
    , "Of those activities, ", n_contributing, " reported on the '"
    , unique(ht_text$d_name), "' disaggregate and contributed to the OU total for this PT.")

  # PSI -------------
  psi_actual_to_outyear <- psi_(dat) %>%
    filter(ou == ou_name
           & !is.na(value)
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
  psi_plot_params <- make_plot_params(this_year = this_year, pt_abline = psi_target, deviation = deviation)
  scores$score[scores$pt == "PT4"] <- as.factor(psi_plot_params["title"])

  ## PLOT -----------
  psi_plot_dat <- bind_rows(psi_actual_to_outyear, psi_target)
  if(nrow(psi_plot_dat) == 0L) {
    psi_plot <- ggplot() +
      geom_text(aes(x=0, y=0, label = "No available PT4 data")
                , family = "Gill Sans MT", color = psi_plot_params["color"]
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    psi_text <- ggplot() +
      geom_text(aes(x=0, y=0, label = psi_plot_params["title"])
                , family = "Gill Sans MT", color = "#C0C0C0"
                , fontface = "bold", size = 16 /.pt) +
      theme_void()
    psi_txt <- paste0("USAID ", trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name )))
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
      filter(! (`Value Type` != "PT4: Private sector investment" & year == 2023))
    psi_plot <- psi_plot_dat  %>%
      # ------------ ggplot
      ggplot(aes(x = `Fiscal Year`, y = `PT4: Private sector investment`
                 , color = `Value Type`, linetype = `Value Type`)) +
      geom_line(linewidth = 1, linetype = 2) + geom_point(size = 2.3) +
      # ------------ Set themes
      ylab("") + xlab("") +
      usaid_color_manual() + common_theme() + theme(legend.position="bottom") +
      geom_label_repel(aes(label = paste0(scales::label_dollar()(`PT4: Private sector investment`/1000000),"M"))
                       , psi_labels, show.legend = FALSE) +
      scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
      scale_x_date(date_breaks="2 years", date_label=paste0("FY", "%y")) +
      # scale_linetype_manual(values = c("FY23 Actual to FY24 Out year target" = 2
      #   , "Trajectory from Baseline* to FY30 Performance Target" = 2, "FY23 Reported value" = NA)) +
      ggtitle(label = "PT4: Private sector investment (3-yr avg)") +
      guides(color=guide_legend(nrow=3,byrow=TRUE), linetype=guide_legend(nrow=2, byrow=TRUE)) +
      labs(caption = "*For this PT, the baseline is the average of FY 2020 to FY 2022.\nAll values are three year averages.")

    ## TEXT ----------------
    psi_text <- dat %>%
      filter(year == 2023 & ro == "USAID" & ou == ou_name) %>%
      select(ro, ou, ic, udn, year,a_code, a_name, d_name, actual, target
             , deviation_percentage, deviation_narrative)
    psi_text <- filter(psi_text, ic %in% c("EG.3.1-14", "EG.3.1-15"))
    psi_n_aligned <- length(psi_text$actual)
    n_contributing <- sum(!is.na(psi_text$actual))
    # Write Text
    activity <- if(psi_n_aligned > 1 | psi_n_aligned == 0L) " activities" else  " activity"
    psi_txt <- paste0("In FY 2023, ", trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name )))
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
      mddw_txt <- paste0("PT5 is not available for USAID "
                         , trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name ))),".")
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
                         , trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name )))," was in "
                         , format(mddw_dat$date[mddw_dat$type == "baseline"], "%B %Y")," and showed that "
                         , mddw_dat$`PT5: MDD-W`[mddw_dat$type=="baseline"], "percent of women in the ZOI had access to a minimally-diverse diet."
                         , "The FY30 target is ", target$value," percent."
                         , " The next PBS is scheduled for "
                         ,  if(!is.na(mddw_dat$date[mddw_dat$type == "actual"])) {
                           format(mddw_dat$date[mddw_dat$type == "actual"], "%B %Y")
                         } else {mddw_dat$date[mddw_dat$type == "outyear"]},". ")

    } else if(has_baseline & has_target) {
      # target <- targets %>% filter(operating_unit == ou_name) %>%
      #   select(value = hl_9_1_d, last_pbs, next_pbs) %>%
      #   mutate(value = value *100, year = 2030L)
      pt_abline <- mddw_dat %>% filter("actual" %in% type, .by=name) %>%
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
        geom_label_repel(aes(label = scales::percent(value)), show.legend = F) +
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

      mddw_txt <- paste0("The baseline population-based survey (PBS) for USAID "
                         , trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name )))," was in "
                         , mddw_dat$year[mddw_dat$type == "baseline"]," and showed that "
                         , scales::percent_format()(mddw_dat$value[mddw_dat$type == "baseline"]), " of women in the ZOI had access to a minimally-diverse diet."
                         , " The FY30 target is ", scales::percent_format()(mddw_dat$value[mddw_dat$type == "performance target"])," percent."
                         , if(!is.na(mddw_dat$date[mddw_dat$type == "actual"]) & is.na(mddw_dat$value[mddw_dat$type == "actual"])) {
                           paste0("The most recent PBS was in ", mddw_dat$year[mddw_dat$type == "actual"],", but results are not yet available.")
                         } else if(!is.na(mddw_dat$date[mddw_dat$type == "actual"]) & !is.na(mddw_dat$value[mddw_dat$type == "actual"])) {
                           paste0("The most recent PBS was in ", mddw_dat$year[mddw_dat$type == "actual"],", and it showed that the updated value is ", mddw_dat$value[mddw_dat$type == "actual"], ".")
                         }
                         , " The following PBS is scheduled for ", mddw_dat$year[mddw_dat$name == "outyear"],".")
    }

    mddw_text <- mddw_text
    mddw_txt <- mddw_txt
    scores$score[scores$pt == "PT5"] <- as.factor(mddw_plot_params["title"])
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
  on_track <- sum_(score == "On track") >= sum_(! score  %in% c("Not available", "Not applicable" )) -1
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
              , "sales_text"= sales_text
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
  ou_budget <- budget %>% filter(year %in% 2020:2024 & ou == ou_name) %>%
    mutate(budget = ifelse(ou == "Bureau for Resilience and Food Security (RFS)", budget/2,
                           ifelse(budget == 0, NA, budget)))
  budget_hline <- ou_budget %>% slice_max(year, n=1) %>%
    select(budget) %>% as.numeric()
  # # ----------- Prep data
  budget_plot <- ou_budget  %>%
    rename(`Fiscal Year` = year, `EG3 Budget (000)` = budget) %>%
    mutate(type = fct_relevel(type, "653(a) Control", after = 2)) %>%
    # ------------ ggplot
    ggplot(aes(x = paste0("FY ",`Fiscal Year`), y = `EG3 Budget (000)`, fill = type)) +
    geom_bar(stat = "identity") + #geom_line(linewidth = 1) +
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
    scale_fill_manual(values = budget_fills) +
    scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA)) +
    scale_x_discrete(expand = expansion(mult = .25)) +
    ylab("") + xlab("") +
    ggtitle(label = "EG.3 Budget Trend ($000s)"
            , subtitle = "Horizontal line is the FY 2024 baseline that was used to make OU-level targets")
  return(budget_plot)
}


# National context -------------
national_plot <- function(x, ou_name) {
  n_plot <- x %>%
    ggplot(aes(x = year, y = value, color = name)) +
    geom_line(linetype = 1, size = 1) +
    geom_point(aes(fill = name, col = name), size = 2) +
    geom_label_repel(data = slice_max(plot_df, order_by = year, by = name)
                     , aes(label = scales::label_percent()(value/100))) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y"
    ) +
    facet_wrap(~name,
               # labeller = label_wrap_gen(width = 25),
               nrow = 2) +
    ylim(c(0, 100)) +
    usaid_plot() + common_theme() +
    labs(x = "", y = ""
         #, caption = "*Values have been carried over to most recent year."
    )
  return(n_plot)
}

ati_plot <- function(x, ou_name) {
  ou_ati <- x %>%
    filter(country == trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name )))) %>%
    mutate(year = as.Date(paste0("01-01-", year), format = "%m-%d-%Y")) %>%
    unique()
  p_ati <- if(nrow (ou_ati) > 0L) {
    ou_ati %>%
      #pivot_longer(cols = c(3:8)) %>%
      ggplot(aes(x = year, y = value), color = "#0067B9") +
      geom_line(alpha = 0.6, linetype = 2, size = 1) +
      geom_point(size = 2, color = "#002F6C") +
      scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = expansion(.2)) +
      ylim(c(0, 1)) +
      usaid_plot() + common_theme() +
      labs(x = "", y = "", caption = "Higher is better, and the maximum value is 1.")

  } else p_ati <- paste0("No Ag Transformation Index available for ", trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou_name ))), ".")
  return(p_ati)
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





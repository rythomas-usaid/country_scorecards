

library(widyr)
library(ggraph)
library(tidyverse)
library(tidytext)
library(ggtext)
library(here)

reload_disR <- function() {
  unloadNamespace("disR")
  library(disR)
}

make_abline <- function(dat = dat, target = target, pt_label = pt_label
                        , tyname = "actual", deviation = .9, psi = FALSE){
  if(psi == FALSE) {
    pt_abline <- sales_dat %>%
    filter(type == tyname) %>% drop_na(value) %>%
    select(year, name, value) %>%
    bind_rows(target) %>%
    mutate(name = pt_label
           , lower = value*deviation)
  } else if (psi==TRUE) {
    pt_abline <- dat %>%
      filter(type == tyname) %>%
      bind_rows(target) %>% drop_na() %>%
      slice(which.min(year), which.max(year)) %>%
      arrange(year) %>%
      complete(year = 2023:2029,
               fill = list(value = NA)) %>%
      arrange(year) %>%
      mutate(name = pt_label,
             value = purrr::accumulate(value, ~ . *1.075))
  }
  return(pt_abline)
}

make_plot_params <- function(this_year, pt_abline = NA, deviation = .9, required = TRUE, mddw = FALSE, gf = FALSE) {
    if(nrow(pt_abline) >= 2L & nrow(filter(this_year, !is.na(value))) > 0L) {
      on_track <- approxfun(x = pt_abline$year, y = pt_abline$value)
      trajectory <- approx(x = pt_abline$year, y = pt_abline$value, n=9)
      # upper <- on_track(this_year$year) + d
      lower <- if(gf == FALSE) on_track(this_year$year)*deviation else min(on_track(this_year$year)*deviation, 0.9)
      title <- if(this_year$value < lower){
        "Not on track"
        } else if(this_year$value >= lower) {
          "On track"
          }
      text_color <- if(this_year$value < lower){
        "#BA0C2F"
        } else if(this_year$value >= lower ) {
          "#00B050"
        }
    }  else if(nrow(pt_abline) >= 2L & nrow(filter(this_year, is.na(value))) == 0L & mddw == FALSE) {
      title <-  "Not on track"
      text_color <- "#BA0C2F"
      trajectory <- approx(x = pt_abline$year, y = pt_abline$value, n=9)
    } else if(is.data.frame(pt_abline)) {
      if(nrow(pt_abline) == 1L) {
        text_color <- "#CFCDC9"
        title <- "Not available"
        trajectory <- list(trajectory.x = NA, trajectory.y = NA)
      } else if(2023 >= min(pt_abline$year) & 2030 %in% pt_abline$year) {
        text_color <- "#CFCDC9"
        title <- "Not available"
        trajectory <- list(trajectory.x = NA, trajectory.y = NA)
      } else if(2023 >= min(pt_abline$year) & 2030 %in% pt_abline$year) {
        text_color <- "#CFCDC9"
        title <- "Not available"
        trajectory <- list(trajectory.x = NA, trajectory.y = NA)
      } else if(! 2023 >= min(pt_abline$year) & ! 2030 %in% pt_abline$year) {
        text_color <- "#CFCDC9"
        title <- "Not available"
        trajectory <- list(trajectory.x = NA, trajectory.y = NA)
      } else if(nrow(filter(this_year, !is.na(value))) == 0L) {
      text_color <- "#CFCDC9"
      title <- "Not available"
      trajectory <- list(trajectory.x = NA, trajectory.y = NA)
      }
      params <- c("title" = title, "color" = text_color, "trajectory" = trajectory)
    }
  # subtitle <- paste0(
  #   "FY30 Target is to significantly increase from "
  #   , scales::dollar_format()(this_year$value)
  #   , " (FY23 value)")

  params <- c("title" = title, "color" = text_color, trajectory = trajectory)
  return(params)
  }

make_plot_subtitle <- function(x) {
  subtitle <- x %>% select(ro, ou, name, year, value) %>%
    filter(year == 2023) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(deviation_pct = round( 1 + (actual - target) / target , 2 )
           , on_ou_track = case_when(
             deviation_pct < 0.9 ~ "Did not meet their own target"
             , between(deviation_pct , 0.9, 1) ~ "Met their Own Target"
             , deviation_pct > 1 ~ "Exceeded their own target")) %>%
    select(on_ou_track) %>% as.character()
  return(subtitle)
}


## Things from usaid_plot
is_windows <- tolower(Sys.info()[["sysname"]]) == "windows"
font_family <- ifelse(is_windows == TRUE, "Gill Sans MT",
                      "Gill Sans")

common_theme <- function(){
  theme(axis.ticks = ggplot2::element_blank()
        , axis.line = ggplot2::element_blank()
        , axis.text=element_text(size=11)
        , axis.title=element_text(size=12)
        , plot.margin = unit(c(0,0,0,0), "lines")
        # , panel.grid.minor = ggplot2::element_line(color = "#CFCDC9")
        , panel.grid.major.y = ggplot2::element_line(color = "#CFCDC9")
        , panel.grid.major.x = ggplot2::element_line(color = "#CFCDC9")
        # , panel.background = ggplot2::element_blank()
        # , panel.background = element_rect(fill='transparent') #transparent panel bg
        , strip.background = element_blank()
        , plot.background = element_rect(fill='transparent', color=NA, linewidth = NA) #transparent plot bg
        , legend.background = element_rect(fill='transparent', color=NA, linewidth = NA) #transparent legend bg
        , legend.title = element_blank()
        , legend.text = element_text(size = 11)
        # , legend.box.background = element_rect(fill='transparent') #transparent legend panel
        , legend.margin=margin(0,0,0,0)
        #, plot.title.position = "plot"
        # , plot.title = element_text(size = 14, family = font_family, color = "black"
        #                             , hjust = 0.5)
        , plot.title = element_blank()
        , plot.subtitle = element_text(size = 12, family = font_family, color = "black"
                                       , hjust = 0)
        , plot.caption = element_text(size = 12, family = font_family, color = "black"
                                      , hjust = 0)
        , text = element_text(size = 12, family = font_family, color = "black"))
        }

usaid_color_manual <- function() {
  ggplot2::scale_color_manual(values = rep(c( "#A7C6ED","#0067B9", "#002F6C",  "#651D32","#BA0C2F"
                    , "#6C6463", "#8C8985"), 500))
}
usaid_fill_manual <- function() {
  ggplot2::scale_fill_manual(values = rep(c("#BA0C2F", "#002F6C", "#A7C6ED", "#0067B9", "#651D32"
                    , "#6C6463", "#8C8985"), 500))
}
colors <- c("On track" = "#0067B9", "Not on track" = "#0067B9", "Not available" = "#CFCDC9", "Not applicable" = "#CFCDC9")
fills <- c("On track" = "#0067B9", "Not on track" = "#FFFFFF", "Not available" = "#CFCDC9", "Not applicable" = "#CFCDC9")
budget_fills <- c("653(a) Control" = "#002F6C", "Ukraine Supplemental" = "#0067B9"
                  , "Request" = "#A7C6ED"
                  , "Enacted Appropriation" = "#0067B9")
mddw_fills <- c("Trajectory from Baseline* to FY30 Performance Target" = "#0067B9"
                , "Midline" = "#002F6C"
                , "Out year" = "#A7C6ED")
plot_colors <- c("Trajectory from Baseline* to FY30 Performance Target" = "#0067B9"
                , "FY23 Reported Value" = "#002F6C"
                , "FY23 Actual to FY24 Out year target" = "#A7C6ED")


### gender_financing_udns ###
financing_udns <- c("3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1"   # value for males
                    , "3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2"  # value for females
                    , "3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1"  # number of males
                    , "3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2") # number of females
# pt_udns <- bind_rows(tibble(ic = "EG.3.2-25", udn = c("3.1.3.12", "3.2.3.12"))
#           , tibble(ic = "EG.3.2-26" , udn = "3")
#           , tibble(ic = "EG.3.2-27", udn = financing_udns)
#           , tibble(ic = "EG.3.1-15" , udn = "3")
#           , tibble(ic = "HL.9.1-d" , udn = "3")
# )



set.seed(123)
theme_set(theme_minimal())

make_bigram <- function(x, seed = 11241985) {
  set.seed(seed) # New York City
  narratives_pair <- x %>%
    unnest_tokens(output = word, input = `sub narrative text`, token = "ngrams", n = 2) %>%
    separate(col = word, into = c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% get_stopwords(source = "smart")$word,
           !word2 %in% get_stopwords(source = "smart")$word) %>%
    drop_na(word1, word2) %>%
    count(word1, word2, sort = TRUE)

  bigram_graph <- narratives_pair %>%
    filter(n > 25) %>%
    igraph::graph_from_data_frame()


  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = FALSE, alpha = .5) +
    geom_node_point(color = "#0052A5", size = 3, alpha = .5) +
    geom_node_text(aes(label = name), vjust = 1.5, alpha = .5) +
    ggtitle("Word Network in FY22-23 Activity Narratives (n > 25)") +
    theme_void()
}


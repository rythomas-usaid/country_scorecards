

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

make_abline <- function(pt_data = pt_data, target = target){
  pt_data %>%
    filter(name == "actual") %>% drop_na() %>%
    slice(which.min(year)) %>%
    # filter(year == year) %>%
    select(year, value) %>%
    bind_rows(target) %>%
    mutate(name = "Priority Target") %>%
    mutate(lower = value - (value*.1), upper = value + (value*.1))
}

make_plot_params <- function(this_year, abline = NA, deviation = .1, psi = NA) {
  if(is.na(psi)){
  on_track <- approxfun(x = abline$year, y = abline$value)
  d <- on_track(this_year$year)*deviation
  # upper <- on_track(this_year$year) + d
  lower <- on_track(this_year$year) - d

  title <- if(this_year$value < lower){
    "Falling behind"
    } else if(this_year$value >= lower) {
      "On track"
    }

  text_color <- if(this_year$value < lower){
    "#651D32"
    } else if(this_year$value >= lower ) {
      "#0067B9"
      }
  } else if (psi == TRUE) {
    lower <- this_year$three_yr_avg[this_year$year == 2022 & this_year$name == "actual"]
    fy23_three_year <- this_year$three_yr_avg[this_year$year == 2023 & this_year$name == "actual"]
    title <- if(fy23_three_year < lower){
      "Falling behind"
    } else if(fy23_three_year >= lower) {
      "On track"
    }

    # subtitle <- paste0(
    #   "FY30 Target is to significantly increase from "
    #   , scales::dollar_format()(this_year$value)
    #   , " (FY23 value)")

    text_color <- if(fy23_three_year < lower){
      "#651D32"
    } else if(fy23_three_year >= lower) {
      "#0067B9"
    }
  }
  params <- c("title" = title, "color" = text_color)
  return(params)
}

make_plot_subtitle <- function(x) {
  subtitle <- x %>% select(ro, ou, name, year, value) %>%
    filter(year == 2023) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(deviation = round( 1 + (actual - target) / target , 2 )
           , on_ou_track = case_when(
             deviation < 0.9 ~ "Did not meet their own target"
             , between(deviation , 0.9, 1) ~ "Met their Own Target"
             , deviation > 1 ~ "Exceeded their own target")) %>%
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
        , plot.margin = unit(c(0,0,0,0), "lines")
        # , panel.grid.minor = ggplot2::element_line(color = "#CFCDC9")
        , panel.grid.major.y = ggplot2::element_line(color = "#CFCDC9")
        , panel.grid.major.x = ggplot2::element_line(color = "#CFCDC9")
        # , panel.background = ggplot2::element_blank()
        # , panel.background = element_rect(fill='transparent') #transparent panel bg
        , plot.background = element_rect(fill='transparent', color=NA, linewidth = NA) #transparent plot bg
        , legend.background = element_rect(fill='transparent', color=NA, linewidth = NA) #transparent legend bg
        , legend.title = element_blank()
        # , legend.box.background = element_rect(fill='transparent') #transparent legend panel
        , plot.title.position = "plot"
        , plot.title = element_text(size = 14, family = font_family, color = "black")
        , plot.subtitle = element_text(size = 12, family = font_family, color = "black")
        , plot.caption = element_text(size = 12, family = font_family, color = "black")
        , text = element_text(size = 12, family = font_family, color = "black"))
        }

usaid_color_manual <- function() {
  ggplot2::scale_color_manual(values = rep(c( "#002F6C", "#A7C6ED", "#0067B9", "#651D32","#BA0C2F"
                    , "#6C6463", "#8C8985"), 500))
}
usaid_fill_manual <- function() {
  ggplot2::scale_fill_manual(values = rep(c("#BA0C2F", "#002F6C", "#A7C6ED", "#0067B9", "#651D32"
                    , "#6C6463", "#8C8985"), 500))
}
colors <- c("On track" = "#0067B9", "Falling behind" = "#0067B9", "Not available" = "#CFCDC9")
fills <- c("On track" = "#0067B9", "Falling behind" = "#FFFFFF", "Not available" = "#CFCDC9")

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


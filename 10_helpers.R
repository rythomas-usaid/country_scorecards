

reload_disR <- function() {
  unloadNamespace("disR")
  library(disR)
}


make_plot_title <- function(this_year_value, lower, upper) {
  title <- if(between(this_year_value, lower, upper)) {
    "On Track for Centrally set Target"
  } else if(this_year_value < lower){
    "Falling behind Centrally set Target"
  } else if(this_year_value > upper) {
    "Exceeding Centrally set Target"
  }
  return(title)
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
        , panel.grid.minor = ggplot2::element_line(color = "#CFCDC9")
        , panel.grid.major.y = ggplot2::element_line(color = "#CFCDC9")
        , panel.grid.major.x = ggplot2::element_line(color = "#CFCDC9")
        , panel.background = ggplot2::element_blank()
        , strip.background = ggplot2::element_rect(fill = "white")
        # , plot.margin = margin(6,0,6,0)
        , plot.title.position = "plot"
        , plot.title = element_text(size = 12, family = font_family, color = "black")
        , plot.subtitle = element_text(size = 10, family = font_family, color = "black")
        , plot.caption = element_text(size = 10, family = font_family, color = "black")
        , text = element_text(size = 9, family = font_family, color = "black"))
}

usaid_colors <- function() {
  ggplot2::scale_color_manual(
    values = rep(c( "#002F6C", "#A7C6ED", "#0067B9", "#651D32","#BA0C2F"
                    , "#6C6463", "#8C8985"), 500))
}

### gender_financing_udns ###
financing_udns <- c("3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1"   # value for males
                    , "3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2"  # value for females
                    , "3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1"  # number of males
                    , "3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2") # number of females
pt_udns <- bind_rows(tibble(ic = "EG.3.2-25", udn = c("3.1.3.12", "3.2.3.12"))
          , tibble(ic = "EG.3.2-26" , udn = "3")
          , tibble(ic = "EG.3.2-27", udn = financing_udns)
          , tibble(ic = "EG.3.1-15" , udn = "3")
          , tibble(ic = "HL.9.1-d" , udn = "3")
)





extract %>%
  select(ro, ou, a_name, a_code, ic, udn
               , year, d_name, target, actual) %>%
  filter((year >=2022 & ic == "EG.3.1-15" & udn == "3") |
           (year < 2022  & ic == "EG.3.1-14" & udn == "3.1.2")) %>%
  mutate(ic = "EG.3.1-15/-14") %>%
  # RAW ABOVE. perhaps split out later
  # pivot_longer(c(target, actual)) %>%
  group_by(ro, ou, ic, year) %>%
  summarise(target = sum_(target)
            , actual = sum_(actual)
            , a_codes = paste0(unique(a_code), collapse="; ")
            , .groups = "drop") %>%
  # psi_() %>%
  # filter(name == "actual") %>% #select(-c(a_codes, name)) %>%
  group_by(ro, ou, ic) %>%
  complete(year = first(year):year(Sys.Date())) %>%
  group_by(ro, ou) %>%
  mutate(lag.2 = lag(actual, n=2, order_by = year),
         lag.1 = lag(actual, n=1, order_by = year)) %>%
  rowwise() %>% mutate(three_yr_avg = mean(c(actual, lag.1, lag.2), na.rm=T), .after = actual) %>%
  select(-starts_with("lag")) %>%
  pivot_longer(c(target, actual, three_yr_avg))

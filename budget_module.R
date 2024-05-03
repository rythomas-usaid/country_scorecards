


b <- googlesheets4::read_sheet("1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
                                    , sheet = "Data") %>%
  rename_with(tolower)

budget <- b %>% rename(ou_upper = `operating unit`) %>%
  rename_extract_columns() %>%
  group_by(ou, award_number, program_area, year) %>%
  summarise(transaction_amt = sum_(transaction_amt))
  # pivot_wider( names_from = fiscal_year, values_from = transaction_amt) %>%
googlesheets4::sheet_write( budget, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
                             , sheet = "Transactions by award and year")

extract <- extract %>%
  mutate(ou = trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou ))))

# sales
sales23 <- extract %>%
  select(award_number, ro, ou, a_name, a_code, ic, udn, year, d_name, target, actual) %>%
  filter(ic == "EG.3.2-26" & udn == "3" ) %>%
  # pivot_longer(c(target, actual))
  group_by(ro, ou, a_name, a_code, award_number, ic, year) %>%
    summarise(sales = sum_(actual)
              , a_codes = paste0(unique(a_code), collapse="; ")
              , .groups = "drop") %>%
  filter(year == 2023)
googlesheets4::sheet_write(sales23, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
                           , sheet = "PT1: Sales")


# financing
gf23 <- extract %>%
  select(award_number, ro, ou, a_name, a_code, ic, udn, year, d_name, actual) %>%
  filter(ic == "EG.3.2-27" & udn %in% financing_udns) %>%
  mutate(d_name = case_when(
    udn %in% c("3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2") ~ "Female Value"
    , udn %in% c("3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2") ~ "Female Number"
    , udn %in% c("3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1") ~ "Male Value"
    , udn %in% c("3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1") ~ "Male Number")) %>%
  # pivot_longer(c(target, actual))  %>%
  select(-c(udn,ic)) %>%
  pivot_wider(names_from = c(d_name)
              , values_from = actual
              , values_fn = sum_) %>%
  mutate(`Male Value` = if_else(is.na(`Male Number`), NA, `Male Value`)
         , `Female Value` = if_else(is.na(`Female Number`), NA, `Female Value`)
         , `Male Number` = if_else(is.na(`Male Value`), NA, `Male Number`)
         , `Female Number` = if_else(is.na(`Female Value`), NA, `Female Number`))  %>%
  mutate(`Female Per Person` = `Female Value` / `Female Number`
         , `Male Per Person` = `Male Value` / `Male Number`
         , gender_financing_ratio = (`Female Per Person` / `Male Per Person`)
         , .by = c(award_number, ro, ou, a_name, a_code, year)) %>%
  ungroup() %>% rowwise() %>%
  mutate(across(7:13, ~ if_else(!is.finite(.), true = NA, false = ., missing = .))) %>%
  filter(year == 2023)
googlesheets4::sheet_write(gf23, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
                           , sheet = "PT2: gender financing")

# hectares
hectares23 <- extract %>%
  select(award_number, ro, ou, a_name, a_code, ic, udn
         , year, d_name, target, actual) %>%
  filter(ic == "EG.3.2-25" & udn %in% c("3.1.3.12", "3.2.3.12")) %>%
  # HECTARES Raw is above.
  # pivot_longer(c(target, actual)) %>%
  group_by(award_number, ro, ou, a_name, a_code, ic, year) %>%
  summarise(hectares = sum_(actual)
            , a_codes = paste0(unique(a_code), collapse="; ")
            , .groups = "drop")%>%
  filter(year == 2023)
googlesheets4::sheet_write(hectares23, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
                           , sheet = "PT3: Hectares")
# psi
psi23 <- extract %>%
  select(award_number, ro, ou, a_name, a_code, ic, udn
         , year, d_name, target, actual) %>%
  filter((year >=2022 & ic == "EG.3.1-15" & udn == "3") |
           (year < 2022  & ic == "EG.3.1-14" & udn == "3.1.2")) %>%
  mutate(ic = "EG.3.1-15/-14") %>%
  # RAW ABOVE. perhaps split out later
  # pivot_longer(c(target, actual)) %>%
  group_by(award_number, ro, ou, a_name, a_code, ic, year) %>%
  summarise(psi = sum_(actual)
            , a_codes = paste0(unique(a_code), collapse="; ")
            , .groups = "drop") %>%
  # psi_() %>%
  # filter(name == "actual") %>% #select(-c(a_codes, name)) %>%
  group_by(ro, ou) %>%
  mutate(lag.2 = lag(psi, n=2, order_by = year),
         lag.1 = lag(psi, n=1, order_by = year)) %>%
  rowwise() %>% mutate(three_yr_avg = mean(c(psi, lag.1, lag.2), na.rm=T), .after = psi) %>%
  select(-starts_with("lag"))%>%
  filter(year == 2023)
googlesheets4::sheet_write(psi23, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
                           , sheet = "PT4: PSI")


by <- join_by(award_number == award_number
              , ou == ou
              , ro == ro
              , a_name == a_name
              , a_code == a_code
              , year == year)

budget_pt_results <- budget  %>%
  pivot_wider(values_from = transaction_amt, names_from = year) %>%
  mutate(ou = tolower(ou)) %>%
  left_join(sales23 %>% mutate(ou = tolower(ou)) %>% select(-c(ic, a_codes))
            , join_by("award_number" == "award_number", "ou" == "ou")
            , suffix = c(".budget", ".sales")
            , relationship = "many-to-many") %>%
  left_join(gf23 %>% mutate(ou = tolower(ou))
            , by
            , suffix = c(".budget", ".gf")
            , relationship = "many-to-many") %>%
  left_join(hectares23 %>% mutate(ou = tolower(ou)) %>% select(-c(ic, a_codes))
            , by
            , suffix = c(".budget", ".hectares")
            , relationship = "many-to-many") %>%
  left_join(psi23 %>% mutate(ou = tolower(ou)) %>% select(-c(ic, a_codes))
            , by
            , suffix = c(".budget", ".psi")
            , relationship = "many-to-many")

googlesheets4::sheet_write(budget_pt_results
                           , "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
                           , sheet = "Budget and PT Results")


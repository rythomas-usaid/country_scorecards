


d <- googlesheets4::read_sheet("1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
       , sheet = "Data") %>% rename_with(tolower)

disbursements <- d %>%
  # rename(ou_join = ou) %>%
  mutate(ou_join = tolower(ou)) %>% select(-`operating unit`) %>%
  rename_extract_columns() %>%
  filter(program_area == "EG.3" & year == 2023) %>%
  mutate(award_number = case_when(is.na(award_number) ~ document_number
                                  , .default = award_number)
         , is_document_number = case_when(award_number == document_number ~ TRUE
                                        , .default = FALSE)
         , ro = "USAID"
         # , in_phoenix = TRUE
         ) %>%
  group_by(ro, ou, ou_join, award_number, is_document_number) %>%
  summarise(transaction_amt = sum_(transaction_amt), .groups="drop")

activities_join <- active_activities_dat %>%
  mutate(ou_join = tolower(trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou ))))) %>%
  filter(year == 2023, ro == "USAID") %>% group_by(ro, ou, ou_join, a_code, a_name, award_number) %>%
  summarize(PT1 = ifelse("EG.3.2-26" %in% ic, "X", "-")
            , PT2 = ifelse("EG.3.2-27" %in% ic, "X", "-")
            , PT3 = ifelse("EG.3.2-25" %in% ic, "X", "-")
            , PT4 = ifelse("EG.3.1-15" %in% ic, "X", "-"))
# activities_join %>%
#   googlesheets4::sheet_write(ss = "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
#                              , sheet = "Active activities with top level indicator value")

by <- join_by(ro == ro, ou_join == ou_join, award_number == award_number)
disbursements_activities_join <- disbursements %>%
  left_join(activities_join
            , by, suffix = c("_pho", "_dis"))
  #
# googlesheets4::sheet_write(disbursements_activities_join, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
#                              , sheet = "Sum of Transactions joined to Active Activities")

# sales
sales23 <- extract %>%
  select(award_number, ro, ou, a_name, a_code, ic, udn, year, d_name, target, actual) %>%
  filter(ic == "EG.3.2-26" & udn == "3" ) %>%
  # pivot_longer(c(target, actual))
  group_by(ro, ou, a_name, a_code, award_number, year) %>%
    summarise(sales = sum_(actual)
              # , a_codes = paste0(unique(a_code), collapse="; ")
              , .groups = "drop") %>%
  filter(year == 2023) %>% arrange(ro, ou)
# googlesheets4::sheet_write(sales23, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
#                           , sheet = "PT1: Sales")


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
  filter(year == 2023) %>% arrange(ro, ou)
# googlesheets4::sheet_write(gf23, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
#                            , sheet = "PT2: gender financing")

# hectares
hectares23 <- extract %>%
  select(award_number, ro, ou, a_name, a_code, ic, udn
         , year, d_name, target, actual) %>%
  filter(ic == "EG.3.2-25" & udn %in% c("3.1.3.12", "3.2.3.12")) %>%
  # HECTARES Raw is above.
  # pivot_longer(c(target, actual)) %>%
  group_by(award_number, ro, ou, a_name, a_code, year) %>%
  summarise(hectares = sum_(actual)
            # , a_codes = paste0(unique(a_code), collapse="; ")
            , .groups = "drop")%>%
  filter(year == 2023) %>% arrange(ro, ou)
# googlesheets4::sheet_write(hectares23, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
#                            , sheet = "PT3: Hectares")
# psi
psi23 <- extract %>%
  select(award_number, ro, ou, a_name, a_code, ic, udn
         , year, d_name, target, actual) %>%
  filter((year >=2022 & ic == "EG.3.1-15" & udn == "3") |
           (year < 2022  & ic == "EG.3.1-14" & udn == "3.1.2")) %>%
  mutate(ic = "EG.3.1-15/-14") %>%
  # RAW ABOVE. perhaps split out later
  # pivot_longer(c(target, actual)) %>%
  group_by(award_number, ro, ou, a_name, a_code, year) %>%
  summarise(psi = sum_(actual)
            # , a_codes = paste0(unique(a_code), collapse="; ")
            , .groups = "drop") %>%
  # psi_() %>%
  # filter(name == "actual") %>% #select(-c(a_codes, name)) %>%
  group_by(ro, ou) %>%
  mutate(lag.2 = lag(psi, n=2, order_by = year),
         lag.1 = lag(psi, n=1, order_by = year)) %>%
  rowwise() %>% mutate(three_yr_avg = mean(c(psi, lag.1, lag.2), na.rm=T), .after = psi) %>%
  select(-starts_with("lag"))%>%
  filter(year == 2023) %>% arrange(ro, ou)
# googlesheets4::sheet_write(psi23, "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
#                            , sheet = "PT4: PSI")

## Begin joins -------------
all_results <- sales23 %>% full_join(gf23) %>% full_join(hectares23) %>%
  full_join(psi23) %>%
  mutate(ou_join = tolower(trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou )))))
# googlesheets4::sheet_write(all_results
#                            , "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
#                            , sheet = "FY2023 PT results by IM/Activity")
by <- join_by(ro == ro, ou_join == ou_join, award_number == award_number)

disbursements_pt_results_lj <- disbursements  %>%
  # pivot_wider(values_from = transaction_amt, names_from = year, values_fn = sum_) %>%
  left_join(all_results, by, suffix = c("_pho", "_dis"))
# googlesheets4::sheet_write(disbursements_pt_results_lj
#                            , "1YSIFJQ3mOfFsKUJsMKaITl86KlhI9GvWx5fbsJDATRI"
#                            , sheet = "Sum of Transactions joined to FY23 PT Results")

top5_disbursements <- disbursements_pt_results_lj %>%
  slice_max(order_by = transaction_amt, by = c(ro,ou_pho), n=5, with_ties = TRUE)

top5_results <- all_results %>%
  slice_max(order_by = tibble(sales, sum(`Male Value`, `Female Value`), hectares, three_yr_avg)
            , n = 5, by = c(ro, ou), with_ties = TRUE) %>%
  mutate(ou_join = tolower(trimws(gsub("\\(.*$", "", gsub("USAID ", "",  ou )))), .before = "a_name") %>%
  left_join(disbursements, by, suffix = c("_dis", "_pho")) %>%
  mutate(award_number = ifelse(is.na(award_number), "!! Not reported !!", award_number))


googlesheets4::sheet_write(top5_disbursements
                           , "13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c"
                           , sheet = "top5_disbursements")
googlesheets4::sheet_write(top5_results
                           , "13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c"
                           , sheet = "top5_results")

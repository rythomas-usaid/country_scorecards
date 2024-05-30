library(ggplot2)
library(ggstar)
library(tidyverse)
library(zoo)
library(readxl)
library(extrafont)
library(extrafontdb)
library(DBI)
library(RSQLite)
library(disR)
library(googlesheets4)


# Load data ------------
# EG.3 budget
# budget <- read_sheet("1xlHsKEhzznJI0ZBH5_9hHhdeXPpamSRD1wc0wtilRIk", sheet = "budget")
# write_csv(budget, "01_data/budget.csv")
budget <- read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c"
                     , sheet = "Final Budget", na = "-") %>%
  pivot_longer(-ou, names_to = "year") %>%
  rename(budget = value) %>%
  separate(year , into = c("type", "year"), sep = " - ", fill = "left") %>%
  mutate(year = as.numeric(year)
         , type = ifelse(ou == "International Food Assistance Division (IFA) (USDA/IFA)", "Enacted Appropriation"
                         , ifelse(is.na(type), "653(a) Control", type))
         , budget = as.numeric(budget)) %>%
  drop_na()

top5_disbursements <- googlesheets4::read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c"
                           , sheet = "top5_disbursements")
top5_results <- googlesheets4::read_sheet("13UfrGUnaDJbCO-Nz6tP0T1ORrJcVAeELGZK7WLKTJ6c"
                           , sheet = "top5_results")

ftf_budget  <- read_excel("data/FY 2023 GFSS Implementation Report Budget Table .xlsx"
                          , sheet = "Table for Input", na = c("footnote", "N/A", "NA"), skip = 4) %>%
  pivot_longer(-c(ro, program), names_to = "year")

ati <- read_excel("data/IFPRI ATI Database (02-26-2024).xlsx", skip = 6) %>%
  pivot_longer(-c(country, iso, income_group), names_to = "year")

targets <- read_excel("data/target_setting_data.xlsx"
                      , sheet = "targets_long"
                      , col_types = c("text", "text", "numeric", "text", "numeric"))

kin <- read_excel("data/kin_section4_narratives.xlsx")

map_files <- read_csv("data/map_files.csv")
sales_ous <- unique(targets$ou[targets$name == "PT1: Sales"])
gf_ous <- unique(targets$ou[targets$name == "PT2: Gender financing ratio"])
ht_ous <- unique(targets$ou[targets$name == "PT3: Climate hectares"])
psi_ous <- unique(targets$ou[targets$name == "PT4: Private sector investment (3-yr avg)"])


ftf_programs <- data.frame(
  stringsAsFactors = FALSE,
  ro = c("USAID","IFAD","USDA",
         "Peace Corps","IFAD","IAF","IFAD","IAF","IFAD",
         "Peace Corps","IFAD","USAID","USAID","IFAD","GAFSP","IAF",
         "Peace Corps","IFAD","IAF","Peace Corps","IFAD",
         "USADF","Peace Corps","IFAD","IFAD","IAF","IFAD",
         "Peace Corps","IFAD","IAF","USAID","USAID","USAID","USAID",
         "IFAD","USADF","GAFSP","USAID","IFAD","GAFSP",
         "Peace Corps","USAID","IFAD","GAFSP","USADF","IFAD",
         "IFAD","USAID","GAFSP","Peace Corps","IFAD",
         "Peace Corps","USAID","IFAD","IFAD","IFAD","USAID","IFAD",
         "Peace Corps","IAF","IFAD","Peace Corps","IAF",
         "Peace Corps","IFAD","USADF","IFAD","USAID","IFAD","USADF",
         "IFAD","USAID","IFAD","Peace Corps","IAF","USAID",
         "IAF","IFAD","Peace Corps","IAF","USAID","IFAD",
         "IAF","IFAD","Peace Corps","USAID","IFAD","IFAD",
         "IFAD","Peace Corps","USAID","IFAD","GAFSP","Peace Corps",
         "Peace Corps","IFAD","Peace Corps","IFAD","USAID",
         "IFAD","Peace Corps","USAID","IFAD","Peace Corps",
         "USADF","IFAD","IAF","Peace Corps","USAID","IFAD",
         "USAID","IFAD","USADF","Peace Corps","IFAD","IFAD",
         "Peace Corps","IAF","USAID","IFAD","GAFSP","USAID",
         "IFAD","IAF","IFAD","USAID","USAID","IFAD",
         "Peace Corps","USAID","Peace Corps","IAF","IFAD","Peace Corps",
         "USAID","IFAD","USADF","IFAD","Peace Corps",
         "Peace Corps","USAID","IFAD","GAFSP","USAID","IFAD",
         "USAID","IFAD","Peace Corps","IFAD","GAFSP","USAID",
         "IFAD","Peace Corps","USADF","Peace Corps","USAID","IFAD",
         "GAFSP","USAID","IFAD","Peace Corps","USADF",
         "IFAD","USAID","IFAD","USADF","GAFSP","USAID","IFAD",
         "USADF","IFAD","Peace Corps","IAF","Peace Corps",
         "IFAD","USAID","IFAD","USAID","Peace Corps","Peace Corps",
         "IFAD","USAID","IFAD","MCC","Peace Corps","USAID",
         "IFAD","Peace Corps","Peace Corps","Peace Corps",
         "GAFSP","USAID","IFAD","IAF","IFAD","MCC","GAFSP",
         "USAID","IFAD","USADF","USAID","IFAD","USADF",
         "Peace Corps","USAID","IFAD","Peace Corps","IAF","IFAD",
         "Peace Corps","IFAD","IAF","IAF","Peace Corps","IFAD",
         "IFAD","Peace Corps","USAID","IFAD","GAFSP",
         "Peace Corps","USAID","IFAD","USADF","USAID","USDA","IAF",
         "Peace Corps","IFAD","IFAD","USAID","Peace Corps",
         "GAFSP","IFAD","USADF","Peace Corps","GAFSP","USAID",
         "IFAD","IFAD","USADF","USAID","Peace Corps","USADF",
         "USAID","Peace Corps","USAID","IFAD","USAID","IFAD",
         "USAID","IFAD","GAFSP","USAID","IFAD","GAFSP",
         "Peace Corps","USADF","USAID","IFAD","Peace Corps",
         "Peace Corps","IFAD","GAFSP","USAID","Peace Corps",
         "USAID","Peace Corps","IFAD","Peace Corps","IFAD","IAF",
         "IFAD","IFAD","USADF","USAID","Peace Corps","GAFSP",
         "IFAD","Peace Corps","USAID","IFAD","USAID","USAID",
         "USDA","USDA","USDA","IFAD","USAID","Peace Corps",
         "USAID","USAID","Peace Corps","IFAD","USAID","IFAD",
         "USAID","Peace Corps","USADF","GAFSP","USAID",
         "IFAD","USADF","USAID","IFAD"),
  ou = c("Afghanistan (OIG/AFG)",
         "Afghanistan (IFAD/Afghanistan)",
         "Agricultural Economic Development (AED) (USDA/AED)","Albania Post (PC/Albania)",
         "Angola (IFAD/Angola)",
         "Antigua & Barbuda (IAF/AntiguaBarbuda)","Argentina (IFAD/Argentina)",
         "Argentina (IAF/Argentina)","Armenia (IFAD/Armenia)",
         "Armenia Post (PC/Armenia)","Azerbaijan (IFAD/Azerbaijan)",
         "USAID Azerbaijan (AZERBAIJAN)","USAID Bangladesh (BANGLADESH)",
         "Bangladesh (IFAD/Bangladesh)","Bangladesh (GAFSP/Bangladesh)",
         "Barbados (IAF/Barbados)",
         "Eastern Caribbean Post (PC/Eastern Caribbean)","Belize (IFAD/Belize)",
         "Belize (IAF/Belize)","Belize Post (PC/Belize)","Benin (IFAD/Benin)",
         "Benin Country Program (USADF/Benin)",
         "Benin Post (PC/Benin)","Bhutan (IFAD/Bhutan)",
         "Bolivia (Plurinational State of) (IFAD/Bolivia)","Bolivia (IAF/Bolivia)",
         "Bosnia and Herzegovina (IFAD/Bosnia-Herzegovina)",
         "Botswana Post (PC/Botswana)","Brazil (IFAD/Brazil)",
         "Brazil (IAF/Brazil)",
         "Bureau for Development, Democracy, and Innovation (DDI)",
         "Bureau for Latin America and the Caribbean (LAC)","Bureau for Resilience and Food Security (RFS)",
         "Burkina Faso (BURKINA)","Burkina Faso (IFAD/Burkina Faso)",
         "Burkina Faso Country Program (USADF/Burkina)",
         "Burkina Faso (GAFSP/BurkinaFaso)","USAID Burma (BURMA)",
         "Myanmar (IFAD/Myanmar)","Burma (GAFSP/Burma)",
         "Burma Post (PC/Burma)","Burundi Program (BURUNDI)",
         "Burundi (IFAD/Burundi)","Burundi (GAFSP/Burundi)",
         "Burundi Country Program (USADF/Burundi)","Cabo Verde (IFAD/Cabo Verde)",
         "Cambodia (IFAD/Cambodia)","USAID Cambodia (CAMBODIA)",
         "Cambodia (GAFSP/Cambodia)","Cambodia Post (PC/Cambodia)",
         "Cameroon (IFAD/Cameroon)",
         "Cameroon Post (PC/Cameroon)","Caribbean Development Program (CAR_DVLPMT)",
         "Central African Republic (IFAD/CAR)","Chad (IFAD/Chad)",
         "China (IFAD/China)","USAID Colombia (COLOMBIA)",
         "Colombia (IFAD/Colombia)","Colombia Post (PC/Colombia)",
         "Colombia (IAF/Colombia)","Comoros (IFAD/Comoros)",
         "Comoros Post (PC/Comoros)","Costa Rica (IAF/CostaRica)",
         "Costa Rica Post (PC/Costa Rica)",
         "CÃƒÆ’Ã‚Â´te d'Ivoire (IFAD/Cote dIvoire)",
         "Cote D'Ivoire Country Program (USADF/Cote DIvoire)","Cuba (IFAD/Cuba)","USAID Dem Rep Congo (DROC)",
         "Democratic Republic of the Congo (IFAD/DRO-Congo)",
         "Democratic Republic of the Congo Country Program (USADF/Congo)","Djibouti (IFAD/Djibouti)",
         "USAID Dominican Republic (DOM REP)",
         "Dominican Republic (IFAD/DOMINICAN REPUBLIC)","Dominican Republic Post (PC/Dominican Republic)",
         "Dominican Republic (IAF/DominicanRepublic)",
         "East Africa (EAST AFRICA)",
         "Eastern Caribbean [group] (IAF/EasternCaribbean)","Ecuador (IFAD/Ecuador)",
         "Ecuador Post (PC/Ecuador)","Ecuador (IAF/Ecuador)","USAID Egypt (EGYPT)",
         "Egypt (IFAD/Egypt)","El Salvador (IAF/ElSalvador)",
         "El Salvador (IFAD/ElSalvador)",
         "El Salvador Post (PC/El Salvador)","USAID El Salvador (EL SALVADOR)",
         "Eritrea (IFAD/Eritrea)","Eswatini (IFAD/Eswatini)",
         "Eswatini (IFAD/Eswatini)","Eswatini Post (PC/Eswatini)",
         "USAID Ethiopia (ETHIOPIA)","Ethiopia (IFAD/Ethiopia)",
         "Ethiopia (GAFSP/Ethiopia)","Ethiopia Post (PC/Ethiopia)",
         "Micronesia Post (PC/Micronesia)","Fiji (IFAD/Fiji)",
         "Fiji Post (PC/Fiji)","Gabon (IFAD/GABON)",
         "Georgia Program (GEORGIA)","Georgia (IFAD/Georgia)",
         "Georgia Post (PC/Georgia)","USAID Ghana (GHANA)","Ghana (IFAD/Ghana)",
         "Ghana Post (PC/Ghana)","Ghana Country Program (USADF/Ghana)",
         "Grenada (IFAD/Grenada)","Guatemala (IAF/Guatemala)",
         "Guatemala Post (PC/Guatemala)",
         "USAID Guatemala (GUATEMALA)","Guatemala (IFAD/Guatemala)","USAID Guinea (GUINEA)",
         "Guinea (IFAD/Guinea)",
         "Guinea Country Program (USADF/Guinea)","Guinea Post (PC/Guinea)",
         "Guinea-Bissau (IFAD/Guinea-Bissau)","Guyana (IFAD/Guyana)",
         "Guyana Post (PC/Guyana)","Haiti (IAF/Haiti)","USAID Haiti (HAITI)",
         "Haiti (IFAD/Haiti)","Haiti (GAFSP/Haiti)",
         "USAID Honduras (HONDURAS)","Honduras (IFAD/Honduras)",
         "Honduras (IAF/Honduras)","India (IFAD/India)","USAID India (INDIA)",
         "USAID Indonesia (INDONESIA)",
         "Indonesia (IFAD/Indonesia)","Indonesia Post (PC/Indonesia)",
         "USAID Iraq (IRAQ)","Jamaica Post (PC/Jamaica)","Jamaica (IAF/Jamaica)",
         "Jordan (IFAD/Jordan)","Kenya Post (PC/Kenya)",
         "USAID Kenya (KENYA)","Kenya (IFAD/Kenya)",
         "Kenya Country Program (USADF/Kenya)","Kiribati (IFAD/Kiribati)",
         "Kosovo Post (PC/Kosovo)",
         "Kyrgyz Republic Post (PC/Kyrgyz Republic)","USAID Kyrgyz Republic (KYRGYZ REP)",
         "Kyrgyzstan (IFAD/Kyrgyzstan)","Kyrgyz Republic (GAFSP/Kyrgyz)",
         "Laos (LAOS)","Lao People's Democratic Republic (IFAD/Laos)",
         "USAID Lebanon (LEBANON)","Lebanon (IFAD/Lebanon)",
         "Lesotho Post (PC/Lesotho)","Lesotho (IFAD/Lesotho)",
         "Liberia (GAFSP/Liberia)","USAID Liberia (LIBERIA)",
         "Liberia (IFAD/Liberia)","Liberia Post (PC/Liberia)",
         "Liberia Country Program (USADF/Liberia)",
         "Madagascar Post (PC/Madagascar)","USAID Madagascar (MADAGASCAR)",
         "Madagascar (IFAD/Madagascar)","Malawi (GAFSP/Malawi)",
         "USAID Malawi (MALAWI)","Malawi (IFAD/Malawi)",
         "Malawi Post (PC/Malawi)","Malawi Country Program (USADF/Malawi)",
         "Maldives (IFAD/Maldives)","USAID Mali (MALI)","Mali (IFAD/Mali)",
         "Mali Country Program (USADF/Mali)",
         "Mali (GAFSP/Mali)","Mauritania Program (MAURITANIA)",
         "Mauritania (IFAD/Mauritania)",
         "Mauritania Country Program (USADF/Mauritania)","Mexico (IFAD/Mexico)","Mexico Post (PC/Mexico)",
         "Mexico (IAF/Mexico)","Moldova Post (PC/Moldova)",
         "Republic of Moldova (IFAD/Moldova)","USAID Moldova (MOLDOVA)",
         "Mongolia (IFAD/Mongolia)","Mongolia (MONGOLIA)",
         "Mongolia Post (PC/Mongolia)",
         "Montenegro Post (PC/Montenegro)","Montenegro (IFAD/Montenegro)",
         "USAID Morocco (MOROCCO)","Morocco (IFAD/Morocco)",
         "Morocco Compact (MCC/Morocco)","Morocco Post (PC/Morocco)",
         "USAID Mozambique (MOZAMBIQUE)","Mozambique (IFAD/Mozambique)",
         "Mozambique Post (PC/Mozambique)","Namibia Post (PC/Namibia)",
         "Nepal Post (PC/Nepal)","Nepal (GAFSP/Nepal)",
         "USAID Nepal (NEPAL)","Nepal (IFAD/Nepal)",
         "Nicaragua (IAF/Nicaragua)","Nicaragua (IFAD/Nicaragua)",
         "Niger Compact (MCC/Niger)","Niger (GAFSP/Niger)","USAID Niger (NIGER)",
         "Niger (IFAD/Niger)","Niger Country Program (USADF/Niger)",
         "USAID Nigeria (NIGERIA)","Nigeria (IFAD/Nigeria)",
         "Nigeria Country Program (USADF/Nigeria)",
         "North Macedonia Post (PC/North Macedonia)","USAID Pakistan (PAKISTAN)",
         "Pakistan (IFAD/Pakistan)","Panama Post (PC/Panama)",
         "Panama (IAF/Panama)",
         "Papua New Guinea (IFAD/Papua New Guinea)","Paraguay Post (PC/Paraguay)",
         "Paraguay (IFAD/Paraguay)","Paraguay (IAF/Paraguay)","Peru (IAF/Peru)",
         "Peru Post (PC/Peru)","Peru (IFAD/Peru)",
         "Philippines (IFAD/Philippines)","Philippines Post (PC/Philippines)",
         "Regional Center for South Africa (S_AFR_REG)",
         "Congo (IFAD/Congo)","Rwanda (GAFSP/Rwanda)",
         "Rwanda Post (PC/Rwanda)","USAID Rwanda (RWANDA)","Rwanda (IFAD/Rwanda)",
         "Rwanda Country Program (USADF/Rwanda)",
         "Sahel Regional Program (SAHEL)",
         "International Food Assistance Division (IFA) (USDA/IFA)",
         "Saint Vincent & the Grenadines (IAF/SaintVincentGrenadines)","Samoa Post (PC/Samoa)",
         "Samoa (IFAD/samoa)",
         "Sao Tome and Principe (IFAD/Sao Tome and Principe)","USAID Senegal (SENEGAL)",
         "Senegal Post (PC/Senegal)","Senegal (GAFSP/Senegal)",
         "Senegal (IFAD/Senegal)","Senegal Country Program (USADF/Senegal)",
         "Sierra Leone Post (PC/Sierra Leone)",
         "Sierra Leone (GAFSP/Sierra Leone)","Sierra Leone (SIERRA LEONE)",
         "Sierra Leone (IFAD/Sierra Leone)",
         "Solomon Islands (IFAD/Solomon Islands)","Somalia Country Program (USADF/Somalia)",
         "USAID Somalia (SOMALIA)","South Africa Post (PC/South Africa)",
         "South Sudan Country Program (USADF/South Sudan)",
         "USAID South Sudan (SOUTH SUDAN)",
         "Sri Lanka Post (PC/Sri Lanka)","USAID Sri Lanka (SRI LANKA)",
         "Sri Lanka (IFAD/Sri Lanka)","USAID Sudan (SUDAN)","Sudan (IFAD/Sudan)",
         "USAID Syria (SYRIA)","Syrian Arab Republic (IFAD/Syria)",
         "Tajikistan (GAFSP/Tajikistan)",
         "USAID Tajikistan (TAJIKISTAN)","Tajikistan (IFAD/Tajikistan)",
         "Tanzania (GAFSP/Tanzania)","Tanzania Post (PC/Tanzania)",
         "Tanzania Country Program (USADF/Tanzania)",
         "USAID Tanzania (TANZANIA)","United Republic of Tanzania (IFAD/Tanzania)",
         "Thailand Post (PC/Thailand)",
         "The Gambia Post (PC/The Gambia)","Gambia (The) (IFAD/Gambia)",
         "The Gambia (GAFSP/Gambia)","USAID Ghana (GHANA)",
         "Timor-Leste Post (PC/Timor-Leste)","USAID Timor Leste (TIMOR LESTE)",
         "Togo Post (PC/Togo)","Togo (IFAD/Togo)","Tonga Post (PC/Tonga)",
         "Tonga (IFAD/Tonga)",
         "Trinidad & Tobago (IAF/TrinidadTobago)","Tunisia (IFAD/Tunisia)","Turkey (IFAD/Turkey)",
         "Uganda Country Program (USADF/Uganda)",
         "USAID Uganda (UGANDA)","Uganda Post (PC/Uganda)","Uganda (GAFSP/Uganda)",
         "Uganda (IFAD/Uganda)","Ukraine Post (PC/Ukraine)",
         "USAID Ukraine (UKRAINE)","Uruguay (IFAD/Uruguay)",
         "USAID Central America Regional (G-CAP_REG)",
         "USAID Regional Development Mission/Asia (RDMA)",
         "Fellowship Programs (FP) (USDA/FP)",
         "International Food Assistance Division (IFA) (USDA/IFA)",
         "Trade and Regulatory Capacity Building (TRCB) (USDA/TRCB)","Uzbekistan (IFAD/Uzbekistan)",
         "USAID Uzbekistan (UZBEKISTAN)","Vanuatu Post (PC/Vanuatu)",
         "Venezuela (VENEZUELA)","USAID Vietnam (VIETNAM)",
         "Vietnam Post (PC/Vietnam)","Vietnam (IFAD/Vietnam)",
         "West Africa Regional Program (WARP)",
         "Palestine (IFAD/Palestine)","USAID Yemen (YEMEN)","Zambia Post (PC/Zambia)",
         "Zambia Country Program (USADF/Zambia)",
         "Zambia (GAFSP/Zambia)","USAID Zambia (ZAMBIA)","Zambia (IFAD/Zambia)",
         "Zimbabwe Country Program (USADF/Zimbabwe)",
         "USAID Zimbabwe (ZIMBABWE)","Zimbabwe (IFAD/Zimbabwe)"),
  program = c("Afghanistan","Afghanistan",
              "Agricultural Economic Development (AED) (USDA/AED)",
              "Albania","Angola","Antigua and Barbuda","Argentina",
              "Argentina","Armenia","Armenia","Azerbaijan","Azerbaijan",
              "Bangladesh","Bangladesh","Bangladesh","Barbados",
              "Barbados","Belize","Belize","Belize","Benin","Benin",
              "Benin","Bhutan","Bolivia","Bolivia",
              "Bosnia and Herzegovina","Botswana","Brazil","Brazil",
              "Bureau for Development, Democracy, and Innovation (DDI)",
              "Bureau for Latin America and the Caribbean (LAC)",
              "Bureau for Resilience and Food Security (RFS)","Burkina Faso",
              "Burkina Faso","Burkina Faso","Burkina Faso","Burma","Burma",
              "Burma","Burma","Burundi","Burundi","Burundi",
              "Burundi","Cabo Verde","Cambodia","Cambodia","Cambodia",
              "Cambodia","Cameroon","Cameroon",
              "Caribbean Development Program (CAR_DVLPMT)","Central African Republic",
              "Chad","China","Colombia","Colombia","Colombia",
              "Colombia","Comoros","Comoros","Costa Rica","Costa Rica",
              "Cote dIvoire","Cote dIvoire","Cuba",
              "Democratic Republic of the Congo","Democratic Republic of the Congo",
              "Democratic Republic of the Congo","Djibouti",
              "Dominican Republic","Dominican Republic","Dominican Republic",
              "Dominican Republic","East Africa (EAST AFRICA)",
              "Eastern Caribbean [group] (IAF/EasternCaribbean)","Ecuador",
              "Ecuador","Ecuador","Egypt","Egypt","El Salvador",
              "El Salvador","El Salvador","El Salvador","Eritrea","Eritrea",
              "Eswatini","Eswatini","Ethiopia","Ethiopia",
              "Ethiopia","Ethiopia","Federated States of Micronesia","Fiji",
              "Fiji","Gabon","Georgia","Georgia","Georgia",
              "Ghana","Ghana","Ghana","Ghana","Grenada","Guatemala",
              "Guatemala","Guatemala","Guatemala","Guinea","Guinea",
              "Guinea","Guinea","Guinea-Bissau","Guyana","Guyana",
              "Haiti","Haiti","Haiti","Haiti","Honduras",
              "Honduras","Honduras","India","India","Indonesia","Indonesia",
              "Indonesia","Iraq","Jamaica","Jamaica","Jordan",
              "Kenya","Kenya","Kenya","Kenya","Kiribati","Kosovo",
              "Kyrgyzstan","Kyrgyzstan","Kyrgyzstan","Kyrgyzstan",
              "Laos","Laos","Lebanon","Lebanon","Lesotho","Lesotho",
              "Liberia","Liberia","Liberia","Liberia","Liberia",
              "Madagascar","Madagascar","Madagascar","Malawi",
              "Malawi","Malawi","Malawi","Malawi","Maldives","Mali",
              "Mali","Mali","Mali","Mauritania","Mauritania",
              "Mauritania","Mexico","Mexico","Mexico","Moldova","Moldova",
              "Moldova","Mongolia","Mongolia","Mongolia",
              "Montenegro","Montenegro","Morocco","Morocco","Morocco",
              "Morocco","Mozambique","Mozambique","Mozambique","Namibia",
              "Nepal","Nepal","Nepal","Nepal","Nicaragua",
              "Nicaragua","Niger","Niger","Niger","Niger","Niger",
              "Nigeria","Nigeria","Nigeria","North Macedonia","Pakistan",
              "Pakistan","Panama","Panama","Papua New Guinea",
              "Paraguay","Paraguay","Paraguay","Peru","Peru","Peru",
              "Philippines","Philippines",
              "Regional Center for South Africa (S_AFR_REG)","Republic of the Congo","Rwanda",
              "Rwanda","Rwanda","Rwanda","Rwanda",
              "Sahel Regional Program (SAHEL)","Sahel Regional Program (SAHEL)",
              "Saint Vincent and the Grenadines","Samoa","Samoa",
              "Sao Tome and Principe","Senegal","Senegal","Senegal","Senegal",
              "Senegal","Sierra Leone","Sierra Leone",
              "Sierra Leone","Sierra Leone","Solomon Islands","Somalia",
              "Somalia","South Africa","South Sudan","South Sudan",
              "Sri Lanka","Sri Lanka","Sri Lanka","Sudan","Sudan","Syria",
              "Syria","Tajikistan","Tajikistan","Tajikistan",
              "Tanzania","Tanzania","Tanzania","Tanzania","Tanzania",
              "Thailand","The Gambia","The Gambia","The Gambia",
              "The Gambia","Timor-Leste","Timor-Leste","Togo","Togo",
              "Tonga","Tonga","Trinidad and Tobago","Tunisia",
              "Turkey","Uganda","Uganda","Uganda","Uganda","Uganda",
              "Ukraine","Ukraine","Uruguay",
              "USAID Central America Regional (G-CAP_REG)",
              "USAID Regional Development Mission/Asia (RDMA)","USDA","USDA","USDA","Uzbekistan",
              "Uzbekistan","Vanuatu","Venezuela","Vietnam","Vietnam",
              "Vietnam","West Africa Regional Program (WARP)","West Bank",
              "Yemen","Zambia","Zambia","Zambia","Zambia",
              "Zambia","Zimbabwe","Zimbabwe","Zimbabwe")
)


ou_target_countries <-c("FTF Initiative",
                        "Afghanistan (OIG/AFG)",
                        "USAID Bangladesh (BANGLADESH)",
                        "Bureau for Resilience and Food Security (RFS)",
                        "Georgia Program (GEORGIA)",
                        "USAID Burma (BURMA)",
                        "USAID Cambodia (CAMBODIA)",
                        "USAID Colombia (COLOMBIA)",
                        "USAID Dem Rep Congo (DROC)",
                        "USAID Ethiopia (ETHIOPIA)",
                        "USAID Egypt (EGYPT)",
                        "USAID Ghana (GHANA)",
                        "Group Target",
                        "USAID Guatemala (GUATEMALA)",
                        "USAID Haiti (HAITI)",
                        "USAID Honduras (HONDURAS)",
                        "International Food Assistance Division (IFA) (USDA/IFA)",
                        "USAID Kenya (KENYA)",
                        "USAID Liberia (LIBERIA)",
                        "USAID Madagascar (MADAGASCAR)",
                        "USAID Malawi (MALAWI)",
                        "USAID Mali (MALI)",
                        # "USAID Mali (MALI) S",
                        "USAID Mozambique (MOZAMBIQUE)",
                        "USAID Nepal (NEPAL)",
                        "USAID Niger (NIGER)",
                        "USAID Nigeria (NIGERIA)",
                        "USAID Pakistan (PAKISTAN)",
                        "East Africa (EAST AFRICA)",
                        "USAID Rwanda (RWANDA)",
                        "Sahel Regional Program (SAHEL)",
                        "USAID Senegal (SENEGAL)",
                        "USAID South Sudan (SOUTH SUDAN)",
                        "Regional Center for South Africa (S_AFR_REG)",
                        "Sri Lanka",
                        "USAID Tajikistan (TAJIKISTAN)",
                        "USAID Tanzania (TANZANIA)",
                        "USAID Uganda (UGANDA)",
                        "West Africa Regional Program (WARP)",
                        "USAID Zambia (ZAMBIA)",
                        "USAID Zimbabwe (ZIMBABWE)")

con <- DBI::dbConnect(RSQLite::SQLite(), "../../data/2024-05-30/dis_extract.db")
# dbWriteTable(con, "pt_udns", pt_udns, overwrite = TRUE)
# con <- DBI::dbDisconnect(con)

# copy_to(con, data)
extract <- tbl(con, "extract") %>%
  filter(str_detect(a_name, stringr::fixed("_HLI_"), negate = TRUE) # Is not HLI
         & str_detect(ou, stringr::fixed("test"), negate = TRUE) # not a test bilateral OU DIS Training Activity - To Be Deleted
         & str_detect(a_name, stringr::fixed("test"), negate = TRUE) # not a test activity name
         & str_detect(a_name, stringr::fixed("DIS Training Activity - To Be Deleted"), negate = TRUE) # not the Training Activity from Ghana
         &( # Is an FTF indicator or activity
           indicator_origin == "FTF" | is_ftf == "Y")) %>%
  janitor::clean_names() %>% as_tibble()

pt_udns <- tbl(con, "pt_udns") %>% as_tibble()
# dbWriteTable(con, "pt_udns", as.data.frame(pt_udns))

## Define active activities -----------
# Active activities are defined on page 3 in this document:
# https://docs.google.com/document/d/1sE11RQUUf8Je3LyoWVq2c2Rvlkq-AFwEk34XGPSXBa4/edit
# Have any FY-2023 reporting on an FTF indicator (meaning, FY-2023 actuals,
# targets, or deviation narratives for an FTF indicator)
# —OR—
# Have any FY-24 Targets for any FTF indicator

## These criteria are excluded -->
## —OR—
## Have an activity ‘initiative association’ of FTF, have an “Ongoing”
## activity status, and have dates that seem applicable, i.e. end dates after October 1, 2022
## or if they don’t have an end date in the system, have start dates after October 1, 2017
## <--
active_activities_dat <- extract %>%
  filter(str_detect(a_name, stringr::fixed("_HLI_"), negate = TRUE) # Is not HLI
         & str_detect(ou, stringr::fixed("test"), negate = TRUE) # not a test bilateral OU DIS Training Activity - To Be Deleted
         & str_detect(a_name, stringr::fixed("test"), negate = TRUE) # not a test activity name
         & str_detect(a_name, stringr::fixed("DIS Training Activity - To Be Deleted"), negate = TRUE) # not the Training Activity from Ghana
         &( # Is an FTF indicator or activity
           indicator_origin == "FTF" | is_ftf == "Y"
         )
         &(# Reported actuals, targets, or deviation narratives in FY23
           (
             year == 2023  & if_any(c(actual, target, deviation_narrative), ~ !is.na(.))
           )|( # OR reported FY24 targets
             year == 2024 & !is.na(target)
           )
         )
  ) %>% as_tibble()
active_activities_unique <- active_activities_dat %>%  group_by(ro, ou, a_code, a_name) %>%
  summarize(unique_indicator_count = length((unique(ic)))
            , indicators = paste0(unique(ic), collapse = "; ")
            , .groups = "drop")
# active_usaid_activities_unique %>%
#   googlesheets4::sheet_write("1qFVbBDLM_8F5tFqMFK_eRsRWWOs0TJy3VXn9WI7vlDI", sheet = "Active FTF Activities")


input_dat <- inner_join(extract, pt_udns) %>%
  as_tibble() %>%
  filter(a_code != "00002339") %>%
  mutate(actual = ifelse(ic == "EG.3.2-26" & year == 2023 & udn == "3" & a_code == "00001612"
                         , 119106690
                  , ifelse(ic == "EG.3.2-26" & year == 2023 & udn == "3" & a_code == "00004553"
                          , 395800000, actual)))
# DBI::dbDisconnect(con)

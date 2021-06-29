setwd("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking")
source(file.path(getwd(), '04_Code', 'setup.R'))



# -------------------------------------------------------------------------



# Load Data ---------------------------------------------------------------

# a) Webdata
df_web <- read_delim(file = file.path(getwd(), '02_Data', '01_Webdata', 'df_web_agg.txt'), delim = '\t', 
                     col_types = cols(crefo = col_character()))

df_web_sample <- read_dta("K:\\!Projekte\\BMWI-2020-Corona-KantarMUPWEB_133244\\Daten\\Web\\corona_on_web.dta")
df_web_sample <- df_web_sample %>% mutate(crefo = as.character(crefo))

# b) Survey data
df_surv <- read_delim(file = file.path(getwd(), '02_Data', '02_Survey', 'df_survey_agg.txt'), delim = '\t', 
                      col_types = cols(crefo = col_character()))

# c) Rating data
df_rate <- read_delim(file = file.path(getwd(), '02_Data', '03_Rating', 'df_rate.txt'), delim = '\t', 
                      col_types = cols(crefo = col_character()))

load("Q:/Meine Bibliotheken/Research/01_Promotion/05_Ideas/07_COVID_Tracking/02_Data/03_Rating/df_panel_2018_2021_full.RData")


# Get Firm Characteristics ------------------------------------------------

# Get all crefos
crefos <- base::union(base::union(base::union(df_web$crefo, df_web_sample$crefo), df_surv$crefo), df_rate$crefo)
crefos[1:10]
length(crefos)

# Add basic data
df_firm <- read_dta("K:\\MUP\\Paneldaten\\Stammdaten\\Aufbereitet\\Stammdaten_w58.dta",
                    encoding = 'UTF-8')

# Select only relevant crefos
df_firm <- df_firm %>%
  mutate(crefo = as.character(crefo)) %>% 
  filter(crefo %in% crefos)

# Sector aggregation
df_firm <- df_firm %>% 
  select(crefo, refo, kreis_l, wzdig5) %>% 
  mutate(refo = to_label(refo)) %>% 
  mutate(wz2 = wz2_converter(wzdig5)) %>% 
  mutate(wz_fct = case_when(
    wz2 %in% c(58:63, 68, 69:82) ~ "Business-related services",
    wz2 %in% c(5:9, 12:19, 23:25, 27, 31:33, 35:39, 41:43) ~ "Manufacturing",
    wz2 %in% c(45:47) ~ "Wholesale & retail trade",
    wz2 %in% c(86:88, 94:96) ~ "Health & social services",
    wz2 %in% c(64:66) ~ "Insurance & banking",
    wz2 %in% c(55, 56) ~ "Accommodation & catering",
    wz2 %in% c(49:53) ~ "Logistics & transport",
    wz2 %in% c(90:93) ~ "Creative industry & entertainment ",
    wz2 %in% c(28:30) ~ "Mechanical engineering",
    wz2 %in% c(10, 11) ~ "Food production",
    wz2 %in% c(20:22) ~ "Chemicals & pharmaceuticals",
    wz2 %in% c(26) ~ "Manufacturing of data processing equipment",
  )) %>% 
  mutate(wz_fct = ifelse(is.na(wz_fct), "Others", wz_fct)) %>% 
  select(crefo, wz_fct, wzdig5, refo, kreis_l)



# Add size data
df_size <- read_dta("K:\\MUP\\Paneldaten\\Groesse\\Aufbereitet\\groesse_w57.dta", 
                    encoding = "UTF-8")
df_size <- df_size %>% 
  mutate(crefo = as.character(crefo))

df_size %>% tab(jahr)

df_firm <- df_firm %>% 
  left_join(df_size[c("crefo", "ma_anz", "ums_betrag")], by = "crefo")


df_firm %>% map_df(.f = function(x) sum(is.na(x))/nrow(df_firm))

df_firm <- df_firm %>% 
  rename(anzma = ma_anz, umsatz = ums_betrag)

# Imputation
load("Q:/Meine Bibliotheken/Research/01_Promotion/05_Ideas/07_COVID_Tracking/02_Data/03_Rating/df_panel_2018_2021_full.RData")

df_temp <- df_rate_all %>% 
  group_by(crefo) %>% 
  summarise(anzma = round(mean(anzma, na.rm = TRUE)),
            umsatz = round(mean(umsatz, na.rm = TRUE)))


df_firm <- df_firm %>% 
  coalesce_join(df_temp, by = "crefo")

df_firm %>% map_df(.f = function(x) sum(is.na(x))/nrow(df_firm))

# Aggregate by size class
df_firm <- df_firm %>% 
  mutate(
    csize_fct = factor(case_when(
      (anzma <= 10) | (umsatz <= 2000000) ~ 'Micro-enterprise',
      ((anzma > 10) & (anzma < 50)) | ((umsatz > 2000000) & (umsatz <= 10000000)) ~ 'Small enterprise', 
      ((anzma >= 50) & (anzma < 250)) | ((umsatz > 10000000) & (umsatz <= 50000000)) ~ 'Medium-sized enterprise', 
      (anzma >= 250) | (umsatz > 50000000) ~ 'Large enterprise'
    ), levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise')))

df_firm %>% map_df(.f = function(x) sum(is.na(x))/nrow(df_firm))


# Add age data
df_age <- read_dta("K:\\MUP\\Paneldaten\\Daten\\Aufbereitet\\gruenddat_aufb_w58.dta", 
         encoding = "UTF-8")
df_age <- df_age %>% 
  mutate(crefo = as.character(crefo))

sum(is.na(df_age$gruend_jahr))/nrow(df_age)
sum(is.na(df_age$gruenddat))/nrow(df_age)
sum(is.na(df_age$erstdat))/nrow(df_age)

# Imputation with erstdat
df_age <- df_age %>% 
  mutate(gruend_jahr = if_else(is.na(gruend_jahr), year(ymd(erstdat)), gruend_jahr))
  
sum(is.na(df_age$gruend_jahr))/nrow(df_age)


df_firm <- df_firm %>% 
  left_join(df_age[c("crefo", "gruend_jahr")], by = "crefo")

df_firm %>% map_dfr(function(x) sum(is.na(x))/nrow(df_firm))  

# Save
write_delim(df_firm, path = file.path(getwd(), "02_Data", "04_Firm_Characteristics", "firmdata.txt"), delim = "\t")



# Aggregated Firm Characteristics Ratings ---------------------------------

df_rate_all <- df_rate_all %>% 
  group_by(crefo) %>% 
  fill(anzma, .direction = 'downup') %>% 
  fill(wz, .direction = 'downup') %>% 
  ungroup() %>% 
  mutate(wz2 = wz2_converter(wz)) %>% 
  mutate(wz_fct = case_when(
    wz2 %in% c(58:63, 68, 69:82) ~ "Business-related services",
    wz2 %in% c(5:9, 12:19, 23:25, 27, 31:33, 35:39, 41:43) ~ "Manufacturing",
    wz2 %in% c(45:47) ~ "Wholesale & retail trade",
    wz2 %in% c(86:88, 94:96) ~ "Health & social services",
    wz2 %in% c(64:66) ~ "Insurance & banking",
    wz2 %in% c(55, 56) ~ "Accommodation & catering",
    wz2 %in% c(49:53) ~ "Logistics & transport",
    wz2 %in% c(90:93) ~ "Creative industry & entertainment ",
    wz2 %in% c(28:30) ~ "Mechanical engineering",
    wz2 %in% c(10, 11) ~ "Food production",
    wz2 %in% c(20:22) ~ "Chemicals & pharmaceuticals",
    wz2 %in% c(26) ~ "Manufacturing of data processing equipment",
  )) %>% 
  mutate(wz_fct = ifelse(is.na(wz_fct), "Others", wz_fct)) %>% 
  mutate(
    csize_fct = factor(case_when(
      (anzma <= 10) | (umsatz <= 2000000) ~ 'Micro-enterprise',
      ((anzma > 10) & (anzma < 50)) | ((umsatz > 2000000) & (umsatz <= 10000000)) ~ 'Small enterprise', 
      ((anzma >= 50) & (anzma < 250)) | ((umsatz > 10000000) & (umsatz <= 50000000)) ~ 'Medium-sized enterprise', 
      (anzma >= 250) | (umsatz > 50000000) ~ 'Large enterprise'
    ), levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise')))



save(df_rate_all, file = file.path(getwd(), '02_Data', '03_Rating', 'df_panel_2018_2021_full.RData'))


setwd("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking")
source(file.path(getwd(), '04_Code', 'setup.R'))


# -------------------------------------------------------------------------
# Add Wave 59 -------------------------------------------------------------


load("Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\05_Old\\df_panel_2017_2020_clean.RData")
df_panel
df_panel <- df_panel %>% 
  mutate(crefo = as.character(crefo)) 

df_panel2 <- read_delim(file = "Q:\\Meine Bibliotheken\\Research\\SME_Corona\\02_Data\\01_PanelData\\05_Old\\df_panel_2018_2020_clean.txt", delim='\t',
                        col_types = cols(crefo = col_character())) 
df_panel2 <- df_panel2 %>% 
  mutate(crefo = as.character(crefo))

df_panel2 %>% tab(jahr)
df_panel2 %>% tab(update)

# Add 

df_rating <- read_dta(paste0("K:\\MUP\\Paneldaten\\Rating\\Aufbereitet\\rating_w59.dta"), 
                      encoding = 'UTF-8')
df_rating <- df_rating %>% 
  mutate(crefo = as.character(crefo))

df_dates <- read_dta(paste0("K:\\MUP\\Paneldaten\\Daten\\Original\\daten_w59.dta"), 
                     encoding = 'UTF-8')
df_dates <- df_dates %>% 
  mutate(crefo = as.character(crefo))
df_dates %>% tab(welle)

# Merge rechdat to rating information
df_rating59 <- df_rating %>% 
  select(crefo, welle, bonitaet) %>% 
  left_join(df_dates[c('crefo', 'rechdat')], by = 'crefo')

# Drop observations w/o rating information
df_rating59 <- df_rating59 %>% 
  mutate(rechdat = ymd(rechdat),
         jahr = 2021) %>% 
  select(crefo, jahr, rechdat, bonitaet) %>% 
  filter(!is.na(bonitaet))

# Drop observations which are not in panel set
df_rating59 <- df_rating59 %>% 
  filter(crefo %in% df_panel2$crefo)


# Append new wave to existing panel
df_panel <- df_panel2 %>% 
  select(crefo, jahr, rechdat, bonitaet) %>% 
  bind_rows(df_rating59) %>% 
  as_tsibble(key = crefo, index = jahr)



# If rechdat has not changed but bonitaet has changed check if archivdat has changed. If yes, impute rechdat with archivdat
# Add archivdat
df_panel <- df_panel %>% 
  as_tibble() %>% 
  left_join(df_dates[,c('crefo', 'archivdat')], by = 'crefo') %>% 
  mutate(archivdat = ymd(archivdat))

# Return back to tsibble and correct rechdat
df_panel <- df_panel %>% 
  as_tsibble(key = crefo, index = jahr)

df_panel <- df_panel %>% 
  group_by_key() %>% 
  mutate(
    check = if_else((bonitaet != dplyr::lag(bonitaet)) & (rechdat == dplyr::lag(rechdat) & (row_number()!=1) & (jahr == 2021)), 1, 0)
    ) %>% 
  ungroup() %>% 
  mutate(rechdat = if_else(check==1, archivdat, rechdat))

df_panel %>% 
  filter(check==1) 

df_panel %>% 
  filter(crefo=="2010000057")

# If rechdat is missing fill with archivdat
df_panel %>% 
  filter(is.na(rechdat))

df_panel <- df_panel %>% 
  mutate(rechdat = if_else(is.na(rechdat), archivdat, rechdat)) 

# One observation with missing rechdat remains. Drop it!
df_panel <- df_panel %>% 
  filter(!is.na(rechdat)) %>% 
  select(-c(archivdat))

# If rechdat takes on non-reasonable values fill with archivdat
# a) to far in the past
df_panel %>% 
  filter(rechdat < "2000-01-01") %>% 
  head(20)

df_panel <- df_panel %>% 
  mutate(rechdat = if_else(rechdat < "2000-01-01", archivdat, rechdat)) 

df_panel %>% 
  filter(rechdat < "2000-01-01") %>% 
  head(20)

# b) in the future
df_panel %>% 
  filter(rechdat > "2021-06-01") %>% 
  head(20)

df_panel <- df_panel %>% 
  mutate(rechdat = if_else(rechdat > "2021-06-01", archivdat, rechdat)) 

df_panel %>% 
  filter(rechdat > "2021-06-01") %>% 
  head(20)

# Drop duplicates in crefo, rechdat, bonitaet
df_panel <- df_panel %>% 
  as_tibble() %>% 
  select(crefo, jahr, rechdat, bonitaet) %>% 
  distinct(across(matches('crefo|rechdat|bonitaet')), .keep_all = 'TRUE')


df_panel <- df_panel %>%  
  as_tsibble(key = crefo, index = jahr) 

df_panel <- df_panel %>% 
  mutate(bonitaet = ifelse(bonitaet==0, NA, bonitaet),
         bonitaet = ifelse(bonitaet==600, 500, bonitaet))

df_panel %>% 
  filter(is.na(jahr))

save(df_panel, file = file.path(getwd(), '02_Data', '03_Rating', 'df_panel_2018_2021.RData'))


# Clean bonitaet (determine mising as 0 and define bonitaet = 600 as 500), 
# indicator for update of data, number of days between updates, change in bonitaet, anzma, umsatz, gesch?ftsgang prior to change in bonitaet
df <- df_panel %>%
  #head(20) %>% 
  as_tibble() %>% 
  mutate(rechdat2 = as.double(rechdat)) %>% 
  group_by(crefo) %>% 
  mutate(
    #updays = rechdat - lag(rechdat),
    updays2 = rechdat2 - lag(rechdat2),
    ch_bonitaet = bonitaet - lag(bonitaet),
  ) %>% 
  ungroup() %>%
  mutate(
    #updays = ifelse((updays == 'NA days'), NA, updays),
    update = case_when(
      updays2 == 0 ~ 0,
      updays2 > 0 ~ 1),
    ch_bonitaet = ifelse(update==0, NA, ch_bonitaet)
  ) 

df_panel <- df %>% 
  select(-c(rechdat2)) %>% 
  rename(updays = updays2) %>% 
  coalesce_join(df_panel2, by = c("crefo", "jahr"), join = dplyr::left_join)



save(df_panel, file = file.path(getwd(), '02_Data', '03_Rating', 'df_panel_2018_2021_full.RData'))


# Select Updates ----------------------------------------------------------

load("Q:/Meine Bibliotheken/Research/01_Promotion/05_Ideas/07_COVID_Tracking/02_Data/03_Rating/df_panel_2018_2021_full.RData")


# Drop observations with missing in ch_bonitaet
df_panel <- df_panel %>% 
  filter(!is.na(ch_bonitaet))

# Select updates after threshhold date
threshold_date <- "2020-07-01"

df_rate_agg <- df_panel %>% 
  filter(rechdat > threshold_date) %>% 
  group_by(crefo) %>% 
  summarise(
    jahr = last(jahr), 
    rechdat = last(rechdat),
    bonitaet = round(mean(bonitaet)),
    ch_bonitaet = round(mean(ch_bonitaet))
    ) %>% 
  ungroup()

write_delim(df_rate_agg, path = file.path(getwd(), '02_Data', '03_Rating', 'df_rate.txt'), delim = '\t')



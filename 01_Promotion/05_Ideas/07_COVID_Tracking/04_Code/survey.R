setwd("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking")
source(file.path(getwd(), '04_Code', 'setup.R'))


# -------------------------------------------------------------------------
# LEGEND:

# Exposure Dimension:
# A	R?ckgang der Nachfrage                                                          -> auswir_negtyp_na
# B	Vor?bergehende Schlie?ung von Betriebsteilen oder ganzen Betrieben              -> 
# C	Gesch?ftsaufgabe                                                                -> 
# D	Schwierigkeiten beim Bezug von Vorleistungen oder Zwischenprodukten             -> auswir_negtyp_vorls
# E	Personelle Engp?sse aufgrund von Krankheit, Quarant?ne oder Kinderbetreuung     -> auswir_negtyp_besch
# F	Logistische Schwierigkeiten beim Absatz der eigenen Produkte                    -> auswir_negtyp_vertr
# G	Liquidit?tsengp?sse                                                             -> auswir_negtyp_liq
# xxx                                                                               -> auswir_negtyp_sonst (does not exist)                                                        

# Date of waves:
# 1: 14. - 23. April
# 2: 08. - 23. Juni
# 3: 24.09 - 13.10 




df_survey1 <- read_delim("Q:\\Meine Bibliotheken\\Research\\02_Projects\\CORONA\\02_Data\\Kantar\\Welle1\\kantar_mup3.txt",
                         delim = '\t',
                         col_types = cols(crefo = col_character()))


df_survey2 <- read_delim("Q:\\Meine Bibliotheken\\Research\\02_Projects\\CORONA\\02_Data\\Kantar\\Welle2\\kantar_mup_w2.txt",
                         delim = '\t',
                         col_types = cols(crefo = col_character()))


df_survey3 <- read_delim("Q:\\Meine Bibliotheken\\Research\\02_Projects\\CORONA\\02_Data\\Kantar\\Welle3\\kantar_mup_w3.txt",
                         delim = '\t',
                         col_types = cols(crefo = col_character()))


# Combine the surveys
df_survey <- df_survey1[, c('crefo', 'F2', 'F6A', 'F6B', 'F6D', 'F6E', 'F6F', 'F6G')] %>% 
  rename("neg_imp"='F2', "exposure_A"='F6A', "exposure_B"='F6B', "exposure_D"='F6D', "exposure_E"='F6E', "exposure_F"='F6F', "exposure_G"='F6G') %>% 
  mutate(welle = 1,
         date = ymd("2020-04-23")) %>% 
  add_row(df_survey2[, c('crefo', 'G2', 'G6A', 'G6B', 'G6D', 'G6E', 'G6F', 'G6G')] %>% 
              rename("neg_imp"='G2', "exposure_A"='G6A', "exposure_B"='G6B', "exposure_D"='G6D', "exposure_E"='G6E', "exposure_F"='G6F', "exposure_G"='G6G') %>%
              mutate(welle = 2,
                     date = ymd("2020-06-23")),
          #  by = 'crefo'
          ) %>% 
  add_row(df_survey3[, c('crefo', 'H2', 'H6A', 'H6B', 'H6D', 'H6E', 'H6F', 'H6G')] %>% 
              rename("neg_imp"='H2', "exposure_A"='H6A', "exposure_B"='H6B', "exposure_D"='H6D', "exposure_E"='H6E', "exposure_F"='H6F', "exposure_G"='H6G') %>%
              mutate(welle = 3,
                     date = ymd("2020-10-13")), 
            #by = 'crefo'
            ) %>% 
  mutate(neg_imp = if_else(neg_imp=="Ja",1,0))

# Clean encodings for unkown or missing (8, 9 or 6, 7 in wave 3)
df_survey <- df_survey %>% 
  mutate_at(vars(contains("exposure")), function(x) na_if(x, 9)) %>% 
  mutate_at(vars(contains("exposure")), function(x) na_if(x, 8)) %>% 
  mutate_at(vars(contains("exposure")), function(x) na_if(x, 7)) %>% 
  mutate_at(vars(contains("exposure")), function(x) na_if(x, 6)) 

# Subtract 1 from each Lickert scale value
df_survey <- df_survey %>% 
  mutate_at(vars(contains("exposure")), function(x) x-1)
            
            
write_delim(df_survey, "Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking\\02_Data\\02_Survey\\df_survey.txt", delim='\t') 

# Aggregate
df_survey_agg <- df_survey %>% 
  select(-c(welle, date)) %>% 
  group_by(crefo) %>% 
  summarise_if(is_double, function(x) mean(x, na.rm = TRUE))

write_delim(df_survey_agg, "Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking\\02_Data\\02_Survey\\df_survey_agg.txt", delim='\t') 

setwd("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking")
source(file.path(getwd(), '04_Code', 'setup.R'))


# -------------------------------------------------------------------------


# Read Data ---------------------------------------------------------------


# Read Impact Data
df <- read_delim("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking\\02_Data\\01_Webdata\\bert_per_firm.csv", delim='\t')

# Reduce to relevant variables only
df <- df %>% 
  select(ID, adaption, information, no_problem, problem, unclear)
  
# Add Firm Characteristics
# Select wave first
wave <- 59
df_temp <- read_dta(paste0("K:\\MUP\\Paneldaten\\Stammdaten\\Aufbereitet\\Stammdaten_w", wave, ".dta"), 
                    encoding = 'UTF-8')

# Merge to Impact Data
df <- df %>% left_join(df_temp, by = c("ID"='crefo'))

df %>% tab(welle)
# Information comes from different waves... which is okay


# Clean Data --------------------------------------------------------------

df %>% head(3)
df <- df %>% mutate(ID = as.character(ID),
              branche2 = as.double(branche2),
              branche3 = as.double(branche3),
              branche4 = as.double(branche4))


all.equal(df$refo, df$refo_ug)

df %>% filter(refo!=refo_ug) # refo_ug seems to distinguish between GmbH and mini-GmbH... take refo
df <- df %>% 
  select(-refo_ug)

df %>% filter(kreisnr!=as.double(kreis_l)) # Seem to differ... keep both


# Check missings
df %>% 
  summarise_all(.funs = ~ sum(is.na(.))/nrow(df))

# Check if brancheX exists of wzdig5 is missing
df %>% 
  filter(is.na(wzdig5) & !is.na(branche2))
# Not the case!

# Convert refo to factor
df <- df %>% mutate(refo = stata2factor(refo))

df <- df %>% rename(wz = wzdig5) %>% 
  mutate(wz_fct = case_when( 
  wz>=1000 & wz<= 4000	~	'Agriculture'	,
  wz>=5000 & wz<= 9999	~	'Mining'	,
  wz>=10000 & wz<= 12999	~	'Food'	,
  wz>=13000 & wz<= 18999	~	'Clothing/Printing'	,
  wz>=19000 & wz<= 19999	~	'Oil'	,
  wz>=20000 & wz<= 20999	~	'Chemicals'	,
  wz>=21000 & wz<= 21999	~	'Pharmaceuticals'	,
  wz>=22000 & wz<= 23999	~	'Plastics'	,
  wz>=24000 & wz<= 25999	~	'Metal/Steel'	,
  wz>=26000 & wz<= 27999	~	'Electronics/Optics'	,
  wz>=28000 & wz<= 28999	~	'Mechnical Engineering'	,
  wz>=29000 & wz<= 30999	~	'Automotive'	,
  wz>=31000 & wz<= 32999	~	'Furniture'	,
  wz>=33000 & wz<= 34999	~	'Repair/Installation'	,
  wz>=35000 & wz<= 39999	~	'Utilities'	,
  wz>=40000 & wz<= 44999	~	'Construction'	,
  wz>=45000 & wz<= 45999	~	'Car repair'	,
  wz>=46000 & wz<= 46999	~	'Wholesale'	,
  (wz>=47000 & wz<= 47199) | (wz>=47400 & wz<= 47729) | (wz>=47760 & wz<= 47799) | (wz>=47820 & wz<= 47899) | (wz>=47990 & wz<= 47999)	~	'Retail (closed)'	,
  (wz>=47200 & wz<= 47299) | (wz>=47300 & wz<= 47399) | (wz>=47730 & wz<= 47759) | (wz>=47800 & wz<= 47819) | (wz>=47900 & wz<= 47919)	~	'Retail (open)'	,
  wz>=49000 & wz<= 53999	~	'Transport'	,
  wz>=55000 & wz<= 55999	~	'Hotels'	,
  wz>=56000 & wz<= 56999	~	'Restaurants'	,
  wz>=58000 & wz<= 60999	~	'Media'	,
  wz>=61000 & wz<= 63999	~	'Software/Telco'	,
  wz>=64000 & wz<= 66999	~	'Financial service/Banks'	,
  wz>=68000 & wz<= 68999	~	'Real estate'	,
  (wz>=69000 & wz<= 71999) | (wz>=73000 & wz<= 73999)	~	'Consulting'	,
  wz>=72000 & wz<= 72999	~	'Research'	,
  wz>=73000 & wz<= 74999	~	'Creative Services'	,
  wz>=75000 & wz<= 75999	~	'Veterinary'	,
  wz>=77000 & wz<= 78999	~	'Leasing'	,
  wz>=79000 & wz<= 79999	~	'Travel'	,
  wz>=80000 & wz<= 82999	~	'Facilities'	,
  wz>=84000 & wz<= 84999	~	'Public service'	,
  wz>=85000 & wz<= 85999	~	'Education'	,
  wz>=86000 & wz<= 88999	~	'Health'	,
  wz>=90000 & wz<= 93999	~	'Entertainment'	,
  (wz>=94000 & wz<= 94999)   | (wz>=99000 & wz<= 99999)	~	'Interest groups'	,
  wz>=95000 & wz<= 98999	~	'Personal service'	
)) 



df <- df %>% 
  rename(wz5 = wz) %>% 
  mutate(wz2 = wz2_converter(wz5)) %>% 
  mutate(wz_fct2 = case_when(
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
  mutate(wz_fct2 = ifelse(is.na(wz_fct2), "Others", wz_fct2))
  
  
  
df %>% tab(wz_fct2)
df %>% tab(problem)

df <- df %>% 
  rename(crefo = ID)

# Save complete
write_delim(df, path = "Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking\\02_Data\\01_Webdata\\df_web_complete.txt", delim='\t')

# Save impact data only complete
df %>% 
  select(crefo, adaption, information, no_problem, problem, unclear) %>% 
  write_delim(path = "Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking\\02_Data\\01_Webdata\\df_web.txt", delim='\t')



# All Waves ---------------------------------------------------------------

path_webfiles <- "H:\\Large_Datasets\\COVID_Tracking\\share\\share"

df_all <- tibble()

for (i in list.files(path_webfiles)){
  
  df <- read_delim(file = file.path("H:\\Large_Datasets\\COVID_Tracking\\share\\share", i), delim="\t")
  
  # Some cleaning
  df <- df %>% 
    select(ID, adaption, information, no_problem, problem, unclear) %>% 
    rename(crefo = ID) %>% 
    mutate(crefo = as.character(crefo)) %>% 
    mutate(welle = str_extract(i, pattern = regex("\\d\\d_\\d\\d"))) %>% 
    select(crefo, welle, everything())
  
  # Bind wave
  df_all <- df_all %>% 
    bind_rows(df)
  
}

df_all %>% tab(welle)

# Save
df_all %>% 
  mutate(date = dmy(paste(str_sub(welle, start = -2), str_sub(welle, start = 1, end = 2), '2020', sep = '-'))) %>% 
  write_delim(path = file.path(getwd(), '02_Data', '01_Webdata', 'df_web_all.txt'), delim = '\t')

# Aggregation
df_agg <- df_all %>% 
  group_by(crefo) %>% 
  summarise_if(is.double, mean)

# Save
write_delim(df_agg, path = file.path(getwd(), '02_Data', '01_Webdata', 'df_web_agg.txt'), delim = '\t')

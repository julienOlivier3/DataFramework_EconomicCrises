setwd("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking")
source(file.path(getwd(), '04_Code', 'setup.R'))



# -------------------------------------------------------------------------

# Aggregate ---------------------------------------------------------------

## Load Data ==============================================================

# a) Webdata
df_web <- read_delim(file = file.path(getwd(), '02_Data', '01_Webdata', 'df_web_agg.txt'), delim = '\t', 
                     col_types = cols(crefo = col_character()))

# b) Survey data
df_surv <- read_delim(file = file.path(getwd(), '02_Data', '02_Survey', 'df_survey_agg.txt'), delim = '\t', 
                      col_types = cols(crefo = col_character()))


# c) Rating data
# Crisis ratings only
df_rate <- read_delim(file = file.path(getwd(), '02_Data', '03_Rating', 'df_rate.txt'), delim = '\t', 
                      col_types = cols(crefo = col_character()))


# Firm characteristics
firmdata <- read_delim(file = file.path(getwd(), '02_Data', '04_Firm_Characteristics', 'firmdata.txt'), delim = '\t', 
                      col_types = cols(crefo = col_character()))



## Webdata ================================================================

### Sample ################################################################
df_web_sample <- read_dta("K:\\!Projekte\\BMWI-2020-Corona-KantarMUPWEB_133244\\Daten\\Web\\corona_on_web.dta")
df_web_sample <- df_web_sample %>% mutate(crefo = as.character(crefo))

df_web_sample <- df_web_sample %>% 
  left_join(df_web, by = 'crefo') %>% 
  left_join(firmdata, by = "crefo")

# Some (less than 1000 firms have in all 5 categories 0, i.e. no-reference found). 
# In this case set the categories to NA indicating that no reference has been found
df_web_sample <- df_web_sample %>% 
  rowwise() %>% 
  mutate(check = sum(adaption, information, no_problem, problem, unclear)) %>% 
  mutate(adaption = ifelse(check==0, NA, adaption),
         information = ifelse(check==0, NA, information),
         no_problem = ifelse(check==0, NA, no_problem),
         problem = ifelse(check==0, NA, problem),
         unclear = ifelse(check==0, NA, unclear)
         ) %>% 
  ungroup()

# Define variable indicating whether reference has been found or not
df_web_sample <- df_web_sample %>% 
  select(-c("corona", "check")) %>% 
  rowwise() %>% 
  mutate(corona = sum(adaption, information, no_problem, problem, unclear),
         corona = if_else(is.na(corona), 0, 1))
  
df_ref1 <- df_web_sample %>% 
  group_by(wz_fct, csize_fct) %>% 
  summarise(ref_frac = formatC(round(mean(corona, na.rm = TRUE)*100, 1), digits = 1, format = 'f')) %>% 
  ungroup() %>% 
  mutate(csize_fct = if_else(is.na(csize_fct), "Unknown", csize_fct),
         csize_fct = factor(csize_fct, 
                            levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise', 'Unknown'),
                            labels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise', 'Unknown'))) %>% 
  arrange(csize_fct) %>% 
  pivot_wider(names_from = csize_fct, values_from = ref_frac) %>% 
  add_column(N = df_web_sample %>% group_by(wz_fct) %>% tally() %>% select(n)) %>% 
  mutate(N = format(as.double(N$n), big.mark = ",")) %>% 
  #select(wz_fct, N, everything()) %>% 
  arrange(desc(N)) 

df_ref2 <- df_web_sample %>% group_by(csize_fct) %>% summarise(ref_frac = formatC(round(mean(corona, na.rm = TRUE)*100, 1), digits = 1, format = 'f')) %>% 
  mutate(csize_fct = if_else(is.na(csize_fct), "Unknown", csize_fct),
         csize_fct = factor(csize_fct, 
                            levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise', 'Unknown'),
                            labels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise', 'Unknown'))) %>% 
  arrange(csize_fct) %>% 
  pivot_wider(names_from = csize_fct, values_from = ref_frac) %>% 
  add_column(N = format(nrow(df_web_sample), big.mark = ","),
             wz_fct = "Total") %>% 
  select(wz_fct, everything())

df_ref <- df_ref1 %>% 
  add_row(df_ref2)

stargazer(df_ref, digits = 2, summary = FALSE, decimal.mark = ",", rownames = FALSE, digit.separate = 4, digit.separator = ",")


### Radarchart: Webdata ###################################################
df_web <- df_web %>%
  left_join(firmdata[c('crefo', 'wz_fct')]) 

# Binarize categories
df_web_binary <- df_web %>% 
  mutate_if(is.double, function(x) ifelse(x > 0, 1, 0))

# Webdata by sector + add rows with max, min and avg values
plot_radar_web <- df_web_binary %>% 
  group_by(wz_fct) %>% 
  summarise(adaption = mean(adaption),
            information = mean(information),
            no_problem = mean(no_problem),
            problem = mean(problem),
            unclear = mean(unclear)) %>% 
  ungroup() 

plot_radar_web <- plot_radar_web %>% 
  add_row(wz_fct = "Avg", adaption = mean(.$adaption), information = mean(.$information), no_problem = mean(.$no_problem), problem = mean(.$problem), unclear = mean(.$unclear), .before = 1) %>% 
  add_row(wz_fct = "Min", adaption = 0, information = 0, no_problem = 0, problem = 0, unclear = 0, .before = 1) %>% 
  add_row(wz_fct = "Max", adaption = 1, information = 1, no_problem = 1, problem = 1, unclear = 1, .before = 1) %>% 
  mutate(wz_fct = map_chr(wz_fct, .f = function(x) str_replace(x, pattern = '&', replacement = '\\\\&'))) %>% 
  filter(!(wz_fct == 'Others')) %>% 
  column_to_rownames(var = "wz_fct")



for (i in 4:nrow(plot_radar_web)) {
  
  tikz(file.path(getwd(), "03_Writing", "01_Figures", str_c("fig_webdata_radar", i-3, ".tex")),
       height = 3.5,
       width = 6)
  
  radarchart(
    plot_radar_web[c(1:3, i), ],
    pfcol = c("#99999980", NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    title = row.names(plot_radar_web)[i], cex.main = 1,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Variable labels
    axistype = 1, axislabcol = 'grey37', 
    vlcex = 0.9, vlabels = c("adaption", "information", "no problem", "problem", "unclear"), seg = 4, caxislabels = c(0, NA, 0.5, NA, 1),
  )
  dev.off()
  
}








### Descriptives: Webdata #################################################
df_web <- df_web %>%
  left_join(firmdata[c('crefo', 'wz_fct')]) 

# Binarize categories
df_web_binary <- df_web %>% 
  mutate_if(is.double, function(x) ifelse(x > 0, 1, 0))

df_desc <- tibble(`Problem`=summary(df_web_binary$problem)) %>% mutate_all(as.double) %>% add_row(`Problem` = sum(df_web_binary$problem)) %>% 
  add_column(tibble(`No problem`=summary(df_web_binary$no_problem)) %>% mutate_all(as.double) %>% add_row(`No problem` = sum(df_web_binary$no_problem))) %>% 
  add_column(tibble(Adaption=summary(df_web_binary$adaption)) %>% mutate_all(as.double) %>% add_row(`Adaption` = sum(df_web_binary$adaption))) %>% 
  add_column(tibble(`Information`=summary(df_web_binary$information)) %>% mutate_all(as.double) %>% add_row(`Information` = sum(df_web_binary$information))) %>% 
  add_column(tibble(`Unclear`=summary(df_web_binary$unclear)) %>% mutate_all(as.double) %>% add_row(`Unclear` = sum(df_web_binary$unclear))) %>% 
  t() %>% 
  as_tibble(rownames = "Categories") %>% 
  mutate(V2 = round(V2),
         V3 = round(V3),
         V4 = round(V4, 2),
         V5 = round(V5)) %>% 
  rename(Min = V1,
         Q1 = V2,
         Med = V3,
         Mean = V4,
         Q3 = V5,
         Max = V6,
         N = V7) #%>% 
  mutate(N = format(N, big.mark = ","))

df_desc <- df_desc %>%
  mutate(Fraction = round(N/1183920, 2)) %>% 
  select(Categories, Fraction, Mean, N) %>% 
  add_row(Categories="Overall", Fraction=round(202076/1183920,2), Mean=NA, N=202076) %>% 
  mutate(N = format(N, big.mark = ",")) 
  
  
stargazer(df_desc, digits = 2, summary = FALSE, decimal.mark = ",", rownames = FALSE, digit.separate = 4, digit.separator = ",")

## Survey =================================================================


### Radarchart: Survey ####################################################
df_surv <- df_surv %>%
  left_join(firmdata[c('crefo', 'wz_fct', 'csize_fct')], by = "crefo") 



# Data by sector + add rows with max, min and avg values
plot_radar_surv <- df_surv %>% 
  group_by(wz_fct) %>% 
  summarise("drop in demand" = mean(exposure_A, na.rm = TRUE),
            "temporary closing" = mean(exposure_B, na.rm = TRUE),
            "supply chain interruption" = mean(exposure_D, na.rm = TRUE),
            "staffing shortage" = mean(exposure_E, na.rm = TRUE),
            "logistical sales problems" = mean(exposure_F, na.rm = TRUE),
            "liquidity shortfalls" = mean(exposure_G, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(!(wz_fct=="Others"))


plot_radar_surv <- plot_radar_surv %>% 
  add_row(wz_fct = "Avg", 
          "drop in demand" = mean(.$`drop in demand`), 
          "temporary closing" = mean(.$`temporary closing`), 
          "supply chain interruption" = mean(.$`supply chain interruption`), 
          "staffing shortage" = mean(.$`staffing shortage`), 
          "logistical sales problems" = mean(.$`logistical sales problems`), 
          "liquidity shortfalls" = mean(.$`liquidity shortfalls`), 
          .before = 1) %>% 
  add_row(wz_fct = "Min", `drop in demand`=0, `temporary closing`=0, `supply chain interruption`=0, `staffing shortage`=0, `logistical sales problems`=0, `liquidity shortfalls`=0, .before = 1) %>% 
  add_row(wz_fct = "Max", `drop in demand`=4, `temporary closing`=4, `supply chain interruption`=4, `staffing shortage`=4, `logistical sales problems`=4, `liquidity shortfalls`=4, .before = 1) %>% 
  mutate(wz_fct = map_chr(wz_fct, .f = function(x) str_replace(x, pattern = '&', replacement = '\\\\&'))) %>% 
  filter(!(wz_fct == 'Others')) %>% 
  column_to_rownames(var = "wz_fct")




for (i in 4:nrow(plot_radar_surv)) {
  
  tikz(file.path(getwd(), "03_Writing", "01_Figures", str_c("fig_survey_radar", i-3, ".tex")),
       height = 3.5,
       width = 6)
  
  radarchart(
    plot_radar_surv[c(1:3, i), ],
    pfcol = c("#99999980", NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    title = row.names(plot_radar_surv)[i], cex.main = 1,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Variable labels
    axistype = 1, axislabcol = 'grey37', 
    vlcex = 0.9, vlabels = c('A', 'B', 'C', 'D', 'E', 'F'), seg = 4, caxislabels = c(0, 1, 2, 3, 4)
  )
  
  dev.off()
}


### Descriptives: Survey ##################################################
df_surv <- df_surv %>%
  left_join(firmdata[c('crefo', 'wz_fct', 'csize_fct')], by = "crefo") 

# Drop one observation w/o crefo
df_surv <- df_surv %>% 
  filter(!is.na(crefo))


df_surv %>% tab(csize_fct)
df_surv %>% tab(wz_fct)
df_surv %>% tab(neg_imp)

df_desc <- tibble(neg_imp=summary(df_surv %>% mutate(neg_imp = if_else(neg_imp > 0, 1, 0)) %>% select(neg_imp) %>% as_vector())) %>% 
  mutate(neg_imp = as.double(neg_imp)) %>%
  add_row(neg_imp=0) %>% 
  add_column(tibble(A=summary(df_surv$exposure_A))) %>% 
  add_column(tibble(B=summary(df_surv$exposure_B))) %>% 
  add_column(tibble(C=summary(df_surv$exposure_D))) %>% 
  add_column(tibble(D=summary(df_surv$exposure_E))) %>%
  add_column(tibble(E=summary(df_surv$exposure_F))) %>% 
  add_column(tibble("F"=summary(df_surv$exposure_G))) %>% 
  t() %>% 
  as_tibble(rownames = "Variable") %>% 
  mutate(V2 = round(V2),
         V3 = round(V3),
         V4 = round(V4, 2),
         V5 = round(V5)) %>% 
  rename(Min = V1,
         Q1 = V2,
         Med = V3,
         Mean = V4,
         Q3 = V5,
         Max = V6,
         non_res = V7) %>% 
  add_column(N = df_surv %>% select(neg_imp, contains("exposure")) %>% map_dfr(function(x) sum(!is.na(x))) %>% as_vector()) %>% 
  mutate(N = as.double(N)) %>% 
  select(-c(non_res))

stargazer(df_desc, digits = 2, summary = FALSE, decimal.mark = ",", rownames = FALSE, digit.separate = 4, digit.separator = ",")

## Rating =================================================================



map_tikz <- firmdata %>% 
  select(wz_fct) %>% 
  distinct() %>% 
  mutate(wz_fct2 = map_chr(wz_fct, .f = function(x) str_replace(x, pattern = '&', replacement = '\\\\&'))) 

df_rate <- df_rate %>% 
  left_join(firmdata[c('crefo', 'wz_fct')], by = 'crefo') %>% 
  left_join(map_tikz, by = "wz_fct") %>% 
  select(-c(wz_fct)) %>% 
  rename(wz_fct = wz_fct2)

threshold_date <- "2020-06-01"

wz_order <- df_rate %>% 
  filter(rechdat > threshold_date) %>% 
  group_by(wz_fct) %>% 
  summarise(q75 = quantile(ch_bonitaet, 0.9, na.rm = TRUE),
            mean = mean(ch_bonitaet, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(mean)) %>% 
  select(wz_fct) %>% 
  as_vector()


# Add pre-crisis updates
load("Q:/Meine Bibliotheken/Research/01_Promotion/05_Ideas/07_COVID_Tracking/02_Data/03_Rating/df_panel_2018_2021_full.RData")

df_rate18 <- df_rate_all %>% 
  select(crefo, jahr, rechdat, bonitaet, updays, ch_bonitaet, wz_fct) %>% 
  left_join(map_tikz, by = "wz_fct") %>% 
  select(-c(wz_fct)) %>% 
  rename(wz_fct = wz_fct2) %>% 
  mutate(wz_fct = factor(wz_fct, levels = wz_order)) %>% 
  filter(year(rechdat) == 2018)
  
df_rate <- df_rate_all %>% 
  select(crefo, jahr, rechdat, bonitaet, updays, ch_bonitaet, wz_fct) %>% 
  left_join(map_tikz, by = "wz_fct") %>% 
  select(-c(wz_fct)) %>% 
  rename(wz_fct = wz_fct2) %>% 
  mutate(wz_fct = factor(wz_fct, levels = wz_order)) %>% 
  filter(rechdat > threshold_date) 

### Density: Rating #######################################################
cc <- scales::seq_gradient_pal("red", "yellow", "Lab")(seq(0,1,length.out=13))

tikz(file.path(getwd(), "03_Writing", "01_Figures", "fig_rating_density.tex"),
     height = 5.5,
     width = 6)


df_rate %>%
  mutate(wz_fct = factor(wz_fct, levels = wz_order)) %>% 
  ggplot(aes(y = wz_fct, x = ch_bonitaet, fill = wz_fct)) +
  geom_density_ridges(aes(color = "white"), linetype = "solid", bandwidth=2, alpha=1) +
  geom_density_ridges(data = df_rate18, aes(color = "green4"), linetype = "dashed", bandwidth=2, alpha=0, size = 0.5, na.rm = T) +
  scale_x_continuous(limits = c(-50, 50)) +
  scale_fill_manual(values=cc) + 
  ylab("Sector") +
  xlab("$\\\\Delta r_t$") +
  theme_jod +
  theme(
    legend.key.width = unit(1, "lines"),
    legend.key.height = unit(0.2, "lines"),
    legend.position = "top",
    legend.direction = "horizontal", 
    legend.box = "horizontal",
    legend.spacing.x = unit(1, "lines"),
    legend.spacing.y = unit(0.25, "lines"),
    legend.text = element_text(size=7,  margin = margin(r = 15, unit = "pt")),
    legend.title = element_text(size=8, face = "plain"),
    legend.box.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
    legend.box.margin = margin(t = 5, l = 10, r = 1, b = 5),
    panel.border = element_blank(),
    axis.text.x = element_text(size = 7, angle = 0, hjust = 0.5),
    axis.ticks = element_blank(),
    axis.text.y = element_text(vjust=-0.75, hjust = 0, margin=margin(l = 20, r = -120))
        ) +
  guides(fill=FALSE, linetype=FALSE) +
  scale_color_manual(name = "Density",
                     labels = c("crisis", "pre-crisis"),
                     values = c("white" = "white", "green4" = "green4"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed"),
                                                              color = c("white", "green4"),
                                                              fill = c("orange", NA)
                                                              ),
                                          title.position="top")
                     )

dev.off()


### Extreme Ratings #######################################################
df_temp <- df_rate %>%
  filter(!is.na(ch_bonitaet)) 

df_tab1 <- df_temp %>% 
  group_by(wz_fct) %>% 
  summarise(
    N = n(),
    frac_above50 = formatC(round(sum(ch_bonitaet>50)/n(), digits = 3)*100, digits = 1, format = 'f'),
    #frac_above100 = sum(ch_bonitaet>100)/n(),
    frac_below50 = formatC(round(sum(ch_bonitaet< -50)/n(), digits = 3)*100, digits = 1, format = 'f')
    #frac_below100 = sum(ch_bonitaet<-100)/n()
    ) %>% 
  ungroup() %>% 
  arrange(frac_above50) %>% 
  add_row(wz_fct = "Overall", 
          N = nrow(df_rate), 
          frac_above50 = formatC(round(sum(df_temp$ch_bonitaet > 50)/nrow(df_temp), digits = 3)*100, digits = 1, format = 'f'), 
          frac_below50 = formatC(round(sum(df_temp$ch_bonitaet < -50)/nrow(df_temp), digits = 3)*100, digits = 1, format = 'f'))


df_temp <- df_rate18 %>%
  filter(!is.na(ch_bonitaet)) 

df_tab2 <- df_temp %>% 
  group_by(wz_fct) %>% 
  summarise(
    N = n(),
    frac_above50 = formatC(round(sum(ch_bonitaet>50)/n(), digits = 3)*100, digits = 1, format = 'f'),
    #frac_above100 = sum(ch_bonitaet>100)/n(),
    frac_below50 = formatC(round(sum(ch_bonitaet< -50)/n(), digits = 3)*100, digits = 1, format = 'f')
    #frac_below100 = sum(ch_bonitaet<-100)/n()
  ) %>% 
  ungroup() %>% 
  arrange(frac_above50) %>% 
  add_row(wz_fct = "Overall", 
          N = nrow(df_temp), 
          frac_above50 = formatC(round(sum(df_temp$ch_bonitaet > 50)/nrow(df_temp), digits = 3)*100, digits = 1, format = 'f'), 
          frac_below50 = formatC(round(sum(df_temp$ch_bonitaet < -50)/nrow(df_temp), digits = 3)*100, digits = 1, format = 'f'))

df_tab <- df_tab1 %>% 
  left_join(df_tab2, by = 'wz_fct') %>% 
  select(wz_fct, N.x, frac_above50.x, N.y, frac_above50.y) %>% 
  mutate(N.x = format(N.x, big.mark = ","),
         N.y = format(N.y, big.mark = ","))

stargazer(df_tab, summary = FALSE, decimal.mark = ",", rownames = FALSE, digit.separate = 4, digit.separator = ",")


### Descriptives: Rating ##################################################
df_rate <- df_rate %>% 
  left_join(df_rate_all[,c('crefo', 'rechdat', 'updays')], by = c("crefo", "rechdat"))
round(mean(df_rate$updays))/30

df_rate %>% tab(wz_fct)
summary(df_rate$ch_bonitaet)
summary(df_rate$rechdat)








# Models ------------------------------------------------------------------

## Load Data ==============================================================

# a) Webdata
df_web <- read_delim(file = file.path(getwd(), '02_Data', '01_Webdata', 'df_web_agg.txt'), delim = '\t', 
                     col_types = cols(crefo = col_character()))

# b) Survey data
df_surv <- read_delim(file = file.path(getwd(), '02_Data', '02_Survey', 'df_survey_agg.txt'), delim = '\t', 
                      col_types = cols(crefo = col_character()))

# c) Rating data
df_rate <- read_delim(file = file.path(getwd(), '02_Data', '03_Rating', 'df_rate.txt'), delim = '\t', 
                      col_types = cols(crefo = col_character()))
# Add pre-crisis updates
load("Q:/Meine Bibliotheken/Research/01_Promotion/05_Ideas/07_COVID_Tracking/02_Data/03_Rating/df_panel_2018_2021_full.RData")
df_rate_all

# Firm characteristics
firmdata <- read_delim(file = file.path(getwd(), '02_Data', '04_Firm_Characteristics', 'firmdata.txt'), delim = '\t', 
                       col_types = cols(crefo = col_character()))



## Web - Survey ===========================================================
# 1) Webdata
df_web_reg <- df_web %>% 
  filter(!is.na(crefo)) %>% 
  left_join(firmdata, by = 'crefo') %>% 
  mutate(age = 2021 - gruend_jahr, 
         age_group = if_else(age < 10, 'Start-up', 'Incumbent')) %>% 
  select(crefo, problem, wz_fct, csize_fct, age, age_group) %>% 
  mutate(neg_imp = if_else(problem > 0, 1, 0)) %>% 
  select(crefo, neg_imp, wz_fct, csize_fct, age, age_group) 
  #filter(wz_fct != "Others")

# Define dummies as factors 
df_web_reg <- df_web_reg %>% 
  mutate(csize_fct = factor(csize_fct, levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise')),
         wz_fct = factor(wz_fct, levels = c(
           "Business-related services",
           "Manufacturing",
           "Wholesale & retail trade",
           "Health & social services",
           "Insurance & banking",
           "Accommodation & catering",
           "Logistics & transport",
           "Creative industry & entertainment ",
           "Mechanical engineering",
           "Food production",
           "Chemicals & pharmaceuticals",
           "Manufacturing of data processing equipment"))) 

# Define baseline factors
df_web_reg <- df_web_reg %>% 
  mutate(wz_fct = relevel(wz_fct, ref = "Accommodation & catering"),
         csize_fct = relevel(csize_fct, ref = "Micro-enterprise"))


# 2) Survey
df_surv_reg <- df_surv %>% 
  filter(!is.na(crefo)) %>% 
  left_join(firmdata, by = 'crefo') %>% 
  mutate(age = 2021 - gruend_jahr,
         age_group = if_else(age < 10, 'Start-up', 'Incumbent')) %>% 
  select(crefo, neg_imp, wz_fct, csize_fct, age, age_group) %>% 
  mutate(neg_imp = if_else(neg_imp > 0, 1, 0)) %>% 
  filter(wz_fct != "Others")

# Define dummies as factors 
df_surv_reg <- df_surv_reg %>% 
  mutate(csize_fct = factor(csize_fct, levels = c('Large enterprise', 'Medium-sized enterprise', 'Small enterprise', 'Micro-enterprise')),
         wz_fct = factor(wz_fct, levels = c(
           "Business-related services",
           "Manufacturing",
           "Wholesale & retail trade",
           "Health & social services",
           "Insurance & banking",
           "Accommodation & catering",
           "Logistics & transport",
           "Creative industry & entertainment ",
           "Mechanical engineering",
           "Food production",
           "Chemicals & pharmaceuticals",
           "Manufacturing of data processing equipment"))) 

# Define baseline factors
df_surv_reg <- df_surv_reg %>% 
  mutate(wz_fct = relevel(wz_fct, ref = "Accommodation & catering"),
         csize_fct = relevel(csize_fct, ref = "Micro-enterprise"))

df_surv_reg %>% tab(wz_fct)
df_web_reg %>% tab(wz_fct)
df_surv_reg %>% tab(csize_fct)
df_web_reg %>% tab(csize_fct)
df_surv_reg %>% tab(age_group)
df_web_reg %>% tab(age_group)

# Regression
formula1 <- as.formula("neg_imp ~ wz_fct + csize_fct + age_group")
formula2 <- as.formula("neg_imp ~ wz_fct + csize_fct + log(age)")


surv_reg <- glm(formula = formula1, data = df_surv_reg, family = binomial(link = "probit"))
nrow(df_surv_reg); nobs(surv_reg)
summary(surv_reg)
summary(margins(surv_reg))
model_surv <- margins_summary(surv_reg) %>% 
  as_tibble() %>% 
  mutate(sig = case_when(p < 0.01 ~ '***',
                         p >= 0.01 & p < 0.05 ~ '**',
                         p >= 0.05 & p < 0.1 ~ '*',
                         p >= 0.1 ~ '')) %>% 
  select(factor, AME, sig, SE, lower, upper)

web_reg <- glm(formula = formula1, data = df_web_reg, family = binomial(link = "probit"))
nrow(df_web_reg); nobs(web_reg)
summary(web_reg)
summary(margins(web_reg))
model_web <- margins_summary(web_reg) %>% 
  as_tibble() %>% 
  mutate(sig = case_when(p < 0.01 ~ '***',
                         p >= 0.01 & p < 0.05 ~ '**',
                         p >= 0.05 & p < 0.1 ~ '*',
                         p >= 0.1 ~ '')) %>% 
  select(factor, AME, sig, SE, lower, upper)

model1 <- model_web %>% mutate(kind = "Webdata") %>% 
  bind_rows(model_surv %>% mutate(kind = "Survey data")) %>% 
  mutate(sig = factor(if_else(sig %in% c("***", "**"), "$p < 0.05$", "$p \\ge 0.05$"), levels = c("$p < 0.05$", "$p \\ge 0.05$"))) %>% 
  mutate(group = factor(case_when(str_detect(factor, "age_group") ~ "Age",
                           str_detect(factor, "csize_fct") ~ "Size",
                           str_detect(factor, "wz_fct") ~ "Sector"), levels = c("Age", "Size", "Sector"))) %>% 
  mutate(factor = factor(str_remove(factor, regex("age_group|csize_fct|wz_fct")))) %>% 
  mutate(kind = factor(kind, levels = c("Webdata", "Survey data"))) %>% 
  mutate(factor = str_replace_all(factor, "&", "\\\\&"))


model1 <- model1 %>% 
  mutate(factor = factor(factor, levels = model1 %>% filter(kind == "Webdata") %>% arrange(desc(AME)) %>% select(factor) %>% as_vector()))



tikz(file.path(getwd(), "03_Writing", "01_Figures", "fig_model_ame.tex"),
     height = 4,
     width = 6)

ggplot(model1, aes(y = factor, x = AME, color = kind, alpha = sig)) + 
  geom_vline(xintercept = 0, linetype = 2, color = "grey") + 
  geom_errorbar(aes(xmin = lower, xmax = upper), width=.25, position=position_dodge(0.5)) +
  geom_point(aes(shape = kind), position=position_dodge(0.5)) +
  xlab("Marginal effect") +
  ylab("Firm characteristics") +
  facet_grid(group~., scales = "free_y", space = "free_y", switch = "y") +
  theme_jod +
  theme(legend.key.width = unit(0.5, "lines"),
        legend.key.height = unit(1, "lines"),
        legend.position = "top",
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        #legend.justification=c(1, 1),
        #legend.margin=margin(t = -.75, b = .75, unit='cm'),
        legend.spacing.x = unit(0.2, "lines"),
        legend.spacing.y = unit(0, "cm"),
        legend.text = element_text(size=7,  margin = margin(r = 15, unit = "pt")),
        legend.title = element_text(size=8, face = "plain"),
        legend.box.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        legend.box.margin = margin(t = 5, l = 10, r = 0, b = 2),
        panel.border = element_blank(),
        axis.text.x = element_text(size = 7, angle = 0, hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text.y = element_text(vjust=-0.75, hjust = 0, margin=margin(l = 30, r = -160)),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text.y = element_text(color = "darkgrey", size = 8),
        plot.margin = unit(c(0,5.5,5.5,5.5), "pt")) +
  scale_x_continuous(limits = c(-0.8, 0.3)) +
  guides(color = guide_legend(order = 1, title.position = "top"),
         shape = guide_legend(order = 1, title.position = "top"),
         alpha = guide_legend(order = 2, title.position = "top")) +
  scale_color_manual(name = "Data source",
                     values = c("green4", "red")) +
  scale_shape_manual(name = "Data source",
                     values = c(17, 19)) +
  scale_alpha_manual(name = "Significance",
                     values = c(1, 0.3),
                     guide = guide_legend(override.aes = list(color = c("red", "red"),
                                                                fill = c("red", "red"),
                                                                shape = c(17, 17)))) 

dev.off()


## Web - Credit ===========================================================
# Binarize categories
df_web_binary <- df_web %>% 
  mutate_if(is.double, function(x) ifelse(x > 0, 1, 0))

# Add pre-crisis updates
load("Q:/Meine Bibliotheken/Research/01_Promotion/05_Ideas/07_COVID_Tracking/02_Data/03_Rating/df_panel_2018_2021_full.RData")
date_threshold <- ymd('20200601')

# Calculate boni prior to update
df_rate <- df_rate_all %>% 
  mutate(p_bonitaet = bonitaet - ch_bonitaet) %>% 
  select(crefo, jahr, rechdat, bonitaet, ch_bonitaet, p_bonitaet) %>% 
  filter(rechdat >= date_threshold)

# Join webdata to credit rating data 
df_model <- df_rate %>% 
  left_join(df_web, by = 'crefo') %>% 
  filter(!is.na(adaption)) 

# Join firm characteristics to data and calculate age
df_model <- df_model %>% 
  left_join(firmdata, by = 'crefo') %>% 
  mutate(age = 2021 - gruend_jahr,
         age = if_else(age < 10, 'Start-up', 'Incumbent')
         )


### Estimation ############################################################


# Regression (1)
formula1 <- as.formula("ch_bonitaet ~ problem + no_problem + adaption + information + unclear + p_bonitaet + age + csize_fct + wz_fct")
model_ols <- lm(formula = formula1, data = df_model)
coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
nobs(model_ols)

# Regression (2)
formula2 <- as.formula("ch_bonitaet ~ problem + no_problem + adaption + information + unclear + p_bonitaet")
model_ols <- lm(formula = formula2, data = df_model)
model_res <- tidy(coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC0")))
model_res1 <- model_res %>% 
  mutate(
    p.value2 = case_when(p.value < 0.01 ~ '***',
                         p.value >= 0.01 & p.value < 0.05 ~ '**',
                         p.value >= 0.05 & p.value < 0.1 ~ '*',
                         p.value >= 0.1 ~ ''),
    est = paste0(formatC(round(estimate, 2), digits = 2, format = 'f'), p.value2),
    se = round(std.error, 2),
    model = 1
    ) %>% 
  select(term, est, se, model)
nobs(model_ols)

# Regression (3)
formula3 <- as.formula("ch_bonitaet ~ problem + no_problem + adaption + information + unclear + p_bonitaet + age")
model_ols <- lm(formula = formula3, data = df_model)
model_res <- tidy(coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC0")))
model_res2 <- model_res %>% 
  mutate(
    p.value2 = case_when(p.value < 0.01 ~ '***',
                         p.value >= 0.01 & p.value < 0.05 ~ '**',
                         p.value >= 0.05 & p.value < 0.1 ~ '*',
                         p.value >= 0.1 ~ ''),
    est = paste0(formatC(round(estimate, 2), digits = 2, format = 'f'), p.value2),
    se = round(std.error, 2),
    model = 2
  ) %>% 
  select(term, est, se, model)
nobs(model_ols)

# Regression (4)
formula4 <- as.formula("ch_bonitaet ~ problem + no_problem + adaption + information + unclear + p_bonitaet + age + csize_fct")
model_ols <- lm(formula = formula4, data = df_model)
model_res <- tidy(coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC0")))
model_res3 <- model_res %>% 
  mutate(
    p.value2 = case_when(p.value < 0.01 ~ '***',
                         p.value >= 0.01 & p.value < 0.05 ~ '**',
                         p.value >= 0.05 & p.value < 0.1 ~ '*',
                         p.value >= 0.1 ~ ''),
    est = paste0(formatC(round(estimate, 2), digits = 2, format = 'f'), p.value2),
    se = round(std.error, 2),
    model = 3
  ) %>% 
  select(term, est, se, model)
nobs(model_ols)

# Regression (5)
formula5 <- as.formula("ch_bonitaet ~ problem + no_problem + adaption + information + unclear + p_bonitaet + age + csize_fct + wz_fct")
model_ols <- lm(formula = formula5, data = df_model)
model_res <- tidy(coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC0")))
model_res4 <- model_res %>% 
  mutate(
    p.value2 = case_when(p.value < 0.01 ~ '***',
                         p.value >= 0.01 & p.value < 0.05 ~ '**',
                         p.value >= 0.05 & p.value < 0.1 ~ '*',
                         p.value >= 0.1 ~ ''),
    est = paste0(formatC(round(estimate, 2), digits = 2, format = 'f'), p.value2),
    se = round(std.error, 2),
    model = 4
  ) %>% 
  select(term, est, se, model)
nobs(model_ols)

model_res <- model_res1 %>% 
  bind_rows(model_res2) %>% 
  bind_rows(model_res3) %>% 
  bind_rows(model_res4) %>% 
  mutate(id = paste0(term, model),
         se = if_else(se==0, "$<0.01$", as.character(se))) %>% 
  select(id, est, se)

xlsx::write.xlsx(model_res, file = file.path(getwd(), '03_Writing', '02_Tables', 'model2_res_temp.xlsx'), sheetName = "raw")

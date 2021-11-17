# Setup -------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(file.path(here(), '04_Code', 'setup.R'))


# Read Out-of-sample test data --------------------------------------------

df_test <- read_delim(here("02_Data//01_Webdata//01_Annotation//corona_predictions_sample.csv"), delim="\t")

# Drop duplicate texts
df_test <- df_test %>% distinct(text, .keep_all = TRUE)
dim(df_test)

# Balance
table(df_test$vote)/sum(table(df_test$vote))

df_test %>%
  arrange(desc(confidence)) %>% 
  select(ID, text) %>% 
  write.xlsx(here("02_Data//01_Webdata//01_Annotation//corona_predictions_sample_to_label.xlsx"))

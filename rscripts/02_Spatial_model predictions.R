# Author: Maxwell Azali
# May 2022
rm(list = ls())
# Model weighted predictions for current (2020) and future (2050) high (RCP 8.5) and low (RCP 2.6) climate scenarios
library(here)
library(tidyverse)
library(gbm)

#----
#Load saved models 

model1_full <- read_rds(here("models", "model1_full.Rds"))
model2_full <- read_rds(here("models", "model2_full.Rds"))
# Load datasets

Grids <- read.csv(here("data", "WIOEnvironmentaldata2020.csv"))

# Import coral cover predictions 
coralcover <- read.csv(here("data", "Coralcoverpredictions2020_RCPs2050.csv"))

coralcover <- coralcover %>%
  dplyr::select(ID, hardcoral, hardcoral26, hardcoral85)

"%ni%" <- Negate("%in%")

# change character columns to factors
Grids <- Grids%>%
  filter(Country %ni% c("Maldives", "British Indian Ocean Territory"))%>%
 dplyr::select(-Coralcover)%>%
  left_join(coralcover %>% dplyr::select(ID, hardcoral) %>% rename("Coralcover" = "hardcoral"))%>%
  mutate_if(is.character,as.factor)

# Weighted predictions 2020

Grids$model1_preds <- predict(model1_full, Grids, type="response",na.action=NULL)

Grids$model2_preds <- predict(model2_full, Grids, type="response",na.action=NULL)

# Assign weights based on model R2 
Grids$model1_wt <- rep(0.84, 7039)
Grids$model2_wt <- rep(0.82, 7039)

# Ensemble prediction weighted by R2
Grids <- Grids %>%
  group_by(ID)%>%
  mutate(
         calc_full= sum(model1_preds*model1_wt, model2_preds*model2_wt), 
         sum_wt_full= sum(model1_wt,model2_wt),
         ensemble_preds = calc_full/sum_wt_full)%>%
  ungroup()

# save 2020 ensemble predictions for further mapping using QGIS
Preds_2020 <- Grids%>%
  dplyr::select(ID,Lat,Lon,Country, Ecoregion, ensemble_preds)%>%
  rename("no_coral_taxa" = "ensemble_preds")

write_csv(Preds_2020, here("data", "Coraltaxa_ensemble_predictions_2020.csv"))

# Table 2 summaries

table2a <- Preds_2020 %>%
  summarise(coral_mn = mean(no_coral_taxa, na.rm = TRUE),
            coral_sd = sd(no_coral_taxa, na.rm=TRUE),
            no_reef_cells = n()) %>%
  mutate(no_coral_taxa = paste0(round(coral_mn,1), " (", round(coral_sd,1), ")"),
         Province = "Western Indian Ocean Province")%>%
  dplyr::select(Province, no_coral_taxa, no_reef_cells)
  

table2b <- Grids %>%
  group_by(Ecoregion)%>%
  summarise(coral_mn = mean(ensemble_preds, na.rm = TRUE),
            coral_sd = sd(ensemble_preds, na.rm=TRUE),
            no_reef_cells = n()) %>%
  mutate(no_coral_taxa = paste0(round(coral_mn,1), " (", round(coral_sd,1), ")"))%>%
  dplyr::select(Ecoregion, no_coral_taxa, no_reef_cells)  


table2c <- Grids %>%
  group_by(Country)%>%
  summarise(coral_mn = mean(ensemble_preds, na.rm = TRUE),
            coral_sd = sd(ensemble_preds, na.rm=TRUE),
            no_reef_cells = n()) %>%
  mutate(no_coral_taxa = paste0(round(coral_mn,1), " (", round(coral_sd,1), ")"))%>%
  dplyr::select(Country, no_coral_taxa, no_reef_cells)  


# table 2d = use arcgis to calculate hotspots and summarise

table2 <- bind_rows(table2a,table2b, table2c)

write_csv(table2, here("outputs", "table2.csv"))


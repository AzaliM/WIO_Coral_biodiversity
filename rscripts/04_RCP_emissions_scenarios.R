# Author: Maxwell Azali
# May 2022

rm(list = ls())

# Future change in coral cover and species richness under high and low climate emission scenarios

library(gbm)
library(tidyverse)
library(here)
#library(pdp)
library(patchwork)
#library(vip)
#----
# Import 2020 coral taxa richness predictions
Preds_2020 <- read.csv(here("data", "Coraltaxa_ensemble_predictions_2020.csv"))

# Import coral cover predictions 
coralcover <- read.csv(here("data", "Coralcoverpredictions2020_RCPs2050.csv"))

coralcover <- coralcover %>%
  dplyr::select(ID, hardcoral, hardcoral26, hardcoral85)


# Import SST variables for RCP 2.5 and RCP 8.6 
RCP85_SST <- read.csv(here("data", "RCP85_SST.csv"))

RCP26_SST <- read.csv(here("data", "RCP26_SST.csv"))

SST_curr <- read.csv(here("data", "Current2020_SST.csv"))


## Predict number of coral taxa in 2050 for RCP 8.5

# Load saved models 
model1_full <- read_rds(here("models", "model1_full.Rds"))
model2_full <- read_rds(here("models", "model2_full.Rds"))


# Prepare spatial grid 

# Load datasets

Grids <- read.csv(here("data", "WIOEnvironmentaldata2020.csv"))

# remove 2020 SST variables and replace with RCP 8.5 and 8.6 variables

# RCP 8.5 
Grids85 <- Grids%>%
  dplyr::select(-c(SST_Median, SST_Skewness, SST_bimodality, SST_Kurtosis, SSTRateofRise, cumDHW, Coralcover )) %>%
  left_join(RCP85_SST)%>%
  left_join(coralcover %>% dplyr::select(ID, hardcoral85)%>% rename("Coralcover" = "hardcoral85" ))%>%
  mutate_if(is.character,as.factor)

Grids26 <- Grids%>%
  dplyr::select(-c(SST_Median, SST_Skewness, SST_bimodality, SST_Kurtosis, SSTRateofRise, cumDHW, Coralcover )) %>%
  left_join(RCP26_SST)%>%
  left_join(coralcover %>% dplyr::select(ID, hardcoral26)%>% rename("Coralcover" = "hardcoral26" ))%>%
  mutate_if(is.character,as.factor)


# 2050 ensemble number of taxa predictions RCP 8.5

Grids85$model1_preds <- predict(model1_full, Grids85, type="response",na.action=NULL)

Grids85$model2_preds <- predict(model2_full, Grids85, type="response",na.action=NULL)

# Assign weights based on model R2 
Grids85$model1_wt <- rep(0.84, 7039)
Grids85$model2_wt <- rep(0.82, 7039)

# Ensemble prediction weighted by R2
Grids85 <- Grids85 %>%
  group_by(ID)%>%
  mutate(
    calc_full= sum(model1_preds*model1_wt, model2_preds*model2_wt), 
    sum_wt_full= sum(model1_wt,model2_wt),
    ensemble_preds = calc_full/sum_wt_full)%>%
  ungroup()

# save 2020 ensemble predictions for further mapping using QGIS
Preds_2050_RCP85 <- Grids85%>%
  dplyr::select(ID,Lat,Lon,Country, Ecoregion, ensemble_preds, SST_Median, SST_Skewness, SST_bimodality, SST_Kurtosis, SSTRateofRise, cumDHW, Coralcover)%>%
  #left_join(coralcover %>% select(ID, hardcoral)%>% rename("Coralcover" = "hardcoral85" ))%>%
  rename("no_coral_taxa" = "ensemble_preds")%>%
  mutate(scenario = "RCP 8.5")

write_csv(Preds_2050_RCP85, here("data", "RCP85_Coraltaxa_ensemble_predictions_2050.csv"))




# 2050 ensemble number of taxa predictions RCP 2.6

Grids26$model1_preds <- predict(model1_full, Grids26, type="response",na.action=NULL)

Grids26$model2_preds <- predict(model2_full, Grids26, type="response",na.action=NULL)

# Assign weights based on model R2 
Grids26$model1_wt <- rep(0.84, 7039)
Grids26$model2_wt <- rep(0.82, 7039)

# Ensemble prediction weighted by R2
Grids26 <- Grids26 %>%
  group_by(ID)%>%
  mutate(
    calc_full= sum(model1_preds*model1_wt, model2_preds*model2_wt), 
    sum_wt_full= sum(model1_wt,model2_wt),
    ensemble_preds = calc_full/sum_wt_full)%>%
  ungroup()

# save 2020 ensemble predictions for further mapping using QGIS
Preds_2050_RCP26 <- Grids26%>%
  dplyr::select(ID,Lat,Lon,Country, Ecoregion, ensemble_preds,SST_Median, SST_Skewness, SST_bimodality, SST_Kurtosis, SSTRateofRise, cumDHW, Coralcover)%>%
  #left_join(coralcover %>% select(ID, hardcoral)%>% rename("Coralcover" = "hardcoral26" ))%>%
  rename("no_coral_taxa" = "ensemble_preds") %>%
  mutate(scenario = "RCP 2.6")

write_csv(Preds_2050_RCP26, here("data", "RCP26_Coraltaxa_ensemble_predictions_2050.csv"))


Preds_2020_curr <- Preds_2020 %>%
  left_join(Grids %>% dplyr::select(ID, SST_Median, SST_Skewness, SST_bimodality, SST_Kurtosis, SSTRateofRise, cumDHW ))%>%
  left_join(coralcover %>% dplyr::select(ID, hardcoral)%>% rename("Coralcover" = "hardcoral" ))%>%
  mutate(scenario = "current")

Preds_all <- bind_rows(Preds_2020_curr, Preds_2050_RCP85, Preds_2050_RCP26)



## Calculations for Table 4
table4_province <- Preds_all %>%
  group_by(scenario) %>%
  summarise(coralcover_mn = mean(Coralcover),
            coralcover_sd = sd(Coralcover),
            no_coral_taxa_mn = mean(no_coral_taxa),
            no_coral_taxa_sd = sd(no_coral_taxa)
  ) %>%
  mutate(no_coral_taxa = paste0(round(no_coral_taxa_mn,1), " (", round(no_coral_taxa_sd,1), ")"),
         coralcover = paste0(round(coralcover_mn,1), " (", round(coralcover_sd,1), ")"))

table4_country <- Preds_all %>%
  group_by(scenario, Country) %>%
  summarise(coralcover_mn = mean(Coralcover),
            coralcover_sd = sd(Coralcover),
            no_coral_taxa_mn = mean(no_coral_taxa),
            no_coral_taxa_sd = sd(no_coral_taxa)
            ) %>%
  mutate(no_coral_taxa = paste0(round(no_coral_taxa_mn,1), " (", round(no_coral_taxa_sd,1), ")"),
         coralcover = paste0(round(coralcover_mn,1), " (", round(coralcover_sd,1), ")"))
  
  table4_ecoregion <- Preds_all %>%
  group_by(scenario, Ecoregion) %>%
  summarise(coralcover_mn = mean(Coralcover),
            coralcover_sd = sd(Coralcover),
            no_coral_taxa_mn = mean(no_coral_taxa),
            no_coral_taxa_sd = sd(no_coral_taxa)
  ) %>%
    mutate(no_coral_taxa = paste0(round(no_coral_taxa_mn,1), " (", round(no_coral_taxa_sd,1), ")"),
           coralcover = paste0(round(coralcover_mn,1), " (", round(coralcover_sd,1), ")"))

  
  # Change in coral cover
  
  #  deltaSST = SST_Median - SST_Median[scenario == "current"]
  
 table4_country_gain_loss<- Preds_all %>%
   group_by(ID,Country) %>%
   # Calculte change in cover, taxa and SST
    mutate(deltacover = Coralcover - Coralcover[scenario == "current"],
           deltataxa = no_coral_taxa - no_coral_taxa[scenario == "current"])  %>%
   # remove current period as it is used for base calculations
   #filter(scenario != "current") %>%
   # classify loss or gain of coral cover and taxa
   mutate(cover_loss_gain = if_else(deltacover < 0, "cover loss", "cover gain"),
          taxa_loss_gain = if_else(deltataxa < 0, "taxa loss", "taxa gain"))%>%
   # combine cover and taxa loss and gains
   mutate(loss_gain = paste0(cover_loss_gain, "/", taxa_loss_gain),
          freq = 1) %>%
   filter( scenario != "current")%>%
   group_by(scenario, Country, loss_gain) %>%
   summarise(loss_gain_count = sum(freq) ) %>%
   pivot_wider(id_cols = c(scenario, Country), names_from = loss_gain, values_from = loss_gain_count, values_fill = 0)
  
 
 table4_ecoregion_gain_loss<- Preds_all %>%
   group_by(ID,Ecoregion) %>%
   # Calculte change in cover, taxa and SST
   mutate(deltacover = Coralcover - Coralcover[scenario == "current"],
          deltataxa = no_coral_taxa - no_coral_taxa[scenario == "current"])  %>%
   # remove current period as it is used for base calculations
   #filter(scenario != "current") %>%
   # classify loss or gain of coral cover and taxa
   mutate(cover_loss_gain = if_else(deltacover < 0, "cover loss", "cover gain"),
          taxa_loss_gain = if_else(deltataxa < 0, "taxa loss", "taxa gain"))%>%
   # combine cover and taxa loss and gains
   mutate(loss_gain = paste0(cover_loss_gain, "/", taxa_loss_gain),
          freq = 1) %>%
   filter( scenario != "current")%>%
   group_by(scenario, Ecoregion, loss_gain) %>%
   summarise(loss_gain_count = sum(freq) ) %>%
   pivot_wider(id_cols = c(scenario, Ecoregion), names_from = loss_gain, values_from = loss_gain_count, values_fill = 0)

 
 ## Delta SST
 
 table4_country_sst <- Grids %>%
   dplyr::select(ID, Country, Ecoregion) %>%
   left_join(RCP26_SST %>% dplyr::select(ID, SST_Median) %>% rename("SST_Median26" = "SST_Median"))%>%
   left_join(RCP85_SST %>% dplyr::select(ID, SST_Median) %>% rename("SST_Median85" = "SST_Median")) %>%
   left_join(SST_curr %>% dplyr::select(ID, SST_Median) %>% rename("SST_curr" = "SST_Median")) %>%
   mutate(deltaSSTrcp26 = SST_Median26 - SST_curr,
          deltaSSTrcp85 = SST_Median85 - SST_curr) %>%
   group_by(Country)%>%
   summarise(deltaSSTrcp26_mn = mean(deltaSSTrcp26),
             deltaSSTrcp26_sd = sd(deltaSSTrcp26),
                deltaSSTrcp85_mn = mean(deltaSSTrcp85),
             deltaSSTrcp85_sd = sd(deltaSSTrcp85))%>%
   mutate(
        deltaSST_RCP26 = paste0(round(deltaSSTrcp26_mn,2), " (", round(deltaSSTrcp26_sd,2), ")"),
          deltaSST_RCP85 = paste0(round(deltaSSTrcp85_mn,2), " (", round(deltaSSTrcp85_sd,2), ")"))
 
 
 table4_ecoregion_sst <- Grids %>%
   dplyr::select(ID, Country, Ecoregion) %>%
   left_join(RCP26_SST %>% dplyr::select(ID, SST_Median) %>% rename("SST_Median26" = "SST_Median"))%>%
   left_join(RCP85_SST %>% dplyr::select(ID, SST_Median) %>% rename("SST_Median85" = "SST_Median")) %>%
   left_join(SST_curr %>% dplyr::select(ID, SST_Median) %>% rename("SST_curr" = "SST_Median")) %>%
   mutate(deltaSSTrcp26 = SST_Median26 - SST_curr,
          deltaSSTrcp85 = SST_Median85 - SST_curr) %>%
   group_by(Ecoregion)%>%
   summarise(deltaSSTrcp26_mn = mean(deltaSSTrcp26),
             deltaSSTrcp26_sd = sd(deltaSSTrcp26),
             deltaSSTrcp85_mn = mean(deltaSSTrcp85),
             deltaSSTrcp85_sd = sd(deltaSSTrcp85))%>%
   mutate(
     deltaSST_RCP26 = paste0(round(deltaSSTrcp26_mn,2), " (", round(deltaSSTrcp26_sd,2), ")"),
     deltaSST_RCP85 = paste0(round(deltaSSTrcp85_mn,2), " (", round(deltaSSTrcp85_sd,2), ")"))
 
 
 # Resilience calculations done in an Excel sheet with formulas (filename = "Resilience calculations.xlsx)
 
 
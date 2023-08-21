rm(list = ls())
#Author:: Maxwell Azali
# 26 April 2022
# Implement and tune a GBM model for Coral diversity
# Choose GBM due to the ability to deal with correlated predictors and Missing values


#----

#Setup


# load packages 
library(here)
library(tidyverse)
library(rsample) ## Split training and test datasets 
library(gbm) ## Implement gbm-BRT model
library(vip) # variable importance plot
library(pdp) # partial dependence plots
library(dismo)
#---

#----

# Data loading and preparation

#Load summarised coral data with predictors

Coral_sum <- read.csv(here("data", "CoralBiodiversityWIODataCoral.csv"))

#View(Coral_sum)

## Uncorrelated set of variables with relationships with coral biodiversity

# Set 1

vars2 <- c("Number_of_genera" ,"Country", "Ecoregion" , "Management", "Habitat" , "Observer" , "Depth"  , "Coralcover", "SSTRateofRise", "PARmax", "Calcite", "Dis_oxygen", "mean.waveNRJ", "Salinity_mean", "andrello_reef_value","andrello_nutrients", "mean.npp", "TT_market_hrs",  "TT_pop_hrs", "cumDHW", "SST_Kurtosis", "SST_Median", "ChlorA_median", "Netflow", "Outdegree", "Indegree", "Retention")

# Set 2

vars3 <- c("Number_of_genera" ,"Country", "Ecoregion" , "Management", "Habitat" , "Observer" , "Depth"  , "Coralcover", "SSTRateofRise", "PARmax", "Calcite", "Dis_oxygen", "mean.waveNRJ", "Salinity_mean", "andrello_reef_value","andrello_sediments", "Current_vel_mean", "Diff_attenuation", "Grav_NP",  "Grav_NC", "PH", "SST_bimodality","SST_Skewness","ClimateStressModel", "Netflow", "Outdegree", "Indegree", "Retention")


## Create training- 70% and test -30% of the Coraldata

set.seed(123)

Coral_split <- initial_split(Coral_sum, prop = 0.7)

Coral_train <- training(Coral_split)

Coral_train <- Coral_train %>% 
  mutate_if(is.character,as.factor)

Coral_test <- testing(Coral_split)


# Predictors set 1 train data

Coral_train_selected <- Coral_train[,vars2]
Coral_train_selected <- as.data.frame(Coral_train_selected)  
Coral_train_selected <- Coral_train_selected %>% 
  mutate_if(is.character,as.factor)

## Check VIFs predictors set 1

vifdat <- Coral_train_selected %>%
  select_if(is.numeric)
x <- glm(Number_of_genera ~., data = vifdat, family = poisson)
summary(x)
car::vif(x)

# Predictors set 2 train data

Coral_train_selected2 <- Coral_train[,vars3]
Coral_train_selected2 <- as.data.frame(Coral_train_selected2)  
Coral_train_selected2 <- Coral_train_selected2 %>% 
  mutate_if(is.character,as.factor)


## Check VIFs predictors set 2

vifdat2 <- Coral_train_selected2 %>%
  select_if(is.numeric)
y <- glm(Number_of_genera ~., data = vifdat2, family = poisson)
summary(y)
car::vif(y)




#----

# Hyper parameter search

# create hyperparameter grid

hyper_grid_gbm <- expand.grid(
  shrinkage = c(.001,.01, .1, .3),
  interaction.depth = c(1, 2, 3,4, 5),
  n.minobsinnode = c(5, 8, 10, 15),
  bag.fraction = c(.5, .55, .60, .65, .75), 
  optimal_trees = 0,               # a place to dump results
  min_Poissondev = 0                     # a place to dump results
)

nrow(hyper_grid_gbm)

# grid search 
for(i in 1:nrow(hyper_grid_gbm)) {
  
  # reproducibility
  set.seed(423)
  # randomize data
  random_index <- sample(1:nrow(Coral_train_selected), nrow(Coral_train_selected))
  random_Coral_train <- Coral_train_selected[random_index, ]
  
  gbm.tune <- gbm(
    formula = Number_of_genera ~ .,
    distribution = "poisson",
    data = random_Coral_train,
    n.trees = 10000,
    interaction.depth = hyper_grid_gbm$interaction.depth[i],
    shrinkage = hyper_grid_gbm$shrinkage[i],
    n.minobsinnode = hyper_grid_gbm$n.minobsinnode[i],
    bag.fraction = hyper_grid_gbm$bag.fraction[i],
    train.fraction = .7,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid_gbm$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  #hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
  hyper_grid_gbm$min_Poissondev[i] <- min(gbm.tune$valid.error)
}

hyper_grid_gbm %>% 
  filter(optimal_trees>999)%>%
  dplyr::arrange(min_Poissondev) %>%
  head(10)


## Pick model with low learning rate = 0.01, interaction depth= 3, minobsinnode = 8, ntrees = 5000, bag.fraction = 0.50 

## Final model
set.seed(423)


model1_train <- gbm(
  formula = Number_of_genera ~ .,
  distribution = "poisson",
  data = Coral_train_selected,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 8,
  cv.folds = 10,
  bag.fraction = 0.5,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


varimp.gbm1 <- vip(model1_train, num_features =29, aesthetics = list(fill = "blue", color= "grey70")) + theme_bw() + geom_hline(yintercept = 100/29, lty=2) + labs(y="Importance, %", x= "Independent variables", subtitle = "CoralBRT1")

varimp.gbm1


Modelvalidation <- data.frame(matrix(nrow= 8, ncol=6))
colnames(Modelvalidation) <- c("Model", "TheilU","Deviance", "Correlation","R2", "RMSE")

Coral_test_selected <- Coral_test[,vars2]
Coral_test_selected <- as.data.frame(Coral_test_selected)  
Coral_test_selected <- Coral_test_selected %>% 
  mutate_if(is.character,as.factor)

preds1 <- predict(model1_train, Coral_test_selected, type="response")


dev1 <- dismo::calc.deviance(Coral_test_selected$Number_of_genera,preds1,calc.mean=T, family = "poisson")%>% round(2)
corel1 <- cor(Coral_test_selected$Number_of_genera, preds1) %>% round(2)
m1 <- lm(Coral_test_selected$Number_of_genera ~ preds1)
r21 <- summary(m1)$r.squared %>% round(2)
rmse1 <- caret::RMSE(Coral_test_selected$Number_of_genera, preds1)
theilU1 <- DescTools::TheilU(Coral_test_selected$Number_of_genera, preds1, type = 2)%>% round(2)



Modelvalidation[1,1] <- "BRTgbm_Predictors1"
Modelvalidation[1,2] <- theilU1
Modelvalidation[1,3] <- dev1
Modelvalidation[1,4] <- corel1
Modelvalidation[1,5] <- r21
Modelvalidation[1,6] <- rmse1


## Model with second set of predictors 

Coral_train_selected2 <- Coral_train[,vars3]
Coral_train_selected2 <- as.data.frame(Coral_train_selected2)  
Coral_train_selected2 <- Coral_train_selected2 %>% 
  mutate_if(is.character,as.factor)

dim(Coral_train_selected2)

set.seed(123)


model2_train <- gbm(
  formula = Number_of_genera ~ .,
  distribution = "poisson",
  data = Coral_train_selected2,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 8,
  cv.folds = 10,
  bag.fraction = 0.5,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


varimp.gbm2 <- vip(model2_train, num_features =27, aesthetics = list(fill = "blue", color= "grey70")) + theme_bw() + geom_hline(yintercept = 100/27, lty=2) + labs(y="Importance, %", x= "Independent variables", subtitle = "CoralBRT1")

varimp.gbm2

### Model validation 
Coral_test_selected2 <- Coral_test[,vars3]
Coral_test_selected2 <- as.data.frame(Coral_test_selected2)  
Coral_test_selected2 <- Coral_test_selected2 %>% 
  mutate_if(is.character,as.factor)

preds2 <- predict(model2_train, Coral_test_selected2, type="response")


dev2 <- calc.deviance(Coral_test_selected2$Number_of_genera,preds2,calc.mean=T, family = "poisson")%>% round(2)
corel2 <- cor(Coral_test_selected2$Number_of_genera, preds2) %>% round(2)
m2 <- lm(Coral_test_selected2$Number_of_genera ~ preds2)
r22 <- summary(m2)$r.squared %>% round(2)
rmse2 <- caret::RMSE(Coral_test_selected2$Number_of_genera, preds2)
theilU2 <- DescTools::TheilU(Coral_test_selected2$Number_of_genera, preds2, type = 2)%>% round(2)




Modelvalidation[2,1] <- "BRTgbm_Predictors2"
Modelvalidation[2,2] <- theilU2
Modelvalidation[2,3] <- dev2
Modelvalidation[2,4] <- corel2
Modelvalidation[2,5] <- r22
Modelvalidation[2,6] <- rmse2



# Final model with full dataset 

Coral_selected <- Coral_sum[,vars2]
Coral_selected <- as.data.frame(Coral_selected)  
Coral_selected <- Coral_selected %>% 
  mutate_if(is.character,as.factor)



set.seed(423)


model1_full <- gbm(
  formula = Number_of_genera ~ .,
  distribution = "poisson",
  data = Coral_selected,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 8,
  cv.folds = 10,
  bag.fraction = 0.5,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  



varimp.gbm1.full <- vip(model1_full, num_features =26, aesthetics = list(fill = "blue", color= "grey70")) + theme_bw() + geom_hline(yintercept = 100/26, lty=2) + labs(y="Importance, %", x= "Independent variables", subtitle = "Coral diversity predictors set1")

varimp.gbm1.full


### Model validation 

Coral_selected <- Coral_sum[,vars2]
Coral_selected <- as.data.frame(Coral_selected)  
Coral_selected <- Coral_selected %>% 
  mutate_if(is.character,as.factor)

preds3 <- predict(model1_full, Coral_selected, type="response")


dev3 <- calc.deviance(Coral_selected$Number_of_genera,preds3,calc.mean=T, family = "poisson")%>% round(2)
corel3 <- cor(Coral_selected$Number_of_genera, preds3) %>% round(2)
m3 <- lm(Coral_selected$Number_of_genera ~ preds3)
r23 <- summary(m3)$r.squared %>% round(2)
rmse3 <- caret::RMSE(Coral_selected$Number_of_genera, preds3)
theilU3 <- DescTools::TheilU(Coral_selected$Number_of_genera, preds3, type = 2)%>% round(2)




Modelvalidation[3,1] <- "BRTgbm_Predictorsfull1"
Modelvalidation[3,2] <- theilU3
Modelvalidation[3,3] <- dev3
Modelvalidation[3,4] <- corel3
Modelvalidation[3,5] <- r23
Modelvalidation[3,6] <- rmse3


## Model with second set of predictors 

Coral_selected2 <- Coral_sum[,vars3]
Coral_selected2 <- as.data.frame(Coral_selected2)  
Coral_selected2 <- Coral_selected2 %>% 
  mutate_if(is.character,as.factor)

dim(Coral_selected2)

set.seed(123)


model2_full <- gbm(
  formula = Number_of_genera ~ .,
  distribution = "poisson",
  data = Coral_selected2,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 8,
  cv.folds = 10,
  bag.fraction = 0.5,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


varimp.gbm2.full <- vip(model2_full, num_features =27, aesthetics = list(fill = "blue", color= "grey70")) + theme_bw() + geom_hline(yintercept = 100/27, lty=2) + labs(y="Importance, %", x= "Independent variables", subtitle = "Coral diversity predictors set2")

varimp.gbm2.full

### Model validation 

## Model with second set of predictors 

Coral_selected2 <- Coral_sum[,vars3]
Coral_selected2 <- as.data.frame(Coral_selected2)  
Coral_selected2 <- Coral_selected2 %>% 
  mutate_if(is.character,as.factor)

preds4 <- predict(model2_full, Coral_selected2, type="response")


dev4 <- calc.deviance(Coral_selected2$Number_of_genera,preds4,calc.mean=T, family = "poisson")%>% round(2)
corel4 <- cor(Coral_selected2$Number_of_genera, preds4) %>% round(2)
m4 <- lm(Coral_selected2$Number_of_genera ~ preds4)
r24 <- summary(m4)$r.squared %>% round(2)
rmse4 <- caret::RMSE(Coral_selected2$Number_of_genera, preds4)
theilU4 <- DescTools::TheilU(Coral_selected2$Number_of_genera, preds4, type = 2)%>% round(2)



Modelvalidation[4,1] <- "BRTgbm_Predictors2full"
Modelvalidation[4,2] <- theilU4
Modelvalidation[4,3] <- dev4
Modelvalidation[4,4] <- corel4
Modelvalidation[4,5] <- r24
Modelvalidation[4,6] <- rmse4

#----
# Extend modeling to preselected Climate and Human variables

varsClim <- c("Number_of_genera", "Coralcover",
              "cumDHW",
              "SST_bimodality",
              "SST_Kurtosis",
              "SST_Median",
              "SST_Skewness",
              "SSTRateofRise",
              "Depth")

varsHum <- c("Number_of_genera", "andrello_nutrients",
             "andrello_reef_value",
             "andrello_sediments",
             "Country",
             "Grav_NC",
             "Grav_NP",
             "Management",
             "TT_market_hrs",
             "TT_pop_hrs")



Coral_selectedClim <- Coral_sum[,c(varsClim)]
Coral_selectedClim <- as.data.frame(Coral_selectedClim)  
Coral_selectedClim <- Coral_selectedClim %>% 
  mutate_if(is.character,as.factor)


Coral_selectedHum <- Coral_sum[,c(varsHum)]
Coral_selectedHum <- as.data.frame(Coral_selectedHum)  
Coral_selectedHum <- Coral_selectedHum %>% 
  mutate_if(is.character,as.factor)

#----

### Climate variables 1 


set.seed(423)

model_climatevars_full <- gbm(
  formula = Number_of_genera ~ .,
  distribution = "poisson",
  data = Coral_selectedClim,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 8,
  cv.folds = 10,
  bag.fraction = 0.5,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
summary(model_climatevars_full)


### Human variables

set.seed(423)

model_humanvars_full <- gbm(
  formula = Number_of_genera ~ .,
  distribution = "poisson",
  data = Coral_selectedHum,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 8,
  cv.folds = 10,
  bag.fraction = 0.5,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
summary(model_humanvars_full)



### Model validation

preds5 <- predict(model_climatevars_full, Coral_selectedClim, type="response")


dev5 <- calc.deviance(Coral_selectedClim$Number_of_genera,preds5,calc.mean=T, family = "poisson")%>% round(2)
corel5 <- cor(Coral_selectedClim$Number_of_genera, preds5) %>% round(2)
m5 <- lm(Coral_selectedClim$Number_of_genera ~ preds5)
r25 <- summary(m5)$r.squared %>% round(2)
rmse5 <- caret::RMSE(Coral_selectedClim$Number_of_genera, preds5)
theilU5 <- DescTools::TheilU(Coral_selectedClim$Number_of_genera, preds5, type = 2)%>% round(2)

Modelvalidation[5,1] <- "Climate vars"
Modelvalidation[5,2] <- theilU5
Modelvalidation[5,3] <- dev5
Modelvalidation[5,4] <- corel5
Modelvalidation[5,5] <- r25
Modelvalidation[5,6] <- rmse5




preds6 <- predict(model_humanvars_full, Coral_selectedHum, type="response")


dev6 <- calc.deviance(Coral_selectedHum$Number_of_genera,preds6,calc.mean=T, family = "poisson")%>% round(2)
corel6 <- cor(Coral_selectedHum$Number_of_genera, preds6) %>% round(2)
m6 <- lm(Coral_selectedHum$Number_of_genera ~ preds6)
r26 <- summary(m6)$r.squared %>% round(2)
rmse6 <- caret::RMSE(Coral_selectedHum$Number_of_genera, preds6)
theilU6 <- DescTools::TheilU(Coral_selectedHum$Number_of_genera, preds6, type = 2)%>% round(2)

Modelvalidation[6,1] <- "Human vars"
Modelvalidation[6,2] <- theilU6
Modelvalidation[6,3] <- dev6
Modelvalidation[6,4] <- corel6
Modelvalidation[6,5] <- r26
Modelvalidation[6,6] <- rmse6

#----

# Test/ train climate and human variables models

Coral_selectedClim_train <- Coral_train[,c(varsClim)]
Coral_selectedClim_train <- as.data.frame(Coral_selectedClim_train)  
Coral_selectedClim_train <- Coral_selectedClim_train %>% 
  mutate_if(is.character,as.factor)



Coral_selectedClim_test <- Coral_test[,c(varsClim)]
Coral_selectedClim_test <- as.data.frame(Coral_selectedClim_test)  
Coral_selectedClim_test <- Coral_selectedClim_test %>% 
  mutate_if(is.character,as.factor)


set.seed(423)

model_climatevars_train <- gbm(
  formula = Number_of_genera ~ .,
  distribution = "poisson",
  data = Coral_selectedClim_train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 8,
  cv.folds = 10,
  bag.fraction = 0.5,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


preds7 <- predict(model_climatevars_train, Coral_selectedClim_test, type="response")


dev7 <- calc.deviance(Coral_selectedClim_test$Number_of_genera,preds7,calc.mean=T, family = "poisson")%>% round(2)
corel7 <- cor(Coral_selectedClim_test$Number_of_genera, preds7) %>% round(2)
m7 <- lm(Coral_selectedClim_test$Number_of_genera ~ preds7)
r27 <- summary(m7)$r.squared %>% round(2)
rmse7 <- caret::RMSE(Coral_selectedClim_test$Number_of_genera, preds7)
theilU7 <- DescTools::TheilU(Coral_selectedClim_test$Number_of_genera, preds7, type = 2)%>% round(2)

Modelvalidation[7,1] <- "Climate vars train"
Modelvalidation[7,2] <- theilU7
Modelvalidation[7,3] <- dev7
Modelvalidation[7,4] <- corel7
Modelvalidation[7,5] <- r27
Modelvalidation[7,6] <- rmse7






Coral_selectedHum_train <- Coral_train[,c(varsHum)]
Coral_selectedHum_train <- as.data.frame(Coral_selectedHum_train)  
Coral_selectedHum_train <- Coral_selectedHum_train %>% 
  mutate_if(is.character,as.factor)



Coral_selectedHum_test <- Coral_test[,c(varsHum)]
Coral_selectedHum_test <- as.data.frame(Coral_selectedHum_test)  
Coral_selectedHum_test <- Coral_selectedHum_test %>% 
  mutate_if(is.character,as.factor)



set.seed(423)

model_humanvars_train <- gbm(
  formula = Number_of_genera ~ .,
  distribution = "poisson",
  data = Coral_selectedHum_train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 8,
  cv.folds = 10,
  bag.fraction = 0.5,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
summary(model_humanvars_train)



preds8 <- predict(model_humanvars_train, Coral_selectedHum_test, type="response")


dev8 <- calc.deviance(Coral_selectedHum_test$Number_of_genera,preds8,calc.mean=T, family = "poisson")%>% round(2)
corel8 <- cor(Coral_selectedHum_test$Number_of_genera, preds8) %>% round(2)
m8 <- lm(Coral_selectedHum_test$Number_of_genera ~ preds8)
r28 <- summary(m8)$r.squared %>% round(2)
rmse8 <- caret::RMSE(Coral_selectedHum_test$Number_of_genera, preds8)
theilU8 <- DescTools::TheilU(Coral_selectedHum_test$Number_of_genera, preds8, type = 2)%>% round(2)

Modelvalidation[8,1] <- "Human vars train"
Modelvalidation[8,2] <- theilU8
Modelvalidation[8,3] <- dev8
Modelvalidation[8,4] <- corel8
Modelvalidation[8,5] <- r28
Modelvalidation[8,6] <- rmse8

## Save models for later use

obj_names <- ls(.GlobalEnv, pattern = "^model.")

savemodels <- function(x) {
  for(i in 1:length(x)) {
    file <- paste0(x[i],".Rds")
    saveRDS(get(x[i], envir = .GlobalEnv),here("models", file)) 
  }
    }

savemodels(obj_names)


# save model perfomance results
write_rds(Modelvalidation, file = here("models","modelvalidationcorals.rds"))


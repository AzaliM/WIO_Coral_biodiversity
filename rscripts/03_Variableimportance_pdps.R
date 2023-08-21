# Author: Maxwell Azali
# May 2022
rm(list = ls())

# Model summaries
library(gbm)
library(tidyverse)
library(here)
library(pdp)
library(patchwork)
library(vip)
#----
# Load packages



# Load saved models 
model1_full <- read_rds(here("models", "model1_full.Rds"))
model2_full <- read_rds(here("models", "model2_full.Rds"))
model_climatevars_full <- read_rds(here("models", "model_climatevars_full.Rds"))
model_humanvars_full <- read_rds(here("models", "model_humanvars_full.Rds"))



## Calculate relative influence for Table 1


relinf_model1 <- summary(
  model1_full, 
  cBars = 32,
  method = relative.influence, 
  las = 2
) %>%
  mutate(model = "Model 1")

relinf_model2 <- summary(
  model2_full, 
  cBars = 32,
  method = relative.influence, 
  las = 2
)%>%
  mutate(model = "Model 2")


relinf_climatevars <- summary(
  model_climatevars_full, 
  cBars = 32,
  method = relative.influence, 
  las = 2
)%>%
  mutate(model = "Climate variables")


relinf_humanvars <- summary(
  model_humanvars_full, 
  cBars = 32,
  method = relative.influence, 
  las = 2
)%>%
  mutate(model = "Human variables")


## Join vars 

relinf <- bind_rows(relinf_model1, relinf_model2, relinf_climatevars, relinf_humanvars) %>%
  pivot_wider(id_cols = var, names_from = model, values_from = rel.inf) %>%
  mutate_if(is.numeric, round,1)

# Modelvalidation

modelvalidationcorals <- read_rds(here("models", "modelvalidationcorals.Rds"))

R_square <- modelvalidationcorals %>%
  filter(!is.na(Model))%>%
mutate(Model = c("Model 1", "Model 2", "Model 1", "Model 2", "Climate variables", "Human variables", "Climate variables", "Human variables"),
       var = c("70% training and 30% testing", "70% training and 30% testing", "Full data", "Full data", "Full data", "Full data","70% training and 30% testing", "70% training and 30% testing")) %>%
  dplyr::select(var, Model, R2)%>%
  pivot_wider(id_cols = var, names_from = Model, values_from = R2) %>%
  mutate_if(is.numeric, round,2)


# Table 1

table1 <- bind_rows(relinf, R_square)
write_csv(table1, file = here("outputs", "table1.csv"))



#----------

## Figure 1 partial dependence plots 

# Load data used to create models
Coral_sum <- read.csv(here("data", "CoralBiodiversityWIODataCoral2.csv"))


# Set 1

vars2 <- c("Number_of_genera" ,"Country", "Ecoregion" , "Management", "Habitat" , "Observer" , "Depth"  , "Coralcover", "SSTRateofRise", "PARmax", "Calcite", "Dis_oxygen", "mean.waveNRJ", "Salinity_mean", "andrello_reef_value","andrello_nutrients", "mean.npp", "TT_market_hrs",  "TT_pop_hrs", "cumDHW", "SST_Kurtosis", "SST_Median", "ChlorA_median", "Netflow", "Outdegree", "Indegree", "Retention")

# Set 2

vars3 <- c("Number_of_genera" ,"Country", "Ecoregion" , "Management", "Habitat" , "Observer" , "Depth"  , "Coralcover", "SSTRateofRise", "PARmax", "Calcite", "Dis_oxygen", "mean.waveNRJ", "Salinity_mean", "andrello_reef_value","andrello_sediments", "Current_vel_mean", "Diff_attenuation", "Grav_NP",  "Grav_NC", "PH", "SST_bimodality","SST_Skewness","ClimateStressModel", "Netflow", "Outdegree", "Indegree", "Retention")

# model1 full dataset 

Coral_selected <- Coral_sum[,vars2]
Coral_selected <- as.data.frame(Coral_selected)  
Coral_selected <- Coral_selected %>% 
  mutate_if(is.character,as.factor)

# model2 full dataset 

Coral_selected2 <- Coral_sum[,vars3]
Coral_selected2 <- as.data.frame(Coral_selected2)  
Coral_selected2 <- Coral_selected2 %>% 
  mutate_if(is.character,as.factor)

# create partial dependence plots

### Observers

obs1c <- partial(model1_full, pred.var = "Observer",  
                 n.trees = model1_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE) %>%
  mutate(grp = "Model 1 (15.6%)")

obs2c <- partial(model2_full, pred.var = "Observer",  
                 n.trees = model2_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE)%>%
  mutate(grp = "Model 2 (15.2%)")

obs1c <- bind_rows(obs1c, obs2c)

#obs1$Observer <- fct_recode(obs1$Observer, Pascale = "Pascale Chabanet", Friedlander = "Alan Friedlander", Julien = "Julien Wickel", Nick = "Nick Graham", Bertrand = "Bertrand wendling")

obsc <- ggplot(obs1c, aes(x=reorder(Observer, -yhat), y=yhat, fill=grp)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("black", "red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.85),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.background = element_rect(fill="transparent"),
        panel.grid = element_blank()) +
  xlab("Observers") +
  ylab("Number of Coral taxa") +
  guides(lty="none") +
  scale_y_continuous(limits = c(0,30))

obsc

### Depth

dpth1c <- partial(model1_full, pred.var = "Depth",  
                  n.trees = model1_full$n.trees, 
                  inv.link=exp,
                  recursive=FALSE,
                  grid.resolution=100,
                  ice=FALSE) %>%
  mutate(grp = "Model 1 (9.2%)")

dpth2c <- partial(model2_full, pred.var = "Depth",  
                  n.trees = model2_full$n.trees, 
                  inv.link=exp,
                  recursive=FALSE,
                  grid.resolution=100,
                  ice=FALSE)%>%
  mutate(grp = "Model 2 (9.4%)")

dpth1c <- bind_rows(dpth1c, dpth2c)

dpth <- ggplot(dpth1c, aes(x=Depth, y=yhat, col=grp)) +
  geom_line(aes(lty=grp), linewidth=1) +
  scale_color_manual(values = c("black", "red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.2),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("Depth (m)") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

dpth


## Skewness

skw2c <- partial(model2_full, pred.var = "SST_Skewness",  
                 n.trees = model2_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE) %>%
  mutate(grp = "Model 2 (7.3%)")



skwc <- ggplot(skw2c, aes(x=SST_Skewness, y=yhat, col=grp)) +
  geom_line(lty=2, linewidth=1) +
  scale_color_manual(values = c("red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.8),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("SST Skewness") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

skwc


### Cumulative DHW

cdhw1c <- partial(model1_full, pred.var = "cumDHW",  
                  n.trees = model1_full$n.trees, 
                  inv.link=exp,
                  recursive=FALSE,
                  grid.resolution=100,
                  ice=FALSE) %>%
  mutate(grp = "Model 1 (6.9%)")



cdhwc <- ggplot(cdhw1c, aes(x=cumDHW, y=yhat, col=grp)) +
  geom_line(aes(lty=grp), linewidth = 1) +
  scale_color_manual(values = c("black"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.8),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("Cumulative DHW (°C-weeks)") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

cdhwc


## Climate stress model

## Skewness

csm2c <- partial(model2_full, pred.var = "ClimateStressModel",  
                 n.trees = model2_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE) %>%
  mutate(grp = "Model 2 (6.0%)")



csmc <- ggplot(csm2c, aes(x=ClimateStressModel, y=yhat, col=grp)) +
  geom_line(lty=2, linewidth = 1) +
  scale_color_manual(values = c("red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.2),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("Climate stress model") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

csmc


## Kurtosis
kurt1c <- partial(model1_full, pred.var = "SST_Kurtosis",  
                  n.trees = model1_full$n.trees, 
                  inv.link=exp,
                  recursive=FALSE,
                  grid.resolution=100,
                  ice=FALSE) %>%
  mutate(grp = "Model 1 (5.6%)")



kurtc <- ggplot(kurt1c, aes(x=SST_Kurtosis, y=yhat, col=grp)) +
  geom_line(aes(lty=grp), linewidth = 1) +
  scale_color_manual(values = c("black"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.2),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("SST Kurtosis") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

kurtc


### Mean wave energy
wav1c <- partial(model1_full, pred.var = "mean.waveNRJ",  
                 n.trees = model1_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE) %>%
  mutate(grp = "Model 1 (5.0%)")

wav2c <- partial(model2_full, pred.var = "mean.waveNRJ",  
                 n.trees = model2_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE)%>%
  mutate(grp = "Model 2 (4.1%)")

wav1c <- bind_rows(wav1c, wav2c)

wavc <- ggplot(wav1c, aes(x=mean.waveNRJ, y=yhat, col=grp)) +
  geom_line(aes(lty=grp), linewidth = 1) +
  scale_color_manual(values = c("black", "red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.8),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("Mean wave energy (kW/m)") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

wavc


## Current velocity
cur2c <- partial(model2_full, pred.var = "Current_vel_mean",  
                 n.trees = model2_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE) %>%
  mutate(grp = "Model 2 (4.6%)")



curc <- ggplot(cur2c, aes(x=Current_vel_mean, y=yhat, col=grp)) +
  geom_line(lty=2, linewidth = 1) +
  scale_color_manual(values = c("red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.2,0.2),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("Current velocity (m/s)") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

curc

## Dissolved oxygen

dis1c <- partial(model1_full, pred.var = "Dis_oxygen",  
                 n.trees = model1_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE) %>%
  mutate(grp = "Model 1 (4.3%)")

dis2c <- partial(model2_full, pred.var = "Dis_oxygen",  
                 n.trees = model2_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE)%>%
  mutate(grp = "Model 2 (2.4%)")

dis1c <- bind_rows(dis1c, dis2c)

disc <- ggplot(dis1c, aes(x=Dis_oxygen, y=yhat, col=grp)) +
  geom_line(aes(lty=grp), linewidth = 1) +
  scale_color_manual(values = c("black", "red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.8),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("Dissolved oxygen (ml/l)") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

disc


### Country

ctry1c <- partial(model1_full, pred.var = "Country",  
                  n.trees = model1_full$n.trees, 
                  inv.link=exp,
                  recursive=FALSE,
                  grid.resolution=100,
                  ice=FALSE) %>%
  mutate(grp = "Model 1 (4.0%)")

ctry2c <- partial(model2_full, pred.var = "Country",  
                  n.trees = model2_full$n.trees, 
                  inv.link=exp,
                  recursive=FALSE,
                  grid.resolution=100,
                  ice=FALSE)%>%
  mutate(grp = "Model 2 (3.6%)")

ctry1c <- bind_rows(ctry1c, ctry2c)

ctry1c$Country <- fct_recode(ctry1c$Country, FrenchEI = "French Southern Territories", S.Africa = "South Africa")

ctryc <- ggplot(ctry1c, aes(x=reorder(Country, -yhat), y=yhat, fill=grp)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("black", "red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.85),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.background = element_rect(fill="transparent"),
        panel.grid = element_blank()) +
  xlab("Country") +
  ylab("Number of Coral taxa") +
  guides(lty="none") +
  scale_y_continuous(limits = c(0,30))

ctryc


### SST rate of rise

ror1c <- partial(model1_full, pred.var = "SSTRateofRise",  
                 n.trees = model1_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE) %>%
  mutate(grp = "Model 1 (3.3%)")

ror2c <- partial(model2_full, pred.var = "SSTRateofRise",  
                 n.trees = model2_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE)%>%
  mutate(grp = "Model 2 (4.0%)")

ror1c <- bind_rows(ror1c, ror2c)

rorc <- ggplot(ror1c, aes(x=SSTRateofRise, y=yhat, col=grp)) +
  geom_line(aes(lty=grp), linewidth = 1) +
  scale_color_manual(values = c("black", "red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.2,0.2),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("SST rate of rise (°C)") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

rorc


### Salinity

sal1c <- partial(model1_full, pred.var = "Salinity_mean",  
                 n.trees = model1_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE) %>%
  mutate(grp = "Model 1 (4.0%)")

sal2c <- partial(model2_full, pred.var = "Salinity_mean",  
                 n.trees = model2_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE)%>%
  mutate(grp = "Model 2 (2.3%)")

sal1c <- bind_rows(sal1c, sal2c)

salc <- ggplot(sal1c, aes(x=Salinity_mean, y=yhat, col=grp)) +
  geom_line(aes(lty=grp), linewidth = 1) +
  scale_color_manual(values = c("black", "red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.2,0.2),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab("Salinity (PSS)") +
  ylab("Number of Coral taxa") +
  guides(lty="none")

salc


## Calcite

clc1c <- partial(model1_full, pred.var = "Calcite",  
                 n.trees = model1_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE) %>%
  mutate(grp = "Model 1 (3.9%)")

clc2c <- partial(model2_full, pred.var = "Calcite",  
                 n.trees = model2_full$n.trees, 
                 inv.link=exp,
                 recursive=FALSE,
                 grid.resolution=100,
                 ice=FALSE)%>%
  mutate(grp = "Model 2 (3.7%)")

clc1c <- bind_rows(clc1c, clc2c)

clcc <- ggplot(clc1c, aes(x=Calcite, y=yhat, col=grp)) +
  geom_line(aes(lty=grp), linewidth = 1) +
  scale_color_manual(values = c("black", "red"), name="Relative influence, %") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.8,0.2),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank()) +
  xlab(expression("Calcite concentration (mol/m" ^"3" *")")) +
  ylab("Number of Coral taxa") +
  guides(lty="none")

clcc

obsc + dpth + skwc + cdhwc + csmc + kurtc + wavc + curc + disc + ctryc + rorc + salc + plot_layout(ncol=4, nrow=3)

ggsave(filename = here("outputs", "figure1.pdf"), height = 12, width = 24)





#-------
# Figure 2a-d

# Load saved models
model_climatevars_full <- read_rds(here("models", "model_climatevars_full.Rds"))
model_humanvars_full <- read_rds(here("models", "model_humanvars_full.Rds"))


# Prepare data used for modeling
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

### Plot variable importance plots

vip.climatevars <- vip(model_climatevars_full, num_features =29, aesthetics = list(fill = "grey70", color= "black")) + 
  theme_bw(base_size = 14) + theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank()) +
  geom_hline(yintercept = 100/8, lty=2) + 
  labs(y="Importance, %", x= "Number of coral taxa", subtitle = "Climate variables") +
  scale_x_discrete(labels = str_wrap(c("Hard coral cover (%)", "SST median (°C)", "SST bimodality", "SST kurtosis", "Cumulative DHW (°C-weeks)", "Depth (m)", "SST rate of rise (°C)", "SST skewness"), width = 15))

vip.climatevars


vip.humanvars <- vip(model_humanvars_full, num_features =29, aesthetics = list(fill = "grey70", color= "black")) +
  theme_bw(base_size = 14) + theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank()) +
  geom_hline(yintercept = 100/9, lty=2) + 
  labs(y="Importance, %", x= "", subtitle = "Human variables") +
  scale_x_discrete(labels = str_wrap(c("Management", "Travel time to the nearest population (hrs)", "Nutrients (nitrogen, tons/km2)", "Sediments (tons/km2)", "Gravity to nearest city (population/travel time (hrs)2)", "Travel time to market (hrs)", "Reef visitation value (No. of tourist visits)", "Gravity to nearest population (population/travel time (hrs)2)", "Country"), width = 35))

vip.humanvars

# join plots
vip.climatevars + vip.humanvars + plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave(here("outputs", "figure2a.pdf"), height = 6, width = 12)



##### Partial dependence plots

#### Skewness 
climatevars.skw <- model_climatevars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "SST_Skewness",  
          n.trees = model_climatevars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedClim) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("SST skewness") +
  ylab("")

climatevars.skw



### Bimodality
climatevars.bmod <- model_climatevars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "SST_bimodality",  
          n.trees = model_climatevars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedClim) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("SST bimodality") +
  ylab("")

climatevars.bmod



### Cumulative DHW

climatevars.cdhw <- model_climatevars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "cumDHW",  
          n.trees = model_climatevars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedClim) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("Cumulative DHW (°C-weeks)") +
  ylab("")

climatevars.cdhw


###Kurtosis
climatevars.kurt <- model_climatevars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "SST_Kurtosis",  
          n.trees = model_climatevars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedClim) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("SST kurtosis") +
  ylab("")

climatevars.kurt

### SST rate of rise
climatevars.ror <- model_climatevars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "SSTRateofRise",  
          n.trees = model_climatevars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedClim) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("SST rate of rise (°C)") +
  ylab("")

climatevars.ror



### Depth
climatevars.dpth <- model_climatevars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Depth",  
          n.trees = model_climatevars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedClim) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("Depth (m)") +
  ylab("")

climatevars.dpth



climatevars.sst <- model_climatevars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "SST_Median",  
          n.trees = model_climatevars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedClim) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("SST median (°C)") +
  ylab("")

climatevars.sst



### Coral cover

climatevars.crl <- model_climatevars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Coralcover",  
          n.trees = model_climatevars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedClim) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("Hard coral cover (%)") +
  ylab("")

climatevars.crl




climatevars.skw + climatevars.ror + climatevars.dpth + climatevars.cdhw + climatevars.kurt + climatevars.bmod + climatevars.sst + climatevars.crl  + plot_layout(ncol=3) + plot_annotation(tag_levels = "i", tag_suffix = ")")

ggsave(here("outputs", "figure2b.pdf"), height = 10, width = 12)


#### Human variables

humanvars.ctry <- model_humanvars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Country",  
          n.trees = model_humanvars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  mutate(Country = recode(Country, `French Southern Territories` = "FrenchEI", `South Africa` = "S.Africa"))%>%
 # mutate(Country = recode(Country, S.Africa = "South Africa"))%>%
  ggplot(aes(x=reorder(Country,-yhat),y=yhat)) +
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12, angle = 90), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("Country") +
  ylab("")

humanvars.ctry



## Gravity

humanvars.grvnp <- model_humanvars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Grav_NP",  
          n.trees = model_humanvars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedHum) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab(str_wrap("Gravity to nearest population (population/travel time (hrs)2)", width = 35)) +
  ylab("")

humanvars.grvnp



## Travel time population

humanvars.ttpop <- model_humanvars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "TT_pop_hrs",  
          n.trees = model_humanvars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedHum) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab(str_wrap("Travel time to the nearest population (hrs)", width=25)) +
  ylab("")

humanvars.ttpop


## Travel time market

humanvars.ttmkt <- model_humanvars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "TT_market_hrs",  
          n.trees = model_humanvars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedHum) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab(str_wrap("Travel time to the nearest market (hrs)", width=25)) +
  ylab("")

humanvars.ttmkt



humanvars.sdmt <- model_humanvars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "andrello_sediments",  
          n.trees = model_humanvars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedHum) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("Sediments (tons/km2)") +
  ylab("")

humanvars.sdmt


### Reef value
humanvars.rval <- model_humanvars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "andrello_reef_value",  
          n.trees = model_humanvars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedHum) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab(str_wrap("Reef visitation value (No. of tourist visits)", width = 25)) +
  ylab("")

humanvars.rval


## Gravity cities

humanvars.grvnc <- model_humanvars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Grav_NC",  
          n.trees = model_humanvars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedHum) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab(str_wrap("Gravity to nearest city (population/travel time (hrs)2)", width = 35)) +
  ylab("")

humanvars.grvnc


### Nutrients 
humanvars.ntrt <- model_humanvars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "andrello_nutrients",  
          n.trees = model_humanvars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  autoplot(smooth = FALSE, rug = TRUE, train = Coral_selectedHum) +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab(str_wrap("Nutrients (nitrogen, tons/km2)", width = c(25))) +
  ylab("")

humanvars.ntrt


### Management

humanvars.mgt <- model_humanvars_full %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Management",  
          n.trees = model_humanvars_full$n.trees, 
          inv.link=exp,
          recursive=FALSE,
          grid.resolution=100,
          ice=FALSE) %>%
  ggplot(aes(x=reorder(Management,-yhat),y=yhat)) +
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) + 
  theme(axis.text = element_text(color = "black", size=12, angle = 90), panel.grid = element_blank())+
  #xlim(5.5,6) +
  xlab("Management") +
  ylab("")

humanvars.mgt



humanvars.ctry  + humanvars.grvnp +  humanvars.rval + 
  humanvars.ttmkt +humanvars.grvnc + humanvars.sdmt +  humanvars.ntrt + humanvars.ttpop + humanvars.mgt + plot_layout(ncol=3) + plot_annotation(tag_levels = "i", tag_suffix = ")")

ggsave(here("outputs", "figure2c.pdf"), height = 10, width = 12)


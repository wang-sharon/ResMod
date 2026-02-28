# load packages
library(lavaan)
library(Rmisc) # to generate data summary, summarySE
library(tidyverse) # to plot moderation graphs
library(semTools) #lavInspect
library(stats)
library(semPlot) #semPaths

# Aim 1 -------------------------------------------------------------------

# no longer needed if dichotomizing CSA, ASA, and creating interaction term CSA_ASA
# make victimization levels into factors
# clean_data$vic <- as.factor(clean_data$vic)
# clean_data$vic <- factor(clean_data$vic, levels = c("no_vic", "CSA_only",
#                                                          "ASA_only", "CSA_ASA"))
# 
# # make CSA the reference group
# clean_data$vic <- relevel(clean_data$vic, ref = "no_vic")

# THE regression model - run this!
Model1 <- lm(PTSD ~ csayn + ASA + csayn_ASA, data = clean_data) 
## i haven't included covariates here because i'm not sure that is needed, 
## conceptually. also b/c when i run it with covariates, some sort of 
## multicollinearity problem occurs

summary(Model1)
confint(Model1) # confidence intervals

# alt regression model - to double check Model1
# Model1.1 <- lm(PTSD ~ vic, data = clean_data)
# summary(Model1.1)

# graph
# reorder vic
clean_data$vic <- factor(clean_data$vic, levels = c("no_vic", "CSA_only",
                                                         "ASA_only", "CSA_ASA"))

data_summary <- summarySE(clean_data[!is.na(clean_data$vic), ], 
                          measurevar = "PTSD", 
                          groupvars = "vic", na.rm = T)

ggplot(data_summary, aes(x = vic, y = PTSD)) +
  geom_bar(position = position_dodge(), stat="identity", fill="gray") +
  geom_errorbar(aes(ymin = PTSD-se, ymax=PTSD+se), width=.2) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA)) +
  ylab("PTSS Severity") +
  xlab("Victimization") +
  coord_cartesian(ylim= c(min(data_summary$PTSD-1.5*data_summary$se),
                          max(data_summary$PTSD+1.5*data_summary$se))) 

# Aim 2 -------------------------------------------------------------------

# standardize/z-score
clean_data$mcDERS <- scale(clean_data$PosDERS, scale = F)
clean_data$mcCSI <- scale(clean_data$CSI_Total, scale = F)
clean_data$mcSMQ <- scale(clean_data$SMQ_tot, scale = F)

## Emotion regulation ------------------------------------------------------

# create interaction terms
clean_data$csayn_mcDERS <- clean_data$csayn * clean_data$mcDERS
clean_data$ASA_mcDERS <- clean_data$ASA * clean_data$mcDERS
clean_data$csayn_ASA <- clean_data$csayn * clean_data$ASA
clean_data$csayn_ASA_mcDERS <- clean_data$csayn * clean_data$ASA * 
  clean_data$mcDERS

Mod.Model1 <- '
  # Main effects
  PTSD ~ csayn + ASA + mcDERS
  
  # Two-way interaction effects
  PTSD ~ csayn_mcDERS + ASA_mcDERS + csayn_ASA
  
  # Three-way interaction effect
  PTSD ~ csayn_ASA_mcDERS
'

Mod.Model1_fit <- sem(Mod.Model1, data = clean_data, estimator = "MLR", 
                      missing = "ML")
summary(Mod.Model1_fit)
parameterestimates(Mod.Model1_fit) # confidence intervals

data_summary2 <- summarySE(clean_data[!is.na(clean_data$vic), ], 
                          measurevar = "mcDERS", 
                          groupvars = "vic", na.rm = T)

ggplot(data_summary2, aes(x = vic, y = mcDERS)) +
  geom_bar(position = position_dodge(), stat="identity", fill="gray") +
  geom_errorbar(aes(ymin = mcDERS-se, ymax=mcDERS+se), width=.2) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA)) +
  ylab("mcDERS") +
  xlab("Victimization")

## how to interpret?

## Mindfulness -------------------------------------------------------------

# create interaction terms
clean_data$csayn_mcSMQ <- clean_data$csayn * clean_data$mcSMQ
clean_data$ASA_mcSMQ <- clean_data$ASA * clean_data$mcSMQ
clean_data$csayn_ASA_mcSMQ <- clean_data$csayn * clean_data$ASA * 
  clean_data$mcSMQ

Mod.Model2 <- '
  # Main effects
  PTSD ~ csayn + ASA + mcSMQ
  
  # Two-way interaction effects
  PTSD ~ csayn_mcSMQ + ASA_mcSMQ + csayn_ASA
  
  # Three-way interaction effect
  PTSD ~ csayn_ASA_mcSMQ
'

Mod.Model2_fit <- sem(Mod.Model2, data = clean_data, estimator = "MLR", 
                      missing = "ML")
summary(Mod.Model2_fit)
parameterestimates(Mod.Model2_fit) # confidence intervals

data_summary3 <- summarySE(clean_data[!is.na(clean_data$vic), ], 
                           measurevar = "mcSMQ", 
                           groupvars = "vic", na.rm = T)

ggplot(data_summary3, aes(x = vic, y = mcSMQ)) +
  geom_bar(position = position_dodge(), stat="identity", fill="gray") +
  geom_errorbar(aes(ymin = mcSMQ-se, ymax=mcSMQ+se), width=.2) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA)) +
  ylab("mcSMQ") +
  xlab("Victimization")

## Coping strategies -------------------------------------------------------

# create interaction terms
clean_data$csayn_mcCSI <- clean_data$csayn * clean_data$mcCSI
clean_data$ASA_mcCSI <- clean_data$ASA * clean_data$mcCSI
clean_data$csayn_ASA_mcCSI <- clean_data$csayn * clean_data$ASA * 
  clean_data$mcCSI

Mod.Model3 <- '
  # Main effects
  PTSD ~ csayn + ASA + mcCSI
  
  # Two-way interaction effects
  PTSD ~ csayn_mcCSI + ASA_mcCSI + csayn_ASA
  
  # Three-way interaction effect
  PTSD ~ csayn_ASA_mcCSI
'

Mod.Model3_fit <- sem(Mod.Model3, data = clean_data, estimator = "MLR", 
                      missing = "ML")
summary(Mod.Model3_fit)
parameterestimates(Mod.Model3_fit) # confidence intervals

data_summary4 <- summarySE(clean_data[!is.na(clean_data$vic), ], 
                           measurevar = "mcCSI", 
                           groupvars = "vic", na.rm = T)

ggplot(data_summary4, aes(x = vic, y = mcCSI)) +
  geom_bar(position = position_dodge(), stat="identity", fill="gray") +
  geom_errorbar(aes(ymin = mcCSI-se, ymax=mcCSI+se), width=.2) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA)) +
  ylab("mcCSI") +
  xlab("Victimization")

# Aim 3 -------------------------------------------------------------------

# CFA
CFA <- '
  # latent variable for resilience (allow DERS loading to be freely estimated)
  Res =~ NA*mcDERS + mcSMQ + mcCSI

  # variances
  Res ~~ 1*Res

  # regression
  PTSD ~ csayn + ASA + csayn_ASA + Res
'

CFA_fit <- cfa(CFA, data = clean_data)
summary(CFA_fit, rsquare = T, standardized = T, fit.measures = T)

# path model
semPaths(CFA_fit, "model", "est",
         edge.color="Black",
         layout="tree",
         label.cex=1.5,
         edge.label.cex=1)

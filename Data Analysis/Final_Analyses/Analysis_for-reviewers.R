# load packages
library(MASS) #glm.nb
library(Rmisc) # to generate data summary, summarySE
library(sjPlot) # to plot moderation graphs
library(ggplot2)
library(stats)
library(emmeans) # pairwise comparisons

# finalize operationalization

# PTSD_CSAonly <- VOI_data$PTSD[VOI_data$vic == "CSA_only"]
# PTSD_ASAonly <- VOI_data$PTSD[VOI_data$vic == "ASA_only"]
# PTSD_CSA_ASA <- VOI_data$PTSD[VOI_data$vic == "CSA_ASA"]
# t.test(PTSD_ASAonly, PTSD_CSA_ASA)
## there's a sig difference between PTSD scores of CSA only, ASA only, and CSA +
## ASA - probably shouldn't lump together

# Aim 1 -------------------------------------------------------------------

# organize vic variable
clean_data$vic <- as.factor(clean_data$vic)
clean_data$vic <- factor(clean_data$vic, levels = c("CSA_only", "ASA_only", "CSA_ASA"))

# THE regression model - run this! Works only if you've subsetted dataset to
# exclude no vic
Model1 <- glm.nb(PTSD ~ vic, data = clean_data)
# can infer that if ASA = 0 and CSA_ASA = 0 then reference group is CSA_only

## i haven't included covariates here because i'm not sure that is needed, 
## conceptually. also b/c when i run it with covariates, some sort of 
## multicollinearity problem occurs

summary(Model1)
confint(Model1) # confidence intervals

# pairwise comparisons
pairs(emmeans(Model1, ~ vic))

# graph
data_summary <- summarySE(clean_data[!is.na(clean_data$vic), ], 
                          measurevar = "PTSD", 
                          groupvars = "vic", na.rm = T)

ggplot(data_summary, aes(x = vic, y = PTSD)) +
  geom_bar(position = position_dodge(), stat="identity", fill="gray") +
  geom_errorbar(aes(ymin = PTSD-se, ymax=PTSD+se), width=.2) +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    axis.title = element_text(size = 16),   # Increase font size for axis titles
    axis.text = element_text(size = 14)    # Increase font size for axis text
  ) +
  ylab("PTSS Severity") +
  xlab("Victimization") +
  coord_cartesian(ylim= c(min(data_summary$PTSD - 1.5 * data_summary$se),
                          max(data_summary$PTSD + 1.5 * data_summary$se)))

# results table
# tab_model(Model1, Main.Model_ERS, Main.Model_SMQ, Main.Model, 
#          pred.labels = c("CSA-only", "ASA-only", "CSA + ASA", "Emotion
#                          Regulation", "Mindfulness"),
#          dv.labels = "PTSS Severity")

tab_model(Model1, pred.labels = c("CSA-only", "ASA-only", "CSA + ASA"),
          dv.labels = "PTSS Severity")

# Aim 2 -------------------------------------------------------------------

# mean center
clean_data$mcERS <- as.numeric(scale(clean_data$ERS, scale = F))
clean_data$mcSMQ <- as.numeric(scale(clean_data$SMQ_tot, scale = F))

## Emotion regulation ------------------------------------------------------

# # create interaction terms
# clean_data$ASA_mcERS <- clean_data$ASA * clean_data$mcERS
# clean_data$CSA_ASA_mcERS <- clean_data$CSA_ASA * clean_data$mcERS

# moderation model
Mod.Model1 <- glm.nb(PTSD ~ vic + mcERS + vic * mcERS, data = clean_data)

summary(Mod.Model1)
confint(Mod.Model1) # confidence intervals

# main effects model
Main.Model_ERS <- glm.nb(PTSD ~ vic + mcERS, data = clean_data)
summary(Main.Model_ERS)

# pairwise comparisons
pairs(emmeans(Main.Model_ERS, ~ vic))

# graphs
data_summary2 <- summarySE(clean_data[!is.na(clean_data$vic), ], 
                          measurevar = "mcERS", 
                          groupvars = "vic", na.rm = T)

ggplot(data_summary2, aes(x = vic, y = mcERS)) +
  geom_bar(position = position_dodge(), stat="identity", fill="gray") +
  geom_errorbar(aes(ymin = mcERS-se, ymax=mcERS+se), width=.2) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA)) +
  ylab("mcERS") +
  xlab("Victimization")

# moderation plot
plot_model(Mod.Model1, type = "int", terms = c("mcERS", "ASA", "CSA_ASA"))

# table
tab_model(Mod.Model1, Main.Model_ERS, 
          pred.labels = c("CSA-only", "ASA-only", "CSA + ASA", "ERS", 
                          "ASA-only × ERS", "CSA + ASA × ERS"),
          dv.labels = "PTSS Severity")

# # descriptives
# x_min <- min(clean_data$vic, na.rm = T)
# x_max <- max(clean_data$vic, na.rm = T)
# 
# # plot: traditional, -1 SD, mean, +1 SD
# sd_mcERS <- sd(clean_data$mcERS, na.rm = T)
# Mod.Model1_plot <- probe2WayMC(Mod.Model1, 
#                                nameX = c("vic", "mcERS", "vic_mcERS"), 
#                                nameY = "PTSD", 
#                                modVar = "mcERS", 
#                                valProbe = c(-sd_mcERS, 0, sd_mcERS))
# plotProbe(Mod.Model1_plot, 
#           xlim = c(x_min, x_max),
#           xlab = "Revictimization", 
#           ylab = "PTSS Severity", 
#           legendArgs = list(x = "topleft"))


## Mindfulness -------------------------------------------------------------

Mod.Model2 <- glm.nb(PTSD ~ vic + mcSMQ + vic * mcSMQ, data = clean_data)
summary(Mod.Model2)
confint(Mod.Model2) # confidence intervals

# main effects model
Main.Model_SMQ <- glm.nb(PTSD ~ vic + mcSMQ, data = clean_data)
summary(Main.Model_SMQ)

# pairwise comparisons
pairs(emmeans(Main.Model_SMQ, ~ vic))

# graphs
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

# moderation plot
plot_model(Mod.Model2, type = "int", terms = c("mcSMQ", "ASA", "CSA_ASA"))

# table
tab_model(Mod.Model2, Main.Model_SMQ, 
          pred.labels = c("CSA-only", "ASA-only", "CSA + ASA", "Mindfulness", 
                          "ASA-only × Mindfulness", "CSA + ASA × Mindfulness"),
          dv.labels = "PTSS Severity")

## Coping strategies -------------------------------------------------------
# 
# # create interaction terms
# clean_data$csayn_mcCSI <- clean_data$csayn * clean_data$mcCSI
# clean_data$ASA_mcCSI <- clean_data$ASA * clean_data$mcCSI
# clean_data$csayn_ASA_mcCSI <- clean_data$csayn * clean_data$ASA * 
#   clean_data$mcCSI
# 
# Mod.Model3 <- '
#   # Main effects
#   PTSD ~ csayn + ASA + mcCSI
#   
#   # Two-way interaction effects
#   PTSD ~ csayn_mcCSI + ASA_mcCSI + csayn_ASA
#   
#   # Three-way interaction effect
#   PTSD ~ csayn_ASA_mcCSI
# '
# 
# Mod.Model3_fit <- sem(Mod.Model3, data = clean_data, estimator = "MLR", 
#                       missing = "ML")
# summary(Mod.Model3_fit)
# parameterestimates(Mod.Model3_fit) # confidence intervals
# 
# data_summary4 <- summarySE(clean_data[!is.na(clean_data$vic), ], 
#                            measurevar = "mcCSI", 
#                            groupvars = "vic", na.rm = T)
# 
# ggplot(data_summary4, aes(x = vic, y = mcCSI)) +
#   geom_bar(position = position_dodge(), stat="identity", fill="gray") +
#   geom_errorbar(aes(ymin = mcCSI-se, ymax=mcCSI+se), width=.2) +
#   theme(panel.background = element_rect(fill='transparent'),
#         plot.background = element_rect(fill='transparent', color=NA)) +
#   ylab("mcCSI") +
#   xlab("Victimization")


# Aim 3 -------------------------------------------------------------------
Main.Model <- glm.nb(PTSD ~ vic + mcERS + mcSMQ, data = clean_data)
summary(Main.Model)
pairs(emmeans(Main.Model, ~ vic))

Cumul.Model <- glm.nb(PTSD ~ vic + mcERS + mcSMQ + vic * mcERS * mcSMQ, 
                      data = clean_data)
summary(Cumul.Model)

# table
tab_model(Cumul.Model, Main.Model,
          pred.labels = c("CSA-only", "ASA-only", "CSA + ASA", "ER", "SMQ",
                          "ASA-only × ER", "CSA + ASA × ER", "ASA-only × SMQ",
                          "CSA + ASA × SMQ", "ER × SMQ", "ASA-only × ER × SMQ",
                          "CSA + ASA × ER × SMQ"),
          dv.labels = "PTSS Severity")

# moderation plot
plot_model(Cumul.Model, type = "int", terms = c("mcSMQ", "mcERS", "ASA",
                                                "CSA_ASA"))

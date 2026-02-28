
# load packages
library(tidyverse) # to plot moderation graphs
library(semTools) #lavInspect
library(Rmisc) # to generate data summary, summarySE
library(sjPlot)
library(sjmisc)
library(ggpubr)


# Internal consistency ----------------------------------------------------
calculate_IC <- function(items) {
  IC <- cronbach.alpha(items, na.rm = T)
  return(IC$alpha)
}

DERS_IC_items <- VOI_data[c("ders01r", "ders02r", "ders03", "ders04", "ders05", 
                            "ders06r", "ders07r", "ders08r", "ders09", 
                            "ders10r", "ders11", "ders12", "ders13", "ders14", 
                            "ders15", "ders16", "ders17r", "ders18", "ders19", 
                            "ders20r", "ders21", "ders22r", "ders23", "ders24r",
                            "ders25", "ders26", "ders27", "ders28", "ders29", 
                            "ders30", "ders31", "ders32", "ders33", "ders34", 
                            "ders35", "ders36")]
SMQ_IC_items <- VOI_data[c("smq01", "smq02r", "smq03r", "smq04", "smq05", 
                           "smq06r", "smq07", "smq08r", "smq09", "smq10", 
                           "smq11", "smq12r", "smq13r", "smq14r", "smq15", 
                           "smq16r")]
IC_list <- list(VOI_data[PTSD], DERS_IC_items, SMQ_IC_items, VOI_data[CSI])

for (item in IC_list) {
  alphas <- calculate_IC(item)
  print(alphas)
}

# # testing
# cronbach.alpha(VOI_data[CSI], na.rm = T)$alpha

# Z-score -----------------------------------------------------------------

clean_data$zDERS <- scale(clean_data$PosDERS)
clean_data$zCSI <- scale(clean_data$CSI_Total)
clean_data$zSMQ <- scale(clean_data$SMQ_tot)

# Aim 1 -------------------------------------------------------------------

Model1 <- '
  PTSD ~ b1*numvic
'

Model1_fit <- sem(Model1, data = clean_data,
                  estimator = "MLR", missing = "ML")
summary(Model1_fit, fit.measures = T, standardized = T, rsquare = T)
parameterestimates(Model1_fit) # confidence intervals

# path diagram
# semPaths(Model1_fit, "model", "est",
#          edge.color="Black",
#          layout = "tree",
#          rotation = 2,
#          sizeMan = 10,
#          nCharNodes = 0,
#          label.cex=1.5,
#          edge.label.cex=1)

# graph
clean_data$numvic <- as.numeric(as.vector(clean_data$numvic))
data_summary <- summarySE(clean_data, 
                          measurevar = "PTSD", 
                          groupvars = "numvic", na.rm = TRUE)

ggplot(data_summary, aes(x = numvic, y = PTSD)) +
  geom_bar(position = position_dodge(), stat="identity", fill="gray") +
  geom_errorbar(aes(ymin = PTSD-se, ymax=PTSD+se), width=.2) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA)) +
  ylab("PTS Symptoms") +
  xlab("Victimization Time Periods") +
  coord_cartesian(ylim= c(min(data_summary$PTSD-1.5*data_summary$se),
                          max(data_summary$PTSD+1.5*data_summary$se))) 


# Aim 2 -------------------------------------------------------------------

## Model 1: emotion regulation as a moderator? -----------------------------
clean_data$vicDERS <-  clean_data$numvic * clean_data$zDERS

Mod.Model1 <- '
  PTSD ~ b1*numvic + b2*zDERS + b3*vicDERS

  numvic ~~ zDERS + vicDERS
  zDERS ~~ vicDERS
'

Mod.Model1_fit <- sem(Mod.Model1, data = clean_data, 
                      estimator = "MLR", missing = "ML")
summary(Mod.Model1_fit, fit.measures = T, standardized = T, rsquare = T)
parameterestimates(Mod.Model1_fit)

# emotion regulation as compensatory?
Mod.Model1.1 <- '
  PTSD ~ b1*numvic + b2*zDERS

  numvic ~~ zDERS
'

Mod.Model1.1_fit <- sem(Mod.Model1.1, data = clean_data, estimator = "MLR",
                        missing = "ML")
summary(Mod.Model1.1_fit, fit.measures = T, standardized = T, rsquare = T)
parameterestimates(Mod.Model1.1_fit)

# histogram of relationship between ptsd and DERS
ggplot(clean_data, aes(x=zDERS, y=PTSD)) + 
  geom_point()  +
  geom_smooth(method = lm, se=FALSE) 

# path diagram
# semPaths(Mod.Model1_fit, "model", "est",
#          residuals = F,
#          intercepts = F,
#          edge.color="Black", 
#          layout = "tree",
#          rotation = 2, 
#          sizeMan = 10, 
#          nCharNodes = 0, 
#          label.cex=1.5, 
#          edge.label.cex=1)

# descriptives
x_min <- min(clean_data$numvic, na.rm = T)
x_max <- max(clean_data$numvic, na.rm = T)

# plot: traditional, -1 SD, mean, +1 SD
Mod.Model1_plot <- probe2WayMC(Mod.Model1_fit, 
                           nameX = c("numvic", "zDERS", "vicDERS"), 
                           nameY = "PTSD", 
                           modVar = "zDERS", 
                           valProbe = c(-1, 0, 1))
plotProbe(Mod.Model1_plot, 
          xlim = c(x_min, x_max), 
          xlab = "Number of Victimization Experiences", 
          ylab = "PTS symptoms", 
          legendArgs = list(x = "topleft"))


## Model 2: mindfulness as a moderator? ------------------------------------
clean_data$vicSMQ <- clean_data$numvic * clean_data$zSMQ

Mod.Model2 <- '
  PTSD ~ b1*numvic + b2*zSMQ + b3*vicSMQ

  numvic ~~ zSMQ + vicSMQ
  zSMQ ~~ vicSMQ
'

Mod.Model2_fit <- sem(Mod.Model2, data = clean_data,
                      estimator = "MLR", missing = "ML")
summary(Mod.Model2_fit, fit.measures = T, standardized = T, rsquare = T)
parameterestimates(Mod.Model2_fit)

# mindfulness as compensatory
Mod.Model2.1 <- '
  PTSD ~ b1*numvic + b2*zSMQ

  numvic ~~ zSMQ
'
Mod.Model2.1_fit <- sem(Mod.Model2.1, data = clean_data,
                      estimator = "MLR", missing = "ML")
summary(Mod.Model2.1_fit, fit.measures = T, standardized = T, rsquare = T)
parameterestimates(Mod.Model2.1_fit)

# histogram of relationship between ptsd and SMQ
ggplot(clean_data, aes(x=zSMQ, y=PTSD)) + 
  geom_point()  +
  geom_smooth(method = lm, se=FALSE) 

# path diagram
# semPaths(Mod.Model2_fit, "model", "est",
#          residuals = F,
#          intercepts = F,
#          edge.color="Black", 
#          layout = "tree",
#          rotation = 2, 
#          sizeMan = 10, 
#          nCharNodes = 0, 
#          label.cex=1.5, 
#          edge.label.cex=1)

# plot: traditional, -1 SD, mean, +1 SD
Mod.Model2_plot <- probe2WayMC(Mod.Model2_fit, 
                               nameX = c("numvic", "zSMQ", "vicSMQ"), 
                               nameY = "PTSD", 
                               modVar = "zSMQ", 
                               valProbe = c(-1,0,1))
plotProbe(Mod.Model2_plot, 
          xlim = c(x_min, x_max), 
          xlab = "Number of Victimization Experiences", 
          ylab = "PTS symptoms", 
          legendArgs = list(x = "topleft"))


## Model 3: coping strategies as a moderator? ------------------------------
clean_data$vicCSI <- clean_data$numvic * clean_data$zCSI

Mod.Model3 <- '
  PTSD ~ b1*numvic + b2*zCSI + b3*vicCSI

  numvic ~~ zCSI + vicCSI
  zCSI ~~ vicCSI
'

Mod.Model3_fit <- sem(Mod.Model3, data = clean_data,
                      estimator = "MLR", missing = "ML")
summary(Mod.Model3_fit, fit.measures = T, standardized = T, rsquare = T)
parameterestimates(Mod.Model3_fit)

# coping strategies as compensatory
Mod.Model3.1 <- '
  PTSD ~ b1*numvic + b2*zCSI

  numvic ~~ zCSI
'
Mod.Model3.1_fit <- sem(Mod.Model3.1, data = clean_data,
                      estimator = "MLR", missing = "ML")
summary(Mod.Model3.1_fit, fit.measures = T, standardized = T, rsquare = T)
parameterestimates(Mod.Model3.1_fit)

# histogram of relationship between ptsd and CSI
ggplot(clean_data, aes(x=zCSI, y=PTSD)) + 
  geom_point()  +
  geom_smooth(method = lm, se=FALSE) 

# path diagram
# semPaths(Mod.Model3_fit, "model", "est",
#          residuals = F,
#          intercepts = F,
#          edge.color="Black", 
#          layout = "tree",
#          rotation = 2, 
#          sizeMan = 10, 
#          nCharNodes = 0, 
#          label.cex=1.5, 
#          edge.label.cex=1)

# plot: traditional, -1 SD, mean, +1 SD
Mod.Model3_plot <- probe2WayMC(Mod.Model3_fit, 
                               nameX = c("numvic", "zCSI", "vicCSI"), 
                               nameY = "PTSD", 
                               modVar = "zCSI", 
                               valProbe = c(-1,0,1))
plotProbe(Mod.Model3_plot, 
          xlim = c(x_min, x_max), 
          xlab = "Number of Victimization Experiences", 
          ylab = "PTS symptoms", 
          legendArgs = list(x = "topleft"))


# Aim 3: additive effects --------------------------------------------------

## CFA? --------------------------------------------------------------------

# Additive.CFA <- '
#   Res =~ NA*PosDERS + SMQ_tot + CSI_Total
#   
#   # variances
#   Res ~~ 1*Res
# '
# 
# Run.CFA <- cfa(Additive.CFA, data = clean_data)
# summary(Run.CFA, rsquare=TRUE, standardized=TRUE, fit.measures = T)
# 
# # path model 
# semPaths(Run.CFA, "model", "est", 
#          edge.color="Black", 
#          layout="tree", 
#          label.cex=1.5, 
#          edge.label.cex=1)


## ## Complete model ----------------------------------------------------------
# 
# to examine the effects of each factor on PTS (holding all else equal)
# Comp.Model <- '
#   PTSD ~ b1*numvic + b2*zDERS + b3*zSMQ +
#     b4*zCSI
# 
#   numvic ~~ zDERS + zSMQ + zCSI
#   zDERS ~~ zSMQ + zCSI
#   zSMQ ~~ zCSI
# '
# 
# Comp.Model_fit <- sem(Comp.Model, data = clean_data,
#                       estimator = "MLR", missing = "ML")
# summary(Comp.Model_fit, fit.measures = T, standardized = T, rsquare = T)
 
## ## 4-way interaction -------------------------------------------------------
# clean_data$vicDERS_SMQ_CSI <- clean_data$numvic * clean_data$zDERS *
#   clean_data$zSMQ * clean_data$zCSI
# clean_data$vicDERS_SMQ <- clean_data$numvic * clean_data$zDERS *
#   clean_data$zSMQ
# clean_data$vicDERS_CSI <- clean_data$numvic * clean_data$zDERS * 
#   clean_data$zCSI
# clean_data$DERS_SMQ_CSI <- clean_data$zDERS *
#   clean_data$zSMQ * clean_data$zCSI
# clean_data$vicSMQ_CSI <- clean_data$numvic *clean_data$zSMQ * 
#   clean_data$zCSI
# clean_data$DERS_SMQ <- clean_data$zDERS * clean_data$zSMQ
# clean_data$DERS_CSI <- clean_data$zDERS * clean_data$zCSI
# clean_data$SMQ_CSI <- clean_data$zSMQ * clean_data$zCSI
# 
# 
# FourWay.Model <- '
#   PTSD ~ numvic + zDERS + zSMQ + zCSI +
#     SMQ_CSI + DERS_CSI + DERS_SMQ + vicDERS + vicSMQ + vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + b1*vicDERS_SMQ_CSI
# 
#   numvic ~~ zDERS + zSMQ + zCSI +
#     SMQ_CSI + DERS_CSI + DERS_SMQ + vicDERS + vicSMQ + vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   zDERS ~~ zSMQ + zCSI +
#     SMQ_CSI + DERS_CSI + DERS_SMQ + vicDERS + vicSMQ + vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   zSMQ ~~ zCSI +
#     SMQ_CSI + DERS_CSI + DERS_SMQ + vicDERS + vicSMQ + vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   zCSI ~~ SMQ_CSI + DERS_CSI + DERS_SMQ + vicDERS + vicSMQ + 
#     vicCSI + vicSMQ_CSI + DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + 
#     vicDERS_SMQ_CSI
#   SMQ_CSI ~~ DERS_CSI + DERS_SMQ + vicDERS + vicSMQ + vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   DERS_CSI ~~ DERS_SMQ + vicDERS + vicSMQ + vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   DERS_SMQ ~~ vicDERS + vicSMQ + vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   vicDERS ~~ vicSMQ + vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   vicSMQ ~~ vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   vicCSI ~~ vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   vicDERS ~~ vicSMQ + vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   vicSMQ ~~ vicCSI + vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   vicCSI ~~ vicSMQ_CSI +
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   vicSMQ_CSI ~~
#     DERS_SMQ_CSI + vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   DERS_SMQ_CSI ~~ vicDERS_CSI + vicDERS_SMQ + vicDERS_SMQ_CSI
#   vicDERS_CSI ~~ vicDERS_SMQ + vicDERS_SMQ_CSI
#   vicDERS_SMQ ~~ vicDERS_SMQ_CSI'
# 
# FourWay.Model_fit <- sem(FourWay.Model, data = clean_data, estimator = "MLR",
#                          missing = "ML")
# summary(FourWay.Model_fit, standardized = T, rsquare = T, fit.measures = T)


## Composite resilience score ----------------------------------------------

# create cumulative score
clean_data$resilience <- clean_data$zDERS + clean_data$zCSI + clean_data$zSMQ

# cumulative model
clean_data$vicRes <- clean_data$numvic * clean_data$resilience

Cumul.Model <- '
  PTSD ~ b1*numvic + b2*resilience + b3*vicRes

  numvic ~~ resilience + vicRes
  resilience ~~ vicRes
'  
Cumul.Model_fit <- sem(Cumul.Model, data = clean_data, estimator = "MLR",
                      missing = "ML")
summary(Cumul.Model_fit, fit.measures = T, standardized = T)
parameterestimates(Cumul.Model_fit)

Cumul.Model2 <- '
  PTSD ~ b1*numvic + b2*resilience

  numvic ~~ resilience
'
Cumul.Model2_fit <- sem(Cumul.Model2, data = clean_data, estimator = "MLR",
                        missing = "ML")
summary(Cumul.Model2_fit, fit.measures = T, standardized = T)
parameterestimates(Cumul.Model2_fit)

# histogram of relationship between ptsd and resilience
ggplot(clean_data, aes(x=resilience, y=PTSD)) + 
  geom_point()  +
  geom_smooth(method = lm, se=FALSE) 

# path diagram
# semPaths(Cumul.Model_fit, "model", "est",
#          residuals = F,
#          intercepts = F,
#          edge.color="Black", 
#          layout = "tree",
#          rotation = 2, 
#          sizeMan = 10, 
#          nCharNodes = 0, 
#          label.cex=1.5, 
#          edge.label.cex=1)

# plot: traditional, -1 SD, mean, +1 SD
Cumul.Model_plot <- probe2WayMC(Cumul.Model_fit, 
                               nameX = c("numvic", "resilience", "vicRes"), 
                               nameY = "PTSD", 
                               modVar = "resilience", 
                               valProbe = c(-1,0,1))
plotProbe(Cumul.Model_plot, 
          xlim = c(x_min, x_max), 
          xlab = "Victimization Experiences", 
          ylab = "PTS symptoms", 
          legendArgs = list(x = "topleft"))

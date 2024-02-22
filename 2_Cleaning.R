# Overview ---------------------------------------------------------------

# descriptives
describe(VOI_data[main])
## st(VOI_data[main]) # outputs in viewer
VOI_data$numvic <- as.numeric(as.vector(VOI_data$numvic))
table(VOI_data$numvic) # counts by levels of numvic

# correlation
cor(VOI_data[, c(main)], use = "complete.obs")

# histogram of relationship between ptsd and vic
ggplot(VOI_data, aes(x=numvic, y=pcltotV)) + 
  geom_point()  +
  geom_smooth(method = lm, se=FALSE) 

# histograms of variables that don't need cleaning
hist(VOI_data$SMQ_tot) # pretty normally distributed
hist(VOI_data$numvic)

# PTSD --------------------------------------------------------------------

hist(VOI_data$pcltotV) # positively skewed

# outliers
# PTSD_outliers <- which(VOI_data$pcltotV < 20)
# PTSD_outlierdata <- VOI_data[PTSD_outliers, ]
# VOI_data <- VOI_data[-PTSD_outliers, ]

# center PTSD on zero
VOI_data$PTSD <- VOI_data$pcltotV - 20

# Make NA where PTSD <0
VOI_data$PTSD <- replace(VOI_data$PTSD, VOI_data$PTSD < 0, NA)
hist(VOI_data$PTSD)

# # transformations
# VOI_data$lPTSD <- log(VOI_data$pcltotV)
# hist(VOI_data$lPTSD)
# describe(VOI_data$lPTSD) # skew = .46; kurtosis = -.042

# DERS --------------------------------------------------------------------

VOI_data$PosDERS <- -VOI_data$DERS_TotalScore
hist(VOI_data$PosDERS) # possible outliers

# # identify outliers
# DERS_outliers <- boxplot.stats(VOI_data$DERS_TotalScore)$out
# DERS_outlier_IDs <- which(VOI_data$DERS_TotalScore %in% DERS_outliers)
# DERS_outlier_data <- VOI_data[DERS_outlier_IDs, ]
# Data_noDERSoutlier <- VOI_data[-DERS_outlier_IDs,]
# describe(Data_noDERSoutlier$DERS_TotalScore) # skew = .56; kurtosis = -.21
# 
# # transformations
# VOI_data$lDERS <- log(VOI_data$DERS_TotalScore)
# hist(VOI_data$lDERS)
# describe(VOI_data$lDERS) # skew = .13; kurtosis = -.39


# CSI Problem Solving -----------------------------------------------------

### use total score###

hist(VOI_data$CSI_ProbSolving) # majorly negatively skewed

# # identify outliers
# CSI_ProbSolving_outliers <- boxplot.stats(VOI_data$CSI_ProbSolving)$out
# CSI_ProbSolving_outlier_IDs <- which(VOI_data$CSI_ProbSolving %in% CSI_ProbSolving_outliers)
# CSI_ProbSolving_outlier_data <- VOI_data[CSI_ProbSolving_outlier_IDs, ]
# Data_noCSIProbSolvingoutlier <- VOI_data[-CSI_ProbSolving_outlier_IDs,]
# describe(Data_noCSIProbSolvingoutlier$CSI_ProbSolving) # skew = -1.55; kurtosis = 1.23

# transformations
# VOI_data$lCSI_ProbSolving <- log(VOI_data$CSI_ProbSolving)
# hist(VOI_data$lCSI_ProbSolving)
# describe(VOI_data$lCSI_ProbSolving) # skew = -2.5; kurtosis = 7.69

# CSI Social Support ------------------------------------------------------

### use total score###

hist(VOI_data$CSI_SocialSupport) # majorly negatively skewed

# # identify outliers
# CSI_SocialSupport_outliers <- boxplot.stats(VOI_data$CSI_SocialSupport)$out
# CSI_SocialSupport_outlier_IDs <- which(VOI_data$CSI_SocialSupport %in% CSI_SocialSupport_outliers)
# CSI_SocialSupport_outlier_data <- VOI_data[CSI_SocialSupport_outlier_IDs, ]
# Data_noCSISocialSupportoutlier <- VOI_data[-CSI_SocialSupport_outlier_IDs,]
# describe(Data_noCSISocialSupportoutlier$CSI_SocialSupport) # skew = -1.26; kurtosis = 1.23
# 
# # transformation
# VOI_data$lCSI_SocialSupport <- log(VOI_data$CSI_SocialSupport)
# hist(VOI_data$lCSI_SocialSupport)
# describe(VOI_data$lCSI_SocialSupport) # skew = -2.53; kurtosis = 6.94

# CSI Avoidance -----------------------------------------------------------

### use total score###

hist(VOI_data$CSI_Avoidance) # negatively skewed

# # identify outliers
# CSI_Avoidance_outliers <- boxplot.stats(VOI_data$CSI_Avoidance)$out
# CSI_Avoidance_outlier_IDs <- which(VOI_data$CSI_Avoidance %in% CSI_Avoidance_outliers)
# CSI_Avoidance_outlier_data <- VOI_data[CSI_Avoidance_outlier_IDs, ]
# Data_noCSIAvoidanceoutlier <- VOI_data[-CSI_Avoidance_outlier_IDs,]
# describe(Data_noCSIAvoidanceoutlier$CSI_Avoidance) # skew = -.4; kurtosis = -.54
# 
# # transformation
# VOI_data$lCSI_Avoidance <- log(VOI_data$CSI_Avoidance)
# hist(VOI_data$lCSI_Avoidance) # still negatively skewed
# describe(VOI_data$lCSI_Avoidance) # skew = -.84; kurtosis = .9

# CSI Total ---------------------------------------------------------------

hist(VOI_data$CSI_Total)

# Finalized ---------------------------------------------------------------

final_main <- c("numvic", "PTSD", "PosDERS", "SMQ_tot", "CSI_Total")

clean_data <- VOI_data[c("ID",final_main)]

final_descriptives <- describe(clean_data[final_main])

# correlations
final_correlations <- cor(clean_data[final_main], use = "complete.obs")

correlation_variables <- c("numvic", "PTSD", "PosDERS", "SMQ_tot", "CSI_Total")
correlation_results <- data.frame(Variable1 = character(), 
                                  Variable2 = character(), 
                                  Correlation = numeric(), P_Value = numeric(), 
                                  stringsAsFactors = FALSE)

for (var1 in 1:(length(correlation_variables) - 1)) {
  for (var2 in (var1 + 1):length(correlation_variables)) {
    # Calculate correlation and p-value
    result <- cor.test(clean_data[[correlation_variables[var1]]], 
                       clean_data[[correlation_variables[var2]]])
    
    # Store results in the data frame
    correlation_results <- rbind(correlation_results, data.frame(
      Variable1 = correlation_variables[var1],
      Variable2 = correlation_variables[var2],
      Correlation = result$estimate,
      P_Value = result$p.value
    ))
  }
}
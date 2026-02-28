## clear environment
rm(list = ls())

# set working directory
# setwd("~/OneDrive - UW/ResMod (Second Year Project)/Data Analysis")

## load packages
library(haven) # reads spss files
library(knitr) # view subset of dataframe
library(psych) # descriptives
library(dplyr) # mutate/reverse code

## load raw spss
raw_data <- read_sav("Fresh2_Main_Study_clean-baseline-inlab-combined updated current (9.3.19).sav")

# Reorganize Variables ----------------------------------------------------

## isolate variables of interest (VOI)

# covariates <- c("ASASep_HR", "csasev")

demo <- c("income", "education", "jobstatus", "race", "hispanic", "age")


## PTSD --------------------------------------------------------------------


PTSD <- c("pcltotV", "ptsd01", "ptsd02", "ptsd03", "ptsd04", "ptsd05", "ptsd06",
          "ptsd07", "ptsd08", "ptsd09", "ptsd10", "ptsd11", "ptsd12", "ptsd13", 
          "ptsd14", "ptsd15", "ptsd16", "ptsd17", "ptsd18", "ptsd19","ptsd20")

# make baseline PTSD = 0
raw_data$PTSD <- raw_data$pcltotV - 20

# Make NA where PTSD < 0
raw_data$PTSD <- replace(raw_data$PTSD, raw_data$PTSD < 0, NA)

# remove under 21
raw_data <- subset(raw_data, age > 20)


## SA ----------------------------------------------------------------------

AdolSA <- grep("__14$", names(raw_data), value = TRUE)
AdultSA <- grep("__18$", names(raw_data), value = TRUE)


## Resilience --------------------------------------------------------------

all_DERS <- c("ders01", "ders02", "ders03", "ders04", "ders05", "ders06", "ders07", 
          "ders08", "ders09", "ders10", "ders11", "ders12", "ders13", "ders14", 
          "ders15", "ders16", "ders17", "ders18", "ders19", "ders20", "ders21", 
          "ders22", "ders23", "ders24", "ders25", "ders26", "ders27", "ders28", 
          "ders29", "ders30", "ders31", "ders32", "ders33", "ders34", "ders35", 
          "ders36", "ders01r", "ders02r", "ders06r", "ders07r", "ders08r",
          "ders10r", "ders17r", "ders20r", "ders22r", "ders24r", "ders34r")


### DERS --------------------------------------------------------------------

DERS <- c("ders01r", "ders02r", "ders03", "ders04", "ders05", "ders06r", "ders07r", 
           "ders08r", "ders09", "ders10r", "ders11", "ders12", "ders13", "ders14", 
           "ders15", "ders16", "ders17r", "ders18", "ders19", "ders20r", "ders21", 
           "ders22r", "ders23", "ders24r", "ders25", "ders26", "ders27", "ders28", 
           "ders29", "ders30", "ders31", "ders32", "ders33", "ders34r", "ders35", 
           "ders36")
# DERS_factors <- c("DERS_NonAccept", "DERS_Goal", "DERS_IMPULSE", "DERS_AWARE",  
  #                "DERS_STRATEGIES", "DERS_CLARITY", "DERS_TotalScore")

## reverse DERS

for (var in DERS) {
  raw_data[[paste0(var, "_reversed")]] <- 6 - raw_data[[var]]
} # this is correct

raw_data$ERS <- rowSums(raw_data[, grep("_reversed$", names(raw_data))], 
                        na.rm = TRUE)

# calculate what the expected ERS score is based off of DERS score
raw_data$expected_ERS <- (6 * 36) - raw_data$DERS_TotalScore

# identify discrepancies where ERS does not match expected_ERS
discrepant_IDs <- raw_data$ID[raw_data$ERS != raw_data$expected_ERS]

# impute mean for NAs
raw_data$ERS[which(raw_data$ID %in% discrepant_IDs)] <- 
  apply(raw_data[raw_data$ID %in% discrepant_IDs, 
                 grep("_reversed$", names(raw_data))], 1,
        function(x) mean(x, na.rm = T) * 36)

# check that it worked
# raw_data[raw_data$ID %in% discrepant_IDs, c("ID", "ERS")]
# describe(raw_data[c("DERS_TotalScore", "ERS")])


SMQ <- c("smq01", "smq02", "smq03", "smq04", "smq05", "smq06", "smq07", "smq08",
         "smq09", "smq10", "smq11", "smq12", "smq13", "smq14", "smq15", "smq16",
         "smq02r", "smq03r","smq06r", "smq08r", "smq12r", "smq13r", "smq14r", 
         "smq16r")

# # original coding for CSI items is wrong
# CSI <- c("csi01", "csi02", "csi03", "csi04", "csi05", "csi06", "csi07", "csi08",
#          "csi09", "csi10", "csi11", "csi12", "csi13", "csi14", "csi15", "csi16",
#          "csi17", "csi18", "csi19", "csi20", "csi21", "csi22", "csi23", "csi24", 
#          "csi25", "csi26", "csi27", "csi28", "csi29", "csi30", "csi31", "csi32", 
#          "csi33")
# CSI_avoidance <- c("csi04", "csi06", "csi10", "csi13", "csi18", "csi21", "csi22",
#                    "csi26", "csi27", "csi28", "csi30")
# 
# reversed_items <- reverse.code(keys = rep(-1, length(CSI_avoidance)),
#                                items = raw_data[, CSI_avoidance],
#                                mini = 1, 
#                                maxi = 3)  
# 
# names(CSI_avoidance_reversed)[names(CSI_avoidance_reversed) %in% CSI_avoidance] <- paste0(CSI_avoidance, "r")
# 
# # CSI_factors <- c("CSI_ProbSolving", "CSI_SocialSupport", "CSI_Avoidance")
# 
# raw_data$CSI_Total <- rowSums(raw_data[, c("csi01", "csi02", "csi03", "csi04", 
#                                            "csi05", "csi06", "csi07", "csi08", 
#                                            "csi09", "csi10", "csi11", "csi12", 
#                                            "csi13", "csi14", "csi15", "csi16", 
#                                            "csi17", "csi18", "csi19", "csi20", 
#                                            "csi21", "csi22", "csi23", "csi24", 
#                                            "csi25", "csi26", "csi27", "csi28", 
#                                            "csi29", "csi30", "csi31", "csi32", 
#                                            "csi33")])

# dataframe with isolated variables of interest
VOI_data <- raw_data[c("ID", "PTSD", "csayn", AdolSA, AdultSA, "ERS",
                       "SMQ_tot", demo)]

# Re-org SA ---------------------------------------------------------------

# Create a variable indicating any adolescent or adult SA
VOI_data$ASA <- apply(VOI_data[, AdolSA], 1, function(x) any(x == 1, na.rm = TRUE)) |
  apply(VOI_data[, AdultSA], 1, function(x) any(x == 1, na.rm = TRUE))

# Convert logical TRUE/FALSE to numeric 1/0
VOI_data$ASA <- as.numeric(VOI_data$ASA)

# # first sum across columns
# VOI_data$adolSAtot <- rowSums(VOI_data[,AdolSA]) # total # adolescent SA
# VOI_data$adultSAtot <- rowSums(VOI_data[,AdultSA]) # total # adult SA
# 
# # dichotomous variables
# # dichotomized any adolescent (14-18) SA, by any tactic, attempted or completed
# VOI_data$adolSAyn <- ifelse(VOI_data$adolSAtot == 0, 0, 1)  
# # dichotomized any adult SA (>18), by any tactic, attempted or completed
# VOI_data$adultSAyn <- ifelse(VOI_data$adultSAtot == 0, 0, 1)
# # ditchotomized ASA = adolescent or adulthood
# VOI_data$ASA <- ifelse(VOI_data$adolSAyn == 1 | VOI_data$adultSAyn == 1, 1, 0)

# needed only for graphing purposes
# CSA only
VOI_data$CSA_only <- ifelse(VOI_data$csayn == 1 & VOI_data$ASA == 0, 1, 0)

# ASA only
VOI_data$ASA_only <- ifelse(VOI_data$csayn == 0 & VOI_data$ASA == 1, 1, 0)

# CSA + ASA
VOI_data$CSA_ASA <- ifelse(VOI_data$csayn == 1 & VOI_data$ASA == 1, 1, 0)

# composite variable
VOI_data$vic <- ifelse(VOI_data$CSA_only == 1, "CSA_only",
                       ifelse(VOI_data$ASA_only == 1, "ASA_only",
                              ifelse(VOI_data$CSA_ASA == 1, "CSA_ASA", NA)))

vic_variables <- c("CSA_only", "ASA_only", "CSA_ASA", "ASA", "csayn")

# Final data ---------------------------------------------------------

final_outcomes <- c("PTSD", "ERS", "SMQ_tot")

clean_data <- VOI_data[c("ID", final_outcomes, "vic", vic_variables, demo)]
clean_data <- subset(clean_data, ASA == 1 | CSA_ASA == 1 | csayn == 1)

# descriptives
final_descriptives <- describe(clean_data[final_outcomes])
describe(clean_data$age)
table(clean_data$race)
table(clean_data$hispanic)
table(clean_data$jobstatus)
table(clean_data$income)
table(clean_data$vic)

## correlations

# optional - corr values also generated below in correlation_results
# final_correlations <- cor(clean_data[final_main], use = "complete.obs")

correlation_variables <- c("PTSD", "ERS", "SMQ_tot")
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


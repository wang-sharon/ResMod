## clear environment
rm(list = ls())

# set working directory
setwd("~/OneDrive - UW/ResMod (Second Year Project)/Data Analysis")

## load packages
library(haven) # reads spss files
library(knitr) # view subset of dataframe
library(psych) # descriptives

## load raw spss
raw_data <- read_sav("Data/Fresh2_Main_Study_clean-baseline-inlab-combined updated current (9.3.19).sav")


# Reorganize Variables ----------------------------------------------------

## isolate variables of interest (VOI)
covariates <- c("ASASep_HR", "csasev")

PTSD <- c("pcltotV", "ptsd01", "ptsd02", "ptsd03", "ptsd04", "ptsd05", "ptsd06",
          "ptsd07", "ptsd08", "ptsd09", "ptsd10", "ptsd11", "ptsd12", "ptsd13", 
          "ptsd14", "ptsd15", "ptsd16", "ptsd17", "ptsd18", "ptsd19","ptsd20")

# center PTSD on zero
raw_data$PTSD <- raw_data$pcltotV - 20

# Make NA where PTSD <0
raw_data$PTSD <- replace(raw_data$PTSD, raw_data$PTSD < 0, NA)

AdolSA <- c("oral_1__14", "oral_2__14", "oral_3__14", "oral_4__14", 
            "oral_5__14", "vagina_1__14", "vagina_2__14","vagina_3__14", 
            "vagina_4__14", "vagina_5__14","anal_1__14", "anal_2__14", 
            "anal_3__14", "anal_4__14", "anal_5__14", "attoral_1__14", 
            "attoral_2__14", "attoral_3__14", "attoral_4__14", "attoral_5__14", 
            "attvag_1__14","attvag_2__14", "attvag_3__14", "attvag_4__14", 
            "attvag_5__14", "attanal_1__14", "attanal_2__14","attanal_3__14", 
            "attanal_4__14", "attanal_5__14")
AdultSA <- c("oral_1__18", "oral_2__18", "oral_3__18", "oral_4__18", 
             "oral_5__18", "vagina_1__18", "vagina_2__18","vagina_3__18", 
             "vagina_4__18", "vagina_5__18","anal_1__18", "anal_2__18", 
             "anal_3__18", "anal_4__18", "anal_5__18", "attoral_1__18", 
             "attoral_2__18", "attoral_3__18", "attoral_4__18", "attoral_5__18",
             "attvag_1__18","attvag_2__18", "attvag_3__18", "attvag_4__18",
             "attvag_5__18", "attanal_1__18", "attanal_2__18","attanal_3__18", 
             "attanal_4__18", "attanal_5__18")

DERS <- c("ders01", "ders02", "ders03", "ders04", "ders05", "ders06", "ders07", 
          "ders08", "ders09", "ders10", "ders11", "ders12", "ders13", "ders14", 
          "ders15", "ders16", "ders17", "ders18", "ders19", "ders20", "ders21", 
          "ders22", "ders23", "ders24", "ders25", "ders26", "ders27", "ders28", 
          "ders29", "ders30", "ders31", "ders32", "ders33", "ders34", "ders35", 
          "ders36", "ders01r", "ders02r", "ders06r", "ders07r", "ders08r",
          "ders10r", "ders17r", "ders20r", "ders22r", "ders24r")
DERS_factors <- c("DERS_NonAccept", "DERS_Goal", "DERS_IMPULSE", "DERS_AWARE",  
                  "DERS_STRATEGIES", "DERS_CLARITY", "DERS_TotalScore")
# reverse DERS
raw_data$PosDERS <- -raw_data$DERS_TotalScore

SMQ <- c("smq01", "smq02", "smq03", "smq04", "smq05", "smq06", "smq07", "smq08",
         "smq09", "smq10", "smq11", "smq12", "smq13", "smq14", "smq15", "smq16",
         "smq02r", "smq03r","smq06r", "smq08r", "smq12r", "smq13r", "smq14r", 
         "smq16r")

CSI <- c("csi01", "csi02", "csi03", "csi04", "csi05", "csi06", "csi07", "csi08",
         "csi09", "csi10", "csi11", "csi12", "csi13", "csi14", "csi15", "csi16",
         "csi17", "csi18", "csi19", "csi20", "csi21", "csi22", "csi23", "csi24", 
         "csi25", "csi26", "csi27", "csi28", "csi29", "csi30", "csi31", "csi32", 
         "csi33")
CSI_factors <- c("CSI_ProbSolving", "CSI_SocialSupport", "CSI_Avoidance")
raw_data$CSI_Total <- rowSums(raw_data[, c("csi01", "csi02", "csi03", "csi04", 
                                            "csi05", "csi06", "csi07", "csi08", 
                                            "csi09", "csi10", "csi11", "csi12", 
                                            "csi13", "csi14", "csi15", "csi16", 
                                            "csi17", "csi18", "csi19", "csi20", 
                                            "csi21", "csi22", "csi23", "csi24", 
                                            "csi25", "csi26", "csi27", "csi28", 
                                            "csi29", "csi30", "csi31", "csi32", 
                                            "csi33")])

VOI_data <- raw_data[c("ID", "PTSD", "csayn", AdolSA, AdultSA, "PosDERS",
                       "SMQ_tot", "CSI_Total", covariates)]


# Re-org SA ---------------------------------------------------------------

# first sum across columns
VOI_data$adolSAtot <- rowSums(VOI_data[,AdolSA]) # total # adolescent SA
VOI_data$adultSAtot <- rowSums(VOI_data[,AdultSA]) # total # adult SA

# dichotomous variables
# dichotomized any adolescent (14-18) SA, by any tactic, attempted or completed
VOI_data$adolSAyn <- ifelse(VOI_data$adolSAtot == 0, 0, 1)  
# dichotomized any adult SA (>18), by any tactic, attempted or completed
VOI_data$adultSAyn <- ifelse(VOI_data$adultSAtot == 0, 0, 1)  

## One-time victimization (Vic1) --------------------------------------------

Vic1 <- list(
  list(name = "CSA_only", condition = VOI_data$csayn == 1 & 
         VOI_data$adolSAyn == 0 & VOI_data$adultSAyn == 0),
  list(name = "adolSA_only", condition = VOI_data$csayn == 0 & 
         VOI_data$adolSAyn == 1 & VOI_data$adultSAyn == 0),
  list(name = "adultSA_only", condition = VOI_data$csayn == 0 & 
         VOI_data$adolSAyn == 0 & VOI_data$adultSAyn == 1)
)

# Iterate over each condition and create corresponding variable
for (cond in Vic1) {
  VOI_data[[cond$name]] <- ifelse(cond$condition, 1, 0)
}

## Two-time victimization (Vic2) ---------------------------------------------

Vic2 <- list(
  list(name = "CSA_adolSA", condition = VOI_data$csayn == 1 & 
         VOI_data$adolSAyn == 1 & VOI_data$adultSAyn == 0),
  list(name = "CSA_adultSA", condition = VOI_data$csayn == 1 & 
         VOI_data$adolSAyn == 0 & VOI_data$adultSAyn == 1),
  list(name = "adolSA_adultSA", condition = VOI_data$csayn == 0 
       & VOI_data$adolSAyn == 1 & VOI_data$adultSAyn == 1)
)

for (cond in Vic2) {
  VOI_data[[cond$name]] <- ifelse(cond$condition, 1, 0)
}

## Three-time victimization (Vic3) -----------------------------------------
VOI_data$Vic3 <- ifelse(VOI_data$csayn == 1 & VOI_data$adolSAyn == 1 & 
                          VOI_data$adultSAyn == 1, 1, 0)
# n = 15


# Revictimization ---------------------------------------------------------

# cut-off = age 14
VOI_data$ASA14 <- ifelse((VOI_data$adolSAyn == 1 | VOI_data$adultSAyn == 1)
                         & VOI_data$csayn == 0, 1, 0)
VOI_data$revic14 <- ifelse(VOI_data$csayn == 1 & VOI_data$ASA14 == 1, 1, 0)

#cut-off = age 18
VOI_data$CSA18 <- ifelse((VOI_data$csayn == 1 | VOI_data$adolSAyn == 1) &
                           VOI_data$adultSAyn == 0, 1, 0)
VOI_data$revic18 <- ifelse(VOI_data$CSA18 == 1 & VOI_data$adultSAyn == 1, 1, 0)

# summary
revic14_frequency_df <- as.data.frame(do.call(cbind, 
                                             lapply(VOI_data[c("CSA_only", "ASA14", 
                                                               "revic14")], table)))
revic14_frequency_df <- revic14_frequency_df[-1,]
revic14_frequency_df_flipped <- t(revic14_frequency_df)

revic18_freq <- as.data.frame(do.call(cbind, lapply(VOI_data[c("CSA18", 
                                                              "adultSA_only", 
                                                              "revic18")], 
                                                   table)))
revic18_freq <- revic18_freq[-1,]
revic18_freq_flipped <- t(revic18_freq)

# compare mean PTSS between revic14 and revic18
PTSD_revic14 <- VOI_data$PTSD[VOI_data$revic14 == 1]
PTSD_revic18 <- VOI_data$PTSD[VOI_data$revic18 == 1]

t_test <- t.test(PTSD_revic14, PTSD_revic18)

# compare mean PTSS between CSA operationalizations (csayn vs. CSA18)
PTSD_csayn <- VOI_data$PTSD[VOI_data$csayn == 1]
PTSD_CSA18 <- VOI_data$PTSD[VOI_data$CSA18 == 1]
t_test2 <- t.test(PTSD_csayn, PTSD_CSA18)

# composite score
VOI_data$numvic1 <- rowSums(VOI_data[, c("csayn", "revic1")]) 
VOI_data$numvic2 <- rowSums(VOI_data[, c("CSA18", "revic2")]) 

# compare mean PTSS between "zero" victimization operationalizations
PTSD_novic1 <- VOI_data$PTSD[VOI_data$numvic1 == 0]
PTSD_novic2 <- VOI_data$PTSD[VOI_data$numvic2 == 0]
t_test3 <- t.test(PTSD_novic1, PTSD_novic2)

# Summary -----------------------------------------------------------------

vic_variables <- c("CSA_only", "adolSA_only", "adultSA_only", "CSA_adolSA", 
               "CSA_adultSA", "adolSA_adultSA", "Vic3")

# Create frequency data frames
frequency_df <- as.data.frame(do.call(cbind, lapply(VOI_data[vic_variables], 
                                                    table)))
frequency_df <- frequency_df[-1, ]
frequency_df_flipped <- t(frequency_df)

# finalized and cleaned variables
final_main <- c(vic_variables, "PTSD", "PosDERS", "SMQ_tot", "CSI_Total")

clean_data <- VOI_data[c("ID",final_main)]

final_descriptives <- describe(clean_data[final_main])

# correlations
final_correlations <- cor(clean_data[final_main], use = "complete.obs")

correlation_variables <- c(vic_variables, "PTSD", "PosDERS", "SMQ_tot", 
                           "CSI_Total")
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

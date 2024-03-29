## clear environment
rm(list = ls())

# set working directory
setwd("~/OneDrive - UW/ResMod (Second Year Project)/Data Analysis")

## load packages
library(haven) # reads spss files
library(dplyr) 
library(tidyselect)
library(lavaan) # path analysis
library(corrplot) # correlation
library(psych) # descriptives
library(ggplot2) # plotting
library(semPlot) # visual of path diagram
library(semptools) # remove control variables from path diagram
library(ltm) # cronbach's alpha

## load raw spss
raw_data <- read_sav("Data/Fresh2_Main_Study_clean-baseline-inlab-combined updated current (9.3.19).sav")

# Reorganize Variables ----------------------------------------------------

## isolate variables of interest (VOI)
covariates <- c("ASASep_HR", "csasev")

PTSD <- c("pcltotV", "ptsd01", "ptsd02", "ptsd03", "ptsd04", "ptsd05", "ptsd06",
          "ptsd07", "ptsd08", "ptsd09", "ptsd10", "ptsd11", "ptsd12", "ptsd13", 
          "ptsd14", "ptsd15", "ptsd16", "ptsd17", "ptsd18", "ptsd19","ptsd20")

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

VOI_data <- raw_data[c("ID", PTSD, "csayn", AdolSA, AdultSA, DERS, DERS_factors,
                       SMQ, "SMQ_tot", CSI, CSI_factors, covariates)]


# Total CSI score ---------------------------------------------------------

# reverse score Avoidance items - 4, 6, 10, 13, 18, 21, 22, 26, 27, 28, 30
reverse_cols <- c("csi04", "csi06", "csi10", "csi13", "csi18", "csi21", "csi22",
                  "csi26", "csi27", "csi28", "csi30")

VOI_data[ , reverse_cols] = 4 - VOI_data[ , reverse_cols]

VOI_data$CSI_Total <- rowSums(VOI_data[, c("csi01", "csi02", "csi03", "csi04", 
                                           "csi05", "csi06", "csi07", "csi08", 
                                           "csi09", "csi10", "csi11", "csi12", 
                                           "csi13", "csi14", "csi15", "csi16", 
                                           "csi17", "csi18", "csi19", "csi20", 
                                           "csi21", "csi22", "csi23", "csi24", 
                                           "csi25", "csi26", "csi27", "csi28", 
                                           "csi29", "csi30", "csi31", "csi32", 
                                           "csi33")])

# Operationalize SA Variables ---------------------------------------------

# first sum across columns
VOI_data$adolSAtot <- rowSums(VOI_data[,AdolSA]) # total # adolescent SA
VOI_data$adultSAtot <- rowSums(VOI_data[,AdultSA]) # total # adult SA

# dichotomous variables
# dichotomized any adolescent (14-18) SA, by any tactic, attempted or completed
VOI_data$adolSAyn <- ifelse(VOI_data$adolSAtot == 0, 0, 1)  
# dichotomized any adult SA (>18), by any tactic, attempted or completed
VOI_data$adultSAyn <- ifelse(VOI_data$adultSAtot == 0, 0, 1)  

# total number of victimizations across 3 time points
VOI_data$numvic <- rowSums(VOI_data[, c("csayn", "adolSAyn", "adultSAyn")]) 
# range 0-3

# Final -------------------------------------------------------------------

# reorder
#VOI_data <- VOI_data[, c(1, "numvic", 2:198, 200, 199, 201:203)]

# main variables
main <- c("pcltotV", "DERS_TotalScore", "SMQ_tot", "CSI_ProbSolving", 
          "CSI_SocialSupport", "CSI_Avoidance", "CSI_Total", "numvic", covariates)


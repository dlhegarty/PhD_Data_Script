# 1. Installing and Loading Packages -----------------------------------------
### loading relevant packages all at once using pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, mvtnorm, loo, coda, outliers, psych, reshape2, corrplot, plyr, mice, tidyverse, corrr, igraph, ggraph, Amelia, MVN, car, MASS, rcompanion, combinat, DMwR, mice, mvoutlier, compute.es, effects, multcomp, pastecs, compute.es, pastecs, broom, WRS2, nortest, apaTables, VIM, boot, statar, stargazer, pander, missForest, data.table, Hmisc, lavaan, GPArotation, nFactors, rlang, brms, BayesFactor, bayesplot, gridExtra, hornpa, ez, lme4, robustbase, bootES, EffectLiteR, robustlmm, kableExtra, nlme, semPlot, qgraph, simsem, simpleboot, viridis, plotly, probemod, jtools, interactions, mediation, xtable, htmlTable, huxtable, sfsmisc, cowplot, irr, interflex, NSM3, clickR, dabestr, effsize, sjstats, pequod, sjmisc, TOSTER)





# 2. Import Data -------------------------------------------------------------
### change the working directory and then import the .CSV file
#setwd("/Users/david/Google Drive/PhD Dependent Variables/2019/R")
data = read.csv("https://raw.githubusercontent.com/dlhegarty/PhD_Data/master/data.csv") ### main data import from Github
fidelity = read.csv("https://raw.githubusercontent.com/dlhegarty/PhD_Data/master/trainingFidelity.csv") ### training fidelity data from Github





#3. Preliminary Data Tidying ------------------------------------------------
### turn off scientific notation
options(scipen = 999)
### make first column row names
rownames(data) <- data[,1]
### make sure some of the data types are correct...
data <- data %>%
  mutate(
    ID = as.factor(ID),
    Group = as.factor(Group),
    Wave = as.factor(Wave),
    Grade = as.factor(Grade)
  )
### prepare for contrasts when / if they occur by modifying 
### $Group: TC (control) = 0; C8 (experimental) = 1
contrasts(data$Group)<-contr.treatment(2, base = 2)
contrasts(data$Grade)<-contr.treatment(2, base = 2)
data$Group <- revalue(data$Group, c("C8"="Exp.")) ### experimental
data$Group <- revalue(data$Group, c("TC"="Ctrl")) ### control group
df1 <- data





#4. Missing Data ------------------------------------------------------------
### percentage of missing data
### % in all EF data time 1
mean(is.na(data[,7:48])) ### which is less than 0.7%
sapply(data[,7:48], function(x) sum(is.na(x)))
### % in all EF data time 2
mean(is.na(data[,50:90])) ### which is less than 0.7%
sapply(data[,50:90], function(x) sum(is.na(x)))
### % in all EF data time 3
mean(is.na(data[,91:132])) ### which is less than 0.5%
sapply(data[,91:132], function(x) sum(is.na(x)))
### % in other data excl. cefi
mean(is.na(data[,133:162])) ### which is less than 1% 
sapply(data[,133:162], function(x) sum(is.na(x)))








# 5. Creation of Variables ---------------------------------------------------
###
### Switcher RT Difference Score
X1SwitchDiffRT <- df1$X1SWITCHavg2featRT - df1$X1SWITCHavg3featRT
df1 <- add_column(df1, X1SwitchDiffRT, .after = "X1SWITCHavg3featRT")
X2SwitchDiffRT <- df1$X2SWITCHavg2featRT - df1$X2SWITCHavg3featRT
df1 <- add_column(df1, X2SwitchDiffRT, .after = "X2SWITCHavg3featRT")
X3SwitchDiffRT <- df1$X3SWITCHavg2featRT - df1$X3SWITCHavg3featRT
df1 <- add_column(df1, X3SwitchDiffRT, .after = "X3SWITCHavg3featRT")
rm("X1SwitchDiffRT","X2SwitchDiffRT","X3SwitchDiffRT")
### Switcher Difference Error Score
X1SwitchDiffError <- df1$X1SWITCHerr2feat - df1$X1SWITCHerr3feat
df1 <- add_column(df1, X1SwitchDiffError, .after = "X1SWITCHerr3feat")
X2SwitchDiffError <- df1$X2SWITCHerr2feat - df1$X2SWITCHerr3feat
df1 <- add_column(df1, X2SwitchDiffError, .after = "X2SWITCHerr3feat")
X3SwitchDiffError <- df1$X3SWITCHerr2feat - df1$X3SWITCHerr3feat
df1 <- add_column(df1, X3SwitchDiffError, .after = "X3SWITCHerr3feat")
rm("X1SwitchDiffError","X2SwitchDiffError","X3SwitchDiffError")
### Switcher Median RT Difference Score
X1SwitchMedDiffRT <- df1$X1SwitchMed2featRT - df1$X1SwitchMed3featRT
df1 <- add_column(df1, X1SwitchMedDiffRT, .after = "X1SwitchMed3featRT")
X2SwitchMedDiffRT <- df1$X2SwitchMed2featRT - df1$X2SwitchMed3featRT
df1 <- add_column(df1, X2SwitchMedDiffRT, .after = "X2SwitchMed3featRT")
X3SwitchMedDiffRT <- df1$X3Switchmed2featRT - df1$X3Switchmed3featRT
df1 <- add_column(df1, X3SwitchMedDiffRT, .after = "X3Switchmed3featRT")
rm("X1SwitchMedDiffRT","X2SwitchMedDiffRT","X3SwitchMedDiffRT")
### NoGo Accuracy - proportion correct on No-Go trials
X1NoGoAcc <- ((df1$X1Go1Rcorr*16)+(df1$X1Go2Pcorr*64))/80
df1 <- add_column(df1, X1NoGoAcc, .after = "X1Go2Pcorr")
X2NoGoAcc <- ((df1$X2GO1Rcorr*16)+(df1$X2GO2Pcorr*64))/80
df1 <- add_column(df1, X2NoGoAcc, .after = "X2GO2Pcorr")
X3NoGoAcc <- ((df1$X3GO1Rcorr*16)+(df1$X3GO2Pcorr*64))/80
df1 <- add_column(df1, X3NoGoAcc, .after = "X3GO2Pcorr")
rm("X1NoGoAcc","X2NoGoAcc","X3NoGoAcc")
### Go Accuracy - proportion correct on Go trials
X1GoAcc <- ((df1$X1Go1Pcorr*64)+(df1$X1Go2Rcorr*16))/80
df1 <- add_column(df1, X1GoAcc, .after ="X1Go2Rcorr")
X2GoAcc <- ((df1$X2GO1Pcorr*64)+(df1$X2GO2Rcorr*16))/80
df1 <- add_column(df1, X2GoAcc, .after ="X2GO2Rcorr")
X3GoAcc <- ((df1$X3GO1Pcorr*64)+(df1$X3GO2Rcorr*16))/80
df1 <- add_column(df1, X3GoAcc, .after ="X3GO2Rcorr")
rm("X1GoAcc","X2GoAcc","X3GoAcc")
### Go Conflict Cost - conflict cost proportion
X1GoCCacc <- df1$X1NoGoAcc / df1$X1GoAcc
df1 <- add_column(df1, X1GoCCacc, .after = "X1GoAcc")
X2GoCCacc <- df1$X2NoGoAcc / df1$X2GoAcc
df1 <- add_column(df1, X2GoCCacc, .after = "X2GoAcc")
X3GoCCacc <- df1$X3NoGoAcc / df1$X3GoAcc
df1 <- add_column(df1, X3GoCCacc, .after = "X3GoAcc")
rm("X1GoCCacc","X2GoCCacc","X3GoCCacc")
### GoRTAvge - average RT in the Go condition
X1GoRTAvge <- ((df1$X1Go1RTP + df1$X1Go2RTR)/2)
df1 <- add_column(df1, X1GoRTAvge, .after = "X1Go2RTR")
X2GoRTAvge <- ((df1$X2GO1RTP + df1$X2GO2RTR)/2)
df1 <- add_column(df1, X2GoRTAvge, .after = "X2GO2RTR")
X3GoRTAvge <- ((df1$X3GO1RTP + df1$X3GO2RTR)/2)
df1 <- add_column(df1, X3GoRTAvge, .after = "X3GO2RTR")
rm("X1GoRTAvge","X2GoRTAvge","X3GoRTAvge")
### NSRTCC - difference between the RT in congruent - incongruent conditions
X1NSRTCC <- (df1$X1NSRTC - df1$X1NSRTI)
df1 <- add_column(df1, X1NSRTCC, .after = "X1NSRTI")
X2NSRTCC <- (df1$X2NSRTC - df1$X2NSRTI)
df1 <- add_column(df1, X2NSRTCC, .after = "X2NSRTI")
X3NSRTCC <- (df1$X3NSRTC - df1$X3NSRTI)
df1 <- add_column(df1, X3NSRTCC, .after = "X3NSRTI")
rm("X1NSRTCC","X2NSRTCC","X3NSRTCC")
### FlankAccCC - difference between the accuracy in congruent - incongruent conditions
X1FlankAccCC <- (df1$X1Flank1Acc - df1$X1Flank.1Acc)
df1 <- add_column(df1, X1FlankAccCC, .after = "X1Flank1Acc")
X2FlankAccCC <- (df1$X2Flank1Acc - df1$X2Flank.1Acc)
df1 <- add_column(df1, X2FlankAccCC, .after = "X2Flank1Acc")
X3FlankAccCC <- (df1$X3Flank1Acc - df1$X3Flank.1Acc)
df1 <- add_column(df1, X3FlankAccCC, .after = "X3Flank1Acc")
rm("X1FlankAccCC","X2FlankAccCC","X3FlankAccCC")
### ConnDiffScore - difference between Connections Single/20 - Switch/20 score
X1ConnDiffScore <- (df1$X1ConnSingle/20)-(df1$X1ConnSwitch/20)
df1 <- add_column(df1, X1ConnDiffScore, .after = "X1ConnSwitch")
X2ConnDiffScore <- (df1$X2ConnSingle/20)-(df1$X2ConnSwitch/20)
df1 <- add_column(df1, X2ConnDiffScore, .after = "X2ConnSwitch")
X3ConnDiffScore <- (df1$X3ConnSingle/20)-(df1$X3ConnSwitch/20)
df1 <- add_column(df1, X3ConnDiffScore, .after = "X3ConnSwitch")
rm("X1ConnDiffScore","X2ConnDiffScore","X3ConnDiffScore")

df1NR <- df1


# 6. Complex Span Data - Removal ---------------------------------------------
### Keeping the original 85% accuracy criteria removes a lot of data. 
### let us work out how many would be removed...
### Operation Span
(length(which(df1$X1OSPANdistract. < 0.85))/nrow(df1))*100 # approx 50%
(length(which(df1$X2OSPNdistract. < 0.85))/nrow(df1))*100 # approx 51%
(length(which(df1$X3OSPNdistract. < 0.85))/nrow(df1))*100 # approx 51%
### Reading Span
(length(which(df1$X1RSPANdistract. < 0.85))/nrow(df1))*100 # approx 27%
(length(which(df1$X2RSPANdistract. < 0.85))/nrow(df1))*100 # approx 29%
(length(which(df1$X3RSPANdistract. < 0.85))/nrow(df1))*100 # approx 16%
### Symmetry Span
(length(which(df1$X1SYMSPANdistract. < 0.85))/nrow(df1))*100 # approx 49%
(length(which(df1$X2SYMSPANdistract. < 0.85))/nrow(df1))*100 # approx 31%
(length(which(df1$X3SYMSPANdistract. < 0.85))/nrow(df1))*100 # approx 29%

### If we were to approximate the 15% of data removal that was obtained by Unsworth et al. 2005: this results in accuracy criterion of 72% for ospan; same of 84% for rspan; and 71% for symspan. 

### Operation Span T1 - T3
df1 <- mutate(df1, X1OSPANpartialscore = ifelse(df1$X1OSPANdistract. < 0.72, NA, df1$X1OSPANpartialscore))
df1$X1OSPNpartialscore <- df1$X1OSPANpartialscore
df1 <- mutate(df1, X2OSPNpartial = ifelse(df1$X2OSPNdistract. < 0.72, NA, df1$X2OSPNpartialscore))
df1$X2OSPNpartialscore <- df1$X2OSPNpartial
df1 <- mutate(df1, X3OSPNpartial = ifelse(df1$X3OSPNdistract. < 0.72, NA, df1$X3OSPNpartialscore))
df1$X3OSPNpartialscore <- df1$X3OSPNpartial
### Reading Span T1 - T3
df1 <- mutate(df1, X1RSPANpartial = ifelse(df1$X1RSPANdistract. < 0.84, NA, df1$X1RSPANpartialscore))
df1$X1RSPANpartialscore <- df1$X1RSPANpartial
df1 <- mutate(df1, X2RSPANpartial = ifelse(df1$X2RSPANdistract. < 0.84, NA, df1$X2RSPANpartialscore))
df1$X2RSPANpartialscore <- df1$X2RSPANpartial
df1 <- mutate(df1, X3RSPANpartial = ifelse(df1$X3RSPANdistract. < 0.84, NA, df1$X3RSPANpartialscore))
df1$X3RSPANpartialscore <- df1$X3RSPANpartial
### Symmetry Span T1 - T3
df1 <- mutate(df1, X1SYMSPANpartial = ifelse(df1$X1SYMSPANdistract. < 0.71, NA, df1$X1SYMSPANpartialscore))
df1$X1SYMSPANpartialscore <- df1$X1SYMSPANpartial
df1 <- mutate(df1, X2SYMSPANpartial = ifelse(df1$X2SYMSPANdistract. < 0.71, NA, df1$X2SYMSPANpartialscore))
df1$X2SYMSPANpartialscore <- df1$X2SYMSPANpartial
df1 <- mutate(df1, X3SYMSPANpartial = ifelse(df1$X3SYMSPANdistract. < 0.71, NA, df1$X3SYMSPANpartialscore))
df1$X3SYMSPANpartialscore <- df1$X3SYMSPANpartial

df1 <- df1[,-c(202:210)]




# 7. Removal of Cases Listwise & Data Table -----------------------------------------------
### I think it is important for the research integrity, to remove training data where
### participants have not completed enough training (as per previous research)
### Therefore, remove data where training < 9 hours
df1 <- df1[!(df1$Train.Hr<9),] ### this removes 6 individual cases
fidelity <- fidelity[!(fidelity$Training..hr.<9),] ### removes same cases from fidelity data
write.csv(df1,"df1.csv")






# 8. Creating Data Summary Table ----------------------------------------
### First let's get rid of unnecessary data in the table
table <- df1[, -c(7:9,11,13:14,16:19,21,23:26,29:34,36:42,44,45,47,48,50,52,53,55,56,58:61,63,65,66,68:71,73,75:78,81:86,88:94,96,97,99,100,102,104,105,107,108,110:113,115,117,118,120:123,125,127:130,133:138,140:146,148,149,151,152,154,156,157,159,160,162)]

### let's then sort them into the correct order...
### this places data in time order (except for ITIM & IEM)
table <- df1[,c("ID",	"Group",	"Age.Yr",	"Train.Hr",	"Wave",	"Grade","X1BCSTPE","X1BCSTCLR",	"X1ConnSwitch", "X1SWITCHerr2feat",	"X1SwitchDiffError", "X1FLANKCC.RT.",	"X1Flank.1Acc",	"X1NoGoAcc",	"X1GoRTAvge",	"X1NSRTI",	"X1NSACCI",	"X1OSPANpartialscore",	"X1RSPANpartialscore",	"X1SYMSPANpartialscore","X1RTaverage",	"X1NAI", "X1PATmScale" ,"X1PATrScale","X1CEFI_P_AT_SS","X1CEFI_P_FX_SS","X1CEFI_P_WM_SS","X1CEFI_T_AT_SS","X1CEFI_T_FX_SS","X1CEFI_T_WM_SS",
              "X2BCSTPE", "X2BCSTCLR", "X2ConnSwitch", "X2SWITCHerr2feat",	"X2SwitchDiffError", "X2FLANKCC.RT.", "X2Flank.1Acc",	"X2NoGoAcc",	"X2GoRTAvge",	"X2NSRTI",	"X2NSACCI",	"X2OSPNpartialscore","X2RSPANpartialscore","X2SYMSPANpartialscore","X2RTaverage","X2NAI","X2PATmScale","X2PATrScale","X2CEFI_P_AT_SS","X2CEFI_P_FX_SS","X2CEFI_P_WM_SS","X2CEFI_T_AT_SS","X2CEFI_T_FX_SS","X2CEFI_T_WM_SS",
              "X3BCSTPE", "X3BCSTCLR", "X3ConnSwitch", "X3SWITCHerr2feat", "X3SwitchDiffError", "X3FLANKCC", "X3Flank.1Acc",      "X3NoGoAcc", "X3GoRTAvge","X3NSRTI","X3NSACCI","X3OSPNpartialscore","X3RSPANpartialscore","X3SYMSPANpartialscore","X3RTaverage","X3NAI","X3PATmScale","X3PATrScale","X3CEFI_P_AT_SS","X3CEFI_P_FX_SS","X3CEFI_P_WM_SS","X3CEFI_T_AT_SS","X3CEFI_T_FX_SS","X3CEFI_T_WM_SS","X1IEMmaths","X1IEMenglish","X1IEMconc.","X1IEMTotalScore","X2IEMmaths","X2IEMenglish","X2IEMconc.","X2IEMTotalScore","ITIM")]

colnames(table)[1:87] <- c("ID",	"Group",	"Age.Yr",	"Train.Hr",	"Wave",	"Grade","BCSTPE_1","BCSTCLR_1",	"ConnSwitch_1", "SWITCHerr2feat_1",	"SwitchDiffError_1", "FLANKCC.RT._1",	"Flank.1Acc_1",	"NoGoAcc_1",	"GoRTAvge_1",	"NSRTI_1",	"NSACCI_1",	"OSPANpartialscore_1",	"RSPANpartialscore_1",	"SYMSPANpartialscore_1","RTaverage_1",	"NAI_1", "PATmScale_1" ,"PATrScale_1","CEFI.P.AT.SS_1","CEFI.P.FX.SS_1","CEFI.P.WM.SS_1","CEFI.T.AT.SS_1","CEFI.T.FX.SS_1","CEFI.T.WM.SS_1",
  "BCSTPE_2", "BCSTCLR_2", "ConnSwitch_2", "SWITCHerr2feat_2",	"SwitchDiffError_2", "FLANKCC.RT._2", "Flank.1Acc_2",	"NoGoAcc_2",	"GoRTAvge_2",	"NSRTI_2",	"NSACCI_2",	"OSPANpartialscore_2","RSPANpartialscore_2","SYMSPANpartialscore_2","RTaverage_2","NAI_2","PATmScale_2","PATrScale_2","CEFI.P.AT.SS_2","CEFI.P.FX.SS_2","CEFI.P.WM.SS_2","CEFI.T.AT.SS_2","CEFI.T.FX.SS_2","CEFI.T.WM.SS_2",
  "BCSTPE_3", "BCSTCLR_3", "ConnSwitch_3", "SWITCHerr2feat_3", "SwitchDiffError_3", "FLANKCC.RT._3", "Flank.1Acc_3","NoGoAcc_3", "GoRTAvge_3","NSRTI_3","NSACCI_3","OSPANpartialscore_3","RSPANpartialscore_3","SYMSPANpartialscore_3","RTaverage_3","NAI_3","PATmScale_3","PATrScale_3","CEFI.P.AT.SS_3","CEFI.P.FX.SS_3","CEFI.P.WM.SS_3","CEFI.T.AT.SS_3","CEFI.T.FX.SS_3","CEFI.T.WM.SS_3","IEMmaths_1","IEMenglish_1","IEMconc._1","IEMTotalScore_1","IEMmaths_2","IEMenglish_2","IEMconc._2","IEMTotalScore_2","ITIM")

write.csv(table, "table.csv")

### percentage of missing data for Time 1
### % in EF data (excl. CST data)
mean(is.na(table[,7:17])) ### which is less than 0.9%
sapply(table[,7:17], function(x) sum(is.na(x)))
### % in RT and achievement tests
mean(is.na(table[,21:24])) ### no missing data
### % in CEFI parent
mean(is.na(table[,25:27])) ### no missing data
### % in CEFI teacher
mean(is.na(table[,28:30])) ### 47.6 % missing data



### Now for table creation: One big table ...
table <- describeBy(table[,7:87], table$Group, mat = TRUE)
### round the mean & SD data to two decimal places
table <- round(table[,4:15], 2)
### make a cell with row names so that nothing gets misplaced
setDT(table, keep.rownames = TRUE) []
### Let's get rid of unnecessary columns
table <- table[, -c(5:7,10)]
### remove every second row into a different data.table
table.c8 <- table[seq(1, nrow(table), 2), ]
table.tc <- table[seq(2, nrow(table), 2), ]
### remove numbers from end of variable names
table.c8$rn <- substr(as.character(table.c8$rn), start= 1, stop= nchar(as.character(table.c8$rn) )-1 )
table.tc$rn <- substr(as.character(table.tc$rn), start= 1, stop= nchar(as.character(table.tc$rn) )-1 )
### put data back together again
table.tc$expN <- table.c8$n
table.tc$expMean <- table.c8$mean
table.tc$expSD <- table.c8$sd
table.tc$expMin <-table.c8$min
table.tc$expMax <- table.c8$max
table.tc$expSkew <- table.c8$skew
table.tc$expKurtosis <- table.c8$kurtosis
table.tc$expSE <- table.c8$se
table <- table.tc
rm("table.c8","table.tc")

### okay, let's split the table up into time 1, 2 & 3 data
table.t1 <- table[1:24,]
table.t2 <- table[25:48,]
table.t3 <- table[49:72,]
### next we need to add these next to each other back into table
table <- cbind(table.t1, table.t2, table.t3)
rm("table.t1","table.t2","table.t3")
### okay, let's remove the variables that are not required
table <- as.data.frame(table)
### add the correct names for variables
row.names(table) <- c("Perseverative Errors",
                      "Conceptual Level Responses",
                      "Connections Switching Completion",
                      "Switching Errors",
                      "Switching Error Difference",
                      "Flanker RT Conflict Cost",
                      "Flanker Incongruent Accuracy",
                      "No-Go Accuracy",
                      "Go RT average",
                      "Stroop RT Incongruent",
                      "Stroop Incongruent Accuracy",
                      "Operation Span",
                      "Reading Span",
                      "Symmetry Span",
                      "Mean Reaction Time",
                      "Naglieri Achievement Index",
                      "Maths",
                      "Reading Comprehension",
                      "Parent - Inhibition",
                      "Parent - Shifting",
                      "Parent - WM",
                      "Teacher - Inhibition",
                      "Teacher - Shifting",
                      "Teacher - WM")
table$rn <- NULL
table$rn <- NULL
table$rn <- NULL
write.csv(table, "TableA.csv")
rm("table")



# 9. Make All Variables Higher = Better --------------------------------------
###  adjust variables to make them all the same direction

adjust_better <- function(x, na.rm = TRUE, ...) {
  x <- (abs(max(x, na.rm = TRUE)+10)) - x
}

adjust_cardSort <- function(x, na.rm = TRUE, ...) {
  x <- 100 - x
}

### adjust using necessary function...
df1[,9:11] <- apply(df1[,9:11], 2, function(x) adjust_cardSort(x)) ## Time 1
df1[,19:27] <- apply(df1[,19:27], 2, function(x) adjust_better(x)) ## Time 1
df1[,43:46] <- apply(df1[,43:46], 2, function(x) adjust_better(x)) ## Time 1

df1[,61:63] <- apply(df1[,61:63], 2, function(x) adjust_cardSort(x)) ## Time 2
df1[,71:79] <- apply(df1[,71:79], 2, function(x) adjust_better(x)) ## Time 2
df1[,95:98] <- apply(df1[,95:98], 2, function(x) adjust_better(x)) ## Time 2

df1[,113:115] <- apply(df1[,113:115], 2, function(x) adjust_cardSort(x)) ## Time 3
df1[,123:131] <- apply(df1[,123:131], 2, function(x) adjust_better(x)) ## Time 3
df1[,147:150] <- apply(df1[,147:150], 2, function(x) adjust_better(x)) ## Time 3

rm("adjust_better","adjust_cardSort")

write.csv(df1, "df1.csv")










# 10. Baseline Differences: Group ----------------------------------------

par(mfrow=c(2,2))

### General info
### Do groups differ on age?
model1 <- lm(Age.Yr ~ Group, data=df1)
Anova(model1, type = "III")
leveneTest(df1$Age.Yr, df1$Group)
for(i in 1:4)plot(model1, which=i) ### definitely NOT normal
t.test(Age.Yr ~ Group, data=df1)
describeBy(df1$Age.Yr, df1$Group)
mes(9.95, 10.1, 1.11, 1.05, 50, 55) ### Cohen's d 


### Do groups differ on the number of hours they trained?
model1 <- lm(Train.Hr ~ Group, data=df1)
Anova(model1, type = "III")
leveneTest(df1$Train.Hr, df1$Group)
for(i in 1:4)plot(model1, which=i) ### definitely NOT normal
t.test(Train.Hr ~ Group, data=df1)
describeBy(df1$Train.Hr, df1$Group)
mes(10.29, 10.25, 0.42, 0.34, 50, 55) ### Cohen's d 


### Individual T1 tests
### Individual Shifting Measures
t.test(X1BCSTPE ~ Group, data=df1)
t.test(X1BCSTCLR ~ Group, data=df1)
t.test(X1ConnSwitch ~ Group, data=df1)
t.test(X1SWITCHerr2feat ~ Group, data=df1)
t.test(X1SwitchDiffError ~ Group, data=df1)

### Individual Inhibition Measures
t.test(X1SwitchDiffError ~ Group, data=df1)
t.test(X1Flank.1Acc ~ Group, data=df1)
t.test(X1NoGoAcc ~ Group, data=df1)
t.test(X1GoRTAvge ~ Group, data=df1)
t.test(X1NSRTI ~ Group, data=df1)
t.test(X1NSACCI ~ Group, data=df1) # * significant difference
describeBy(df1$X1NSACCI, df1$Group)
mes(0.8, 0.84, 0.09, 0.09, 50, 55) ### Cohen's d 

### Individual Working Memory measures
t.test(X1OSPANpartialscore ~ Group, data=df1)# * significant difference
describeBy(df1$X1OSPANpartialscore, df1$Group)
mes(0.54, 0.67, 0.2, 0.22, 39, 48) ### Cohen's d 
TOSTtwo(m1=0.54,m2=0.67,sd1=0.2,sd2=0.22,n1=39,n2=48,low_eqbound_d=-0.3, high_eqbound_d=0.3, alpha = 0.05, var.equal=FALSE) #using TOSTER (Lakens et al., 2018), is non-significant TOST but significant NHST
t.test(X1RSPANpartialscore ~ Group, data=df1)# * significant difference
describeBy(df1$X1RSPANpartialscore, df1$Group)
mes(0.57, 0.67, 0.23, 0.19, 44, 46) ### Cohen's d 
TOSTtwo(m1=0.57,m2=0.67,sd1=0.23,sd2=0.19,n1=44,n2=46,low_eqbound_d=-0.3, high_eqbound_d=0.3, alpha = 0.05, var.equal=FALSE) #using TOSTER (Lakens et al., 2018), is non-significant TOST but significant NHST
t.test(X1SYMSPANpartialscore ~ Group, data=df1)


### Other individual measures
t.test(X1RTaverage ~ Group, data=df1)
t.test(X1NAI ~ Group, data=df1) # * significant difference
describeBy(df1$X1NAI, df1$Group)
mes(110.2, 115.96, 11.48, 13.27, 50, 55) ### Cohen's d 
TOSTtwo(m1=110.2,m2=115.96,sd1=11.48,sd2=13.27,n1=50,n2=50,low_eqbound_d=-0.3, high_eqbound_d=0.3, alpha = 0.05, var.equal=FALSE) #using TOSTER (Lakens et al., 2018), is non-significant TOST but significant NHST
t.test(X1PATmScale ~ Group, data=df1) # * significant difference
describeBy(df1$X1PATmScale, df1$Group)
mes(130.27, 134.21, 11, 8.74, 50, 55) ### Cohen's d 
t.test(X1PATrScale ~ Group, data=df1)
t.test(X1CEFI_P_AT_SS ~ Group, data=df1)
t.test(X1CEFI_P_FX_SS ~ Group, data=df1)
t.test(X1CEFI_P_WM_SS ~ Group, data=df1)
t.test(X1CEFI_T_AT_SS ~ Group, data=df1) # * significant difference
describeBy(df1$X1CEFI_T_AT_SS, df1$Group)
mes(96.73, 109.52, 13.41, 14.66, 26, 29) ### Cohen's d 
t.test(X1CEFI_T_FX_SS ~ Group, data=df1)# * significant difference
describeBy(df1$X1CEFI_T_FX_SS, df1$Group)
mes(100.69, 110.55, 13.94, 14.62, 26, 29) ### Cohen's d 
t.test(X1CEFI_T_WM_SS ~ Group, data=df1)# * significant difference
describeBy(df1$X1CEFI_T_WM_SS, df1$Group)
mes(97.35, 108.24, 12.7, 14.12, 26, 29) ### Cohen's d 


## Initial difference between groups p-value adjustment
p <- c(.485, .809, .064, .580, .575, .937, .750, .172, .537, .194, .043, .009, .017, .099, .129, .019, .047, .120, .148, .068, .290, .001, .013, .004) ### 24 individual results
p.adjust(p, method = "holm")

## Initial difference between groups p-value adjustment
p <- c(.001, .013, .004) ### CEFI obs for teacher
p.adjust(p, method = "holm")



### The only measure that was similar to games played by the experimental group was BCST which was like the Ducks! game
## want to test if the experimental group improved in BCST:
temp <- subset(df1, df1$Group == "Exp.")
psych::describe(temp$X1BCSTPE)
psych::describe(temp$X2BCSTPE)
mes(74.65, 78.34, 6.75, 5.73, 50, 50) ### Cohen's d 
### ES Cohen's d = -0.59 [-0.99, -0.18]

psych::describe(temp$X1BCSTCLR)
psych::describe(temp$X2BCSTCLR)
mes(44.51, 49.73, 14.27, 10.84, 50, 50) ### Cohen's d 
### ES Cohen's d = -0.41 [-0.81, -0.01]

### So the experimental group did improve significantly on each measure

tempC <- subset(df1, df1$Group == "Ctrl")
psych::describe(tempC$X1BCSTPE)
psych::describe(tempC$X2BCSTPE)
mes(73.66, 79.1, 7.76, 6.29, 50, 50) ### Cohen's d 
### ES Cohen's d = -0.77 [-1.18, -0.36]

psych::describe(tempC$X1BCSTCLR)
psych::describe(tempC$X2BCSTCLR)
mes(45.21, 51.61, 15.13, 14.31, 50, 50) ### Cohen's d 
### ES Cohen's d = -0.43 [-0.84, -0.03]

### So the control group improved too...







# 11. Multivariate Normality Testing ------------------------------------------
### let's set subsets of the data for testing

### Create subset of df1 based upon group
citation(package = "MVN")
df1E <- subset(df1, df1$Group == "Exp.")
df1C <- subset(df1, df1$Group == "Ctrl")

### Create subset of each EF construct
ShiftingT1E <- df1E[,c(10,12,15,20,22)] ## T1
ShiftingT1C <- df1C[,c(10,12,15,20,22)] ## T1
ShiftingT1E <- na.omit(ShiftingT1E)
ShiftingT1C <- na.omit(ShiftingT1C)
InhibitionT1E <- df1E[,c(27,28,35,43,46,49)] ## T1
InhibitionT1C <- df1C[,c(27,28,35,43,46,49)] ## T1
InhibitionT1E <- na.omit(InhibitionT1E)
InhibitionT1C <- na.omit(InhibitionT1C)
wmT1E <- df1E[,c(51,54,57)] ## T1
wmT1C <- df1C[,c(51,54,57)] ## T1
wmT1E <- na.omit(wmT1E)
wmT1C <- na.omit(wmT1C)

### Experimental
mvn(ShiftingT1E, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
mvn(InhibitionT1E, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
mvn(wmT1E, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)

### Control
mvn(ShiftingT1C, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
mvn(InhibitionT1C, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
mvn(wmT1C, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
rm("df1C","df1E","InhibitionT1C","InhibitionT1E","ShiftingT1C","ShiftingT1E","wmT1C","wmT1E")



#...----------------------------------------------
#...----------------------------------------------

#    *** CFA Iterative Process ***-----------------------------------------
# 12. CFA data creation ----------------------------------------------
df1CFA<-df1[,7:58] ### time 1
df1CFA2<-df1[,59:110] ### time 2
df1CFA3<-df1[,111:162] ### time 3
df1CFA <- scale(df1CFA)
df1CFA2 <- scale(df1CFA2)
df1CFA3 <- scale(df1CFA3)
df1CFA <- as.data.frame(df1CFA)
df1CFA2 <- as.data.frame(df1CFA2)
df1CFA3 <- as.data.frame(df1CFA3)
colnames(df1CFA)[1:52] <- c("A1","A2","A3","A4","A5","A6","B1","B2","B3","B4","C1","C2","C3","C4","C5","C6","C7","C8","C9","D1","D2","D3","D4","D5","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","F1","F2","F3","F4","F5","F6","G1","G2","G3","H1","H2","H3","I1","I2","I3")
colnames(df1CFA2)[1:52] <- c("A1","A2","A3","A4","A5","A6","B1","B2","B3","B4","C1","C2","C3","C4","C5","C6","C7","C8","C9","D1","D2","D3","D4","D5","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","F1","F2","F3","F4","F5","F6","G1","G2","G3","H1","H2","H3","I1","I2","I3")
colnames(df1CFA3)[1:52] <- c("A1","A2","A3","A4","A5","A6","B1","B2","B3","B4","C1","C2","C3","C4","C5","C6","C7","C8","C9","D1","D2","D3","D4","D5","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","F1","F2","F3","F4","F5","F6","G1","G2","G3","H1","H2","H3","I1","I2","I3")
#df1CFA <- cbind(df1[,1:6],df1CFA,df1CFA2,df1CFA3)
#df1CFA <- cbind(df1[,1:6],df1CFA)
#rm("df1CFA2","df1CFA3")

write.csv(df1CFA, "df1CFA.csv")

citation("lavaan")


# 13. CFA Iterative process -----------------------------
### ITERATIVE PROCESS OF REFINING MODEL...

### this is the fit indices I am after
fit = c("df","chisq.scaled","pvalue.scaled","aic","cfi.robust","tli.robust","rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr")

### This initial model was the one suggested by the literature and what was outlined in the research plan
CFA.model.A <- '
shifting =~ A4 + A6 + B3 + C4 + C6
inhibition =~ D2 + D3 + E5 + F3 + F6 + E13 
wm =~ G2 + H2 + I2
'


CFA.model.A.fit <- cfa(CFA.model.A, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", meanstructure = TRUE)
fitMeasures(CFA.model.A.fit, fit.measures = fit)
summary(CFA.model.A.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
modificationIndices(CFA.model.A.fit, sort.=TRUE, minimum.value = 3.84)
residuals(CFA.model.A.fit, type = "cor")
### OUTPUT:
#df          chisq.scaled         pvalue.scaled                   aic 
#74.000               281.208                 0.000              3937.138 
#cfi.robust            tli.robust          rmsea.robust rmsea.ci.lower.robust 
#0.385                 0.243                 0.153                 0.134 
#rmsea.ci.upper.robust                  srmr 
#0.172                 0.130 
###
### modification indices (mi) suggested removal of:
### C4 (48.622), A4 (35.393), E13 (24.119) (they shared residual correlation with other measures within the same construct)
### other mi suggestions did not make theoretical sense to change (ie. changing the loading of B3 to WM or inhibition)
rm("CFA.model.A")



### Model B
CFA.model.B <- '
shifting =~ A6 + B3 + C6
inhibition =~ D2 + D3 + E5 + F3 + F6
wm =~ G2 + H2 + I2
'

CFA.model.B.fit <- cfa(CFA.model.B, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000), meanstructure = TRUE)
fitMeasures(CFA.model.B.fit, fit.measures = fit)
summary(CFA.model.B.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
modificationIndices(CFA.model.B.fit, sort.=TRUE, minimum.value = 3.84)
### OUTPUT:
#df          chisq.scaled         pvalue.scaled                   aic 
#41.000                62.775                 0.016              3069.016 
#cfi.robust            tli.robust          rmsea.robust rmsea.ci.lower.robust 
#0.811                 0.746                 0.072                 0.032 
#rmsea.ci.upper.robust                  srmr 
#0.106                 0.078 
### made for an overfitted model - indistinguishable constructs with the problem being covariance between shifting ~~ inhibition > 1.2 
### so the factor loadings were looked at
### latent variable z scores suggested removal of:
### c6 (estimate of 0.053, SE 0.158, p = .737)
### F6 (estimate of 0.128, SE 0.136, p = .347)
### d2 (estimate of 0.100, SE 0.101, p = .324)

rm("CFA.model.B")

anova(CFA.model.B.fit, CFA.model.A.fit)

### Model C
CFA.model.C <- '
shifting =~ A6 + B3
inhibition =~ D3 + E5 + F3
wm =~ G2 + H2 + I2
'
CFA.model.C.fit <- cfa(CFA.model.C, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000), meanstructure = TRUE)
fitMeasures(CFA.model.C.fit, fit.measures = fit)
summary(CFA.model.C.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
modificationIndices(CFA.model.C.fit, sort.=TRUE, minimum.value = 3.84)
### OUTPUT:
#df          chisq.scaled         pvalue.scaled                   aic 
#17.000                19.875                 0.281              2174.605 
#cfi.robust            tli.robust          rmsea.robust rmsea.ci.lower.robust 
#0.972                 0.954                 0.040                 0.000 
#rmsea.ci.upper.robust                  srmr 
#0.100                 0.053 
### made for overfitted model  - indistinguishable constructs. The covariance between shifting and inhibition was 1.268. Therefore, just combine shifting and inhibition into one EF factor.
rm("CFA.model.C")
anova(CFA.model.C.fit, CFA.model.B.fit)


### Model D
CFA.model.D <- '
EF =~ A6 + B3 + D3 + E5 + F3
wm =~ G2 + H2 + I2
'

CFA.model.D.fit <- cfa(CFA.model.D, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000), meanstructure = TRUE)
fitMeasures(CFA.model.D.fit, fit.measures = fit)
summary(CFA.model.D.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
modificationIndices(CFA.model.D.fit, sort.=TRUE, minimum.value = 3.84)
### OUTPUT:
#df          chisq.scaled         pvalue.scaled                   aic 
#19.000                21.106                 0.331              2171.983 
#cfi.robust            tli.robust          rmsea.robust rmsea.ci.lower.robust 
#0.979                 0.969                 0.032                 0.000 
#rmsea.ci.upper.robust                  srmr 
#0.093                 0.054 
### This model output was a good fit 
anova(CFA.model.D.fit, CFA.model.C.fit)

rm("CFA.model.D","CFA.model.A.fit","CFA.model.B.fit","CFA.model.C.fit","CFA.model.D.fit")



# 14. CFA model T1 - T3 -----------------------------------------------------

### MODEL CREATION
### different models

CFA.model.wmEF <- '
EF =~ A6 + B3 + D3 + E5 + F3
wm =~ G2 + H2 + I2
'
CFA.model.shiftEF <- '
shifting =~ A6 + B3
EF =~ D3 + E5 + F3 + G2 + H2 + I2
'
CFA.model.inhibEF <- '
inhibition =~ D3 + E5 + F3
EF =~ A6 + B3 + G2 + H2 + I2
'
CFA.model.1factor <- '
EF =~ A6 + B3 + D3 + E5 + F3 + G2 + H2 + I2
'
CFA.model.3factor <- '
shifting =~ A6 + B3
inhibition =~ D3 + E5 + F3
wm =~ G2 + H2 + I2
'

### Time 1 Models
### 3 Factor Model
CFA.model.3factor.fit <- cfa(CFA.model.3factor, data = df1CFA, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.3factor.fit, fit.measures = fit)
### 1 Factor Model
CFA.model.1factor.fit <- cfa(CFA.model.1factor, data = df1CFA, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.1factor.fit, fit.measures = fit)
### wmEF Model
CFA.model.wmEF.fit <- cfa(CFA.model.wmEF, data = df1CFA, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.wmEF.fit, fit.measures = fit)
### shiftEF Model
CFA.model.shiftEF.fit <- cfa(CFA.model.shiftEF, data = df1CFA, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.shiftEF.fit, fit.measures = fit)
### inhibEF Model
CFA.model.inhibEF.fit <- cfa(CFA.model.inhibEF, data = df1CFA, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000)) ### overfitted model - covariance between inhibition ~~ EF = 1.2
summary(CFA.model.inhibEF.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(CFA.model.inhibEF.fit, fit.measures = fit)
### compare models
anova(CFA.model.wmEF.fit, CFA.model.1factor.fit) ### comparing model to one with just one latent factor (nested comparison) - this shows the 2 factor model is significantly better p < .001
rm("CFA.model.3factor.fit","CFA.model.1factor.fit","CFA.model.shiftEF.fit","CFA.model.inhibEF.fit")



### Time 2 Models
### 3 Factor Model
CFA.model.3factor.fit2 <- cfa(CFA.model.3factor, data = df1CFA2, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.3factor.fit2, fit.measures = fit)
### 1 Factor Model
CFA.model.1factor.fit2 <- cfa(CFA.model.1factor, data = df1CFA2, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.1factor.fit2, fit.measures = fit)
### wmEF Model
CFA.model.wmEF.fit2 <- cfa(CFA.model.wmEF, data = df1CFA2, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.wmEF.fit2, fit.measures = fit)
### shiftEF Model
CFA.model.shiftEF.fit2 <- cfa(CFA.model.shiftEF, data = df1CFA2, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
### overfitted model - covariance between shifting ~~ EF = 1.051
summary(CFA.model.shiftEF.fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(CFA.model.shiftEF.fit2, fit.measures = fit)
### inhibEF Model
CFA.model.inhibEF.fit2 <- cfa(CFA.model.inhibEF, data = df1CFA2, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.inhibEF.fit2, fit.measures = fit)
### compare models
anova(CFA.model.wmEF.fit2, CFA.model.1factor.fit2)
rm("CFA.model.3factor.fit2","CFA.model.1factor.fit2","CFA.model.shiftEF.fit2","CFA.model.inhibEF.fit2")


### Time 3 Models
### 3 Factor Model
CFA.model.3factor.fit3 <- cfa(CFA.model.3factor, data = df1CFA3, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.3factor.fit3, fit.measures = fit)
### 1 Factor Model
CFA.model.1factor.fit3 <- cfa(CFA.model.1factor, data = df1CFA3, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.1factor.fit3, fit.measures = fit)
### wmEF Model
CFA.model.wmEF.fit3 <- cfa(CFA.model.wmEF, data = df1CFA3, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000))
fitMeasures(CFA.model.wmEF.fit3, fit.measures = fit)
### shiftEF Model
CFA.model.shiftEF.fit3 <- cfa(CFA.model.shiftEF, data = df1CFA3, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500000)) ### overfitted model - covariance between shifting ~~ wm = 1.155
summary(CFA.model.shiftEF.fit3, fit.measures = TRUE, standardized = TRUE)
fitMeasures(CFA.model.shiftEF.fit3, fit.measures = fit)
### inhibEF Model
CFA.model.inhibEF.fit3 <- cfa(CFA.model.inhibEF, data = df1CFA3, std.lv=TRUE, missing="fiml", estimator="MLR", control=list(iter.max=500)) ### model does not converge - no solution found even at 500,000 iterations
fitMeasures(CFA.model.inhibEF.fit3, fit.measures = fit)
### compare models
anova(CFA.model.wmEF.fit3, CFA.model.1factor.fit3)


rm("CFA.model.3factor.fit3","CFA.model.1factor.fit3","CFA.model.shiftEF.fit3","CFA.model.inhibEF.fit3")
rm("df1CFA","df1CFA2","df1CFA3")
rm("CFA.model.1factor","CFA.model.3factor","CFA.model.shiftEF","CFA.model.inhibEF","fit")





# 15. Path Model for CFA T1 -----------------------------------------------------------
par(mfrow=c(1,1))
analyzeModel <- '
EF =~ A6 + B3 + D3 + E5 + F3
wm =~ G2 + H2 + I2
'
### Time 1
fitted(CFA.model.wmEF.fit)
semPaths(CFA.model.wmEF.fit, "path", whatLabels = "std", layout = "tree3", nodeLabels = c("CardSort\nCLR","Switching","Flanker\nIncong.\nAcc.","No-Go\nAcc.","Stroop RT\nIncong.","OSpan","RSpan","SymSpan","Combined\nEF","WM"),
         curvePivot = TRUE, fade = F, intercepts = F, thresholds = T, bifactor = "g", exoCov = T, exoVar = T, residuals = T, edge.color = "black", nCharNodes = 0, sizeMan = 5, sizeMan2 = 4, label.scale = FALSE, label.cex = .9, edge.label.cex = .9, residScale = 9, mar = c(5,5,10,5))
### Make it easier to read for indicator measures
semPaths(CFA.model.wmEF.fit, "path", whatLabels = "std", layout = "tree3", nodeLabels = c("A","B","C","D","E.","F","G","H","Combined\nEF","WM"),
         curvePivot = TRUE, fade = F, intercepts = F, thresholds = T, bifactor = "g", exoCov = T, exoVar = T, residuals = T, edge.color = "black", nCharNodes = 0, sizeMan = 5, sizeMan2 = 4, label.scale = FALSE, label.cex = .9, edge.label.cex = .9, residScale = 9, mar = c(5,5,10,5))

### Time 2
### Make it easier to read for indicator measures
semPaths(CFA.model.wmEF.fit2, "path", whatLabels = "std", layout = "tree3", nodeLabels = c("A","B","C","D","E.","F","G","H","Combined\nEF","WM"),
         curvePivot = TRUE, fade = F, intercepts = F, thresholds = T, bifactor = "g", exoCov = T, exoVar = T, residuals = T, edge.color = "black", nCharNodes = 0, sizeMan = 5, sizeMan2 = 4, label.scale = FALSE, label.cex = .9, edge.label.cex = .9, residScale = 9, mar = c(5,5,10,5))

### Time 3
### Make it easier to read for indicator measures
semPaths(CFA.model.wmEF.fit3, "path", whatLabels = "std", layout = "tree3", nodeLabels = c("A","B","C","D","E.","F","G","H","Combined\nEF","WM"),
         curvePivot = TRUE, fade = F, intercepts = F, thresholds = T, bifactor = "g", exoCov = T, exoVar = T, residuals = T, edge.color = "black", nCharNodes = 0, sizeMan = 5, sizeMan2 = 4, label.scale = FALSE, label.cex = .9, edge.label.cex = .9, residScale = 9, mar = c(5,5,10,5))
rm("analyzeModel")




# 16. A. SimSEM -----------------------------------------------------------
citation(package = "simsem")
### Use the path model output to place here:
popModel <- '
EF =~ 0.41*A6 + 0.79*B3 + 0.34*D3 + 0.23*E5+ 0.59*F3
wm =~ 0.63*G2 + 0.73*H2 + 0.51*I2
EF ~~ 1.0*EF
wm ~~ 1.0*wm
EF ~~ 0.73*wm
A6 ~~ 0.83*A6
B3 ~~ 0.37*B3
D3 ~~ 0.88*D3
F3 ~~ 0.66*F3
G2 ~~ 0.60*G2
H2 ~~ 0.46*H2
I2 ~~ 0.74*I2
'
### Run a Monte Carlo simulation of the data
### this simulation tests the effect of varying sample size
Output.Fit.nvar <- sim(NULL, analyzeModel, n=80:500, generate=popModel, lavaanfun = "cfa", std.lv = TRUE, seed = 2702, multicore = TRUE)
### this simulation just runs the model n numbers of times (500)
Output.Fit <- sim(500, analyzeModel, n=105, generate=popModel, lavaanfun = "cfa", std.lv = TRUE, seed = 2702, multicore = TRUE)

### Get a summary of the simulations 
summary(Output.Fit.nvar)
summary(Output.Fit)

### Get a summary of the simulation fit data

plotCutoff(Output.Fit, 0.05)
plotCutoff(Output.Fit, 0.01)
getCutoff(Output.Fit, 0.05, nVal = 200)	
Cpow <- getPower(Output.Fit)
Cpow2 <- getPower(Output.Fit, nVal = 200)
Cpow <- as.data.frame(Cpow)
Cpow2 <- as.data.frame(Cpow2)
findPower(Cpow2, "N", 0.80)
print(Cpow)

### power calculations
getPower(Output.Fit, alpha = 0.05) ### this demonstrates the power on each parameter (the proportion of generated datasets in which a particular parameter of interest is significantly different from zero).
getPower(Output.Fit.nvar, nVal = 188) ### this is the sample size required to have a minimum power of 80% for all parameters

### simulation data
cutoff <- c(RMSEA = 0.0489, CFI = 0.9485, SRMR = 0.0674) ### the simulation demonstrated an 80% power of detecting these cutoff values
getPowerFit(Output.Fit, cutoff)
plotPowerFit(Output.Fit, cutoff=cutoff)
plotPowerFit(Output.Fit, cutoff=cutoff, usedFit=c("RMSEA", "SRMR", "CFI"))
rm("Output.Fit","Output.Fit.nvar")
#### POWER ANALYSIS 
### http://www.quantpsy.org/rmsea/rmsea.htm 
#Power analysis for CSM
alpha <- 0.05 #alpha level
d <- 19 #degrees of freedom
n <- 105 #sample size
rmsea0 <- 0.08 #null hypothesized RMSEA
rmseaa <- 0.06 #alternative hypothesized RMSEA

#Code below this point need not be changed by user
ncp0 <- (n-1)*d*rmsea0^2
ncpa <- (n-1)*d*rmseaa^2

#Compute power
if(rmsea0<rmseaa) {
  cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
  pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
if(rmsea0>rmseaa) {
  cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
  pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
print(pow)


### Computation of minimum sample size for test of fit

rmsea0 <- 0.08 #null hypothesized RMSEA
rmseaa <- 0.06 #alternative hypothesized RMSEA
d <- 19 #degrees of freedom
alpha <- 0.05 #alpha level
desired <- 0.8 #desired power


#Code below need not be changed by user
#initialize values
pow <- 0.0
n <- 0
#begin loop for finding initial level of n
while (pow<desired) {
  n <- n+100
  ncp0 <- (n-1)*d*rmsea0^2
  ncpa <- (n-1)*d*rmseaa^2
  #compute power
  if(rmsea0<rmseaa) {
    cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
    pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  else {
    cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
    pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
}

#begin loop for interval halving
foo <- -1
newn <- n
interval <- 200
powdiff <- pow - desired
while (powdiff>.001) {
  interval <- interval*.5
  newn <- newn + foo*interval*.5
  ncp0 <- (newn-1)*d*rmsea0^2
  ncpa <- (newn-1)*d*rmseaa^2
  #compute power
  if(rmsea0<rmseaa) {
    cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
    pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  else {
    cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
    pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  powdiff <- abs(pow-desired)
  if (pow<desired) {
    foo <- 1
  }
  if (pow>desired) {
    foo <- -1
  }
}

minn <- newn
print(minn)
rm("rmsea0","rmseaa","d","alpha","desired","pow","n","foo","newn","interval","powdiff","minn")






# 17. Data Tidying & Removal -----------------------------------------------
### let's remove the unnecessary columns as a result of CFA
df1Final <- df1[, -c(7:11,13:14,16:27,29:34,36:45,47:50,52,53,55,56,58:63,65:66,68:79,81:86,88:97,99:102,104,105,107,108,110:115,117:118,120:131,133:138,140:149,151:154,156,157,159,160,162)]
write.csv(df1Final,"df1Final.csv")
### let's then sort them into the correct order...
### this places data in time order (except for ITIM & IEM)
df1Final <- df1[,c("ID",	"Group",	"Age.Yr",	"Train.Hr",	"Wave",	"Grade",
                   "X1BCSTCLR",	"X1ConnSwitch", "X1Flank.1Acc",	"X1NoGoAcc","X1NSRTI","X1OSPANpartialscore",	"X1RSPANpartialscore",	"X1SYMSPANpartialscore","X1RTaverage",	"X1NAI", "X1PATmScale" ,"X1PATrScale","X1CEFI_P_AT_SS","X1CEFI_P_FX_SS","X1CEFI_P_WM_SS","X1CEFI_T_AT_SS","X1CEFI_T_FX_SS","X1CEFI_T_WM_SS",
                  "X2BCSTCLR", "X2ConnSwitch", "X2Flank.1Acc",	"X2NoGoAcc",	"X2NSRTI",	"X2OSPNpartialscore","X2RSPANpartialscore","X2SYMSPANpartialscore","X2RTaverage","X2NAI","X2PATmScale","X2PATrScale","X2CEFI_P_AT_SS","X2CEFI_P_FX_SS","X2CEFI_P_WM_SS","X2CEFI_T_AT_SS","X2CEFI_T_FX_SS","X2CEFI_T_WM_SS",
                   "X3BCSTCLR", "X3ConnSwitch", "X3Flank.1Acc", "X3NoGoAcc", "X3NSRTI", "X3OSPNpartialscore","X3RSPANpartialscore","X3SYMSPANpartialscore","X3RTaverage","X3NAI","X3PATmScale","X3PATrScale","X3CEFI_P_AT_SS","X3CEFI_P_FX_SS","X3CEFI_P_WM_SS","X3CEFI_T_AT_SS","X3CEFI_T_FX_SS","X3CEFI_T_WM_SS","X1IEMmaths","X1IEMenglish","X1IEMconc.","X1IEMTotalScore","X2IEMmaths","X2IEMenglish","X2IEMconc.","X2IEMTotalScore","ITIM")]

colnames(df1Final)[1:69] <- c("ID",	"Group",	"Age.Yr",	"Train.Hr",	"Wave",	"Grade",
                              "BCSTCLR_1",	"ConnSwitch_1", "Flank.1Acc_1",	"NoGoAcc_1",	"NSRTI_1","OSPANpartialscore_1",	"RSPANpartialscore_1",	"SYMSPANpartialscore_1","RTaverage_1",	"NAI_1", "PATmScale_1" ,"PATrScale_1","CEFI.P.AT.SS_1","CEFI.P.FX.SS_1","CEFI.P.WM.SS_1","CEFI.T.AT.SS_1","CEFI.T.FX.SS_1","CEFI.T.WM.SS_1",
                             "BCSTCLR_2", "ConnSwitch_2", "Flank.1Acc_2",	"NoGoAcc_2",	"NSRTI_2","OSPANpartialscore_2","RSPANpartialscore_2","SYMSPANpartialscore_2","RTaverage_2","NAI_2","PATmScale_2","PATrScale_2","CEFI.P.AT.SS_2","CEFI.P.FX.SS_2","CEFI.P.WM.SS_2","CEFI.T.AT.SS_2","CEFI.T.FX.SS_2","CEFI.T.WM.SS_2",
                             "BCSTCLR_3", "ConnSwitch_3", "Flank.1Acc_3","NoGoAcc_3", "NSRTI_3","OSPANpartialscore_3","RSPANpartialscore_3","SYMSPANpartialscore_3","RTaverage_3","NAI_3","PATmScale_3","PATrScale_3","CEFI.P.AT.SS_3","CEFI.P.FX.SS_3","CEFI.P.WM.SS_3","CEFI.T.AT.SS_3","CEFI.T.FX.SS_3","CEFI.T.WM.SS_3","IEMmaths_1","IEMenglish_1","IEMconc._1","IEMTotalScore_1","IEMmaths_2","IEMenglish_2","IEMconc._2","IEMTotalScore_2","ITIM")

write.csv(df1Final, "df1Final.csv")
### okay, now reshape the data
df1FinalLong <- reshape(df1Final, varying=c(7:60), direction="long", idvar="ID", sep="_")

### let's replace the control group with 0 and experimental with 1 just coz
df1FinalLong$Group <- revalue(df1FinalLong$Group, c("Exp."="Exp")) ### experimental
df1FinalLong$Group <- revalue(df1FinalLong$Group, c("Ctrl"="Control")) ### control group
### let's rename Group to be Intervention, just coz
colnames(df1FinalLong)[2] <- "Intervention"
write.csv(df1FinalLong,"df1FinalLong.csv")






# 18. Correlation among individual EF measures ------------------------------------------------------

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))
  
  ## trunctuate the correlation matrix to three decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 



df1FinalRem <- df1Final[, c(7:30)]
EFindCorr <- corstars(df1FinalRem, method = "spearman", removeTriangle = "lower")
htmlTable(EFindCorr)
rm("df1FinalRem")



# 19. Test-retest reliability (ICC) ------------------------------------------------------
### 
### intraclass correlation coefficient
### have calculated ICC as a whole and for groups
predictedE <- subset(predicted, predicted$Group == "Exp.")
predictedC <- subset(predicted, predicted$Group == "Ctrl")
df1E <- subset(df1, df1$Group == "Exp.")
df1C <- subset(df1, df1$Group == "Ctrl")


### BY GROUPS ###
### Factor Scores
irr::icc(predictedC[,c(8,10,12)], model = "oneway") ## Combined EF Score: 0.77
irr::icc(predictedE[,c(8,10,12)], model = "oneway") ## Combined EF Score: 0.828
irr::icc(predictedC[,c(9,11,13)], model = "oneway") ## WM Score: 0.713
irr::icc(predictedE[,c(9,11,13)], model = "oneway") ## WM Score: 0.753


### Individual EF Measures
### Combined EF Tasks
irr::icc(df1C[,c(12,64,116)], model = "oneway") ## CLR: 0.249
irr::icc(df1E[,c(12,64,116)], model = "oneway") ## CLR: 0.439
irr::icc(df1C[,c(15,67,119)], model = "oneway") ## ConnSwitch: 0.499
irr::icc(df1E[,c(15,67,119)], model = "oneway") ## ConnSwitch: 0.588
irr::icc(df1C[,c(28,80,132)], model = "oneway") ## Flanker Incon Acc: 0.191
irr::icc(df1E[,c(28,80,132)], model = "oneway") ## Flanker Incon Acc: 0.402
irr::icc(df1C[,c(35,87,139)], model = "oneway") ## NoGo Accuracy: 0.222
irr::icc(df1E[,c(35,87,139)], model = "oneway") ## NoGo Accuracy: 0.403
irr::icc(df1C[,c(46,98,150)], model = "oneway") ## NStroop Incon RT: 0.415
irr::icc(df1E[,c(46,98,150)], model = "oneway") ## NStroop Incon RT: 0.602

### WM Tasks
irr::icc(df1C[,c(51,103,155)], model = "oneway") ## Operation Span: 0.338
irr::icc(df1E[,c(51,103,155)], model = "oneway") ## Operation Span: 0.242
irr::icc(df1C[,c(54,106,158)], model = "oneway") ## Reading Span: 0.36
irr::icc(df1E[,c(54,106,158)], model = "oneway") ## Reading Span: 0.662
irr::icc(df1C[,c(57,109,161)], model = "oneway") ## Symmetry Span: 0.466
irr::icc(df1E[,c(57,109,161)], model = "oneway") ## Symmetry Span: 0.239
### Other Tasks
irr::icc(df1C[,c(163:165)], model = "oneway") ## RT: 0.509
irr::icc(df1E[,c(163:165)], model = "oneway") ## RT: 0.442

### Far-transfer tasks
irr::icc(df1C[,c(166:168)], model = "oneway") ## NAI: 0.63
irr::icc(df1C[,c(169:171)], model = "oneway") ## PATm: 0.562
irr::icc(df1C[,c(172:174)], model = "oneway") ## PATr: 0.576
irr::icc(df1E[,c(166:168)], model = "oneway") ## NAI: 0.617
irr::icc(df1E[,c(169:171)], model = "oneway") ## PATm: 0.747
irr::icc(df1E[,c(172:174)], model = "oneway") ## PATr: 0.63

### Behavioural Reports (Parents)
irr::icc(df1C[,c(184,187,190)], model = "oneway") ## CEFIpAT: 0.424
irr::icc(df1C[,c(185,188,191)], model = "oneway") ## CEFIpFx: 0.438
irr::icc(df1C[,c(186,189,192)], model = "oneway") ## CEFIpWM: 0.551
irr::icc(df1E[,c(184,187,190)], model = "oneway") ## CEFIpAT: 0.505
irr::icc(df1E[,c(185,188,191)], model = "oneway") ## CEFIpFx: 0.341
irr::icc(df1E[,c(186,189,192)], model = "oneway") ## CEFIpWM: 0.564
### Behavioural Reports (Teachers)
### T1 v T2
irr::icc(df1C[,c(193,196)], model = "oneway") ## CEFItAT: 0.746
irr::icc(df1C[,c(194,197)], model = "oneway") ## CEFItFx: 0.672
irr::icc(df1C[,c(195,198)], model = "oneway") ## CEFItWM: 0.698
irr::icc(df1E[,c(193,196)], model = "oneway") ## CEFItAT: 0.675
irr::icc(df1E[,c(194,197)], model = "oneway") ## CEFItFx: 0.539
irr::icc(df1E[,c(195,198)], model = "oneway") ## CEFItWM: 0.711
### T2 v T3
irr::icc(df1C[,c(196,199)], model = "twoway", type = "agreement") ## CEFItAT: 0.614
irr::icc(df1C[,c(197,200)], model = "twoway", type = "agreement") ## CEFItFx: 0.465
irr::icc(df1C[,c(198,201)], model = "twoway", type = "agreement") ## CEFItWM: 0.358
irr::icc(df1E[,c(196,199)], model = "twoway", type = "agreement") ## CEFItAT: 0.514
irr::icc(df1E[,c(197,200)], model = "twoway", type = "agreement") ## CEFItFx: 0.417
irr::icc(df1E[,c(198,201)], model = "twoway", type = "agreement") ## CEFItWM: 0.481
### T1, T2, T3
irr::icc(df1C[,c(193,196,199)], model = "twoway", type = "agreement") ## CEFItAT: 0.705
irr::icc(df1C[,c(194,197,200)], model = "twoway", type = "agreement") ## CEFItFx: 0.594
irr::icc(df1C[,c(195,198,201)], model = "twoway", type = "agreement") ## CEFItWM: 0.562
irr::icc(df1E[,c(193,196,199)], model = "twoway", type = "agreement") ## CEFItAT: 0.61
irr::icc(df1E[,c(194,197,200)], model = "twoway", type = "agreement") ## CEFItFx: 0.508
irr::icc(df1E[,c(195,198,201)], model = "twoway", type = "agreement") ## CEFItWM: 0.529







### ALL PARTICIPANTS ALL TOGETHER ### 
### Factor Scores
### all participants together
irr::icc(predicted[,c(8,10,12)], model = "oneway") ## Combined EF Score: 0.816
irr::icc(predicted[,c(9,11,13)], model = "oneway") ## WM Score: 0.761

### Individual EF Measures
### Combined EF Tasks
### All participants together
irr::icc(df1[,c(12,64,116)], model = "oneway") ## CLR: 0.342
irr::icc(df1[,c(15,67,119)], model = "oneway") ## ConnSwitch: 0.553
irr::icc(df1[,c(28,80,132)], model = "oneway") ## Flanker Incon Acc: 0.287
irr::icc(df1[,c(35,87,139)], model = "oneway") ## NoGo Accuracy: 0.306
irr::icc(df1[,c(46,98,150)], model = "oneway") ## NStroop Incon RT: 0.55
### WM Tasks
irr::icc(df1[,c(51,103,155)], model = "oneway") ## Operation Span: 0.317
irr::icc(df1[,c(54,106,158)], model = "oneway") ## Reading Span: 0.57
irr::icc(df1[,c(57,109,161)], model = "oneway") ## Symmetry Span: 0.384
### Other Tasks
irr::icc(df1[,c(163:165)], model = "oneway") ## RT: 0.482


### Far-transfer tasks
irr::icc(df1[,c(166:168)], model = "oneway") ## NAI: 0.628
irr::icc(df1[,c(169:171)], model = "oneway") ## PATm: 0.671
irr::icc(df1[,c(172:174)], model = "oneway") ## PATr: 0.605

### Behavioural Reports (Parents)
irr::icc(df1[,c(184,187,190)], model = "oneway") ## CEFIpAT: 0.462
irr::icc(df1[,c(185,188,191)], model = "oneway") ## CEFIpFx: 0.39
irr::icc(df1[,c(186,189,192)], model = "oneway") ## CEFIpWM: 0.558
### Behavioural Reports (Teachers)
### T1 v T2
irr::icc(df1[,c(193,196)], model = "oneway") ## CEFItAT: 0.758
irr::icc(df1[,c(194,197)], model = "oneway") ## CEFItFx: 0.668
irr::icc(df1[,c(195,198)], model = "oneway") ## CEFItWM: 0.744
### T2 v T3
irr::icc(df1[,c(196,199)], model = "twoway") ## CEFItAT: 0.623
irr::icc(df1[,c(197,200)], model = "twoway") ## CEFItFx: 0.507
irr::icc(df1[,c(198,201)], model = "twoway") ## CEFItWM: 0.464
### T1, T2, T3
irr::icc(df1[,c(193,196,199)], model = "twoway", type = "agreement") ## CEFItAT: 0.708
irr::icc(df1[,c(194,197,200)], model = "twoway", type = "agreement") ## CEFItFx: 0.607
irr::icc(df1[,c(195,198,201)], model = "twoway", type = "agreement") ## CEFItWM: 0.594


### Original EF Measures that were dropped as part of CFA
irr::icc(df1[,c(10,62,114)], model = "oneway") ## PE: 0.27
irr::icc(df1[,c(20,72,124)], model = "oneway") ## SwitchErr2Feat: 0.301
irr::icc(df1[,c(22,74,126)], model = "oneway") ## SwitchDiffError: -0.022
irr::icc(df1[,c(27,79,131)], model = "oneway") ## Flanker CC RT: -0.052
irr::icc(df1[,c(43,95,147)], model = "oneway") ## Go RT average: 0.4
irr::icc(df1[,c(49,101,153)], model = "oneway") ## NStroop Incon Acc: 0.402



# 20. Data for LCM ----------------------------------------------
df1CFA<-df1[,7:58] ### time 1
df1CFA2<-df1[,59:110] ### time 2
df1CFA3<-df1[,111:162] ### time 3
df1CFA <- scale(df1CFA)
df1CFA2 <- scale(df1CFA2)
df1CFA3 <- scale(df1CFA3)
df1CFA <- as.data.frame(df1CFA)
df1CFA2 <- as.data.frame(df1CFA2)
df1CFA3 <- as.data.frame(df1CFA3)
colnames(df1CFA)[1:52] <- c("A1","A2","A3","A4","A5","A6","B1","B2","B3","B4","C1","C2","C3","C4","C5","C6","C7","C8","C9","D1","D2","D3","D4","D5","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","F1","F2","F3","F4","F5","F6","G1","G2","G3","H1","H2","H3","I1","I2","I3")
colnames(df1CFA2)[1:52] <- c("A1_2","A2_2","A3_2","A4_2","A5_2","A6_2","B1_2","B2_2","B3_2","B4_2","C1_2","C2_2","C3_2","C4_2","C5_2","C6_2","C7_2","C8_2","C9_2","D1_2","D2_2","D3_2","D4_2","D5_2","E1_2","E2_2","E3_2","E4_2","E5_2","E6_2","E7_2","E8_2","E9_2","E10_2","E11_2","E12_2","E13_2","F1_2","F2_2","F3_2","F4_2","F5_2","F6_2","G1_2","G2_2","G3_2","H1_2","H2_2","H3_2","I1_2","I2_2","I3_2")
colnames(df1CFA3)[1:52] <- c("A1_3","A2_3","A3_3","A4_3","A5_3","A6_3","B1_3","B2_3","B3_3","B4_3","C1_3","C2_3","C3_3","C4_3","C5_3","C6_3","C7_3","C8_3","C9_3","D1_3","D2_3","D3_3","D4_3","D5_3","E1_3","E2_3","E3_3","E4_3","E5_3","E6_3","E7_3","E8_3","E9_3","E10_3","E11_3","E12_3","E13_3","F1_3","F2_3","F3_3","F4_3","F5_3","F6_3","G1_3","G2_3","G3_3","H1_3","H2_3","H3_3","I1_3","I2_3","I3_3")
df1CFA <- cbind(df1[,1:6],df1CFA,df1CFA2,df1CFA3)
#df1CFA <- cbind(df1[,1:6],df1CFA)
rm("df1CFA2","df1CFA3")

fit = c("df","chisq.scaled","pvalue.scaled","aic","cfi.robust","tli.robust","rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr")




# 21. LCM - Iterative process -----------------------------
### **try iterative approach as in Beaujean (2014) ###
### model for a mean latent intercept and constrained residual variances
CFA.model.A <- '
EF1 =~ A6 + B3 + D3 + E5 + F3 
wm1 =~ G2 + H2 + I2

EF2 =~ A6_2 + B3_2 + D3_2 + E5_2 + F3_2 
wm2 =~ G2_2 + H2_2 + I2_2

EF3 =~ A6_3 + B3_3 + D3_3 + E5_3 + F3_3 
wm3 =~ G2_3 + H2_3 + I2_3

wmi =~ 1*wm1 + 1*wm2 + 1*wm3
wmi ~~ 0*wmi
efi =~ 1*EF1 + 1*EF2 + 1*EF3
efi ~~ 0*efi

EF1 ~~ e*EF1
EF2 ~~ e*EF2
EF3 ~~ e*EF3

wm1 ~~ w*wm1
wm2 ~~ w*wm2
wm3 ~~ w*wm3

A6 ~~ a*A6_2
A6 ~~ a*A6_3
A6_2 ~~ a*A6_3
B3 ~~ b*B3_2
B3 ~~ b*B3_3
B3_2 ~~ b*B3_3
D3 ~~ d*D3_2
D3 ~~ d*D3_3
D3_2 ~~ d*D3_3
E5 ~~ e*E5_2
E5 ~~ e*E5_3
E5_2 ~~ e*E5_3
F3 ~~ f*F3_2
F3 ~~ f*F3_3
F3_2 ~~ f*F3_3
G2 ~~ g*G2_2
G2 ~~ g*G2_3
G2_2 ~~ g*G2_3
H2 ~~ h*H2_2
H2 ~~ h*H2_3
H2_2 ~~ h*H2_3
I2 ~~ i*I2_2
I2 ~~ i*I2_3
I2_2 ~~ i*I2_3

# constrain latent residual covariances
EF1 ~~ z*EF2
EF1 ~~ z*EF3
EF2 ~~ z*EF3

wm1 ~~ y*wm2
wm1 ~~ y*wm3
wm2 ~~ y*wm3


'
CFA.model.A.fit <- growth(CFA.model.A, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", meanstructure = TRUE)
fitMeasures(CFA.model.A.fit, fit.measures = fit)


# model for a mean latent intercept that is allowed to vary, and constrained residual variances
CFA.model.B <- '
EF1 =~ A6 + B3 + D3 + E5 + F3 
wm1 =~ G2 + H2 + I2

EF2 =~ A6_2 + B3_2 + D3_2 + E5_2 + F3_2 
wm2 =~ G2_2 + H2_2 + I2_2

EF3 =~ A6_3 + B3_3 + D3_3 + E5_3 + F3_3 
wm3 =~ G2_3 + H2_3 + I2_3

wmi =~ 1*wm1 + 1*wm2 + 1*wm3
efi =~ 1*EF1 + 1*EF2 + 1*EF3

EF1 ~~ e*EF1
EF2 ~~ e*EF2
EF3 ~~ e*EF3

wm1 ~~ w*wm1
wm2 ~~ w*wm2
wm3 ~~ w*wm3

A6 ~~ a*A6_2
A6 ~~ a*A6_3
A6_2 ~~ a*A6_3
B3 ~~ b*B3_2
B3 ~~ b*B3_3
B3_2 ~~ b*B3_3
D3 ~~ d*D3_2
D3 ~~ d*D3_3
D3_2 ~~ d*D3_3
E5 ~~ e*E5_2
E5 ~~ e*E5_3
E5_2 ~~ e*E5_3
F3 ~~ f*F3_2
F3 ~~ f*F3_3
F3_2 ~~ f*F3_3
G2 ~~ g*G2_2
G2 ~~ g*G2_3
G2_2 ~~ g*G2_3
H2 ~~ h*H2_2
H2 ~~ h*H2_3
H2_2 ~~ h*H2_3
I2 ~~ i*I2_2
I2 ~~ i*I2_3
I2_2 ~~ i*I2_3

# constrain latent residual covariances
EF1 ~~ z*EF2
EF1 ~~ z*EF3
EF2 ~~ z*EF3

wm1 ~~ y*wm2
wm1 ~~ y*wm3
wm2 ~~ y*wm3


'
CFA.model.B.fit <- growth(CFA.model.B, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", meanstructure = TRUE)
fitMeasures(CFA.model.B.fit, fit.measures = fit)
anova(CFA.model.B.fit,CFA.model.A.fit)

# model with a mean latent intercept that is allowed to vary, mean latent slope, and constrained residual variances
CFA.model.C <- '
EF1 =~ A6 + B3 + D3 + E5 + F3 
wm1 =~ G2 + H2 + I2

EF2 =~ A6_2 + B3_2 + D3_2 + E5_2 + F3_2 
wm2 =~ G2_2 + H2_2 + I2_2

EF3 =~ A6_3 + B3_3 + D3_3 + E5_3 + F3_3 
wm3 =~ G2_3 + H2_3 + I2_3

wmi =~ 1*wm1 + 1*wm2 + 1*wm3
efi =~ 1*EF1 + 1*EF2 + 1*EF3

efs =~ 0*EF1 + 1*EF2 + 3*EF3
wms =~ 0*wm1 + 1*wm2 + 3*wm3
wms ~ 0*1
wms ~~ 0*wmi
efs ~ 0*1
wms ~~ 0*wms

EF1 ~~ e*EF1
EF2 ~~ e*EF2
EF3 ~~ e*EF3

wm1 ~~ w*wm1
wm2 ~~ w*wm2
wm3 ~~ w*wm3

A6 ~~ a*A6_2
A6 ~~ a*A6_3
A6_2 ~~ a*A6_3
B3 ~~ b*B3_2
B3 ~~ b*B3_3
B3_2 ~~ b*B3_3
D3 ~~ d*D3_2
D3 ~~ d*D3_3
D3_2 ~~ d*D3_3
E5 ~~ e*E5_2
E5 ~~ e*E5_3
E5_2 ~~ e*E5_3
F3 ~~ f*F3_2
F3 ~~ f*F3_3
F3_2 ~~ f*F3_3
G2 ~~ g*G2_2
G2 ~~ g*G2_3
G2_2 ~~ g*G2_3
H2 ~~ h*H2_2
H2 ~~ h*H2_3
H2_2 ~~ h*H2_3
I2 ~~ i*I2_2
I2 ~~ i*I2_3
I2_2 ~~ i*I2_3

# constrain latent residual covariances
EF1 ~~ z*EF2
EF1 ~~ z*EF3
EF2 ~~ z*EF3

wm1 ~~ y*wm2
wm1 ~~ y*wm3
wm2 ~~ y*wm3


'
CFA.model.C.fit <- growth(CFA.model.C, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", meanstructure = TRUE)
fitMeasures(CFA.model.C.fit, fit.measures = fit)
anova(CFA.model.C.fit,CFA.model.B.fit) ### does not fit any better than B


# model for a mean latent intercept that is allowed to vary, mean latent slope that is allowed to vary, and constrained residual variances
CFA.model.D <- '
EF1 =~ A6 + B3 + D3 + E5 + F3 
wm1 =~ G2 + H2 + I2

EF2 =~ A6_2 + B3_2 + D3_2 + E5_2 + F3_2 
wm2 =~ G2_2 + H2_2 + I2_2

EF3 =~ A6_3 + B3_3 + D3_3 + E5_3 + F3_3 
wm3 =~ G2_3 + H2_3 + I2_3

wmi =~ 1*wm1 + 1*wm2 + 1*wm3
efi =~ 1*EF1 + 1*EF2 + 1*EF3

efs =~ 0*EF1 + 1*EF2 + 3*EF3
wms =~ 0*wm1 + 1*wm2 + 3*wm3

EF1 ~~ e*EF1
EF2 ~~ e*EF2
EF3 ~~ e*EF3

wm1 ~~ w*wm1
wm2 ~~ w*wm2
wm3 ~~ w*wm3

A6 ~~ a*A6_2
A6 ~~ a*A6_3
A6_2 ~~ a*A6_3
B3 ~~ b*B3_2
B3 ~~ b*B3_3
B3_2 ~~ b*B3_3
D3 ~~ d*D3_2
D3 ~~ d*D3_3
D3_2 ~~ d*D3_3
E5 ~~ e*E5_2
E5 ~~ e*E5_3
E5_2 ~~ e*E5_3
F3 ~~ f*F3_2
F3 ~~ f*F3_3
F3_2 ~~ f*F3_3
G2 ~~ g*G2_2
G2 ~~ g*G2_3
G2_2 ~~ g*G2_3
H2 ~~ h*H2_2
H2 ~~ h*H2_3
H2_2 ~~ h*H2_3
I2 ~~ i*I2_2
I2 ~~ i*I2_3
I2_2 ~~ i*I2_3

# constrain latent residual covariances
EF1 ~~ z*EF2
EF1 ~~ z*EF3
EF2 ~~ z*EF3

wm1 ~~ y*wm2
wm1 ~~ y*wm3
wm2 ~~ y*wm3


'
CFA.model.D.fit <- growth(CFA.model.D, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", meanstructure = TRUE)
fitMeasures(CFA.model.D.fit, fit.measures = fit)
anova(CFA.model.D.fit,CFA.model.C.fit) ### does not fit any better than C


# unconstrained model
CFA.model.E <- '
EF1 =~ A6 + B3 + D3 + E5 + F3 
wm1 =~ G2 + H2 + I2

EF2 =~ A6_2 + B3_2 + D3_2 + E5_2 + F3_2 
wm2 =~ G2_2 + H2_2 + I2_2

EF3 =~ A6_3 + B3_3 + D3_3 + E5_3 + F3_3 
wm3 =~ G2_3 + H2_3 + I2_3

wmi =~ 1*wm1 + 1*wm2 + 1*wm3
efi =~ 1*EF1 + 1*EF2 + 1*EF3

efs =~ 0*EF1 + 1*EF2 + 3*EF3
wms =~ 0*wm1 + 1*wm2 + 3*wm3

A6 ~~ a*A6_2
A6 ~~ a*A6_3
A6_2 ~~ a*A6_3
B3 ~~ b*B3_2
B3 ~~ b*B3_3
B3_2 ~~ b*B3_3
D3 ~~ d*D3_2
D3 ~~ d*D3_3
D3_2 ~~ d*D3_3
E5 ~~ e*E5_2
E5 ~~ e*E5_3
E5_2 ~~ e*E5_3
F3 ~~ f*F3_2
F3 ~~ f*F3_3
F3_2 ~~ f*F3_3
G2 ~~ g*G2_2
G2 ~~ g*G2_3
G2_2 ~~ g*G2_3
H2 ~~ h*H2_2
H2 ~~ h*H2_3
H2_2 ~~ h*H2_3
I2 ~~ i*I2_2
I2 ~~ i*I2_3
I2_2 ~~ i*I2_3

# constrain latent residual covariances
EF1 ~~ z*EF2
EF1 ~~ z*EF3
EF2 ~~ z*EF3

wm1 ~~ y*wm2
wm1 ~~ y*wm3
wm2 ~~ y*wm3


' 
CFA.model.E.fit <- growth(CFA.model.E, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", meanstructure = TRUE)
fitMeasures(CFA.model.E.fit, fit.measures = fit)
anova(CFA.model.E.fit,CFA.model.D.fit) ### fits better than D
residuals(CFA.model.E.fit, type = "cor")
lavInspect(CFA.model.E.fit, "cov.lv")


# unconstrained model + ADD IN GROUPS
CFA.model.F <- '
EF1 =~ A6 + B3 + D3 + E5 + F3 
wm1 =~ G2 + H2 + I2

EF2 =~ A6_2 + B3_2 + D3_2 + E5_2 + F3_2 
wm2 =~ G2_2 + H2_2 + I2_2

EF3 =~ A6_3 + B3_3 + D3_3 + E5_3 + F3_3 
wm3 =~ G2_3 + H2_3 + I2_3

wmi =~ 1*wm1 + 1*wm2 + 1*wm3
efi =~ 1*EF1 + 1*EF2 + 1*EF3

efs =~ 0*EF1 + 1*EF2 + 3*EF3
wms =~ 0*wm1 + 1*wm2 + 3*wm3

A6 ~~ a*A6_2
A6 ~~ a*A6_3
A6_2 ~~ a*A6_3
B3 ~~ b*B3_2
B3 ~~ b*B3_3
B3_2 ~~ b*B3_3
D3 ~~ d*D3_2
D3 ~~ d*D3_3
D3_2 ~~ d*D3_3
E5 ~~ e*E5_2
E5 ~~ e*E5_3
E5_2 ~~ e*E5_3
F3 ~~ f*F3_2
F3 ~~ f*F3_3
F3_2 ~~ f*F3_3
G2 ~~ g*G2_2
G2 ~~ g*G2_3
G2_2 ~~ g*G2_3
H2 ~~ h*H2_2
H2 ~~ h*H2_3
H2_2 ~~ h*H2_3
I2 ~~ i*I2_2
I2 ~~ i*I2_3
I2_2 ~~ i*I2_3


# constrain latent residual covariances
EF1 ~~ z*EF2
EF1 ~~ z*EF3
EF2 ~~ z*EF3

wm1 ~~ y*wm2
wm1 ~~ y*wm3
wm2 ~~ y*wm3

### add groups
wmi + wms ~ Group
efi + efs ~ Group
' 
CFA.model.F.fit <- growth(CFA.model.F, data = df1CFA, std.lv = TRUE, missing="fiml", estimator="MLR", meanstructure = TRUE)
fitMeasures(CFA.model.F.fit, fit.measures = fit)
anova(CFA.model.F.fit,CFA.model.E.fit) # fits better than E
residuals(CFA.model.A.fit, type = "cor")
lavInspect(CFA.model.A.fit, "cov.lv")












#...----------------------------------------------
#...----------------------------------------------

#  *** DATA ANALYSIS ***-----------------------------------------

# 22. Creation of CFA Factor Scores (Near-Transfer) -----------------------------------------

#### predicted scores 2 factor...
predictT1 <- lavPredict(CFA.model.wmEF.fit)
predictT1 <- as.data.frame(predictT1)
names(predictT1) <- c("EF_1","wm_1")
predictT2 <- lavPredict(CFA.model.wmEF.fit2)
predictT2 <- as.data.frame(predictT2)
names(predictT2) <- c("EF_2","wm_2")
predictT3 <- lavPredict(CFA.model.wmEF.fit3)
predictT3 <- as.data.frame(predictT3)
names(predictT3) <- c("EF_3","wm_3")
predicted <- cbind(df1[,1:6],predictT1,predictT2,predictT3)
predicted <- cbind(predicted,df1Final[,7:69])
row.names(predicted) <- 1:nrow(predicted)
predictedLong <- reshape(predicted, varying=c(7:66), direction="long", idvar="ID", sep="_")
colnames(predictedLong)[2] <- "Intervention"
rm("predictT1","predictT2","predictT3")
#rm("CFA.model.wmEF.fit","CFA.model.wmEF.fit2","CFA.model.wmEF.fit3")

###
### add the dummy coded column (Int = Intervention) as this only works for some packages later
dummycol <- dummy.code(predicted$Group)
dummycol <- as.data.frame(dummycol)
predicted <- add_column(predicted, dummycol$Exp., .after = "Group")
colnames(predicted)[3] <- "Int"
predicted$Int = as.factor(predicted$Int)
rm("dummycol")

write.csv(predicted,"factor scores.csv")

#predicted <- predicted[,-c(4)]


### Differences between time 1 scores?
t.test(EF_1 ~ Group, data=predicted)
describeBy(predicted$EF_1, predicted$Group)
mes(-0.2, 0.18, 0.91, 0.8, 50, 55) ### Cohen's d 

t.test(wm_1 ~ Group, data=predicted)
describeBy(predicted$wm_1, predicted$Group)
mes(-0.25, 0.23, 0.86, 0.77, 50, 55) ### Cohen's d 





# 23. Creating Difference Scores ----------------------------------------------
### This will allow us to determine if participants scoring higher on achievement tests at T1 scored greater gains (high positive correlation) or whether lower ability students scored greater gains (high negative correlation).
### change (or gain) scores have been used before. This study follows the process of Karbach, Strobach  Schubert (2015) where these change scores are used as raw values. However, these are sometimes standardised (change score divided by standard deviation of the whole sample at pretest e.g. Jaeggi et al. 2011, Studer-Luethi et al. 2016) or are used to regress on T1 scores (e.g. Dahlin, 2011)
diffScores <- predicted
### create time 2 - time 1 scores
EF2EF1 <- diffScores$EF_2 - diffScores$EF_1
diffScores <- add_column(diffScores, EF2EF1, .after = "Grade")
WM2WM1 <- diffScores$wm_2 - diffScores$wm_1
diffScores <- add_column(diffScores, WM2WM1, .after = "EF2EF1")
### create other two scores required (time 3 - time 1)
EF3EF1 <- diffScores$EF_3 - diffScores$EF_1
diffScores <- add_column(diffScores, EF3EF1, .after = "WM2WM1")
WM3WM1 <- diffScores$wm_3 - diffScores$wm_1
diffScores <- add_column(diffScores, WM3WM1, .after = "EF3EF1")
rm("EF2EF1", "WM2WM1","EF3EF1","WM3WM1")

### add in the far-transfer difference scores
NAI2NAI1 <- diffScores$NAI_2 - diffScores$NAI_1
diffScores <- add_column(diffScores, NAI2NAI1, .after = "ITIM")
PATm2PATm1 <- diffScores$PATmScale_2 - diffScores$PATmScale_1
diffScores <- add_column(diffScores, PATm2PATm1, .after = "NAI2NAI1")
PATr2PATr1 <- diffScores$PATrScale_2 - diffScores$PATrScale_1
diffScores <- add_column(diffScores, PATr2PATr1, .after = "PATm2PATm1")
write.csv(diffScores, "diffscores.csv")



### Let's also create difference scores that are 'standardised' as per Jaeggi et al etc.
diffScoresSTD <- predicted
### create time 2 - time 1 scores
EF2EF1std <- (diffScores$EF_2 - diffScores$EF_1)/sd(diffScores$EF_1)
diffScoresSTD <- add_column(diffScoresSTD, EF2EF1std, .after = "Grade")
WM2WM1std <- (diffScores$wm_2 - diffScores$wm_1)/sd(diffScores$wm_1)
diffScoresSTD <- add_column(diffScoresSTD, WM2WM1std, .after = "EF2EF1std")
### create other two scores required (time 3 - time 1)
EF3EF1std <- (diffScores$EF_3 - diffScores$EF_1)/sd(diffScores$EF_1)
diffScoresSTD <- add_column(diffScoresSTD, EF3EF1std, .after = "WM2WM1std")
WM3WM1std <- (diffScores$wm_3 - diffScores$wm_1)/sd(diffScores$wm_1)
diffScoresSTD <- add_column(diffScoresSTD, WM3WM1std, .after = "EF3EF1std")
rm("EF2EF1std", "WM2WM1std","EF3EF1std","WM3WM1std")

### add in the far-transfer difference scores
NAI2NAI1std <- (diffScores$NAI_2 - diffScores$NAI_1)/sd(diffScores$NAI_1)
diffScoresSTD <- add_column(diffScoresSTD, NAI2NAI1std, .after = "ITIM")
PATm2PATm1std <- (diffScores$PATmScale_2 - diffScores$PATmScale_1)/sd(diffScores$PATmScale_1)
diffScoresSTD <- add_column(diffScoresSTD, PATm2PATm1std, .after = "NAI2NAI1std")
PATr2PATr1std <- (diffScores$PATrScale_2 - diffScores$PATrScale_1)/sd(diffScores$PATrScale_1)
diffScoresSTD <- add_column(diffScoresSTD, PATr2PATr1std, .after = "PATm2PATm1std")
write.csv(diffScoresSTD, "diffscoresSTD.csv")





# 24. Box Plots of Near-Transfer & Far-Transfer-----------------------------------------

### Plots
level_order <- c('Ctrl', 'Exp.') #this vector might be useful for other plots/analyses

### Boxplot EF
ggplot(predictedLong, aes(factor(Intervention, level = level_order), EF, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "Combined EF Factor Score", limits = c(-3,4), breaks=seq(-3,3,1/2)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))

### Boxplot WM
ggplot(predictedLong, aes(factor(Intervention, level = level_order), wm, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "WM Factor Score", limits = c(-3,4), breaks=seq(-3,3,1/2)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))
#theme_apa()



### Boxplot PATm
ggplot(predictedLong, aes(factor(Intervention, level = level_order),  PATmScale, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "PAT Maths", limits = c(80,200), breaks=seq(80,180,10)) + 
  #theme(axis.ticks = element_blank()) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))
#theme_apa(legend.pos = "bottom", x.font.size = 10, y.font.size = 10, facet.title.size = 10)




### Boxplots PATr
ggplot(predictedLong, aes(factor(Intervention, level = level_order),PATrScale, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "PAT Reading", limits = c(80,200), breaks=seq(80,180,10)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))
#theme_apa()



### Boxplots NAI
ggplot(predictedLong, aes(factor(Intervention, level = level_order),  NAI, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "gF", limits = c(80,180), breaks=seq(80,160,10)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))




### Boxplots CEFI.P.AT.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order),  CEFI.P.AT.SS, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "CEFI (Parent) - Inhibition", limits = c(70,160), breaks=seq(70,140,10)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplots CEFI.P.FX.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.P.FX.SS, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "CEFI (Parent) - Shifting", limits = c(70,160), breaks=seq(70,140,10)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplots CEFI.P.WM.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.P.WM.SS, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "CEFI (Parent) - WM", limits = c(70,160), breaks=seq(70,140,10)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))




### Boxplots CEFI.T.AT.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.AT.SS, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "CEFI (Teacher) - Inhibition", limits = c(70,160), breaks=seq(70,140,10)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplots CEFI.T.FX.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.FX.SS, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "CEFI (Teacher) - Shifting", limits = c(70,160), breaks=seq(70,140,10)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplots CEFI.T.WM.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.WM.SS, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_y_continuous(name = "CEFI (Teacher) - WM", limits = c(70,160), breaks=seq(70,140,10)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


#### Longitudinal Individual plots
## EF
ggplot(data = predictedLong, aes(x = time, y = EF, group = ID)) + 
  geom_line(size=0.1) + 
  stat_smooth(aes(group = 1)) +
  scale_x_continuous(name = "Time", limits = c(1,3), breaks = seq(1,3,1) ) +
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 2) + 
  facet_grid(. ~ Intervention) + 
  scale_fill_manual(values=c("#CC6666", "#9999CC")) + 
  scale_color_manual(values=c("#CC6666", "#9999CC")) + 
  theme_apa()

## wm
ggplot(data = predictedLong, aes(x = time, y = wm, group = ID)) + 
  geom_line(size=0.1) + 
  stat_smooth(aes(group = 1)) + 
  scale_x_continuous(name = "Time", limits = c(1,3), breaks = seq(1,3,1) ) +
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 2) + 
  facet_grid(. ~ Intervention) + 
  #scale_fill_manual(values=c("#CC6666", "#9999CC")) + 
  #scale_color_manual(values=c("#CC6666", "#9999CC")) + 
  theme_apa()









# 25. Line Plots of Near-Transfer & Far-Transfer-------------------
### Need to create a new dataframe of summary data:
### gF
x1<-describeBy(predicted$NAI_1, predicted$Group)
x2<-describeBy(predicted$NAI_2, predicted$Group)
x3<-describeBy(predicted$NAI_3, predicted$Group)
xPATnai <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "gF", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### PAT Maths
x1<-describeBy(predicted$PATmScale_1, predicted$Group)
x2<-describeBy(predicted$PATmScale_2, predicted$Group)
x3<-describeBy(predicted$PATmScale_3, predicted$Group)
xPATm <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "Maths", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### PAT Reading
x1<-describeBy(predicted$PATrScale_1, predicted$Group)
x2<-describeBy(predicted$PATrScale_2, predicted$Group)
x3<-describeBy(predicted$PATrScale_3, predicted$Group)
xPATr <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "Reading", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### WM
x1<-describeBy(predicted$wm_1, predicted$Group)
x2<-describeBy(predicted$wm_2, predicted$Group)
x3<-describeBy(predicted$wm_3, predicted$Group)
xWM <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "WM", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### EF
x1<-describeBy(predicted$EF_1, predicted$Group)
x2<-describeBy(predicted$EF_2, predicted$Group)
x3<-describeBy(predicted$EF_3, predicted$Group)
xEF <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "EF", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))


####
####
### The plots ###
### EF line plot
ggplot(xEF, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Combined EF\nFactor Score", limits = c(-2.5,2.5), breaks=seq(-2.5,2.5,0.5)) +
  scale_x_continuous(breaks=seq(1,3,1)) 

### WM line plot
ggplot(xWM, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "WM Factor Score", limits = c(-2.5,2.5), breaks=seq(-2.5,2.5,0.5)) +
  scale_x_continuous(breaks=seq(1,3,1)) 

### PAT Maths line plot
ggplot(xPATm, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "PAT Maths", limits = c(110,160), breaks=seq(110,160,10)) +
  scale_x_continuous(breaks=seq(1,3,1)) 

### PAT Reading line plot
ggplot(xPATr, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "PAT Reading", limits = c(110,160), breaks=seq(110,160,10)) +
  scale_x_continuous(breaks=seq(1,3,1)) 

### gF line plot
ggplot(xPATnai, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Fluid Intelligence", limits = c(90,140), breaks=seq(90,140,10)) +
  scale_x_continuous(breaks=seq(1,3,1)) 

###
###
### GArdner-Altman Plots:
unpaired_mean_diff <- dabest(predicted, Group, idx = c("Exp.","Ctrl"), wm_1, paired = FALSE)
unpaired_mean_diff <- dabest(predicted, Group, idx = c("Exp.","Ctrl"), EF_1, paired = FALSE)
unpaired_mean_diff <- dabest(predicted, Group, idx = c("Exp.","Ctrl"), EF_2, paired = FALSE)
plot(unpaired_mean_diff)





# 26. Box Plots of Transfer Measures by Grade -----------------------------------------

### Now look at them separated by Grade
### Boxplot EF
ggplot(predictedLong, aes(factor(Intervention, level = level_order), EF, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "Combined EF Factor Score", limits = c(-2.7, 2), breaks=seq(-3,2,1/2)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10), legend.title = element_text(size=10))


### Boxplot WM
ggplot(predictedLong, aes(factor(Intervention, level = level_order), wm, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "WM Factor Score", limits = c(-2.7, 2), breaks=seq(-3,2,1/2)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10), legend.title = element_text(size=10))


### Boxplot PATm
ggplot(predictedLong, aes(factor(Intervention, level = level_order), PATmScale, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "PAT Maths", limits = c(80,200), breaks=seq(80,180,10)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplot PATr
ggplot(predictedLong, aes(factor(Intervention, level = level_order), PATrScale, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "PAT Reading", limits = c(80,200), breaks=seq(80,180,10)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplot NAI
ggplot(predictedLong, aes(factor(Intervention, level = level_order), NAI, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "NAI", limits = c(80,200), breaks=seq(80,180,10)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplot CEFI-p-AT
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.P.AT.SS, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "CEFI Parent Inhibition", limits = c(70,160), breaks=seq(70,140,10)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplot CEFI-p-FX
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.P.FX.SS, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "CEFI Parent Shifting", limits = c(70,160), breaks=seq(70,140,10)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplot CEFI-p-WM
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.P.WM.SS, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "CEFI Parent WM", limits = c(70,160), breaks=seq(70,140,10)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplot CEFI-t-AT
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.AT.SS, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "CEFI Teacher Inhibition", limits = c(70,160), breaks=seq(70,140,10)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplot CEFI-t-FX
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.FX.SS, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "CEFI Teacher Shifting", limits = c(70,160), breaks=seq(70,140,10)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplot CEFI-t-WM
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.WM.SS, fill=factor(time), color = factor(Grade))) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Time:", breaks = c("1","2","3"), labels = c("1","2","3")) +
  scale_color_discrete(labels = c("Year 3","Year 5")) +
  scale_color_manual(name = "Grade:", values=c("red", "black")) +
  labs(color = "Grade") +
  scale_y_continuous(name = "CEFI Teacher WM", limits = c(70,160), breaks=seq(70,140,10)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))







# 27. Box Plots of Transfer by Grade (Time-separated) ------------------------------------

### Separate Grade plots (facets) for Time data...
### PATm
ggplot(predictedLong, aes(factor(Intervention, level = level_order), PATmScale, fill=factor(Grade))) + 
  geom_boxplot() + 
  scale_y_continuous(name = "PAT Maths", breaks=seq(-3,3,1/2)) + 
  #scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10)) + facet_grid(~time)


### PATr
ggplot(predictedLong, aes(factor(Intervention, level = level_order), PATrScale, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "PAT Reading", breaks=seq(-3,3,1/2)) + 
  #scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~time)


### NAI
ggplot(predictedLong, aes(factor(Intervention, level = level_order), NAI, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "Fluid Intelligence", breaks=seq(-3,3,1/2)) + 
  #scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~time)


### Boxplots CEFI.T.AT.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.AT.SS, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "CEFI (Teacher) - Inhibition") +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~time)


### Boxplots CEFI.T.FX.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.FX.SS, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "CEFI (Teacher) - Shifting") +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~time)


### Boxplots CEFI.T.WM.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.WM.SS, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "CEFI (Teacher) - WM") +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~time)







# 28. Box Plots of Transfer by Wave  ------------------------------------

### Separate Wave plots (facets) for Grade data...
### Plots

### Boxplot EF
ggplot(predictedLong, aes(factor(Intervention, level = level_order), EF, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2","3"), labels = c("Time 1","Time 2","Time 3")) +
  scale_y_continuous(name = "Combined EF Factor Score", breaks=seq(-3,3,1/2)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10)) + facet_grid(~Wave)


### Boxplot WM
ggplot(predictedLong, aes(factor(Intervention, level = level_order), wm, fill=factor(Grade))) +
  geom_boxplot() + 
  #scale_fill_discrete(name = element_blank(), breaks = c("1","2","3"), labels = c("Time 1","Time 2","Time 3")) +
  scale_y_continuous(name = "WM Factor Score", breaks=seq(-3,3,1/2)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10)) + facet_grid(~Wave)
#theme_apa()



### PATm
ggplot(predictedLong, aes(factor(Intervention, level = level_order), PATmScale, fill=factor(time))) + 
  geom_boxplot() + 
  scale_y_continuous(name = "PAT Maths", breaks=seq(-3,3,1/2)) + 
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10)) + 
  facet_grid(~Wave)


### PATr
ggplot(predictedLong, aes(factor(Intervention, level = level_order), PATrScale, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "PAT Reading", breaks=seq(-3,3,1/2)) + 
  #scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~Wave)


### NAI
ggplot(predictedLong, aes(factor(Intervention, level = level_order), NAI, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "Fluid Intelligence", breaks=seq(-3,3,1/2)) + 
  #scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~Wave)


### Boxplots CEFI.T.AT.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.AT.SS, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "CEFI (Teacher) - Inhibition") +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~Wave)


### Boxplots CEFI.T.FX.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.FX.SS, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "CEFI (Teacher) - Shifting") +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~Wave)


### Boxplots CEFI.T.WM.SS
ggplot(predictedLong, aes(factor(Intervention, level = level_order), CEFI.T.WM.SS, fill=factor(Grade))) +
  geom_boxplot() + 
  scale_y_continuous(name = "CEFI (Teacher) - WM") +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))+ facet_grid(~Wave)








# 29. CFA Factor Scores - Multivariate Normality Testing ---------------------------------
### Create subset of predicted based upon group
predictedE <- subset(predicted, predicted$Group == "Exp.")
predictedC <- subset(predicted, predicted$Group == "Ctrl")

### Create subset of each EF construct
predictedE1 <- predictedE[,8:9] ## T1
predictedC1 <- predictedC[,8:9] ## T1
predictedE2 <- predictedE[,10:11] ## T2
predictedC2 <- predictedC[,10:11] ## T2
predictedE3 <- predictedE[,12:13] ## T3
predictedC3 <- predictedC[,12:13] ## T3
rm("predictedE","predictedC")

### Experimental
mvn(predictedE1, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
mvn(predictedE2, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE) ### Non-normal
mvn(predictedE3, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
rm("predictedE1","predictedE2","predictedE3")

### Control
mvn(predictedC1, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
mvn(predictedC2, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
mvn(predictedC3, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)
rm("predictedC1","predictedC2","predictedC3")









# 30. CFA Factor Scores - Correlations & Plots ---------------------------------
### CORRELATIONS-FACTOR SCORES
### correlation of predicted scores with achievement tests?
apa.cor.table(predicted[,c(8:9,23:25)], filename = "Factor_Table.doc", table.number = 1)
apa.cor.table(predicted[,c(14:31)], filename = "EF Measures_Table.doc", table.number = 2)
apa.cor.table(predicted[,c(8:9,29:31)]) ## EF v CEFI-t
apa.cor.table(predicted[,c(8:9,26:28)]) ## EF v CEFI-p


### EF correlations
cor.test(predicted$EF_1, predicted$NAI_1, method = "p") ## EF - NAI r = 0.364***
cor.test(predicted$EF_1, predicted$PATmScale_1, use = "p") ## EF - PATm r = 0.734***
cor.test(predicted$EF_1, predicted$PATrScale_1, use = "p") ## EF - PATr r = 0.510***

### wm correlations
cor.test(predicted$wm_1, predicted$NAI_1, use = "p") ## wm - NAI r = 0.346***
cor.test(predicted$wm_1, predicted$PATmScale_1, use = "p") ## wm - PATm r = 0.638***
cor.test(predicted$wm_1, predicted$PATrScale_1, use = "p") ## wm - PATr r = 0.471***



### Consistency...
### time 1 vs time 2
cor.test(predicted$EF_1, predicted$EF_2, use = "p") ## r = 0.804***
cor.test(predicted$wm_1, predicted$wm_2, use = "p") ## r = 0.778***
### time 1 vs time 3
cor.test(predicted$EF_1, predicted$EF_3, use = "p") ## r = 0.844***
cor.test(predicted$wm_1, predicted$wm_3, use = "p") ## r = 0.781***
### time 2 vs time 3
cor.test(predicted$EF_2, predicted$EF_3, use = "p") ## r = 0.802***
cor.test(predicted$wm_2, predicted$wm_3, use = "p") ## r = 0.726***



### Plots of some of the correlations:
### Plot WM v PATr
qplot(x = EF_1, y = PATrScale_1, color = Group, data = predicted) +
  geom_smooth(method = lm, se = FALSE, aes(color = Group))
qplot(x = EF_1, y = PATrScale_1, color = Group, facets =  ~Grade, data = predicted) +
  geom_smooth(method = lm, se = FALSE, aes(color = Group))

### Plot EF v PATm
qplot(x = EF_1, y = PATmScale_1, color = Group, data = predicted) +
  geom_smooth(method = lm, se = FALSE, aes(color = Group)) +
  scale_y_continuous(name = "Maths") + 
  scale_x_continuous(name = "EF Factor Score") +
  theme_bw(base_size = 11, base_family = "sans")

qplot(x = EF_1, y = PATmScale_1, color = Group, facets =  ~Grade, data = predicted) +
  geom_smooth(method = lm, se = FALSE, aes(color = Group)) +
  scale_y_continuous(name = "Maths") + 
  scale_x_discrete(name = "EF Factor Score") +
  theme_bw(base_size = 11, base_family = "sans")
### Plot EF v NAI
qplot(x = EF_1, y = NAI_1, color = Group, data = predicted) +
  geom_smooth(method = lm, se = FALSE, aes(color = Group)) + 
  scale_y_continuous(name = "Fluid Intelligence") + 
  scale_x_discrete(name = "EF Factor Score") +
  theme_bw(base_size = 11, base_family = "sans")

qplot(x = EF_1, y = NAI_1, color = Group, facets =  ~Grade, data = predicted) +
  geom_smooth(method = lm, se = FALSE, aes(color = Group)) +
  scale_y_continuous(name = "Fluid Intelligence") + 
  scale_x_discrete(name = "EF Factor Score") +
  theme_bw(base_size = 11, base_family = "sans")


### PS correlations with latent variables
### predicted scores vs reaction time
cor.test(predicted$EF_1, df1$X1RTaverage, use = "p") ## EF - RT r = -0.591***
cor.test(predicted$wm_1, df1$X1RTaverage, use = "p") ## wm - RT r = -0.465***
### predicted scores vs reaction time
cor.test(predicted$EF_1, df1$X1RTaverage, method = "kendall") ## EF - RT r = -0.591***
cor.test(predicted$wm_1, df1$X1RTaverage, use = "p") ## wm - RT r = -0.465***

cor.test(diffScores$EF2EF1, diffScores$PPPP.plat, method = "kendall")
set.seed(2702)
kendall.ci(diffScores$EF2EF1, diffScores$PPPP.plat, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)


### ITIM
scatterplot(predicted$EF_1 ~ df1$ITIM | predicted$Group)
cor.test(predicted$EF_1, df1$ITIM, use = "p") # r = 0.205*
cor.test(predicted$EF_2, df1$ITIM, use = "p") 
cor.test(predicted$EF_3, df1$ITIM, use = "p")
cor.test(predicted$wm_1, df1$ITIM, use = "p") ## r = 0.241*
cor.test(predicted$wm_2, df1$ITIM, use = "p") ## r = 0.206*
cor.test(predicted$wm_3, df1$ITIM, use = "p") 








#...----------------------------------------------
#...----------------------------------------------

#    *** ANCOVA: Near-Transfer ***-----------------------------------------
# 31. Near-Transfer ANCOVA - Parametric & Robust --------------------------------------
### Time 2
### Time 2 vs Time 1
###
###
### EF T2 vs T1
### This shows a difference between groups for scores on T2 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(EF_2 ~ EF_1 + Group, data = predicted)
model2 <- lm(EF_2 ~ EF_1 * Group, data = predicted)
simpleModel <- lm(EF_2 ~ Group, data = predicted)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$EF_2 ~ predicted$Group, center = mean) ## *** significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(EF_2 ~ EF_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)
 
par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(EF_2 ~ EF_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$EF_2, predicted$Group)


### Robust
### remember that the p-value does not take into account multiple testing
### so need to multiply each value by 5 (number of tests taken)
ancova(predicted$EF_2 ~ predicted$EF_1 + predicted$Group) ### no significant differences
p <- c(.1086, .0616, .0626, .0345, .0209)
p.adjust(p, method = "holm")
ggplot(data = predicted, mapping = aes(x = EF_1, y = EF_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "EF Factor Score T1", y = "EF Factor Score T2") +
  geom_vline(xintercept = c(-1.1, -0.15, 0.31, 0.75, 1.5), col = "orange", lty=3) +
  theme_apa()


### EF T3 vs T1
### Not Significant
model1 <- lm(EF_3~ EF_1 + Group, data = predicted)
model2 <- lm(EF_3 ~ EF_1 * Group, data = predicted)
simpleModel <- lm(EF_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test for homogeneity of variance
leveneTest(predicted$EF_3 ~ predicted$Group, center = mean) ## not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(EF_3~ EF_1 + Group, data = predicted), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## not significant
Anova(aov(EF_3 ~ EF_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$EF_3, predicted$Group)

### Robust
ancova(predicted$EF_3 ~ predicted$EF_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = EF_1, y = EF_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "EF Factor Score T1", y = "EF Factor Score T3") +
  geom_vline(xintercept = c(-1.1, -0.15, 0.31, 0.75, 1.5), col = "orange", lty=3) +
  theme_apa()



## WM T2 vs T1
### This shows a difference between groups for scores on T2 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(wm_2 ~ wm_1 + Group, data = predicted)
model2 <- lm(wm_2 ~ wm_1 * Group, data = predicted)
simpleModel <- lm(EF_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test for homogeneity of variance
leveneTest(predicted$wm_2 ~ predicted$Group, center = mean) ## significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(wm_2 ~ wm_1 + Group, data = predicted), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## * significant: different between groups for scores on T2 when accounting for T1 scores
Anova(aov(wm_2 ~ wm_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$wm_2, predicted$Group)

### Robust
ancova(predicted$wm_2 ~ predicted$wm_1 + predicted$Group ) ### sig
p <- c(.0972, .0006, .0135, .0148, .0023)
p.adjust(p, method = "holm")
### post-hoc analysis using plots
ggplot(data = predicted, mapping = aes(x = wm_1, y = wm_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "WM Factor Score T1", y = "WM Factor Score T2") +
  geom_vline(xintercept = c(-1.09, -0.24, 0.25, 0.52, 1.3), col = "black", lty=3) +
  theme_apa()
ggplot(data = predicted, mapping = aes(x = wm_1, y = wm_2, color = factor(Group, level = level_order))) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  scale_x_continuous(name = "WM Factor Score T1", limits = c(-2.5,2), breaks=seq(-2.5,2,0.5)) +
  scale_y_continuous(name = "WM Factor Score T2", limits = c(-2.6,2), breaks=seq(-2.5,2,0.5)) +
  scale_color_discrete(name = "Group:", labels = c("Control","Experimental")) +
  geom_vline(xintercept = c(-1.09, -0.24, 0.25, 0.52, 1.3), col = "black", lty=3) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10), legend.title = element_text(size = 10), axis.title = element_text(size=10), axis.text = element_text(size=10)) 


## WM T3 vs T1
### Not Significant
model1 <- lm(wm_3 ~ wm_1 + Group, data = predicted)
model2 <- lm(wm_3 ~ wm_1 * Group, data = predicted)
simpleModel <- lm(EF_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$wm_3 ~ predicted$Group, center = mean) ## Not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(wm_3 ~ wm_1 + Group, data = predicted), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not significant
Anova(aov(wm_3 ~ wm_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$wm_3, predicted$Group)

### Robust
ancova(predicted$wm_3 ~ predicted$wm_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = wm_1, y = wm_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "WM Factor Score T1", y = "WM Factor Score T3") +
  geom_vline(xintercept = c(-1.1, -0.24, 0.25, 0.52, 1.3), col = "orange", lty=3) +
  theme_apa()










# 32. Near-Transfer - Grade Interactions ------------------------

### 2-way ANOVAs for interaction of Group / Covariate
IntNames <- c('0' = "Control Group", '1' = "Experimental Group")

### non outlier removed data
summ(lm(EF_2 ~ EF_1 * Grade, data = predicted)) # * sig
summ(lm(wm_2 ~ wm_1 * Grade, data = predicted)) # * sig

### Johnson-Neyman approach to simple interactions
### WM
myModel <- lm(wm_2 ~ wm_1 * Grade * Int, data = predicted) # sig
summ(myModel)
sim_slopes(myModel, pred = wm_1, modx = Grade, mod2 = Int, johnson_neyman = TRUE, jnplot = TRUE)
interact_plot(myModel, wm_1, Grade, plot = TRUE)

### Plot of T1 v T2 by Grade & Group
qplot(x = EF_1, y = EF_2, color = Grade, data = predicted) +
  geom_smooth(method = lm, se = FALSE, aes(color = Grade)) + 
  scale_y_continuous(name = "EF Time 2", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) + 
  scale_x_continuous(name = "EF Time 1", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) +
  facet_grid(~Int, labeller = labeller(Int = IntNames)) +
  theme_bw(base_size = 11, base_family = "sans")

qplot(x = wm_1, y = wm_2, color = Grade, data = predicted) +
  geom_smooth(method = lm, se = FALSE, aes(color = Grade)) + 
  scale_y_continuous(name = "WM Time 2", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) + 
  scale_x_continuous(name = "WM Time 1", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) +
  facet_grid(~Int, labeller = labeller(Int = IntNames)) +
  theme_bw(base_size = 11, base_family = "sans")











### EF T1 vs Grade:Group Interaction
model1 <- lm(EF_2 ~ Group * Grade, data = predicted)
for(i in 1:4)plot(model1, which=i)

summary(model1)
summ(model1, confint = TRUE)
export_summs(model1)
Anova(model1, type = "III")

adjustedMeans <- effect("Age.Yr", model1, se = TRUE)
effect_plot(model1, pred = Age.Yr, plot.points = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary.lm(model1)

postHocs <- glht(model1, linfct = mcp(Group = "Tukey"))
summary(postHocs)
confint(postHocs)
Anova(aov(EF_1 ~ Group * Age.Yr, data = predicted), type = "III")

### WM T1 vs Grade:Group Interaction
model1 <- lm(wm_1 ~ Group * Grade, data = predicted)
op=par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
par(op)

summary(model1)
summ(model1, robust = TRUE, scale = TRUE, confint = TRUE)
export_summs(model1)
Anova(model1, type = "III")

adjustedMeans <- effect("Group", model1, se = TRUE)
effect_plot(model1, pred = Group, plot.points = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary.lm(model1)

postHocs <- glht(model1, linfct = mcp(Group = "Tukey"))
summary(postHocs)
confint(postHocs)
Anova(aov(EF_2 ~ EF_1 * Group, data = predicted), type = "III")









# 33. Multivariate Removal of Outliers in Factor Scores ---------------------------------
### Create subset of predicted based upon group
predictedE <- subset(predicted, predicted$Group == "Exp.")
predictedC <- subset(predicted, predicted$Group == "Ctrl")

### Create subset of each EF construct
predictedE1 <- predictedE[,8:9] ## Exp. T1
predictedC1 <- predictedC[,8:9] ## Control T1
predictedE2 <- predictedE[,10:11] ## Exp. T2
predictedC2 <- predictedC[,10:11] ## Control T2
predictedE3 <- predictedE[,12:13] ## Exp. T3
predictedC3 <- predictedC[,12:13] ## Control T3
rm("predictedE","predictedC")



### this is removing of multivariate outliers in Experimental Group
set.seed(2702)
par(mfrow = c(1, 2))
newDataE1 <- mvn(predictedE1, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)

newDataE2 <- mvn(predictedE2, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)

newDataE3 <- mvn(predictedE3, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)

# removal of outliers
newDataE1$multivariateOutliers$Observation
newDataE2$multivariateOutliers$Observation
newDataE3$multivariateOutliers$Observation
predictedOR <- predicted ### create new data for outlier removal
predictedOR <- predictedOR[-c(7,8,9,13,60,68),]
row.names(predictedOR) <- 1:nrow(predictedOR)
rm("newDataE1","newDataE2","newDataE3")
rm("predictedE1","predictedE2","predictedE3")



## this is removing of multivariate outliers in Control Group
newDataC1 <- mvn(predictedC1, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)

newDataC2 <- mvn(predictedC2, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)

newDataC3 <- mvn(predictedC3, mvnTest = "mardia", scale = TRUE, desc = TRUE, univariateTest = "AD", multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)

# removal of outliers
newDataC1$multivariateOutliers$Observation
newDataC2$multivariateOutliers$Observation
newDataC3$multivariateOutliers$Observation
### 24 = 20; 45 = 41; 99 = 93; 104 = 98
predictedOR <- predictedOR[-c(20,41, 93, 98),]
row.names(predictedOR) <- 1:nrow(predictedOR)

# create long data for graphing
predictedORLong <- reshape(predictedOR, varying=c(8:67), direction="long", idvar="ID", sep="_")
colnames(predictedORLong)[2] <- "Intervention"
rm("newDataC1","newDataC2","newDataC3")
rm("predictedC1","predictedC2","predictedC3")


#### So What are the numbers in control v experimental now?
table(predictedOR$Group)




# 34. Plots of Near-Transfer (outlier removed) --------------------------------------


### Plots
ggplot(predictedORLong, aes(factor(Intervention, level = level_order), EF, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2","3"), labels = c("Time 1","Time 2","Time 3")) +
  scale_y_continuous(name = "Combined EF Factor Score", breaks=seq(-3,3,1/2)) + 
  #theme(axis.ticks = element_blank()) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))
#theme_apa(legend.pos = "bottom", x.font.size = 10, y.font.size = 10, facet.title.size = 10)


ggplot(predictedORLong, aes(factor(Intervention, level = level_order), wm, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2","3"), labels = c("Time 1","Time 2","Time 3")) +
  scale_y_continuous(name = "WM Factor Score", breaks=seq(-3,3,1/2)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))
#theme_apa()
#### Longitudinal plots
## EF
ggplot(data = predictedORLong, aes(x = time, y = EF, group = ID)) + 
  geom_line(size=0.1) + 
  stat_smooth(aes(group = 1)) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 2) + 
  facet_grid(. ~ Intervention) + 
  scale_fill_manual(values=c("#CC6666", "#9999CC")) + 
  scale_color_manual(values=c("#CC6666", "#9999CC")) + 
  theme_apa()

## wm
ggplot(data = predictedORLong, aes(x = time, y = wm, group = ID)) + 
  geom_line(size=0.1) + 
  stat_smooth(aes(group = 1)) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 2) + 
  facet_grid(. ~ Intervention) + 
  #scale_fill_manual(values=c("#CC6666", "#9999CC")) + 
  #scale_color_manual(values=c("#CC6666", "#9999CC")) + 
  theme_apa()








# 35. Near-Transfer (outlier removed) - ANCOVAs --------------------------------------
## ANCOVA

### EF T2 vs T1
### This shows a difference between groups for scores on T2 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(EF_2 ~ EF_1 + Group, data = predictedOR)
model2 <- lm(EF_2 ~ EF_1 * Group, data = predictedOR)
simpleModel <- lm(EF_2 ~ Group, data = predictedOR)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predictedOR$EF_2 ~ predictedOR$Group, center = mean) ## significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(EF_2 ~ EF_1 + Group, data = predictedOR), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### ES (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)
citation("sjstats")

model1 <- lm(EF_2 ~ EF_1 + Group, data = predictedOR)
par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(EF_2 ~ EF_1 * Group, data = predictedOR), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
n <- c(44,51) ### sample size
adjustedMeans$se*sqrt(n) ### standard deviation
mes(-.0283, 0.269, 0.472, 0.471, 44, 51) ### Cohen's d
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$EF_2, predictedOR$Group)


### EF T3 vs T1
### Not Significant
model1 <- lm(EF_3~ EF_1 + Group, data = predictedOR)
model2 <- lm(EF_3 ~ EF_1 * Group, data = predictedOR)
simpleModel <- lm(EF_3 ~ Group, data = predictedOR)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test for homogeneity of variance
leveneTest(predictedOR$EF_3 ~ predictedOR$Group, center = mean) ## not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(EF_3~ EF_1 + Group, data = predictedOR), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9) ### report partial eta squared
sjstats::eta_sq(model1, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## not significant
Anova(aov(EF_3 ~ EF_1 * Group, data = predictedOR), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$EF_3, predictedOR$Group)


## WM T2 vs T1
### This shows a difference between groups for scores on T2 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(wm_2 ~ wm_1 + Group, data = predictedOR)
model2 <- lm(wm_2 ~ wm_1 * Group, data = predictedOR)
simpleModel <- lm(EF_2 ~ Group, data = predictedOR)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test for homogeneity of variance
leveneTest(predictedOR$wm_2 ~ predictedOR$Group, center = mean) ## significant
### Robust
ancova(predictedOR$wm_2 ~ predictedOR$wm_1 + predictedOR$Group ) ### sig
p <- c(.0203, .0076, .0144, .0213, .0034)
p.adjust(p, method = "holm")
### post-hoc analysis using plots
ggplot(data = predictedOR, mapping = aes(x = wm_1, y = wm_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "WM Factor Score T1", y = "WM Factor Score T2") +
  geom_vline(xintercept = c(-0.7, 0, 0.3, 0.5, 1.2), col = "black", lty=3) +
  theme_apa()


## WM T3 vs T1
### Not Significant
model1 <- lm(wm_3 ~ wm_1 + Group, data = predictedOR)
model2 <- lm(wm_3 ~ wm_1 * Group, data = predictedOR)
simpleModel <- lm(EF_2 ~ Group, data = predictedOR)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predictedOR$wm_3 ~ predictedOR$Group, center = mean) ## Not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(wm_3 ~ wm_1 + Group, data = predictedOR), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::eta_sq(model1, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not significant
Anova(aov(wm_3 ~ wm_1 * Group, data = predictedOR), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$wm_3, predictedOR$Group)




### 36. Near-Transfer (outlier removed) - Interactions -------------------
### 2-way ANOVAs for interaction of Group / Covariate
### none of these are significant - it seems that the outliers were the ones that impacted an interaction
summ(lm(EF_2 ~ EF_1 * Age.Yr, data = predictedOR)) 
summ(lm(EF_3 ~ EF_1 * Age.Yr, data = predictedOR))
summ(lm(wm_2 ~ wm_1 * Age.Yr, data = predictedOR))
summ(lm(wm_3 ~ wm_1 * Age.Yr, data = predictedOR))

### non outlier removed data
summ(lm(EF_2 ~ EF_1 * Age.Yr, data = predicted)) # * sig
summ(lm(EF_3 ~ EF_1 * Age.Yr, data = predicted))
summ(lm(wm_2 ~ wm_1 * Age.Yr, data = predicted)) # * sig
summ(lm(wm_3 ~ wm_1 * Age.Yr, data = predicted))



### can use this to test if assumptions are normal?
### try the model and graphs
myModel <- lm(wm_2 ~ wm_1 + Group, data = predictedOR)
myModel <- lm(wm_2 ~  + Int +(wm_1 *Int), data = predictedOR)
myModel <- lm(EF_2 ~  + Int +(EF_1 *Int), data = predictedOR)

plot(myModel)
summ(myModel)
summary(myModel)
plot(effect("Int", myModel))
sim_slopes(myModel, pred = Int, modx = EF_1, johnson_neyman = TRUE, jnplot = TRUE)
sim_slopes(myModel, pred = Int, modx = wm_1)
interact_plot(myModel, pred = Int, modx = wm_1, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
slopemodel <- sim_slopes(myModel, pred = Int, modx = wm_1)
probe_interaction(myModel, pred = Int, modx = wm_1)
interact_plot(myModel, pred = Int, modx = wm_1, x.label = "Group", y.label = "Posttest Score") + theme_apa()



### EF T1 vs Grade:Group Interaction
model1 <- lm(EF_2 ~ Group * Grade, data = predictedOR)
for(i in 1:4)plot(model1, which=i)

summary(model1)
summ(model1, robust = TRUE, scale = TRUE, confint = TRUE)
export_summs(model1)
Anova(model1, type = "III")

adjustedMeans <- effect("Age.Yr", model1, se = TRUE)
effect_plot(model1, pred = Age.Yr, plot.points = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary.lm(model1)

postHocs <- glht(model1, linfct = mcp(Group = "Tukey"))
summary(postHocs)
confint(postHocs)
Anova(aov(EF_1 ~ Group * Age.Yr, data = predictedOR), type = "III")

### WM T1 vs Grade:Group Interaction
model1 <- lm(wm_1 ~ Group * Grade, data = predictedOR)
op=par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
par(op)

summary(model1)
summ(model1, robust = TRUE, scale = TRUE, confint = TRUE)
export_summs(model1)
Anova(model1, type = "III")

adjustedMeans <- effect("Group", model1, se = TRUE)
effect_plot(model1, pred = Group, plot.points = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary.lm(model1)

postHocs <- glht(model1, linfct = mcp(Group = "Tukey"))
summary(postHocs)
confint(postHocs)
Anova(aov(EF_2 ~ EF_1 * Group, data = predictedOR), type = "III")




# *** CST Data analysed individually ***  -------------------------------------------
# *** CST Data removed listwise ***  -------------------------------------------
# 37. Near-Transfer (CSTs) - plots -------------------------------------------

### Line graphs
### Operation Span
x1<-describeBy(df1$X1OSPANpartialscore, df1$Group)
x2<-describeBy(df1$X2OSPNpartialscore, df1$Group)
x3<-describeBy(df1$X3OSPNpartialscore, df1$Group)
xWM <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "Operation Span", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### WM line plot
ggplot(xWM, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Operation Span", limits = c(0.4, 0.9), breaks=seq(0.4,0.9,0.1)) +
  scale_x_continuous(breaks=seq(1,3,1)) 


### Reading Span
x1<-describeBy(df1$X1RSPANpartialscore, df1$Group)
x2<-describeBy(df1$X2RSPANpartialscore, df1$Group)
x3<-describeBy(df1$X3RSPANpartialscore, df1$Group)
xWM <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "Reading Span", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### WM line plot
ggplot(xWM, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Reading Span", limits = c(0.4, 0.9), breaks=seq(0.4,0.9,0.1)) +
  scale_x_continuous(breaks=seq(1,3,1)) 



### Symmetry Span
x1<-describeBy(df1$X1SYMSPANpartialscore, df1$Group)
x2<-describeBy(df1$X2SYMSPANpartialscore, df1$Group)
x3<-describeBy(df1$X3SYMSPANpartialscore, df1$Group)
xWM <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "Reading Span", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### WM line plot
ggplot(xWM, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Symmetry Span", limits = c(0.4, 0.9), breaks=seq(0.4,0.9,0.1)) +
  scale_x_continuous(breaks=seq(1,3,1)) 






# 38. Near-Transfer (CSTs) - Operation Span -------------------------------------------


### recap on baseline differences:
### Individual Working Memory measures
t.test(X1OSPANpartialscore ~ Group, data=df1)# * significant difference
describeBy(df1$X1OSPANpartialscore, df1$Group)
mes(0.54, 0.67, 0.2, 0.22, 39, 48) ### Cohen's d = -0.62 [ -1.05, -0.18]
t.test(X1RSPANpartialscore ~ Group, data=df1)# * significant difference
describeBy(df1$X1RSPANpartialscore, df1$Group)
mes(0.57, 0.67, 0.23, 0.19, 44, 46) ### Cohen's d  = -0.48 [ -0.9, -0.05]
t.test(X1SYMSPANpartialscore ~ Group, data=df1) # no sig difference


                                  ### ANCOVAs ###


                                  ### OPERATION SPAN ###

### Time 2
### Time 2 vs Time 1
###
### Operation Span T2 vs T1
model1 <- lm(X2OSPNpartialscore ~ X1OSPANpartialscore + Group, data = df1)
model2 <- lm(X2OSPNpartialscore ~ X1OSPANpartialscore * Group, data = df1)
simpleModel <- lm(X2OSPNpartialscore ~ Group, data = df1)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1$X2OSPNpartialscore ~ df1$Group, center = mean) ## *** significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X2OSPNpartialscore ~ X1OSPANpartialscore + Group, data = df1), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X2OSPNpartialscore ~ X1OSPANpartialscore * Group, data = df1), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1$X2OSPNpartialscore, df1$Group)


### Robust
### remember that the p-value does not take into account multiple testing
### so need to multiply each value by 5 (number of tests taken)
ancova(df1$X2OSPNpartialscore ~ df1$X1OSPANpartialscore + df1$Group) ### no significant differences
p <- c(.0879, .1004, .0872, .1884, .0336)
p.adjust(p, method = "holm")
ggplot(data = df1, mapping = aes(x = X1OSPANpartialscore, y = X2OSPNpartialscore, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Operation Span Score T1", y = "Operation Span Score T2") +
  geom_vline(xintercept = c(0.48, 0.64, 0.72, 0.8, 0.88), col = "blue", lty=3) +
  theme_apa()


### Time 3
### Time 3 vs Time 1
###
### Operation Span T3 vs T1
model1 <- lm(X3OSPNpartialscore ~ X1OSPANpartialscore + Group, data = df1)
model2 <- lm(X3OSPNpartialscore ~ X1OSPANpartialscore * Group, data = df1)
simpleModel <- lm(X3OSPNpartialscore ~ Group, data = df1)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1$X3OSPNpartialscore ~ df1$Group, center = mean) ## non significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X3OSPNpartialscore ~ X1OSPANpartialscore + Group, data = df1), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X3OSPNpartialscore ~ X1OSPANpartialscore * Group, data = df1), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1$X3OSPNpartialscore, df1$Group)




# 39. Near-Transfer (CSTs) - Reading Span -------------------------------------------

                                    ### READING SPAN ###

### Time 2
### Time 2 vs Time 1
###
### Operation Span T2 vs T1
model1 <- lm(X2RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1)
model2 <- lm(X2RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1)
simpleModel <- lm(X2RSPANpartialscore ~ Group, data = df1)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1$X2RSPANpartialscore ~ df1$Group, center = mean) ## non significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X2RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X2RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1$X2RSPANpartialscore, df1$Group)

### Calculate the difference and effect size for significant effect of Group
model1 <- lm(X2RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1)
par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X2RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
### Trying to easily calc sample size...
df1copy <- df1[!(df1$X1RSPANpartialscore=="" | df1$X2RSPANpartialscore==""),]
count(df1copy, df1copy$Group == "Exp.") ### now have exp n = 40 and control n = 40
n <- c(40,40) ### sample size
adjustedMeans$se*sqrt(n) ### standard deviation
mes(0.640, 0.736, 0.138, 0.138, 40, 40) ### Cohen's d
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1$X2RSPANpartialscore, df1$Group)







### Time 3
### Time 3 vs Time 1
###
### Operation Span T3 vs T1
model1 <- lm(X3RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1)
model2 <- lm(X3RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1)
simpleModel <- lm(X3RSPANpartialscore ~ Group, data = df1)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1$X3RSPANpartialscore ~ df1$Group, center = mean) ## non- significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X3RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X3RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1$X3RSPANpartialscore, df1$Group)







# 40. Near-Transfer (CSTs) - Symmetry Span -------------------------------------------

                                  ### SYMMETRY SPAN ###

### Time 2
### Time 2 vs Time 1
###
### Operation Span T2 vs T1
model1 <- lm(X2SYMSPANpartialscore ~ X1SYMSPANpartialscore + Group, data = df1)
model2 <- lm(X2SYMSPANpartialscore ~ X1SYMSPANpartialscore * Group, data = df1)
simpleModel <- lm(X2SYMSPANpartialscore ~ Group, data = df1)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1$X2SYMSPANpartialscore ~ df1$Group, center = mean) ## non significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X2SYMSPANpartialscore ~ X1SYMSPANpartialscore + Group, data = df1), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X2SYMSPANpartialscore ~ X1SYMSPANpartialscore * Group, data = df1), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1$X2SYMSPANpartialscore, df1$Group)



### Time 3
### Time 3 vs Time 1
###
### Operation Span T3 vs T1
model1 <- lm(X3SYMSPANpartialscore ~ X1SYMSPANpartialscore + Group, data = df1)
model2 <- lm(X3SYMSPANpartialscore ~ X1SYMSPANpartialscore * Group, data = df1)
simpleModel <- lm(X3SYMSPANpartialscore ~ Group, data = df1)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ###  problems here!!
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1$X3SYMSPANpartialscore ~ df1$Group, center = mean) ## *** significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X3SYMSPANpartialscore ~ X1SYMSPANpartialscore + Group, data = df1), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X3SYMSPANpartialscore ~ X1SYMSPANpartialscore * Group, data = df1), type = "III") ## interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1$X3SYMSPANpartialscore, df1$Group)


### Robust
### remember that the p-value does not take into account multiple testing
### so need to multiply each value by 5 (number of tests taken)
ancova(df1$X3SYMSPANpartialscore ~ df1$X1SYMSPANpartialscore + df1$Group) ### no significant differences
p <- c(.3512, .8597, .6071, .2173, .1589)
p.adjust(p, method = "holm")
ggplot(data = df1, mapping = aes(x = X1SYMSPANpartialscore, y = X3SYMSPANpartialscore, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "EF Factor Score T1", y = "EF Factor Score T2") +
  geom_vline(xintercept = c(0.285, 0.428, 0.571, 0.642, 0.786), col = "blue", lty=3) +
  theme_apa()






# *** CST Data not removed ***  -------------------------------------------
df1NR <- df1NR[!(df1NR$Train.Hr<9),] ### this removes 6 individual cases

### Baseline comparisons
### Individual Working Memory measures
t.test(X1OSPANpartialscore ~ Group, data=df1NR)# * significant difference
describeBy(df1NR$X1OSPANpartialscore, df1NR$Group)
mes(0.52, 0.64, 0.21, 0.23, 50, 55) ### Cohen's d 

t.test(X1RSPANpartialscore ~ Group, data=df1NR)# * significant difference
describeBy(df1NR$X1RSPANpartialscore, df1NR$Group)
mes(0.55, 0.65, 0.24, 0.19, 50, 54) ### Cohen's d 

t.test(X1SYMSPANpartialscore ~ Group, data=df1NR)

# 41. Near-Transfer (CSTs) - plots -------------------------------------------

### Line graphs
### Operation Span
x1<-describeBy(df1NR$X1OSPANpartialscore, df1NR$Group)
x2<-describeBy(df1NR$X2OSPNpartialscore, df1NR$Group)
x3<-describeBy(df1NR$X3OSPNpartialscore, df1NR$Group)
xWM <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "Operation Span", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### WM line plot
ggplot(xWM, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Operation Span", limits = c(0.4, 0.9), breaks=seq(0.4,0.9,0.1)) +
  scale_x_continuous(breaks=seq(1,3,1)) 


### Reading Span
x1<-describeBy(df1NR$X1RSPANpartialscore, df1NR$Group)
x2<-describeBy(df1NR$X2RSPANpartialscore, df1NR$Group)
x3<-describeBy(df1NR$X3RSPANpartialscore, df1NR$Group)
xWM <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "Reading Span", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### WM line plot
ggplot(xWM, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Reading Span", limits = c(0.4, 0.9), breaks=seq(0.4,0.9,0.1)) +
  scale_x_continuous(breaks=seq(1,3,1)) 



### Symmetry Span
x1<-describeBy(df1NR$X1SYMSPANpartialscore, df1NR$Group)
x2<-describeBy(df1NR$X2SYMSPANpartialscore, df1NR$Group)
x3<-describeBy(df1NR$X3SYMSPANpartialscore, df1NR$Group)
xWM <- data.frame("Group" = c("Control","Experimental"), "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "Reading Span", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
### WM line plot
ggplot(xWM, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Symmetry Span", limits = c(0.4, 0.9), breaks=seq(0.4,0.9,0.1)) +
  scale_x_continuous(breaks=seq(1,3,1)) 






# 42. Near-Transfer (CSTs) - Operation Span -------------------------------------------


### recap on baseline differences:
### Individual Working Memory measures
t.test(X1OSPANpartialscore ~ Group, data=df1NR)# * significant difference
describeBy(df1NR$X1OSPANpartialscore, df1NR$Group)
mes(0.52, 0.64, 0.21, 0.23, 50, 55) ### Cohen's d = -0.54 [ -0.94, -0.15]
t.test(X1RSPANpartialscore ~ Group, data=df1NR)# * significant difference
describeBy(df1NR$X1RSPANpartialscore, df1NR$Group)
mes(0.55, 0.65, 0.24, 0.19, 50, 54) ### Cohen's d  = -0.46 [ -0.86, -0.07]
t.test(X1SYMSPANpartialscore ~ Group, data=df1NR)


                                          ### ANCOVAs ###


                                        ### OPERATION SPAN ###

### Time 2
### Time 2 vs Time 1
###
### Operation Span T2 vs T1
model1 <- lm(X2OSPNpartialscore ~ X1OSPANpartialscore + Group, data = df1NR)
model2 <- lm(X2OSPNpartialscore ~ X1OSPANpartialscore * Group, data = df1NR)
simpleModel <- lm(X2OSPNpartialscore ~ Group, data = df1NR)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1NR$X2OSPNpartialscore ~ df1NR$Group, center = mean) ## *** significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X2OSPNpartialscore ~ X1OSPANpartialscore + Group, data = df1NR), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X2OSPNpartialscore ~ X1OSPANpartialscore * Group, data = df1NR), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1NR$X2OSPNpartialscore, df1NR$Group)


### Robust
### remember that the p-value does not take into account multiple testing
### so need to multiply each value by 5 (number of tests taken)
ancova(df1NR$X2OSPNpartialscore ~ df1NR$X1OSPANpartialscore + df1NR$Group) ### no significant differences
p <- c(.0316, .0398, .0261, .0401, .0480)
p.adjust(p, method = "holm")
ggplot(data = df1NR, mapping = aes(x = X1OSPANpartialscore, y = X2OSPNpartialscore, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Operation Span Score T1", y = "Operation Span Score T2") +
  geom_vline(xintercept = c(0.24, 0.44, 0.64, 0.76, 0.88), col = "blue", lty=3) +
  theme_apa()


### Time 3
### Time 3 vs Time 1
###
### Operation Span T3 vs T1
### This shows a difference between groups for scores on T3 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(X3OSPNpartialscore ~ X1OSPANpartialscore + Group, data = df1NR)
model2 <- lm(X3OSPNpartialscore ~ X1OSPANpartialscore * Group, data = df1NR)
simpleModel <- lm(X3OSPNpartialscore ~ Group, data = df1NR)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1NR$X3OSPNpartialscore ~ df1NR$Group, center = mean) ## non significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X3OSPNpartialscore ~ X1OSPANpartialscore + Group, data = df1NR), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X3OSPNpartialscore ~ X1OSPANpartialscore * Group, data = df1NR), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1NR$X3OSPNpartialscore, df1NR$Group)




# 43. Near-Transfer (CSTs) - Reading Span -------------------------------------------

### READING SPAN ###

### Time 2
### Time 2 vs Time 1
###
### Operation Span T2 vs T1
### This shows a difference between groups for scores on T2 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(X2RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1NR)
model2 <- lm(X2RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1NR)
simpleModel <- lm(X2RSPANpartialscore ~ Group, data = df1NR)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1NR$X2RSPANpartialscore ~ df1NR$Group, center = mean) ## non significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X2RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1NR), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X2RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1NR), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1NR$X2RSPANpartialscore, df1NR$Group)

### Calculate the difference and effect size for significant effect of Group
model1 <- lm(X2RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1NR)
par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X2RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1NR), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
### Trying to easily calc sample size...
df1copy <- df1NR[!(df1NR$X1RSPANpartialscore=="" | df1NR$X2RSPANpartialscore==""),]
count(df1copy, df1copy$Group == "Exp.") ### now have exp n = 40 and control n = 40
n <- c(54,49) ### sample size
adjustedMeans$se*sqrt(n) ### standard deviation
mes(0.674, 0.743, 0.161, 0.146, 54, 49) ### Cohen's d
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1NR$X2RSPANpartialscore, df1NR$Group)







### Time 3
### Time 3 vs Time 1
###
### Operation Span T3 vs T1
### This shows a difference between groups for scores on T3 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(X3RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1NR)
model2 <- lm(X3RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1NR)
simpleModel <- lm(X3RSPANpartialscore ~ Group, data = df1NR)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1NR$X3RSPANpartialscore ~ df1NR$Group, center = mean) ## non- significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X3RSPANpartialscore ~ X1RSPANpartialscore + Group, data = df1NR), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X3RSPANpartialscore ~ X1RSPANpartialscore * Group, data = df1NR), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1NR$X3RSPANpartialscore, df1NR$Group)







# 44. Near-Transfer (CSTs) - Symmetry Span -------------------------------------------

### SYMMETRY SPAN ###

### Time 2
### Time 2 vs Time 1
###
### Operation Span T2 vs T1
model1 <- lm(X2SYMSPANpartialscore ~ X1SYMSPANpartialscore + Group, data = df1NR)
model2 <- lm(X2SYMSPANpartialscore ~ X1SYMSPANpartialscore * Group, data = df1NR)
simpleModel <- lm(X2SYMSPANpartialscore ~ Group, data = df1NR)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1NR$X2SYMSPANpartialscore ~ df1NR$Group, center = mean) ## non significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X2SYMSPANpartialscore ~ X1SYMSPANpartialscore + Group, data = df1NR), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

### Calculate the difference and effect size for significant effect of Group
model1 <- lm(X2SYMSPANpartialscore ~ X1SYMSPANpartialscore + Group, data = df1NR)
par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X2SYMSPANpartialscore ~ X1SYMSPANpartialscore * Group, data = df1NR), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
### Trying to easily calc sample size...
df1copy <- df1NR[!(df1NR$X1SYMSPANpartialscore=="" | df1NR$X2SYMSPANpartialscore==""),]
count(df1copy, df1copy$Group == "Exp.") ### now have exp n = 40 and control n = 40
n <- c(55,50) ### sample size
adjustedMeans$se*sqrt(n) ### standard deviation
mes(0.583, 0.667, 0.196, 0.178, 55, 50) ### Cohen's d
confint(glht(model1, linfct = mcp(Group = "Tukey")))



### Time 3
### Time 3 vs Time 1
###
### Operation Span T3 vs T1
### This shows a difference between groups for scores on T3 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(X3SYMSPANpartialscore ~ X1SYMSPANpartialscore + Group, data = df1NR)
model2 <- lm(X3SYMSPANpartialscore ~ X1SYMSPANpartialscore * Group, data = df1NR)
simpleModel <- lm(X3SYMSPANpartialscore ~ Group, data = df1NR)
summ(model1)
summary(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ###  no problem here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(df1NR$X3SYMSPANpartialscore ~ df1NR$Group, center = mean) ## no problem

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(X3SYMSPANpartialscore ~ X1SYMSPANpartialscore + Group, data = df1NR), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(X3SYMSPANpartialscore ~ X1SYMSPANpartialscore * Group, data = df1NR), type = "III") ## interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(df1NR$X3SYMSPANpartialscore, df1NR$Group)


### Robust
### remember that the p-value does not take into account multiple testing
### so need to multiply each value by 5 (number of tests taken)
ancova(df1NR$X3SYMSPANpartialscore ~ df1NR$X1SYMSPANpartialscore + df1NR$Group) ### no significant differences
p <- c(.3512, .8597, .6071, .2173, .1589)
p.adjust(p, method = "holm")
ggplot(data = df1NR, mapping = aes(x = X1SYMSPANpartialscore, y = X3SYMSPANpartialscore, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Symmetry Span Score T1", y = "Symmetry Span Score T3") +
  geom_vline(xintercept = c(0.214, 0.357, 0.571, 0.642, 0.786), col = "blue", lty=3) +
  theme_apa()



#...----------------------------------------------
#...----------------------------------------------

#  *** ANCOVA: Far-Transfer ***-----------------------------------------
# 45. Far-Transfer ANCOVA - Parametric & Robust -------------------------------------------

### PAT Maths T2 vs T1
### Not Significant
model1 <- lm(PATmScale_2 ~ PATmScale_1 + Group, data = predicted)
model2 <- lm(PATmScale_2 ~ PATmScale_1 * Group, data = predicted)
simpleModel <- lm(PATmScale_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$PATmScale_2 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(PATmScale_2 ~ PATmScale_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
Anova(aov(PATmScale_2 ~ PATmScale_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$PATmScale_2, predicted$Group)

### Robust
ancova(predicted$PATmScale_2 ~ predicted$PATmScale_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = PATmScale_1, y = PATmScale_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Maths T1", y = "Maths T2") +
  geom_vline(xintercept = c(118.8, 127.8, 132.4, 140, 150.6), col = "orange", lty=3) +
  theme_apa()


### PAT Maths T3 vs T1
### Not Significant
model1 <- lm(PATmScale_3 ~ PATmScale_1 + Group, data = predicted)
model2 <- lm(PATmScale_3 ~ PATmScale_1 * Group, data = predicted)
simpleModel <- lm(PATmScale_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$PATmScale_3 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(PATmScale_3 ~ PATmScale_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(PATmScale_3 ~ PATmScale_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$PATmScale_2, predicted$Group)

### Robust
ancova(predicted$PATmScale_3 ~ predicted$PATmScale_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = PATmScale_1, y = PATmScale_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Maths T1", y = "Maths T3") +
  geom_vline(xintercept = c(118.8, 127.8, 132.4, 140, 150.6), col = "orange", lty=3) +
  theme_apa()



### PAT Reading T2 vs T1
### Not Significant
model1 <- lm(PATrScale_2 ~ PATrScale_1 + Group, data = predicted)
model2 <- lm(PATrScale_2 ~ PATrScale_1 * Group, data = predicted)
simpleModel <- lm(PATrScale_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$PATrScale_2 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(PATrScale_2 ~ PATrScale_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(PATrScale_2 ~ PATrScale_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$PATrScale_2, predicted$Group)

### Robust
ancova(predicted$PATrScale_2 ~ predicted$PATrScale_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = PATrScale_1, y = PATrScale_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Reading T1", y = "Reading T2") +
  geom_vline(xintercept = c(113, 127.3, 132.1, 140.3, 151.2), col = "orange", lty=3) +
  theme_apa()


### PAT Reading T3 vs T1
### Not Significant
model1 <- lm(PATrScale_3 ~ PATrScale_1 + Group, data = predicted)
model2 <- lm(PATrScale_3 ~ PATrScale_1 * Group, data = predicted)
simpleModel <- lm(PATrScale_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$PATrScale_3 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(PATrScale_3 ~ PATrScale_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$PATrScale_3 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(PATrScale_3 ~ PATrScale_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$PATrScale_3, predicted$Group)

### Robust
ancova(predicted$PATrScale_3 ~ predicted$PATrScale_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = PATrScale_1, y = PATrScale_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Reading T1", y = "Reading T3") +
  geom_vline(xintercept = c(113, 127.3, 132.1, 140.3, 151.2), col = "orange", lty=3) +
  theme_apa()



### NAI T2 vs T1
### Not Significant
model1 <- lm(NAI_2 ~ NAI_1 + Group, data = predicted)
model2 <- lm(NAI_2 ~ NAI_1 * Group, data = predicted)
simpleModel <- lm(NAI_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### *** There is an interaction
model1 <- lmres(NAI_2 ~ NAI_1 * Group, centered = c("NAI_2", "NAI_1"), data = predicted)
summary.lmres(model1)
model1Slope <- simpleSlope(model1, pred = "NAI_1", mod1 = "Group1")
summary.simpleSlope(model1Slope)
PlotSlope(model1Slope)
### Robust
ancova(predicted$NAI_2 ~ predicted$NAI_1 + predicted$Group) ### no significant differences
### PLOT IT...
ggplot(data = predicted, mapping = aes(x = NAI_1, y = NAI_2, color = factor(Group, level = level_order))) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  scale_x_continuous(name = "gF T1", limits = c(80,150), breaks=seq(80,150,10)) +
  scale_y_continuous(name = "gF T2", limits = c(80,160), breaks=seq(80,160,10)) +
  scale_color_discrete(name = "Group:", labels = c("Control","Experimental")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10), legend.title = element_text(size = 10), axis.title = element_text(size=10), axis.text = element_text(size=10)) 
  #geom_vline(xintercept = c(94, 104, 112, 122, 130), col = "black", lty=3) +
  #theme_apa()

 
  
### Need long form data (dfLong)

### As per Field, Andy. Discovering Statistics Using R (p. 876 + )
### intercept only
model1 <- gls(NAI_2 ~ 1, data = predicted, method = "ML", na.action = na.exclude)
summary(model1)
### random intercept only
model2 <- lme(NAI_2 ~ 1, data = predicted, random = ~1|Group, method = "ML", na.action = na.exclude, control = list(maxIter = 100000, opt="nlminb"))
summary(model2)
anova(model1,model2)
### allow intercepts to vary by intervention
model3 <- lme(NAI_2 ~ Group, data = predicted, random = ~1|Group, method = "ML", na.action = na.exclude, control = list(maxIter = 100000, opt="nlminb"))
anova(model1, model2, model3)
### add in random slopes
model4 <- lme(NAI ~ Intervention + time, data = predicted, random = ~time|Group, method = "ML", na.action = na.exclude, control = list(maxIter = 100000, opt="nlminb"))
### model the covariance structure
model5 <- lme(NAI ~ Intervention + time, data = predicted, random = ~time|Group, correlation = corAR1(0, form = ~time|Group), method = "ML", na.action = na.exclude, control = list(maxIter = 100000, opt="nlminb"))

anova(model1, model2, model3)




### NAI T3 vs T1
### Not Significant
model1 <- lm(NAI_3 ~ NAI_1 + Group, data = predicted)
model2 <- lm(NAI_3 ~ NAI_1 * Group, data = predicted)
simpleModel <- lm(NAI_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### *no problem here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$NAI_3 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(NAI_3 ~ NAI_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
Anova(aov(NAI_3 ~ NAI_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$NAI_3, predicted$Group)

### Robust
ancova(predicted$NAI_3 ~ predicted$NAI_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = NAI_1, y = NAI_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "gF T1", y = "gF T3") +
  geom_vline(xintercept = c(94, 104, 112, 122, 130), col = "orange", lty=3) +
  theme_apa()




# 46. Far-Transfer ANCOVA: Behavioural Reports  -------------------------------------------

### (a) Parents  -------------------------------------------
### Parent: Inhibition T2 vs T1
### Not Significant
model1 <- lm(CEFI.P.AT.SS_2 ~ CEFI.P.AT.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.P.AT.SS_2 ~ CEFI.P.AT.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.P.AT.SS_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.P.AT.SS_2 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.P.AT.SS_2 ~ CEFI.P.AT.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.P.AT.SS_2 ~ CEFI.P.AT.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
Anova(aov(CEFI.P.AT.SS_2 ~ CEFI.P.AT.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.P.AT.SS_2, predicted$Group)

### Robust
ancova(predicted$CEFI.P.AT.SS_2 ~ predicted$CEFI.P.AT.SS_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = CEFI.P.AT.SS_1, y = CEFI.P.AT.SS_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Parent: Inhibition T1", y = "Parent: Inhibition T2") +
  geom_vline(xintercept = c(78, 90, 99, 105, 120), col = "orange", lty=3) +
  theme_apa()


### Parent: Inhibition T3 vs T1
### Not Significant
model1 <- lm(CEFI.P.AT.SS_3 ~ CEFI.P.AT.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.P.AT.SS_3 ~ CEFI.P.AT.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.P.AT.SS_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.P.AT.SS_3 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.P.AT.SS_3 ~ CEFI.P.AT.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.P.AT.SS_3 ~ CEFI.P.AT.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.P.AT.SS_3 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.P.AT.SS_3 ~ CEFI.P.AT.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.P.AT.SS_3, predicted$Group)

### Robust
ancova(predicted$CEFI.P.AT.SS_3 ~ predicted$CEFI.P.AT.SS_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = CEFI.P.AT.SS_1, y = CEFI.P.AT.SS_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Parent: Inhibition T1", y = "Parent: Inhibition T3") +
  geom_vline(xintercept = c(78, 90, 99, 105, 120), col = "orange", lty=3) +
  theme_apa()



### Parent: Shifting T2 vs T1
### Not Significant
model1 <- lm(CEFI.P.FX.SS_2 ~ CEFI.P.FX.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.P.FX.SS_2 ~ CEFI.P.FX.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.P.FX.SS_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.P.FX.SS_2 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.P.FX.SS_2 ~ CEFI.P.FX.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.P.FX.SS_2 ~ CEFI.P.FX.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.P.FX.SS_2 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.P.FX.SS_2 ~ CEFI.P.FX.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.P.FX.SS_2, predicted$Group)

### Robust
ancova(predicted$CEFI.P.FX.SS_2 ~ predicted$CEFI.P.FX.SS_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = CEFI.P.FX.SS_1, y = CEFI.P.FX.SS_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Parent: Shifting T1", y = "Parent: Shifting T2") +
  geom_vline(xintercept = c(80, 92, 95, 100, 106), col = "orange", lty=3) +
  theme_apa()


### Parent: Shifting T3 vs T1
### Not Significant
model1 <- lm(CEFI.P.FX.SS_3 ~ CEFI.P.FX.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.P.FX.SS_3 ~ CEFI.P.FX.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.P.FX.SS_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.P.FX.SS_3 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.P.FX.SS_3 ~ CEFI.P.FX.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.P.FX.SS_3 ~ CEFI.P.FX.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.P.FX.SS_3 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.P.FX.SS_3 ~ CEFI.P.FX.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.P.FX.SS_3, predicted$Group)

### Robust
ancova(predicted$CEFI.P.FX.SS_3 ~ predicted$CEFI.P.FX.SS_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = CEFI.P.FX.SS_1, y = CEFI.P.FX.SS_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Parent: Shifting T1", y = "Parent: Shifting T3") +
  geom_vline(xintercept = c(86, 92, 95, 100, 106), col = "orange", lty=3) +
  theme_apa()


### Parent: WM T2 vs T1
### Not Significant
model1 <- lm(CEFI.P.WM.SS_2 ~ CEFI.P.WM.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.P.WM.SS_2 ~ CEFI.P.WM.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.P.WM.SS_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.P.WM.SS_2 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.P.WM.SS_2 ~ CEFI.P.WM.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.P.WM.SS_2 ~ CEFI.P.WM.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.P.WM.SS_2 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.P.WM.SS_2 ~ CEFI.P.WM.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.P.WM.SS_2, predicted$Group)

### Robust
ancova(predicted$CEFI.P.WM.SS_2 ~ predicted$CEFI.P.WM.SS_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = CEFI.P.WM.SS_1, y = CEFI.P.WM.SS_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Parent: WM T1", y = "Parent: WM T2") +
  geom_vline(xintercept = c(79, 90, 95, 106, 110), col = "orange", lty=3) +
  theme_apa()


### Parent: WM T3 vs T1
### Not Significant
model1 <- lm(CEFI.P.WM.SS_3 ~ CEFI.P.WM.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.P.WM.SS_3 ~ CEFI.P.WM.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.P.WM.SS_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.P.WM.SS_3 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.P.WM.SS_3 ~ CEFI.P.WM.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.P.WM.SS_3 ~ CEFI.P.WM.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.P.WM.SS_3 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.P.WM.SS_3 ~ CEFI.P.WM.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.P.WM.SS_3, predicted$Group)

### Robust
ancova(predicted$CEFI.P.WM.SS_3 ~ predicted$CEFI.P.WM.SS_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = CEFI.P.WM.SS_1, y = CEFI.P.WM.SS_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Parent: WM T1", y = "Parent: WM T3") +
  geom_vline(xintercept = c(82, 90, 94, 97, 102), col = "orange", lty=3) +
  theme_apa()





### (b) Teacher  -------------------------------------------
### Behaviour Reports - Teacher
### Teacher: Inhibition T2 vs T1
### Not Significant
model1 <- lm(CEFI.T.AT.SS_2 ~ CEFI.T.AT.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.T.AT.SS_2 ~ CEFI.T.AT.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.T.AT.SS_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.T.AT.SS_2 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.T.AT.SS_2 ~ CEFI.T.AT.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.T.AT.SS_2 ~ CEFI.T.AT.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.T.AT.SS_2 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.T.AT.SS_2 ~ CEFI.T.AT.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.T.AT.SS_2, predicted$Group)

### Robust
ancova(predicted$CEFI.T.AT.SS_2 ~ predicted$CEFI.T.AT.SS_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = CEFI.T.AT.SS_1, y = CEFI.T.AT.SS_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Teacher: Inhibition T1", y = "Teacher: Inhibition T2") +
  geom_vline(xintercept = c(86, 94, 98, 101, 110), col = "orange", lty=3) +
  theme_apa()


### Teacher: Inhibition T3 vs T1
### Not Significant
model1 <- lm(CEFI.T.AT.SS_3 ~ CEFI.T.AT.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.T.AT.SS_3 ~ CEFI.T.AT.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.T.AT.SS_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.T.AT.SS_3 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.T.AT.SS_3 ~ CEFI.T.AT.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.T.AT.SS_3 ~ CEFI.T.AT.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.T.AT.SS_3 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.T.AT.SS_3 ~ CEFI.T.AT.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.T.AT.SS_3, predicted$Group)

### Robust
ancova(predicted$CEFI.T.AT.SS_3 ~ predicted$CEFI.T.AT.SS_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = CEFI.T.AT.SS_1, y = CEFI.T.AT.SS_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Teacher: Inhibition T1", y = "Teacher: Inhibition T3") +
  geom_vline(xintercept = c(86, 94, 99, 102, 112), col = "orange", lty=3) +
  theme_apa()



### Teacher: Shifting T2 vs T1
### Not Significant
model1 <- lm(CEFI.T.FX.SS_2 ~ CEFI.T.FX.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.T.FX.SS_2 ~ CEFI.T.FX.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.T.FX.SS_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.T.FX.SS_2 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.T.FX.SS_2 ~ CEFI.T.FX.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.T.FX.SS_2 ~ CEFI.T.FX.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.T.FX.SS_2 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.T.FX.SS_2 ~ CEFI.T.FX.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.T.FX.SS_2, predicted$Group)

### Robust
ancova(predicted$CEFI.T.FX.SS_2 ~ predicted$CEFI.T.FX.SS_1 + predicted$Group) ### *** significant differences
p <- c(.1128, .0363, .0224, .0080, .0100)
p.adjust(p, method = "holm")
ggplot(data = predicted, mapping = aes(x = CEFI.T.FX.SS_1, y = CEFI.T.FX.SS_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Teacher: Shifting T1", y = "Teacher: Shifting T2") +
  geom_vline(xintercept = c(113, 115), col = "black", lty=3) +
  theme_apa()


### Teacher: Shifting T3 vs T1
### Not Significant
model1 <- lm(CEFI.T.FX.SS_3 ~ CEFI.T.FX.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.T.FX.SS_3 ~ CEFI.T.FX.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.T.FX.SS_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.T.FX.SS_3 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.T.FX.SS_3 ~ CEFI.T.FX.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.T.FX.SS_3 ~ CEFI.T.FX.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.T.FX.SS_3 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.T.FX.SS_3 ~ CEFI.T.FX.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.T.FX.SS_3, predicted$Group)

### Robust
ancova(predicted$CEFI.T.FX.SS_3 ~ predicted$CEFI.T.FX.SS_1 + predicted$Group) ### *** significant differences
p <- c(.1390, .1452, .0067, .0118, .0072)
p.adjust(p, method = "holm")
ggplot(data = predicted, mapping = aes(x = CEFI.T.FX.SS_1, y = CEFI.T.FX.SS_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Teacher: Shifting T1", y = "Teacher: Shifting T3") +
  geom_vline(xintercept = c(111, 113, 115), col = "black", lty=3) +
  theme_apa()



### Teacher: WM T2 vs T1
### Not Significant
model1 <- lm(CEFI.T.WM.SS_2 ~ CEFI.T.WM.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.T.WM.SS_2 ~ CEFI.T.WM.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.T.WM.SS_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.T.WM.SS_2 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.T.WM.SS_2 ~ CEFI.T.WM.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.T.WM.SS_2 ~ CEFI.T.WM.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.T.WM.SS_2 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.T.WM.SS_2 ~ CEFI.T.WM.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.T.WM.SS_2, predicted$Group)

### Robust
ancova(predicted$CEFI.T.WM.SS_2 ~ predicted$CEFI.T.WM.SS_1 + predicted$Group) ### *** significant differences
p <- c(.0235, .0146, .0011, .0054, .0199)
p.adjust(p, method = "holm")
ggplot(data = predicted, mapping = aes(x = CEFI.T.WM.SS_1, y = CEFI.T.WM.SS_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Teacher: WM T1", y = "Teacher: WM T2") +
  geom_vline(xintercept = c(98, 103, 105, 106, 109), col = "black", lty=3) +
  theme_apa()


### Teacher: WM T3 vs T1
### Not Significant
model1 <- lm(CEFI.T.WM.SS_3 ~ CEFI.T.WM.SS_1 + Group, data = predicted)
model2 <- lm(CEFI.T.WM.SS_3 ~ CEFI.T.WM.SS_1 * Group, data = predicted)
simpleModel <- lm(CEFI.T.WM.SS_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$CEFI.T.WM.SS_3 ~ predicted$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(CEFI.T.WM.SS_3 ~ CEFI.T.WM.SS_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(CEFI.T.WM.SS_3 ~ CEFI.T.WM.SS_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$CEFI.T.WM.SS_3 ~ predicted$Group, center = mean) ## Not Significant
Anova(aov(CEFI.T.WM.SS_3 ~ CEFI.T.WM.SS_1 * Group, data = predicted), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$CEFI.T.WM.SS_3, predicted$Group)

### Robust
ancova(predicted$CEFI.T.WM.SS_3 ~ predicted$CEFI.T.WM.SS_1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = CEFI.T.WM.SS_1, y = CEFI.T.WM.SS_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "Teacher: WM T1", y = "Teacher: WM T3") +
  geom_vline(xintercept = c(98, 103, 105, 106, 109), col = "orange", lty=3) +
  theme_apa()








# 47. Far-Transfer (parametric) ANCOVAs (outliers removed) -------------------------------------------
par(mfrow=c(2,2))

## ANCOVA
### PATm T2 vs T1
model1 <- lm(PATmScale_2 ~ PATmScale_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$PATmScale_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(PATmScale_2 ~ PATmScale_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$PATmScale_2, predictedOR$Group)



### PATm T3 vs T1
model1 <- lm(PATmScale_3 ~ PATmScale_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$PATmScale_3 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(PATmScale_3 ~ PATmScale_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$PATmScale_3, predictedOR$Group)


### PATr T2 vs T1
model1 <- lm(PATrScale_2 ~ PATrScale_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$PATrScale_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(PATrScale_2 ~ PATrScale_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$PATrScale_2, predictedOR$Group)


### PATr T3 vs T1
model1 <- lm(PATrScale_3 ~ PATrScale_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$PATrScale_3 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(PATrScale_3 ~ PATrScale_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$PATrScale_3, predictedOR$Group)


### NAI T2 vs T1
model1 <- lm(NAI_2 ~ NAI_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$PATmScale_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(NAI_2 ~ NAI_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$NAI_2, predictedOR$Group)
### look at the interaction
ggplot(data = predictedOR, mapping = aes(x = NAI_1, y = NAI_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  labs (x = "NAI T1", y = "NAI T2") +
  theme_apa()
myModel <- lm(NAI_2 ~ NAI_1 * Int, data = predictedOR)
summ(myModel)
sim_slopes(myModel, pred = Int, modx = NAI_1, johnson_neyman = TRUE, jnplot = TRUE)
sim_slopes(myModel, pred = Int, modx = NAI_1)
interact_plot(myModel, pred = Int, modx = NAI_1, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
slopemodel <- sim_slopes(myModel, pred = Int, modx = NAI_1)
probe_interaction(myModel, pred = Int, modx = NAI_1)
interact_plot(myModel, pred = Int, modx = NAI_1, x.label = "Group", y.label = "Posttest Score", robust = TRUE) + theme_apa()


### NAI T3 vs T1
model1 <- lm(NAI_3 ~ NAI_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$NAI_3 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(NAI_3 ~ NAI_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$NAI_3, predictedOR$Group)



### BEHAVIOURAL REPORTS
### PARENT
### CEFIpAT T2 vs T1
model1 <- lm(CEFI.P.AT.SS_2 ~ CEFI.P.AT.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.P.AT.SS_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.P.AT.SS_2 ~ CEFI.P.AT.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.P.AT.SS_2, predictedOR$Group)


### CEFIpAT T3 vs T1
model1 <- lm(CEFI.P.AT.SS_3 ~ CEFI.P.AT.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.P.AT.SS_3 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.P.AT.SS_3 ~ CEFI.P.AT.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.P.AT.SS_3, predictedOR$Group)


### CEFIpFX T2 vs T1
model1 <- lm(CEFI.P.FX.SS_2 ~ CEFI.P.FX.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.P.FX.SS_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.P.FX.SS_2 ~ CEFI.P.FX.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.P.FX.SS_2, predictedOR$Group)


### CEFIpFX T3 vs T1
model1 <- lm(CEFI.P.FX.SS_3 ~ CEFI.P.FX.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.P.FX.SS_3 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.P.FX.SS_3 ~ CEFI.P.FX.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.P.FX.SS_3, predictedOR$Group)


### CEFIpWM T2 vs T1
model1 <- lm(CEFI.P.WM.SS_2 ~ CEFI.P.WM.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.P.WM.SS_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.P.WM.SS_2 ~ CEFI.P.WM.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.P.WM.SS_2, predictedOR$Group)


### CEFIpWM T3 vs T1
model1 <- lm(CEFI.P.WM.SS_3 ~ CEFI.P.WM.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.P.WM.SS_3 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.P.WM.SS_3 ~ CEFI.P.WM.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.P.WM.SS_3, predictedOR$Group)


### BEHAVIOURAL REPORTS
### TEACHER
### CEFItAT T2 vs T1
model1 <- lm(CEFI.T.AT.SS_2 ~ CEFI.T.AT.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.T.AT.SS_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.T.AT.SS_2 ~ CEFI.T.AT.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.T.AT.SS_2, predictedOR$Group)


### CEFItAT T3 vs T1
model1 <- lm(CEFI.T.AT.SS_3 ~ CEFI.T.AT.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.T.AT.SS_3 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.T.AT.SS_3 ~ CEFI.T.AT.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.T.AT.SS_3, predictedOR$Group)


### CEFItFX T2 vs T1
model1 <- lm(CEFI.T.FX.SS_2 ~ CEFI.T.WM.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.T.FX.SS_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.T.FX.SS_2 ~ CEFI.T.WM.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.T.FX.SS_2, predictedOR$Group)


### CEFItFX T3 vs T1
model1 <- lm(CEFI.T.FX.SS_3 ~ CEFI.T.WM.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.T.FX.SS_3 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.T.FX.SS_3 ~ CEFI.T.WM.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.T.FX.SS_3, predictedOR$Group)


### CEFItWM T2 vs T1
model1 <- lm(CEFI.T.WM.SS_2 ~ CEFI.T.WM.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.T.WM.SS_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.T.WM.SS_2 ~ CEFI.T.WM.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.T.WM.SS_2, predictedOR$Group)


### CEFItWM T3 vs T1
model1 <- lm(CEFI.T.WM.SS_3 ~ CEFI.T.WM.SS_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$CEFI.T.WM.SS_3 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(CEFI.T.WM.SS_3 ~ CEFI.T.WM.SS_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$CEFI.T.WM.SS_3, predictedOR$Group)





#...----------------------------------------------
#...----------------------------------------------

#  *** IEM ***-----------------------------------------

# 48. IEM Tables ----------------------------------------------
### Create a data summary table first
### Now for table creation: One big table ...
table <- describeBy(df1Final[,c(61,62,63,65,66,67)], df1Final$Group, mat = TRUE)
### round the mean & SD data to two decimal places
table <- round(table[,4:15], 2)
### make a cell with row names so that nothing gets misplaced
setDT(table, keep.rownames = TRUE) []
### Let's get rid of unnecessary columns
table <- table[, -c(5:7,10)]
### remove every second row into a different data.table
table.c8 <- table[seq(1, nrow(table), 2), ]
table.tc <- table[seq(2, nrow(table), 2), ]
### remove numbers from end of variable names
table.c8$rn <- substr(as.character(table.c8$rn), start= 1, stop= nchar(as.character(table.c8$rn) )-1 )
table.tc$rn <- substr(as.character(table.tc$rn), start= 1, stop= nchar(as.character(table.tc$rn) )-1 )
### put data back together again
table.tc$expN <- table.c8$n
table.tc$expMean <- table.c8$mean
table.tc$expSD <- table.c8$sd
table.tc$expMin <-table.c8$min
table.tc$expMax <- table.c8$max
table.tc$expSkew <- table.c8$skew
table.tc$expKurtosis <- table.c8$kurtosis
table.tc$expSE <- table.c8$se
table <- table.tc
rm("table.c8","table.tc")


### okay, let's remove the variables that are not required
table <- as.data.frame(table)
### add the correct names for variables
row.names(table) <- c("IEM Maths T1",
                      "IEM English T1",
                      "IEM Concentration T1",
                      "IEM Maths T2",
                      "IEM English T2",
                      "IEM Concentration T2")
table$rn <- NULL
table$rn <- NULL
table$rn <- NULL
write.csv(table, "Table IEM.csv")
rm("table")



# 49.  Differences in IEM at T1 & T2----------------------------------------
### Time 1
### IEM: Maths
t.test(IEMmaths_1 ~ Group, data=predicted)
describeBy(predicted$IEMmaths_1, predicted$Group)
mes(3.22, 2.55, 1.75, 2.02, 50, 55) ### Cohen's d 

### IEM: English
t.test(IEMenglish_1 ~ Group, data=predicted) # *** sig
describeBy(predicted$IEMenglish_1, predicted$Group)
mes(2.22, 3.98, 1.87, 1.3, 50, 55) ### Cohen's d 

### IEM: Concentration
t.test(IEMconc._1 ~ Group, data=predicted)
describeBy(predicted$IEMconc._1, predicted$Group)
mes(4.02, 3.95, 1.83, 1.69, 50, 55) ### Cohen's d 


### Time 2
### IEM: Maths
model1 <- lm(IEMmaths_2 ~ Group, data=predicted) # No difference
summ(model1)
leveneTest(predicted$IEMmaths_2, predicted$Group)
for(i in 1:4)plot(model1, which=i)
t.test(IEMmaths_2 ~ Group, data=predicted)

### IEM: English
model1 <- lm(IEMenglish_2 ~ Group, data=predicted) # Significant difference
summ(model1)
leveneTest(predicted$IEMenglish_2, predicted$Group) # unequal variances
for(i in 1:4)plot(model1, which=i)
t.test(IEMenglish_2 ~ Group, data=predicted) # *** sig

### IEM: Concentration
model1 <- lm(IEMconc._2 ~ Group, data=predicted) # No difference
summ(model1)
leveneTest(predicted$IEMconc._2, predicted$Group)
for(i in 1:4)plot(model1, which=i)
t.test(IEMconc._2 ~ Group, data=predicted)


LongIEM <- reshape(predicted, varying=c(68:70,72:74), direction="long", idvar="ID", sep="_")

### Plot it?
ggplot(LongIEM, aes(Group, IEMmaths, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2"), labels = c("Time 1","Time 2")) +
  scale_y_continuous(name = "IEM: Maths") + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))

ggplot(LongIEM, aes(Group, IEMenglish, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2"), labels = c("Time 1","Time 2")) +
  scale_y_continuous(name = "IEM: English") + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))

ggplot(LongIEM, aes(Group, IEMconc., fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2"), labels = c("Time 1","Time 2")) +
  scale_y_continuous(name = "IEM: Concentration") + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))

ggplot(predicted, aes(IEMTotalScore_1, IEMTotalScore_2, color = factor(Group))) +
  geom_point() +
  geom_line(data=predicted,aes(group=Group))

ggplot(data = predicted, mapping = aes(x = IEMTotalScore_1, y = IEMTotalScore_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "IEM: Total Score T1", y = "IEM: Total Score T2") +
  theme_apa()





# 50.  Changes in IEM - Parametric & Robust----------------------------------------

### ANCOVAs for the IEM measures
### IEM: Maths
### Not Significant
model1 <- lm(IEMmaths_2 ~ IEMmaths_1 + Group, data = predicted)
model2 <- lm(IEMmaths_2 ~ IEMmaths_1 * Group, data = predicted)
simpleModel <- lm(IEMmaths_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### interaction present
### test assumption of normal distribution of residuals

model2 <- aov(IEMmaths_2 ~ IEMmaths_1 * Group, data = predicted)
summary(model2)
adjustedMeans <- effect("IEMmaths_1:Group", model2, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$IEMmaths_2, predicted$Group)

### Robust
ancova(predicted$IEMmaths_2 ~ predicted$IEMmaths_1 + predicted$Group) ### no significant differences

### Look at Interaction for IEM: Maths
myModel <- lm(IEMmaths_2 ~ IEMmaths_1 * Int, data = predicted) # sig
summ(myModel)
sim_slopes(myModel, pred = IEMmaths_1, modx = Int , johnson_neyman = TRUE, jnplot = TRUE)

sim_slopes(myModel, pred = IEMmaths_1, modx = Int)
interact_plot(myModel, pred = wm_1, modx = ITIMc, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
slopemodel <- sim_slopes(myModel, pred = wm_1, modx = ITIMc)
probe_interaction(myModel, pred = wm_1, modx = ITIMc)
interact_plot(myModel, pred = wm_1, modx = ITIMc, mod2 = Int, x.label = "Group", y.label = "Posttest Score") + theme_apa()
### Plot the interaction
ggplot(data = predicted, mapping = aes(x = IEMmaths_1, y = IEMmaths_2, color = factor(Group, level = level_order))) +
  geom_jitter(width = 0.1, height = 0.1) +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  scale_x_continuous(name = "IEM: Maths T1") +
  scale_y_continuous(name = "IEM: Maths T2") +
  scale_color_discrete(name = "Group:", labels = c("Control","Experimental")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10), legend.title = element_text(size = 10), axis.title = element_text(size=10), axis.text = element_text(size=10)) 


### IEM: Concentration
### Not Significant
model1 <- lm(IEMconc._2 ~ IEMconc._1 + Group, data = predicted)
model2 <- lm(IEMconc._2 ~ IEMconc._1 * Group, data = predicted)
simpleModel <- lm(IEMconc._2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$IEMconc._2 ~ predicted$Group, center = mean) ### *** significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(IEMconc._2 ~ IEMconc._1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(IEMconc._2 ~ IEMconc._1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not Significant
leveneTest(predicted$IEMconc._2 ~ predicted$Group, center = mean) ## *** Significant
Anova(aov(IEMconc._2 ~ IEMconc._1 * Group, data = predicted), type = "III") ## No interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$IEMconc._2, predicted$Group)

### Robust
ancova(predicted$IEMconc._2 ~ predicted$IEMconc._1 + predicted$Group) ### no significant differences
ggplot(data = predicted, mapping = aes(x = IEMconc._1, y = IEMconc._2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "IEM: Concentration T1", y = "IEM: Concentration T2") +
  theme_apa()



### IEM: English
### Significant but unequal variances
model1 <- lm(IEMenglish_2 ~ IEMenglish_1 + Group, data = predicted)
model2 <- lm(IEMenglish_2 ~ IEMenglish_1 * Group, data = predicted)
simpleModel <- lm(IEMenglish_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$IEMenglish_2 ~ predicted$Group, center = mean) ### *** significant

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(IEMenglish_2 ~ IEMenglish_1 + Group, data = predicted), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
model1 <- lm(IEMenglish_2 ~ IEMenglish_1 + Group, data = predicted)
for(i in 1:4)plot(model1, which=i)
summ(model1) ## *** Significant
leveneTest(predicted$IEMenglish_2 ~ predicted$Group, center = mean) ## *** Significant
Anova(aov(IEMenglish_2 ~ IEMenglish_1 * Group, data = predicted), type = "III") ## No interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$IEMenglish_1, predicted$Group)
describeBy(predicted$IEMenglish_2, predicted$Group)

ancova(predicted$IEMenglish_2 ~ predicted$IEMenglish_1 + predicted$Group) ### SIG 
p <- c(.0218, .0406, .0096, .0147, .0330)
p.adjust(p, method = "holm") ### but only one p-value is significant
### PLot the rANCOVA
ggplot(data = predicted, mapping = aes(x = IEMenglish_2, y = IEMenglish_1, color = factor(Group, level = level_order))) +
  geom_jitter(width = 0.1, height = 0.1) +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  scale_x_continuous(name = "IEM: English T1") +
  scale_y_continuous(name = "IEM: English T2") +
  scale_color_discrete(name = "Group:", labels = c("Control","Experimental")) +
  geom_vline(xintercept = c(4), col = "black", lty=3) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10), legend.title = element_text(size = 10), axis.title = element_text(size=10), axis.text = element_text(size=10)) 







# 51. Changes in IEM (outliers removed)----------------------------------------------

### IEMmaths T2 vs T1
model1 <- lm(IEMmaths_2 ~ IEMmaths_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$IEMmaths_2 ~ predictedOR$Group, center = mean) ## non-sig
summ(model1)
summary(model1)
summary.lm(model1)
Anova(aov(IEMmaths_2 ~ IEMmaths_1 * Group, data = predictedOR), type = "III") ## no interaction
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predictedOR$IEMmaths_2, predictedOR$Group)


### IEMconc. T2 vs T1
model1 <- lm(IEMconc._2 ~ IEMconc._1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## non-sig
leveneTest(predictedOR$IEMconc._2 ~ predictedOR$Group, center = mean) ## significant: not interpretable
Anova(aov(IEMconc._2 ~ IEMconc._1 * Group, data = predictedOR), type = "III") ## no interaction



### IEMenglish T2 vs T1
model1 <- lm(IEMenglish_2 ~ IEMenglish_1 + Group, data = predictedOR)
for(i in 1:4)plot(model1, which=i)
Anova(model1, type = "III") ## significant difference
leveneTest(predictedOR$IEMenglish_2 ~ predictedOR$Group, center = mean) ## significant: not interpretable
Anova(aov(IEMenglish_2 ~ IEMenglish_1 * Group, data = predictedOR), type = "III") ## no interaction





#...----------------------------------------------
#...----------------------------------------------

#  *** MODERATION (ITIM) ***-----------------------------------------
### t2way does not work unless it has two factors to work with, so need to change ITIM to a factor
### from R companion - can see why there are problems if stick with ITIM without splitting - many values of ITIM have only one group member - therefore cannot do estimations on these values
### let's use a different dataset
predictedM <- diffScores
### dichotomous median split for ITIM
predictedM$ITIMgroups <- as.numeric(dicho(predictedM$ITIM, dich.by = "median"))
describeBy(predictedM$ITIM, predictedM$ITIMgroups)
### make it a factor
predictedM$ITIMgroups <- as.factor(predictedM$ITIMgroups)

### check baseline differences
t.test(ITIM ~ Group, data=predicted) 
describeBy(predicted$ITIM, predicted$Group)
mes(26.18, 26.53, 6.34, 5.25, 50, 55) ### Cohen's d 

## is there a difference in T2 scores dependent upon ITIM group (by median)?
summary(lm(EF2EF1 ~ ITIMgroups, data = predictedM))
summary(lm(EF_2 ~ EF_1 + ITIMgroups, data = predictedM))
summary(lm(WM2WM1 ~ ITIMgroups, data = predictedM))
summary(lm(wm_2 ~ wm_1 + ITIMgroups, data = predictedM))

### is the correlation different for each group?
expITIM <- subset(predictedM, predictedM$Group == "Exp.")
ctrlITIM <- subset(predictedM, predictedM$Group == "Ctrl")


# group correlations
cor.test(expITIM$EF2EF1, expITIM$ITIM, use = "p") ## -0.12
cor.test(expITIM$WM2WM1, expITIM$ITIM, use = "p") ## -0.05
cor.test(ctrlITIM$EF2EF1, ctrlITIM$ITIM, use = "p") ## -0.01
cor.test(ctrlITIM$WM2WM1, ctrlITIM$ITIM, use = "p") ## -0.04 






# 52. Robust Interactions ----------------------------------------------
### Let's just look at a 2-way interaction first (without groups)
# see here for interpretation: https://datascienceplus.com/interpreting-three-way-interactions-in-r/
### rlm
myModelwm <- rlm(wm_2 ~ ITIM + wm_1, data = predicted) 
myModelef <- rlm(EF_2 ~ ITIM + EF_1, data = predicted)
summary(myModelwm)
summary(myModelef)
myModelwm <- rlm(wm_2 ~ ITIM * wm_1, data = predicted) 
myModelef <- rlm(EF_2 ~ ITIM * EF_1, data = predicted)
summary(myModelwm)
summary(myModelef)
f.robftest(myModelwm, var = "ITIM:wm_1")
f.robftest(myModelef, var = "ITIM:EF_1")
stargazer(myModelef, myModelwm, type = "html",  dep.var.labels =c("EF Factor Score T2","WM Factor Score T2"), covariate.labels = c("ITIM", "EF Factor Score T1", "ITIM:EF Factor Score T1", "WM Factor Score T1","ITIM:WM Factor Score T1"),style = "default" , out = "model.html")
### Check linearity assumptions
interact_plot(myModelwm, pred = wm_1, modx = ITIM, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
interact_plot(myModelef, pred = EF_1, modx = ITIM, modxvals = "plus-minus", linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
probe_interaction(myModelwm, pred = wm_1, modx = ITIM)

### Let's  look at a 3-way interaction 
### rlm
myModelwm <- rlm(wm_2 ~ ITIM * wm_1 * Int, data = predicted) 
myModelef <- rlm(EF_2 ~ ITIM * EF_1 * Int, data = predicted)
summary(myModelwm)
summary(myModelef)
f.robftest(myModelwm, var = "ITIM:wm_1:Int1")
f.robftest(myModelef, var = "ITIM:EF_1:Int1")
stargazer(myModelef, myModelwm, type = "text")
stargazer(myModelef, myModelwm, type = "html",  dep.var.labels =c("EF Factor Score T2","WM Factor Score T2"), covariate.labels = c("ITIM", "EF Factor Score T1","WM Factor Score T1", "Group","ITIM:EF Factor Score T","ITIM:WM Factor Score T1","ITIM:Group","EF Factor Score T1:Group","ITIM:EF Factor Score T1:Group","WM Factor Score T1:Group","ITIM:WM Factor Score T1:Group"),style = "default" , out = "model3way.html")
### Check linearity assumptions
interact_plot(myModelwm, pred = wm_1, modx = ITIM, mod2 = Int, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
interact_plot(myModelef, pred = EF_1, modx = ITIM, mod2 = Int, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)


### Let's  look at a 3-way interaction with Outliers removed
### rlm
myModelwm <- rlm(wm_2 ~ ITIM * wm_1 * Int, data = predictedOR) 
myModelef <- rlm(EF_2 ~ ITIM * EF_1 * Int, data = predictedOR)
summary(myModelwm)
summary(myModelef)
f.robftest(myModelwm, var = "ITIM:wm_1:Int1")
f.robftest(myModelef, var = "ITIM:EF_1:Int1")
stargazer(myModelef, myModelwm, type = "text")
stargazer(myModelef, myModelwm, type = "html",  dep.var.labels =c("EF Factor Score T2","WM Factor Score T2"), covariate.labels = c("ITIM", "EF Factor Score T1","WM Factor Score T1", "Group","ITIM:EF Factor Score T","ITIM:WM Factor Score T1","ITIM:Group","EF Factor Score T1:Group","ITIM:EF Factor Score T1:Group","WM Factor Score T1:Group","ITIM:WM Factor Score T1:Group"),style = "default" , out = "model3way.html")
### Check linearity assumptions
interact_plot(myModelwm, pred = wm_1, modx = ITIM, mod2 = Int, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
interact_plot(myModelef, pred = EF_1, modx = ITIM, mod2 = Int, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)






#### 53. Trying to plot interactions ----------------------
### see https://datascienceplus.com/interpreting-three-way-interactions-in-r/
### 2/3 of the way down the page about interpretting interactions between 2 continuous
### and 1 categorical variable
### probably want to mean centre our ITIM to make it easier to graph predicted scores
#predicted$ITIMc <- c(scale(predicted$ITIM, scale = TRUE))


### Allow it to rename the Factors appropriately
IntNames <- c('0' = "Control Group", '1' = "Experimental Group")
par(mfrow=c(2,2))



### Create predicted scores to be able to graph the lines
myModel <- rlm(wm_2 ~ ITIM * wm_1 * Int, data = predicted)
pred <- expand.grid(wm_1=c(min(predicted$wm_1),0,max(predicted$wm_1)),ITIM=c(min(predicted$ITIM),mean(predicted$ITIM),max(predicted$ITIM)),Int=factor(1:0))
pred$wm_2 <- predict(myModel, pred) 
### now plot it
attach(predicted)
ggplot(predicted,aes(x=wm_1, y=wm_2, color=ITIM))+
  geom_point()+
  facet_grid(~Int, labeller = labeller(Int = IntNames))+
  scale_y_continuous(name = "WM Time 2", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) + 
  scale_x_continuous(name = "WM Time 1", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) +
  geom_line(data=pred,aes(group=ITIM))

### try and plot it to allow for curved regression lines (and plot linear too)
ggplot(data = predictedM, aes(x = wm_1, y = wm_2, color = ITIMgroups)) + 
  geom_point()+
  scale_y_continuous(name = "WM Time 2", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) + 
  scale_x_continuous(name = "WM Time 1", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) +
  geom_smooth(method = "loess", se=FALSE, span = 0.9) +
  #geom_smooth(method = "lm", se = FALSE, linetype = "twodash", size = 0.75) +
  scale_color_discrete(name = "ITIM Score", labels = c("Lower Tercile", "Middle Tercile", "Upper Tercile")) +
  facet_wrap(~Int, labeller = labeller(Int = IntNames)) 





### EF model
### Create predicted scores to be able to graph the lines
myModel <- rlm(EF_2 ~ ITIM * EF_1 * Int, data = predicted) # sig
pred <- expand.grid(EF_1=c(min(predicted$EF_1),0,max(predicted$EF_1)),ITIM=c(min(predicted$ITIM),mean(predicted$ITIM),max(predicted$ITIM)),Int=factor(0:1))
pred$EF_2 <- predict(myModel, pred) 
### now plot it
attach(predicted)
ggplot(predicted,aes(x=EF_1,y=EF_2,color=ITIM))+
  geom_point()+
  facet_grid(~Int, labeller = labeller(Int = IntNames))+
  scale_y_continuous(name = "EF Time 2", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) + 
  scale_x_continuous(name = "EF Time 1", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) +
  geom_line(data=pred,aes(group=ITIM))

### try and plot it to allow for curved regression lines
ggplot(data = predictedM, aes(x = EF_1, y = EF_2, color = ITIMgroups)) + 
  geom_point()+
  scale_y_continuous(name = "EF Time 2", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) + 
  scale_x_continuous(name = "EF Time 1", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) +
  geom_smooth(method = "loess", se=FALSE, span = 0.9) +
  scale_color_discrete(name = "ITIM Score", labels = c("Lower Tercile", "Middle Tercile", "Upper Tercile")) +
  facet_wrap(~Int, labeller = labeller(Int = IntNames))




### PATm model
### robust check
myModel <- rlm(PATmScale_2 ~ ITIM * PATmScale_1 * Int, data = predicted)
summary(myModel)
f.robftest(myModel, var = "ITIM:PATmScale_1:Int1")
### model creation
myModel <- lm(PATmScale_2 ~ ITIM * PATmScale_1 * Int, data = predicted)
summ(myModel)

### Create predicted scores to be able to graph the lines
pred <- expand.grid(PATmScale_1=c(min(predicted$PATmScale_1),mean(predicted$PATmScale_1),max(predicted$PATmScale_1)),ITIM=c(min(predicted$ITIM),mean(predicted$ITIM),max(predicted$ITIM)),Int=factor(0:1))
pred$PATmScale_2 <- predict(myModel, pred) 
### now plot it
ggplot(predicted,aes(x=PATmScale_1,y=PATmScale_2,color=ITIM))+
  geom_point()+
  facet_grid(~Int, labeller = labeller(Int = IntNames))+
  scale_y_continuous(name = "Maths Time 2", limits = c(90,180), breaks=seq(90,180,10)) + 
  scale_x_continuous(name = "Maths Time 1", limits = c(90,180), breaks=seq(90,180,10)) +
  geom_line(data=pred,aes(group=ITIM))



### PATr model
### robust check
myModel <- rlm(PATrScale_2 ~ ITIM * PATrScale_1 * Int, data = predicted)
summary(myModel)
f.robftest(myModel, var = "ITIM:PATrScale_1:Int1")
### model creation
myModel <- lm(PATrScale_2 ~ ITIM * PATrScale_1 * Int, data = predicted)
summ(myModel)

### Create predicted scores to be able to graph the lines
pred <- expand.grid(PATrScale_1=c(min(predicted$PATrScale_1),mean(predicted$PATrScale_1),max(predicted$PATrScale_1)),ITIM=c(min(predicted$ITIM),mean(predicted$ITIM),max(predicted$ITIM)),Int=factor(0:1))
pred$PATrScale_2 <- predict(myModel, pred) 
### now plot it
ggplot(predicted,aes(x=PATrScale_1,y=PATrScale_2,color=ITIM))+
  geom_point()+
  facet_grid(~Int, labeller = labeller(Int = IntNames))+
  scale_y_continuous(name = "Reading Time 2", limits = c(90,180), breaks=seq(90,180,10)) + 
  scale_x_continuous(name = "Reading Time 1", limits = c(90,180), breaks=seq(90,180,10)) +
  geom_line(data=pred,aes(group=ITIM))



### NAI model
### robust check
myModel <- rlm(NAI_2 ~ ITIM * NAI_1 * Int, data = predicted)
summary(myModel)
f.robftest(myModel, var = "ITIM:NAI_1:Int1")
### model creation
### probably want to mean centre our PATm to make it easier
myModel <- lm(NAI_2 ~ ITIM * NAI_1 * Int, data = predicted)
summ(myModel)

### Create predicted scores to be able to graph the lines
pred <- expand.grid(NAI_1=c(min(predicted$NAI_1),mean(predicted$NAI_1),max(predicted$NAI_1)),ITIM=c(min(predicted$ITIM),mean(predicted$ITIM),max(predicted$ITIM)),Int=factor(0:1))
pred$NAI_2 <- predict(myModel, pred) 
### now plot it
ggplot(predicted,aes(x=NAI_1,y=NAI_2,color=ITIM))+
  geom_point()+
  facet_grid(~Int)+
  scale_y_continuous(name = "NAI Time 2", limits = c(80,170), breaks=seq(80,170,10)) + 
  scale_x_continuous(name = "NAI Time 1", limits = c(80,170), breaks=seq(80,170,10)) +
  geom_line(data=pred,aes(group=ITIM))




#### OUTLIERS REMOVED? ###

### Is it outliers that affect the model and make it a significant interaction?
### WM model
### model creation
myModel <- lm(wm_2 ~ ITIM * wm_1 * Int, data = predictedOR) # sig
summ(myModel)

### Create predicted scores to be able to graph the lines
pred <- expand.grid(wm_1=c(min(predictedOR$wm_1),0,max(predictedOR$wm_1)),ITIM=c(min(predictedOR$ITIM),mean(predictedOR$ITIM),max(predictedOR$ITIM)),Int=factor(0:1))
pred$wm_2 <- predict(myModel, pred) 
### now plot it
attach(predictedOR)
ggplot(predictedOR,aes(x=wm_1, y=wm_2, color=ITIM))+
  geom_point()+
  facet_grid(~Int)+
  geom_line(data=pred,aes(group=ITIM))



### Is it outliers that affect the model and make it a significant interaction?
### EF model
### model creation
myModel <- lm(EF_2 ~ ITIM * EF_1 * Int, data = predictedOR) # sig
summ(myModel)
attach(predictedOR)
### Create predicted scores to be able to graph the lines
pred <- expand.grid(EF_1=c(min(predictedOR$EF_1),0,max(predictedOR$EF_1)),ITIM=c(min(predictedOR$ITIM),mean(predictedOR$ITIM),max(predictedOR$ITIM)),Int=factor(0:1))
pred$EF_2 <- predict(myModel, pred) 
### now plot it
ggplot(predictedOR,aes(x=EF_1,y=EF_2,color=ITIM))+geom_point()+facet_grid(~Int)+
  geom_line(data=pred,aes(group=ITIM))






### 54. Johnson-Neyman interactions ----------------
# Need to scale ITIM to get it to work in Johnson-Neyman analysis
predicted$ITIMc <- c(scale(predicted$ITIM, scale = TRUE))
predictedOR$ITIMc <- c(scale(predictedOR$ITIM, scale = TRUE))

### Johnson-Neyman approach to simple interactions
### WM

myModel <- lm(wm_2 ~ ITIMc * wm_1 * Int, data = predicted) # sig
summ(myModel)
sim_slopes(myModel, pred = wm_1, modx = ITIMc, mod2 = Int, johnson_neyman = TRUE, jnplot = TRUE)

myModel <- lm(wm_2 ~ ITIMc * wm_1, data = predicted) # sig
summ(myModel)
sim_slopes(myModel, pred = wm_1, modx = ITIMc, johnson_neyman = TRUE, jnplot = TRUE)

inter.raw(Y = "wm_2", D = "Int", X = "ITIM", data = predicted, weights = NULL, Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")
inter.gam(Y="wm_2", D="Int", X="ITIM", Z=c("wm_1"), data=predicted)
inter.binning(Y = "wm_2", D = "Int", X = "ITIM", Z="wm_1", data = predicted, FE = "Int", vartype = "robust", Xdistr = "density")

### simple slopes analysis:
model1 <- lmres(wm_2 ~ ITIMc * wm_1 * Int, centered = c("wm_2", "wm_1"), data = predicted)
summary.lmres(model1)
model1Slope <- simpleSlope(model1, pred = "wm_1", mod1 = "ITIMc", mod2 = "Int1")
summary.simpleSlope(model1Slope)
PlotSlope(model1Slope)


sim_slopes(myModel, pred = wm_1, modx = ITIMc)
### Check linearity assumptions
interact_plot(myModel, pred = wm_1, modx = ITIMc, mod2 = Int, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
slopemodel <- sim_slopes(myModel, pred = wm_1, modx = ITIMc)
probe_interaction(myModel, pred = wm_1, modx = ITIMc, mod2 = Int)
interact_plot(myModel, pred = wm_1, modx = ITIMc, mod2 = Int, x.label = "Group", y.label = "Posttest Score") + theme_apa()





### Allow it to rename the Factors appropriately
ITIMNames <- c('Lower tercile of ITIMc' = "Lower tercile of ITIM", '1' = "Experimental Group")
par(mfrow=c(2,2))



ggplot(predicted,aes(x=wm_1, y=wm_2, color=ITIM))+
  geom_point()+
  facet_grid(~Int, labeller = labeller(Int = IntNames))+
  scale_y_continuous(name = "WM Time 2", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) + 
  scale_x_continuous(name = "WM Time 1", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) +
  geom_line(data=pred,aes(group=ITIM))




### EF
myModel <- lm(EF_2 ~ ITIMc * EF_1 * Int, data = predicted) # sig
summ(myModel)
sim_slopes(myModel, pred = EF_1, modx = ITIMc, mod2 = Int, johnson_neyman = TRUE, jnplot = TRUE)
sim_slopes(myModel, pred = EF_1, modx = ITIMc)
### test for linearity
interact_plot(myModel, pred = EF_1, modx = ITIMc, mod2 = Int, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE) +
  scale_y_continuous(name = "EF Time 2", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) + 
  scale_x_continuous(name = "EF Time 1", limits = c(-2.7,2.7), breaks=seq(-2.5,2.5,1)) 
  
slopemodel <- sim_slopes(myModel, pred = EF_1, modx = ITIMc)
probe_interaction(myModel, pred = EF_1, modx = ITIMc, mod2 = Int)
interact_plot(myModel, pred = EF_1, modx = ITIMc, mod2 = Int, x.label = "Group", y.label = "Posttest Score") 

# See here for Interflex: http://yiqingxu.org/software/interaction/RGuide.html 
inter.raw(Y = "EF_2", D = "Int", X = "ITIM", data = predicted, weights = NULL, Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")
inter.gam(Y="wm_2", D="Int", X="ITIM", Z=c("wm_1"), data=predicted)
inter.binning(Y = "wm_2", D = "Int", X = "ITIM", Z="wm_1", data = predicted, FE = "Int", vartype = "robust", Xdistr = "density")







# 55. Post-hoc interactions ----------------------

## does EF or WM moderate the improvement in PAT & NAI?
myModel <- lm(PATmScale_2 ~ PATmScale_1 * EF_1 , data = predicted)
summ(myModel)
myModel <- lm(PATmScale_2 ~ PATmScale_1 * wm_1 , data = predicted)
summ(myModel)

myModel <- lm(PATrScale_2 ~ PATrScale_1 * EF_1 , data = predicted) # sig Int.
summ(myModel)
sim_slopes(myModel, pred = PATrScale_1, modx = EF_1, johnson_neyman = TRUE, jnplot = TRUE)
sim_slopes(myModel, pred = PATrScale_1, modx = EF_1)
interact_plot(myModel, pred = PATrScale_1, modx = EF_1, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
slopemodel <- sim_slopes(myModel, pred = PATrScale_1, modx = EF_1)
probe_interaction(myModel, pred = PATrScale_1, modx = EF_1)
interact_plot(myModel, pred = PATrScale_1, modx = EF_1, x.label = "PATr Pretest", y.label = "PATr Postest") + 
  
  theme_apa()

myModel <- lm(PATrScale_2 ~ PATrScale_1 * wm_1 , data = predicted) # sig Int.
summ(myModel)
sim_slopes(myModel, pred = PATrScale_1, modx = wm_1, johnson_neyman = TRUE, jnplot = TRUE)
sim_slopes(myModel, pred = PATrScale_1, modx = wm_1)
interact_plot(myModel, pred = PATrScale_1, modx = wm_1, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
slopemodel <- sim_slopes(myModel, pred = PATrScale_1, modx = wm_1)
probe_interaction(myModel, pred = PATrScale_1, modx = wm_1)
interact_plot(myModel, pred = PATrScale_1, modx = wm_1, x.label = "PATr Pretest", y.label = "PATr Postest") + 
  
  theme_apa()



myModel <- lm(EF_2 ~ Int * Age.Yr , data = predicted) # sig Int.
summ(myModel)
sim_slopes(myModel, pred = Int, modx = Age.Yr, johnson_neyman = TRUE, jnplot = TRUE)
sim_slopes(myModel, pred = Int, modx = Age.Yr)
interact_plot(myModel, pred = Int, modx = Age.Yr, linearity.check = TRUE, plot.points = TRUE, modx.values = "terciles", robust = TRUE)
slopemodel <- sim_slopes(myModel, pred = Int, modx = Age.Yr)
probe_interaction(myModel, pred = Int, modx = Age.Yr)
interact_plot(myModel, pred = Int, modx = Age.Yr, x.label = "Group", y.label = "EF postest") + 
  
  theme_apa()



#...----------------------------------------------
#...----------------------------------------------

#  *** Processing Speed ***-----------------------------------------

# 56. Processing Speed Analysis --------------------------------------------------

### Reaction Time T2 vs T1 ###
### Not Significant
model1 <- lm(RTaverage_2 ~ RTaverage_1 + Group, data = predicted)
model2 <- lm(RTaverage_2 ~ RTaverage_1 * Group, data = predicted)
simpleModel <- lm(RTaverage_2 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$RTaverage_2 ~ predicted$Group, center = mean) ## Not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(RTaverage_2 ~ RTaverage_1 + Group, data = predicted), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$RTaverage_2, predicted$Group)





### Reaction Time T3 vs T1
model1 <- lm(RTaverage_3 ~ RTaverage_1 + Group, data = predicted)
model2 <- lm(RTaverage_3 ~ RTaverage_1 * Group, data = predicted)
simpleModel <- lm(RTaverage_3 ~ Group, data = predicted)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(predicted$RTaverage_3 ~ predicted$Group, center = mean) ## Not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(RTaverage_3 ~ RTaverage_1 + Group, data = predicted), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(predicted$RTaverage_3, predicted$Group)




# For Fry & Hale's (1996) developmental cascade theory for PS we should see an improvement in WM if we see an improvment in PS.
### Did PS change from T1 to T2?
yuend(predicted$RTaverage_1, predicted$RTaverage_2, tr = 0.2)
psych::describe(predicted$RTaverage_1)
psych::describe(predicted$RTaverage_2)
mes(384.81, 408.78, 56.11, 61.29, 105, 105) ### Cohen's d 

### What about from T1 to T3?
yuend(predicted$RTaverage_1, predicted$RTaverage_3)
psych::describe(predicted$RTaverage_1)
psych::describe(predicted$RTaverage_3)
mes(384.81, 408.78, 56.11, 61.29, 105, 105) ### Cohen's d 




#...----------------------------------------------
#...----------------------------------------------
#...*** TREATMENT FIDELITY ***----------------------------------------------
# 57. Exp. Near-Transfer Improvement -----------------------------
### Add fidelity measures to the back end of diffScores data table
diffScores <- cbind(diffScores,fidelity[,7:33])
write.csv(diffScores,"diffScores.csv")

apa.cor.table(diffScores[,c(8:9,85:106)], filename = "fidelity_Exp.doc", table.number = 3)
apa.cor.table(diffScores[,c(8:9,107:111)], filename = "fidelity_Ctrl.doc", table.number = 4)

### If you want to draw a scatterplot
scatterplot(diffScores$WM2WM1 ~  fidelity$Duck.points, xlab = "Duck.points", ylab = "WM Difference", smooth = TRUE, cex = 0.5)
### To try and get confidence intervals for Kendall's tau (seems to give different tau than cor.test though so be careful)
DescTools::KendallTauA(diffScores$WM2WM1, diffScores$PPPP.plat,  conf.level = 0.95) 

##
##
##
##

### Packing Pete - Points (Normal according to Shapiro-Wilk)
citation("NSM3")
cor.test(diffScores$EF2EF1, diffScores$PPPP.points,  method = "pearson", conf.level = 0.95) 
cor.test(diffScores$WM2WM1, diffScores$PPPP.points,  method = "pearson", conf.level = 0.95)
### Packing Pete - Plat
cor.test(diffScores$EF2EF1, diffScores$PPPP.plat, method = "kendall")
set.seed(2702)
kendall.ci(diffScores$EF2EF1, diffScores$PPPP.plat, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$PPPP.plat, method = "kendall")
kendall.ci(diffScores$WM2WM1, diffScores$PPPP.plat, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)

### Ducks - Points (normal according to Shapiro-Wilk)
cor.test(diffScores$EF2EF1, diffScores$Duck.points, method = "pearson", conf.level = 0.95) 
cor.test(diffScores$WM2WM1, diffScores$Duck.points, method = "pearson", conf.level = 0.95) 
### Ducks - Plat
cor.test(diffScores$EF2EF1, diffScores$Duck.plat,  method = "kendall") 
kendall.ci(diffScores$EF2EF1, diffScores$Duck.plat, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$Duck.plat,  method = "kendall")
kendall.ci(diffScores$WM2WM1, diffScores$Duck.plat, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)

### TT - Points (non-normal)
cor.test(diffScores$EF2EF1, diffScores$TT.points,  method = "kendall") 
kendall.ci(diffScores$EF2EF1, diffScores$TT.points, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$TT.points,  method = "kendall") 
kendall.ci(diffScores$WM2WM1, diffScores$TT.points, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
### TT - Plat
cor.test(diffScores$EF2EF1, diffScores$TT.plat,  method = "kendall")  # -0.283 ** Sig
kendall.ci(diffScores$EF2EF1, diffScores$TT.plat, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$TT.plat,  method = "kendall")
kendall.ci(diffScores$WM2WM1, diffScores$TT.plat, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)

### ML - Points (non-normal)
cor.test(diffScores$EF2EF1, diffScores$ML.points,  method = "kendall") 
kendall.ci(diffScores$EF2EF1, diffScores$ML.points, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$ML.points,  method = "kendall") 
kendall.ci(diffScores$WM2WM1, diffScores$ML.points, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
### ML - Plat
cor.test(diffScores$EF2EF1, diffScores$ML.plat,  method = "kendall")  # -0.236 * Sig
kendall.ci(diffScores$EF2EF1, diffScores$ML.plat, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$ML.plat,  method = "kendall")  
kendall.ci(diffScores$WM2WM1, diffScores$ML.plat, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)

### MT - Points
cor.test(diffScores$EF2EF1, diffScores$MT.points, method = "pearson", conf.level = 0.95) 
cor.test(diffScores$WM2WM1, diffScores$MT.points, method = "pearson", conf.level = 0.95) 
### MT - longSeq
cor.test(diffScores$EF2EF1, diffScores$MT.longSeq,  method = "kendall")  # 0.247 * sig
kendall.ci(diffScores$EF2EF1, diffScores$MT.longSeq, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$MT.longSeq,  method = "kendall")  
kendall.ci(diffScores$WM2WM1, diffScores$MT.longSeq, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
### MT - avgLeng
cor.test(diffScores$EF2EF1, diffScores$MT.avgLeng,  method = "kendall")  # 0.258 * sig
kendall.ci(diffScores$EF2EF1, diffScores$MT.avgLeng, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$MT.avgLeng,  method = "kendall")  # 0.225 * sig
kendall.ci(diffScores$WM2WM1, diffScores$MT.avgLeng, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)

### GA - Points
cor.test(diffScores$EF2EF1, diffScores$GA.points, method = "pearson", conf.level = 0.95) 
cor.test(diffScores$WM2WM1, diffScores$GA.points, method = "pearson", conf.level = 0.95) 
### GA - longSeq
cor.test(diffScores$EF2EF1, diffScores$GA.longSeq,  method = "kendall")
kendall.ci(diffScores$EF2EF1, diffScores$GA.longSeq, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$GA.longSeq,  method = "kendall") 
kendall.ci(diffScores$WM2WM1, diffScores$GA.longSeq, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
### GA - avgLeng
cor.test(diffScores$EF2EF1, diffScores$GA.avgLeng,  method = "kendall") 
kendall.ci(diffScores$EF2EF1, diffScores$GA.avgLeng, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$GA.avgLeng,  method = "kendall")
kendall.ci(diffScores$WM2WM1, diffScores$GA.avgLeng, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)


# Count of how many N for the C8 Sciences data
count(diffScores, vars = ML.plat) # N = 50






# 58. Ctrl Near-Transfer Improvement----------------------------------------------
### Score
cor.test(diffScores$EF2EF1, diffScores$TC.Score, method = "pearson", conf.level = 0.95) 
cor.test(diffScores$WM2WM1, diffScores$TC.Score, method = "pearson", conf.level = 0.95) # -0.333 * sig

### Avg.Acc
cor.test(diffScores$EF2EF1, diffScores$TC.Avg.Acc,  method = "kendall")
kendall.ci(diffScores$EF2EF1, diffScores$TC.Avg.Acc, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$TC.Avg.Acc,  method = "kendall") 
kendall.ci(diffScores$WM2WM1, diffScores$TC.Avg.Acc, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)

### Avg.WPM
cor.test(diffScores$EF2EF1, diffScores$TC.Avg.WPM,  method = "kendall") 
kendall.ci(diffScores$EF2EF1, diffScores$TC.Avg.WPM, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$TC.Avg.WPM,  method = "kendall") # -.246 *Sig
kendall.ci(diffScores$WM2WM1, diffScores$TC.Avg.WPM, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)

### TC.Attempts
cor.test(diffScores$EF2EF1, diffScores$TC.Attempts,  method = "kendall") 
kendall.ci(diffScores$EF2EF1, diffScores$TC.Attempts, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
cor.test(diffScores$WM2WM1, diffScores$TC.Attempts,  method = "kendall") 
kendall.ci(diffScores$WM2WM1, diffScores$TC.Attempts, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)


# Count of how many N for the Typing Club data
count(diffScores, vars = TC.Attempts) # N = 48 (7 missing data for fidelity)





#...----------------------------------------------
#...----------------------------------------------
#...*** INDIVIDUAL DIFFERENCES***----------------------------------------------
### raw values are used as per Karbach, Strobach & Schubert (2015)


# 59. Prove that sample > normal  -----------------------------------------

#Demonstrate that the sample is better than norm on NNAT2
####                  Norm    Ctrl    Exp     
####                  1         2       3 
Mean <- c(      100,    115.96,  110.2)
SD <- c(        16,     13.27,   11.48)
SampleSize <- c(100000, 55,      50)

### This function was from here: https://stackoverflow.com/questions/29260139/r-function-to-perform-anova-and-tukeyhsd-from-sample-mean-sd-and-n
gen_data <- function(means, sds, samplesizes){
  n.grp <- length(means)
  grps <- factor(rep(1:n.grp, samplesizes))
  dat <- lapply(1:n.grp, function(i) {scale(rnorm(samplesizes[i]))*sds[i] + means[i]})
  y <- do.call(rbind, dat)
  out <- data.frame(group = grps, y = y)
  out
}

simulated_data <- gen_data(Mean, SD, SampleSize)
av <- aov(y ~ group, data = simulated_data)
summary(av)
TukeyHSD(av)


mes(100, 115.96, 16, 13.27, 100000, 55) ### Cohen's d 1 v 2
mes(100, 110.2, 16, 11.48, 100000, 50) ### Cohen's d 1 v 3
mes(110.2, 115.96, 11.48, 13.27, 50, 55) ### Cohen's d 2 v 3



# 60. Correlations: Magnification v. Compensation -----------------------------------------

### This is asking the question of whether those of higher ability improved more
### Asking whether baseline cognitive should be +ve correlated with training gains

### PATm
cor.test(diffScores$EF2EF1, diffScores$PATmScale_1, use = "p") 
cor.test(diffScores$WM2WM1, diffScores$PATmScale_1, use = "p")
### PATr
cor.test(diffScores$EF2EF1, diffScores$PATrScale_1, use = "p")
cor.test(diffScores$WM2WM1, diffScores$PATrScale_1, use = "p") 
### NAI
cor.test(diffScores$EF2EF1, diffScores$NAI_1, use = "p") 
cor.test(diffScores$WM2WM1, diffScores$NAI_1, use = "p") 
# None of these correlations p < .05

### These are the 'standardised' values for change scores
### PATm
cor.test(diffScoresSTD$EF2EF1std, diffScoresSTD$PATmScale_1, use = "p") 
cor.test(diffScoresSTD$WM2WM1std, diffScoresSTD$PATmScale_1, use = "p")
### PATr
cor.test(diffScoresSTD$EF2EF1std, diffScoresSTD$PATrScale_1, use = "p")
cor.test(diffScoresSTD$WM2WM1std, diffScoresSTD$PATrScale_1, use = "p") 
### NAI
cor.test(diffScoresSTD$EF2EF1std, diffScoresSTD$NAI_1, use = "p") 
cor.test(diffScoresSTD$WM2WM1std, diffScoresSTD$NAI_1, use = "p") 
# None of these correlations p < .05


### But this is generally... Need to ask this question of the Exp. Group
### Separate the data
Exp <- subset(diffScores, diffScores$Group == "Exp.")
ExpSTD <- subset(diffScoresSTD, diffScoresSTD$Group == "Exp.") ### For standardised gain scores

## test for normality - all p > .05
shapiro.test(Exp$EF2EF1)
shapiro.test(Exp$WM2WM1)
shapiro.test(Exp$PATmScale_1)
shapiro.test(Exp$PATrScale_1) 
shapiro.test(Exp$NAI_1) 

### PATm
cor.test(Exp$EF2EF1, Exp$PATmScale_1, use = "p") 
cor.test(Exp$WM2WM1, Exp$PATmScale_1, use = "p")
### PATr
cor.test(Exp$EF2EF1, Exp$PATrScale_1, use = "p")
cor.test(Exp$WM2WM1, Exp$PATrScale_1, use = "p") 
### NAI
cor.test(Exp$EF2EF1, Exp$NAI_1, use = "p") 
cor.test(Exp$WM2WM1, Exp$NAI_1, use = "p") 
### None of this correlations P < .05


### And again for the standardised scores...
### PATm
cor.test(ExpSTD$EF2EF1std, ExpSTD$PATmScale_1, use = "p") 
cor.test(ExpSTD$WM2WM1std, ExpSTD$PATmScale_1, use = "p")
### PATr
cor.test(ExpSTD$EF2EF1std, ExpSTD$PATrScale_1, use = "p")
cor.test(ExpSTD$WM2WM1std, ExpSTD$PATrScale_1, use = "p") 
### NAI
cor.test(ExpSTD$EF2EF1std, ExpSTD$NAI_1, use = "p") 
cor.test(ExpSTD$WM2WM1std, ExpSTD$NAI_1, use = "p") 
### None of this correlations P < .05





# 61. Magnification: Split by Improvement -----------------------------
### let's look at those who improved more than others... what is it about them that makes them interesting to look at?
### We are just looking at the experimental group here
### let's use a different dataset
diffScores2 <- subset(diffScores, diffScores$Group == "Exp.")
### dichotomous median split for improvement
diffScores2$EF2EF1groups <- as.numeric(dicho(diffScores2$EF2EF1, dich.by = "median"))
diffScores2$WM2WM1groups <- as.numeric(dicho(diffScores2$WM2WM1, dich.by = "median"))
diffScores2$EF2EF1groups <- as.factor(diffScores2$EF2EF1groups)
diffScores2$WM2WM1groups <- as.factor(diffScores2$WM2WM1groups)
diffScores2$EF2EF1groups <- revalue(diffScores2$EF2EF1groups, c("1"="Low"))
diffScores2$EF2EF1groups <- revalue(diffScores2$EF2EF1groups, c("2"="High"))
diffScores2$WM2WM1groups <- revalue(diffScores2$WM2WM1groups, c("1"="Low"))
diffScores2$WM2WM1groups <- revalue(diffScores2$WM2WM1groups, c("2"="High"))
write.csv(diffScores2, "diffscores2.csv")

describeBy(diffScores2$EF2EF1, diffScores2$EF2EF1groups)
describeBy(diffScores2$WM2WM1, diffScores2$WM2WM1groups)
describeBy(diffScores2$ITIM, diffScores2$EF2EF1groups)
describeBy(diffScores2$ITIM, diffScores2$WM2WM1groups)

ggplot(data = diffScores2, mapping = aes(x = EF_1, y = EF_2, colour = EF2EF1groups)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  facet_grid(~ EF2EF1groups) +
  facet_grid(~ Grade) +
  labs (x = "EF T1", y = "EF T2") 

ggplot(data = diffScores2, mapping = aes(x = wm_1, y = wm_2, colour = WM2WM1groups)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  facet_grid(~ EF2EF1groups) +
  facet_grid(~ Grade) +
  labs (x = "WM T1", y = "WM T2") 


# 62. Baseline differences -----------------------------

### Age? Is there a difference by Age
t.test(Age.Yr ~ EF2EF1groups, data=diffScores2) # p = .88
describeBy(diffScores2$Age.Yr, diffScores2$EF2EF1groups)
mes(10, 9.9, 1.14, 1.09, 25, 25) ### Cohen's d 
t.test(Age.Yr ~ WM2WM1groups, data=diffScores2) # p = .99
describeBy(diffScores2$Age.Yr, diffScores2$WM2WM1groups)
mes(9.94, 9.95, 1.05, 1.18, 25, 25) ### Cohen's d 

###
###
### GArdner-Altman Plots:
unpaired_mean_diff <- dabest(diffScores2, EF2EF1groups, idx = c("Low","High"), PATmScale_1, paired = FALSE)
plot(unpaired_mean_diff)


### Look at the individual measures 
### Baseline cognitive measures
### look at PATm?
t.test(PATmScale_1 ~ EF2EF1groups, alternative = c("less"), data=diffScores2) # p = .561
describeBy(diffScores2$PATmScale_1, diffScores2$EF2EF1groups)
mes(130.51, 130.03, 10.35, 11.82, 25, 25) ### Cohen's d 
t.test(PATmScale_1 ~ WM2WM1groups, alternative = c("less"), data=diffScores2) # p = .263
describeBy(diffScores2$PATmScale_1, diffScores2$WM2WM1groups)
mes(129.27, 131.27, 10.82, 11.31, 25, 25) ### Cohen's d 


### look at PATr?
t.test(PATrScale_1 ~ EF2EF1groups, alternative = c("less"), data=diffScores2) # p = .719
describeBy(diffScores2$PATrScale_1, diffScores2$EF2EF1groups)
mes(131.06, 128.96, 12.37, 13.06, 25, 25) ### Cohen's d 
t.test(PATrScale_1 ~ WM2WM1groups, alternative = c("less"), data=diffScores2) # p = .418
describeBy(diffScores2$PATrScale_1, diffScores2$WM2WM1groups)
mes(129.63, 130.38, 14.03, 11.35, 25, 25) ### Cohen's d 


### look at NAI?
t.test(NAI_1 ~ EF2EF1groups, alternative = c("less"), data=diffScores2) # p = .442
describeBy(diffScores2$NAI_1, diffScores2$EF2EF1groups)
mes(109.96, 110.44, 10.31, 12.75, 25, 25) ### Cohen's d 
t.test(NAI_1 ~ WM2WM1groups, alternative = c("less"), data=diffScores2) # p = .106
describeBy(diffScores2$NAI_1, diffScores2$WM2WM1groups)
mes(108.16, 112.24, 12.03, 10.75, 25, 25) ### Cohen's d 


### look at PS?
t.test(RTaverage_1 ~ EF2EF1groups, alternative = c("greater"), data=diffScores2) # p = .439
describeBy(diffScores2$RTaverage_1, diffScores2$EF2EF1groups)
mes(394.82, 392.29, 64.24, 49.99, 25, 25) ### Cohen's d 
t.test(RTaverage_1 ~ WM2WM1groups, alternative = c("greater"), data=diffScores2) # p = .073
describeBy(diffScores2$RTaverage_1, diffScores2$WM2WM1groups)
mes(405.33, 381.79, 65.28, 45.6, 25, 25) ### Cohen's d 


### look at ITIM?
t.test(ITIM ~ EF2EF1groups, alternative = c("less"), data=diffScores2) # p = .491
describeBy(diffScores2$ITIM, diffScores2$EF2EF1groups)
mes(26.16, 26.2, 5.67, 7.06, 25, 25) ### Cohen's d 
t.test(ITIM ~ WM2WM1groups, alternative = c("less"), data=diffScores2) # p = .027
describeBy(diffScores2$ITIM, diffScores2$WM2WM1groups)
mes(24.6, 27.76, 6.9, 5.4, 25, 25) ### Cohen's d 
ggplot(diffScores2) + 
  geom_bar(aes(WM2WM1groups, ITIM, fill = as.factor(WM2WM1groups)), 
           position = "dodge", stat = "summary", fun.y = "mean")

high <- subset(diffScores2, diffScores2$WM2WM1groups == "High")
scatterplot(high$WM2WM1 ~ high$ITIM)
cor.test(high$WM2WM1, high$ITIM,  method = "kendall") 
kendall.ci(high$WM2WM1, high$ITIM, alpha=0.05, type="t", bootstrap=T, B=1000, example=F)
### GArdner-Altman Plots:
unpaired_mean_diff <- dabest(diffScores2, WM2WM1groups, idx = c("Low","High"), ITIM, paired = FALSE)
plot(unpaired_mean_diff, rawplot.ylabel = "Implicit Theory of Intelligence Measure")


### look at IEM Conc.?
t.test(IEMconc._1 ~ EF2EF1groups, alternative = c("less"), data=diffScores2) # p = .753
describeBy(diffScores2$IEMconc._1, diffScores2$EF2EF1groups)
mes(4.2, 3.84, 1.58, 2.08, 25, 25) ### Cohen's d 
t.test(IEMconc._1 ~ WM2WM1groups, alternative = c("less"), data=diffScores2) # p = .027
describeBy(diffScores2$IEMconc._1, diffScores2$WM2WM1groups)
mes(3.52, 4.52, 2.06, 1.45, 25, 25) ### Cohen's d 
### GArdner-Altman Plots:
unpaired_mean_diff <- dabest(diffScores2, WM2WM1groups, idx = c("Low","High"), IEMconc._1, paired = FALSE)
plot(unpaired_mean_diff, rawplot.ylabel = "Improvement Expectancy: Concentration", rawplot.markersize = 1, rawplot.groupwidth = 0.2)


### look at IEM Maths?
t.test(IEMmaths_1 ~ EF2EF1groups, alternative = c("less"), data=diffScores2) # p = .406
describeBy(diffScores2$IEMmaths_1, diffScores2$EF2EF1groups)
mes(3.16, 3.28, 1.82, 1.72, 25, 25) ### Cohen's d 
t.test(IEMmaths_1 ~ WM2WM1groups, alternative = c("less"), data=diffScores2) # p = .045
describeBy(diffScores2$IEMmaths_1, diffScores2$WM2WM1groups)
mes(2.8, 3.64, 1.96, 1.44, 25, 25) ### Cohen's d 


### look at IEM English?
t.test(IEMenglish_1 ~ EF2EF1groups, alternative = c("less"), data=diffScores2) # p = .076
describeBy(diffScores2$IEMenglish_1, diffScores2$EF2EF1groups)
mes(1.84, 2.6, 1.93, 1.76, 25, 25) ### Cohen's d 
t.test(IEMenglish_1 ~ WM2WM1groups, alternative = c("less"), data=diffScores2) # p = .165
describeBy(diffScores2$IEMenglish_1, diffScores2$WM2WM1groups)
mes(1.96, 2.48, 1.74, 1.98, 25, 25) ### Cohen's d 






#63. Bayes Factors ----------------------------------------------

# Calculations done in JASP







#...----------------------------------------------
#...----------------------------------------------
#*** GRADE DIFFERENCES ***----------------------------------------------
# 64. Split by Grade: Baseline differences -----------------------------
### let's create a group with just each Grade
g3 <- subset(diffScores, diffScores$Grade == "3")
g5 <- subset(diffScores, diffScores$Grade == "5")

### Year 3
### Differences between time 1 scores?
t.test(EF_1 ~ Group, data=g3) # *** significant
describeBy(g3$EF_1, g3$Group)
mes(-0.93, -0.26, 0.63, 0.65, 22, 20) ### Cohen's d 

t.test(wm_1 ~ Group, data=g3) # *** significant
describeBy(g3$wm_1, g3$Group)
mes(-0.91, -0.22, 0.63, 0.66, 22, 20) ### Cohen's d 

### Year 5
## Differences between time 1 scores?
t.test(EF_1 ~ Group, data=g5)
describeBy(g5$EF_1, g5$Group)
mes(0.37, 0.44, 0.65, 0.78, 28, 35) ### Cohen's d 

t.test(wm_1 ~ Group, data=g5)
describeBy(g5$wm_1, g5$Group)
mes(0.27, 0.48, 0.62, 0.72, 28, 35) ### Cohen's d 

### General info
### Do groups differ on age?
t.test(Age.Yr ~ Group, data=g3)
describeBy(g3$Age.Yr, g3$Group)

### Do groups differ on age?
t.test(Age.Yr ~ Group, data=g5)
describeBy(g5$Age.Yr, g5$Group)

### Do groups differ on the number of hours they trained?
t.test(Train.Hr ~ Group, data=g3)
describeBy(g3$Train.Hr, g3$Group)

t.test(Train.Hr ~ Group, data=g5)
describeBy(g5$Train.Hr, g5$Group)

### Individual T1 tests
### Individual Working Memory measures
t.test(OSPANpartialscore_1 ~ Group, data=g3)
describeBy(g3$OSPANpartialscore_1, g3$Group)
t.test(RSPANpartialscore_1 ~ Group, data=g3)# * significant difference
describeBy(g3$RSPANpartialscore_1, g3$Group)
t.test(SYMSPANpartialscore_1 ~ Group, data=g3)
describeBy(g3$SYMSPANpartialscore_1, g3$Group)

t.test(OSPANpartialscore_1 ~ Group, data=g5) # * significant difference
describeBy(g5$OSPANpartialscore_1, g5$Group)
t.test(RSPANpartialscore_1 ~ Group, data=g5)
describeBy(g5$RSPANpartialscore_1, g5$Group)
t.test(SYMSPANpartialscore_1 ~ Group, data=g5)
describeBy(g5$SYMSPANpartialscore_1, g5$Group)


### Other individual measures
t.test(PATmScale_1 ~ Group, data=g3) # * significant difference
describeBy(g3$PATmScale_1, g3$Group)
mes(122.31, 128.03, 8.15, 6.25, 22, 20) ### Cohen's d 
t.test(PATrScale_1 ~ Group, data=g3)
describeBy(g3$PATrScale_1, g3$Group)
mes(121.77, 127.13, 12.35, 12.22, 22, 20) ### Cohen's d 
t.test(NAI_1 ~ Group, data=g3) # * significant difference
describeBy(g3$NAI_1, g3$Group)
mes(108, 118.65, 10, 12.47, 22, 20) ### Cohen's d 
t.test(CEFI.P.AT.SS_1 ~ Group, data=g3) # * sig
t.test(CEFI.P.FX.SS_1 ~ Group, data=g3)# * sig
t.test(CEFI.P.WM.SS_1 ~ Group, data=g3)
t.test(CEFI.T.AT.SS_1 ~ Group, data=g3) # * significant difference
t.test(CEFI.T.FX.SS_1 ~ Group, data=g3)# * significant difference
t.test(CEFI.T.WM.SS_1 ~ Group, data=g3)# * significant difference


### Other individual measures
t.test(PATmScale_1 ~ Group, data=g5) 
describeBy(g5$PATmScale_1, g5$Group)
mes(136.52, 137.74, 8.71, 8.02, 28, 35) ### Cohen's d 
t.test(PATrScale_1 ~ Group, data=g5)
describeBy(g5$PATrScale_1, g5$Group)
mes(136.48, 137.67, 8.51, 10.83, 28, 35) ### Cohen's d 
t.test(NAI_1 ~ Group, data=g5) 
describeBy(g5$NAI_1, g5$Group)
mes(111.93, 114.43, 12.43, 13.65, 28, 35) ### Cohen's d 
t.test(CEFI.P.AT.SS_1 ~ Group, data=g5)
t.test(CEFI.P.AT.SS_1 ~ Group, data=g5)
t.test(CEFI.P.FX.SS_1 ~ Group, data=g5)
t.test(CEFI.P.WM.SS_1 ~ Group, data=g5)
t.test(CEFI.T.AT.SS_1 ~ Group, data=g5) 
t.test(CEFI.T.FX.SS_1 ~ Group, data=g5)
t.test(CEFI.T.WM.SS_1 ~ Group, data=g5)


#65. near-transfer ANCOVAs (Grade 5)----------------------------------------------
### EF T2 vs T1
### This shows a difference between groups for scores on T2 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(EF_2 ~ EF_1 + Group, data = g5)
model2 <- lm(EF_2 ~ EF_1 * Group, data = g5)
simpleModel <- lm(EF_2 ~ Group, data = g5)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### interaction present
sim_slopes(model1, pred = EF_1, modx = Group, johnson_neyman = TRUE, jnplot = TRUE) ### Can't run for factor moderators...
## simple slopes
model1 <- lmres(EF_2 ~ EF_1 * Group, centered = c("EF_2", "EF_1"), data = g5)
summary.lmres(model1)
model1Slope <- simpleSlope(model1, pred = "EF_1", mod1 = "Group1")
summary.simpleSlope(model1Slope)
PlotSlope(model1Slope)


### Robust
### remember that the p-value does not take into account multiple testing
### so need to multiply each value by 5 (number of tests taken)
ancova(g5$EF_2 ~ g5$EF_1 + g5$Group) ### no significant differences
ggplot(data = g5, mapping = aes(x = EF_1, y = EF_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "EF Factor Score T1", y = "EF Factor Score T2") +
  geom_vline(xintercept = c(-1.1, -0.15, 0.31, 0.75, 1.5), col = "orange", lty=3) +
  theme_apa()


### EF T3 vs T1
### Not Significant
model1 <- lm(EF_3~ EF_1 + Group, data = g5)
model2 <- lm(EF_3 ~ EF_1 * Group, data = g5)
simpleModel <- lm(EF_3 ~ Group, data = g5)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test for homogeneity of variance
leveneTest(g5$EF_3 ~ g5$Group, center = mean) ## not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(EF_3~ EF_1 + Group, data = g5), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## not significant
Anova(aov(EF_3 ~ EF_1 * Group, data = g5), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(g5$EF_3, g5$Group)

### Robust
ancova(g5$EF_3 ~ g5$EF_1 + g5$Group) ### no significant differences
ggplot(data = g5, mapping = aes(x = EF_1, y = EF_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "EF Factor Score T1", y = "EF Factor Score T3") +
  geom_vline(xintercept = c(-1.1, -0.15, 0.31, 0.75, 1.5), col = "orange", lty=3) +
  theme_apa()



## WM T2 vs T1
### This shows a difference between groups for scores on T2 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(wm_2 ~ wm_1 + Group, data = g5)
model2 <- lm(wm_2 ~ wm_1 * Group, data = g5)
simpleModel <- lm(EF_2 ~ Group, data = g5)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test for homogeneity of variance
leveneTest(g5$wm_2 ~ g5$Group, center = mean) ## significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(wm_2 ~ wm_1 + Group, data = g5), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## * significant: different between groups for scores on T2 when accounting for T1 scores
Anova(aov(wm_2 ~ wm_1 * Group, data = g5), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(g5$wm_2, g5$Group)

### Robust
ancova(g5$wm_2 ~ g5$wm_1 + g5$Group ) ### sig
p <- c(.0972, .0006, .0135, .0148, .0023)
p.adjust(p, method = "holm")
### post-hoc analysis using plots
ggplot(data = g5, mapping = aes(x = wm_1, y = wm_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "WM Factor Score T1", y = "WM Factor Score T2") +
  geom_vline(xintercept = c(-1.09, -0.24, 0.25, 0.52, 1.3), col = "black", lty=3) +
  theme_apa()



## WM T3 vs T1
### Not Significant
model1 <- lm(wm_3 ~ wm_1 + Group, data = g5)
model2 <- lm(wm_3 ~ wm_1 * Group, data = g5)
simpleModel <- lm(EF_2 ~ Group, data = g5)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(g5$wm_3 ~ g5$Group, center = mean) ## Not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(wm_3 ~ wm_1 + Group, data = g5), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not significant
Anova(aov(wm_3 ~ wm_1 * Group, data = g5), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(g5$wm_3, g5$Group)

### Robust
ancova(g5$wm_3 ~ g5$wm_1 + g5$Group) ### no significant differences
ggplot(data = g5, mapping = aes(x = wm_1, y = wm_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "WM Factor Score T1", y = "WM Factor Score T3") +
  geom_vline(xintercept = c(-1.1, -0.24, 0.25, 0.52, 1.3), col = "orange", lty=3) +
  theme_apa()






#66. Year 5 graphs ----------------------------------------------

predictedLong5 <- reshape(g5, varying=c(12:17), direction="long", idvar="ID", sep="_")

### Boxplot EF
ggplot(predictedLong5, aes(Group, EF, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2","3"), labels = c("Time 1","Time 2","Time 3")) +
  scale_y_continuous(name = "Combined EF Factor Score", breaks=seq(-3,3,1/2)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))


### Boxplot WM
ggplot(predictedLong5, aes(Group, wm, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2","3"), labels = c("Time 1","Time 2","Time 3")) +
  scale_y_continuous(name = "WM Factor Score", breaks=seq(-3,3,1/2)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))
#theme_apa()







#67. near-transfer ANCOVAs (Grade 3)----------------------------------------------
### EF T2 vs T1
model1 <- lm(EF_2 ~ EF_1 + Group, data = g3)
model2 <- lm(EF_2 ~ EF_1 * Group, data = g3)
simpleModel <- lm(EF_2 ~ Group, data = g3)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### interaction present
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(g3$EF_2 ~ g3$Group, center = mean)

# getting the sums squared for each effect using the Anova function from the car package
# https://stats.stackexchange.com/questions/183026/r%C2%B2-of-ancova-mostly-driven-by-covariate
sstable <- car::Anova(lm(EF_2 ~ EF_1 + Group, data = g3), type = 3)
# partial eta squared: https://www-sciencedirect-com.ezproxy1.library.usyd.edu.au/science/article/pii/S1747938X11000029
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)
sjstats::anova_stats(model1)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
Anova(aov(EF_2 ~ EF_1 * Group, data = g3), type = "III") ## interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(g3$EF_2, g3$Group)

model1 <-lm(wm_2 ~ wm_1 * Grade, data = predicted)
adjustedMeans <- effect("wm_1:Grade", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(wm_1:Grade = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))

### Robust
### remember that the p-value does not take into account multiple testing
### so need to multiply each value by 5 (number of tests taken)
ancova(g3$EF_2 ~ g3$EF_1 + g3$Group) ### no significant differences
ggplot(data = g3, mapping = aes(x = EF_1, y = EF_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "EF Factor Score T1", y = "EF Factor Score T2") +
  geom_vline(xintercept = c(-1.1, -0.15, 0.31, 0.75, 1.5), col = "orange", lty=3) +
  theme_apa()


### EF T3 vs T1
### Not Significant
model1 <- lm(EF_3~ EF_1 + Group, data = g3)
model2 <- lm(EF_3 ~ EF_1 * Group, data = g3)
simpleModel <- lm(EF_3 ~ Group, data = g3)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test for homogeneity of variance
leveneTest(g3$EF_3 ~ g3$Group, center = mean) ## not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(EF_3~ EF_1 + Group, data = g3), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## not significant
Anova(aov(EF_3 ~ EF_1 * Group, data = g3), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(g3$EF_3, g3$Group)

### Robust
ancova(g3$EF_3 ~ g3$EF_1 + g3$Group) ### no significant differences
ggplot(data = g3, mapping = aes(x = EF_1, y = EF_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "EF Factor Score T1", y = "EF Factor Score T3") +
  geom_vline(xintercept = c(-1.1, -0.15, 0.31, 0.75, 1.5), col = "orange", lty=3) +
  theme_apa()



## WM T2 vs T1
### This shows a difference between groups for scores on T2 when accounting for T1 scores (CTRL > EXP)
model1 <- lm(wm_2 ~ wm_1 + Group, data = g3)
model2 <- lm(wm_2 ~ wm_1 * Group, data = g3)
simpleModel <- lm(EF_2 ~ Group, data = g3)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test for homogeneity of variance
leveneTest(g3$wm_2 ~ g3$Group, center = mean) ## significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(wm_2 ~ wm_1 + Group, data = g3), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## * significant: different between groups for scores on T2 when accounting for T1 scores
Anova(aov(wm_2 ~ wm_1 * Group, data = g3), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(g3$wm_2, g3$Group)

### Robust
ancova(g3$wm_2 ~ g3$wm_1 + g3$Group ) ### sig
p <- c(.0972, .0006, .0135, .0148, .0023)
p.adjust(p, method = "holm")
### post-hoc analysis using plots
ggplot(data = g3, mapping = aes(x = wm_1, y = wm_2, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "WM Factor Score T1", y = "WM Factor Score T2") +
  geom_vline(xintercept = c(-1.09, -0.24, 0.25, 0.52, 1.3), col = "black", lty=3) +
  theme_apa()



## WM T3 vs T1
### Not Significant
model1 <- lm(wm_3 ~ wm_1 + Group, data = g3)
model2 <- lm(wm_3 ~ wm_1 * Group, data = g3)
simpleModel <- lm(EF_2 ~ Group, data = g3)
summ(model1)
summ(model2)
summ(simpleModel)
### test assumption of homogeneity of slopes 
anova(model1, model2) ### no problems here
### test assumption of normal distribution of residuals
par(mfrow=c(1,1))
hist(residuals(model1), col="darkgray")
plot(fitted(model1), residuals(model1))
### test assumption of homogeneity of variance
leveneTest(g3$wm_3 ~ g3$Group, center = mean) ## Not significant
# getting the sums squared for each effect using the Anova function from the car package
sstable <- car::Anova(lm(wm_3 ~ wm_1 + Group, data = g3), type = 3)
# partial eta squared:
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)
sstable
### alternative method (using the sjstats package)
sjstats::eta_sq(model1, partial = TRUE, ci.lvl = .9)

par(mfrow=c(2,2))
for(i in 1:4)plot(model1, which=i)
summ(model1) ## Not significant
Anova(aov(wm_3 ~ wm_1 * Group, data = g3), type = "III") ## no interaction present
adjustedMeans <- effect("Group", model1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se
summary(glht(model1, linfct = mcp(Group = "Tukey")))
confint(glht(model1, linfct = mcp(Group = "Tukey")))
describeBy(g3$wm_3, g3$Group)

### Robust
ancova(g3$wm_3 ~ g3$wm_1 + g3$Group) ### no significant differences
ggplot(data = g3, mapping = aes(x = wm_1, y = wm_3, color = Group)) +
  geom_jitter() +
  geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(method = "loess", se=FALSE, linetype = "dashed", span = 0.75) +
  labs (x = "WM Factor Score T1", y = "WM Factor Score T3") +
  geom_vline(xintercept = c(-1.1, -0.24, 0.25, 0.52, 1.3), col = "orange", lty=3) +
  theme_apa()






#68. Year 3 graphs----------------------------------------------

predictedLong3 <- reshape(g3, varying=c(12:17), direction="long", idvar="ID", sep="_")

### Boxplot EF
ggplot(predictedLong3, aes(Group, EF, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2","3"), labels = c("Time 1","Time 2","Time 3")) +
  scale_y_continuous(name = "Combined EF Factor Score", breaks=seq(-3,3,1/2)) + 
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))

dabest(predicted, Group, wm_2, idx = c("Exp.","Ctrl"), paired = FALSE)

### Boxplot WM
ggplot(predictedLong3, aes(Group, wm, fill=factor(time))) +
  geom_boxplot() + 
  scale_fill_discrete(name = element_blank(), breaks = c("1","2","3"), labels = c("Time 1","Time 2","Time 3")) +
  scale_y_continuous(name = "WM Factor Score", breaks=seq(-3,3,1/2)) +
  scale_x_discrete(name = element_blank(), labels =c("Control Group", "Experimental Group")) +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(legend.position = c(0.5,0.9), legend.direction = "horizontal", panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10), legend.text = element_text(size = 10))
#theme_apa()


# 69. Line Plots----------------------------------------------
### gF

x1<-describeBy(g3$NAI_1, g3$Group)
x2<-describeBy(g3$NAI_2, g3$Group)
x3<-describeBy(g3$NAI_3, g3$Group)
xNAIg3 <- data.frame("Group" = c("Control","Experimental"), "Grade" = 3, "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "gF", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
x1<-describeBy(g5$NAI_1, g5$Group)
x2<-describeBy(g5$NAI_2, g5$Group)
x3<-describeBy(g5$NAI_3, g5$Group)
xNAIg5 <- data.frame("Group" = c("Control","Experimental"), "Grade" = 5, "Time" = c(1, 1, 2, 2, 3, 3), "Test" = "gF", "Mean" = c(x1$Ctrl$mean,x1$Exp.$mean,x2$Ctrl$mean,x2$Exp.$mean,x3$Ctrl$mean,x3$Exp.$mean), "SE" = c(x1$Ctrl$se, x1$Exp.$se, x2$Ctrl$se, x2$Exp.$se, x3$Ctrl$se, x3$Exp.$se))
xNAI <- rbind(xNAIg3, xNAIg5)
xNAI$Grade <- as.factor(xNAI$Grade)
xNAI$Time <- as.factor(xNAI$Time)

### NAI line plot
ggplot(xNAI, aes(x=Time, y=Mean, shape=Group, color = Grade)) + 
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.1)) +
  scale_y_continuous(name = "NAI", limits = c(100,130), breaks=seq(100,130,10)) +
  scale_x_discrete(breaks=seq(1,3,1)) 

### WM line plot
ggplot(xWM, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "WM Factor Score", limits = c(-2.5,2.5), breaks=seq(-2.5,2.5,0.5)) +
  scale_x_continuous(breaks=seq(1,3,1)) 

### PAT Maths line plot
ggplot(xPATm, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "PAT Maths", limits = c(110,160), breaks=seq(110,160,10)) +
  scale_x_continuous(breaks=seq(1,3,1)) 

### PAT Reading line plot
ggplot(xPATr, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "PAT Reading", limits = c(110,160), breaks=seq(110,160,10)) +
  scale_x_continuous(breaks=seq(1,3,1)) 

### gF line plot
ggplot(xPATnai, aes(x=Time, y=Mean, group=Group, color=Group)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Fluid Intelligence", limits = c(90,140), breaks=seq(90,140,10)) +
  scale_x_continuous(breaks=seq(1,3,1)) 



 

#...----------------------------------------------
#...----------------------------------------------
#*** COMPLEX SPAN DATA ANAYLYSIS ***------------

# 70. Removal of entire cases of data for Complex Span Tasks -------------------------------------------
### As FIML (which occurs as part of calculation for factors scores) can not usually be done when the data is NOT missing at random, then this does affect the validity of the WM scores. However, it does remove a significant number of the subjects if items are removed completely.
df1alt <- df1[!(is.na(df1$X1OSPANpartialscore) | is.na(df1$X2OSPNpartialscore) | is.na(df1$X3OSPNpartialscore)), ]
df1alt <- df1alt[!(is.na(df1$X1RSPANpartialscore) | is.na(df1$X2RSPANpartialscore) | is.na(df1$X3RSPANpartialscore)), ]
df1alt <- df1alt[!(is.na(df1$X1SYMSPANpartialscore) | is.na(df1$X2SYMSPANpartialscore) | is.na(df1$X3SYMSPANpartialscore)), ]
df1alt <-df1alt[c(1:41),]

count(df1alt, df1alt$Group == "Exp.") ### now have exp n = 14 and control n = 27


# 71. Dealing with the removed data - are the removed data participants different?  -------------------------------------------
df1CST = read.csv("https://raw.githubusercontent.com/dlhegarty/PhD_Data/master/df1CST.csv")
df1CST$CSTprob = as.factor(df1CST$CSTprob)

### General info
### Do groups differ on age? YES
t.test(Age.Yr ~ CSTprob, data=df1CST)
describeBy(df1CST$Age.Yr, df1CST$CSTprob)
mes(10.5, 9.69, 0.85, 1.09, 44, 61) ### Cohen's d 


### Individual T1 tests
### Far-transfer measures
t.test(X1NAI ~ CSTprob, data=df1CST)
describeBy(df1CST$X1NAI, df1CST$CSTprob)
mes(117.14, 110.39, 12.49, 12.23, 44, 61) ### Cohen's d = 0.55 [0.15, 0.95]




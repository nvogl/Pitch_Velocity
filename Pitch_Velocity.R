#Setting up the data file
setwd("C:/Users/Nick/Documents/MLB/Pitch_Velocity")
PitchStats <- read.csv("2020_Pitcher_Stats.csv")
#Remove missing data
PitchStats <- na.omit(PitchStats)
#Add extra variables to data set
PitchStats$fastball_breaking_diff <- PitchStats$fastball_avg_speed - PitchStats$breaking_avg_speed
PitchStats$fastball_offspeed_diff <- PitchStats$fastball_avg_speed - PitchStats$offspeed_avg_speed
PitchStats$offspeed_breaking_diff <- PitchStats$offspeed_avg_speed - PitchStats$breaking_avg_speed
Year = "2020"
 
  

#Scatter Plots
makeplot <- function(i, j){
  #Determine Response Variable
  if (i == 1) {
    ResponseName <- "ERA"
    ResponseVar <- PitchStats$p_era
  } else 
  if (i == 2) { 
    ResponseName <- "OppBA"
    ResponseVar <- PitchStats$batting_avg
  } else
  if (i == 3) {
    ResponseName <- "xOppBA"
    ResponseVar <- PitchStats$xba
  } else
  if (i == 4) {
    ResponseName <- "%K"
    ResponseVar <- PitchStats$p_k_percent
  } else
  if (i == 5){
    ResponseName <- "IP"
    ResponseVar <- PitchStats$p_formatted_ip
  }
  #Determine Predictor Variable
  if (j == 1) {
    PredictorName <- "Avg Velo: Fastball"
    PredictorVar <- PitchStats$fastball_avg_speed
  } else
  if (j == 2) {
    PredictorName <- "Range of Velo: Fastball"
    PredictorVar <- PitchStats$fastball_range_speed
  } else
  if (j == 3) {
    PredictorName <- "Avg Velo: Breaking Ball"
    PredictorVar <- PitchStats$breaking_avg_speed
  } else
  if (j == 4) {
    PredictorName <- "Range of Velo: Breaking Ball"
    PredictorVar <- PitchStats$breaking_range_speed
  } else
  if (j == 5) {
    PredictorName <- "Avg Velo: Off Speed"
    PredictorVar <- PitchStats$offspeed_avg_speed
  } else
  if (j == 6) {
    PredictorName <- "Range of Velo: Off Speed"
    PredictorVar <- PitchStats$offspeed_range_speed
  } else
  if (j == 7) {
    PredictorName <- "Avg Velo Diff: FB and BB"
    PredictorVar <- PitchStats$fastball_breaking_diff
  } else
  if (j == 8) {
    PredictorName <- "Avg Velo Diff: FB and OS"
    PredictorVar <- PitchStats$fastball_offspeed_diff
  } else
  if (j == 9) {
    PredictorName <- "Avg Velo Diff: OS and BB"
    PredictorVar <- PitchStats$offspeed_breaking_diff
  }
  #Produce Plot
  plot(PredictorVar, ResponseVar, xlab = PredictorName, ylab = ResponseName, 
     main = paste(paste(ResponseName, PredictorName, sep = " vs "), Year, sep = " "))
  abline(lsfit(PredictorVar,ResponseVar), col = "blue")
  abline(h = mean(ResponseVar), col = "red")
}
#Produce all plots at once
makeallplots <- function(numResponse, numPredictor){
  par(mfrow = c(3, 3))
  i = 1
  j = 1
  while (i <= numResponse) {
    while (j <= numPredictor){
      makeplot(i, j)
      j = j + 1
    }
    j = 1
    i = i + 1
  }
}
#Call Plot Functions
makeplot(1, 1)
makeallplots(1,9)



#Principal Components Analysis

#Adding Libraries
#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
#Narrow down data to just predictor variables, and remove rows with unknown data (2)
#PitchPredictors <- PitchStats[, c(1:2, 40, 45, 47, 52, 54, 59:62)]
PitchPredictors <- PitchStats[, c("last_name", "first_name", "fastball_avg_speed", "fastball_range_speed", 
          "offspeed_avg_speed", "offspeed_range_speed", "breaking_avg_speed", "breaking_range_speed",
          "fastball_breaking_diff", "fastball_offspeed_diff", "offspeed_breaking_diff")]
#Correlation Matrix
CorMat <- round(cor(PitchPredictors[, c(3:11)]),2)
print(CorMat)
#Run PCA
PitchPCA <- PCA(PitchPredictors[, c(3:11)])
#Eigenvalues and % of variation explained
eigenvalues <- round(PitchPCA$eig,3)
print(eigenvalues[, 1:2])
#Scree Plots
fviz_screeplot(PitchPCA)



#Predictive Models


#Standardize variables
PitchStats$fastball_avg_speed_std <- scale(PitchStats$fastball_avg_speed)
PitchStats$fastball_range_speed_std <- scale(PitchStats$fastball_range_speed)
PitchStats$breaking_avg_speed_std <- scale(PitchStats$breaking_avg_speed)
PitchStats$breaking_range_speed_std <- scale(PitchStats$breaking_range_speed)
PitchStats$offspeed_avg_speed_std <- scale(PitchStats$offspeed_avg_speed)
PitchStats$offspeed_range_speed_std <- scale(PitchStats$offspeed_range_speed)
PitchStats$fastball_breaking_diff_std <- scale(PitchStats$fastball_breaking_diff)
PitchStats$fastball_offspeed_diff_std <- scale(PitchStats$fastball_offspeed_diff)
PitchStats$offspeed_breaking_diff_std <- scale(PitchStats$offspeed_breaking_diff)


#Create Indicator Variables
k = 1
while (k <= nrow(PitchStats)) {
  if(PitchStats$fastball_avg_speed[k] > 94){
    PitchStats$fastball94plus[k] <- 1
  } else {
    PitchStats$fastball94plus[k] <-0
  }
  if(PitchStats$fastball_avg_speed[k] > 96){
    PitchStats$fastball96plus[k] <- 1
  } else {
    PitchStats$fastball96plus[k] <- 0
  }
  if(PitchStats$fastball_avg_speed[k] > 94 & PitchStats$fastball_avg_speed[k] <= 96){
    PitchStats$fastball94to96[k] <- 1
  } else {
    PitchStats$fastball94to96[k] <- 0
  }
  if(PitchStats$fastball_range_speed[k] > 3){
   PitchStats$fastball_range3.0plus[k] <- 1
  } else {
    PitchStats$fastball_range3.0plus[k] <- 0
  }
  if(PitchStats$fastball_range_speed[k] > 3.5){
    PitchStats$fastball_range3.5plus[k] <- 1
  } else {
    PitchStats$fastball_range3.5plus[k] <- 0
  }
  if(PitchStats$breaking_avg_speed[k] < 75){
    PitchStats$breaking75minus[k] <- 1
  } else {
    PitchStats$breaking75minus[k] <- 0
  }
  if(PitchStats$offspeed_avg_speed[k] > 90){
    PitchStats$offspeed90plus[k] <- 1
  } else {
    PitchStats$offspeed90plus[k] <- 0
  }
  k = k + 1
}


#ERA Models

#ERAModel1 - GLM without indicator variables
ERAModel1.1 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_avg_speed_std + 
                     breaking_range_speed_std + offspeed_avg_speed_std + offspeed_range_speed_std + 
                     fastball_breaking_diff_std + fastball_offspeed_diff_std + offspeed_breaking_diff_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.1)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.1)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.1")
par(mfrow = c(2, 2))
plot(ERAModel1.1)
#Drop three difference variables
ERAModel1.2 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_avg_speed_std + 
                     breaking_range_speed_std + offspeed_avg_speed_std + offspeed_range_speed_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.2)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.2)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.2")
par(mfrow = c(2, 2))
plot(ERAModel1.2)
#Drop breaking avg speed
ERAModel1.3 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_range_speed_std + 
                     offspeed_avg_speed_std + offspeed_range_speed_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.3)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.3)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.3")
par(mfrow = c(2, 2))
plot(ERAModel1.3)
#Drop off speed avg speed
ERAModel1.4 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_range_speed_std + 
                     offspeed_range_speed_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.4)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.4)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.4")
par(mfrow = c(2, 2))
plot(ERAModel1.4)
#Drop breaking range speed
ERAModel1.5 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + offspeed_range_speed_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.5)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.5)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.5")
par(mfrow = c(2, 2))
plot(ERAModel1.5)
#Drop off speed range speed
ERAModel1.6 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.6)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.6)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.6")
par(mfrow = c(2, 2))
plot(ERAModel1.6)
#fastball avg speed only
ERAModel1.7 <- glm(p_era ~ fastball_avg_speed_std, family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.7)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.7)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.7")
par(mfrow = c(2, 2))
plot(ERAModel1.7)
#go back to 1.2 and drop range of offspeed
ERAModel1.8 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_avg_speed_std + 
                     breaking_range_speed_std + offspeed_avg_speed_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.8)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.8)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.8")
par(mfrow = c(2, 2))
plot(ERAModel1.8)
#remove breaking avg speed
ERAModel1.9 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_range_speed_std + 
                     offspeed_avg_speed_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.9)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.9)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.9")
par(mfrow = c(2, 2))
plot(ERAModel1.9)
#remove offspeed avg speed
ERAModel1.10 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_range_speed_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.10)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.10)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.10")
par(mfrow = c(2, 2))
plot(ERAModel1.10)
#remove breaking range speed
ERAModel1.11 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std,
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel1.11)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel1.11)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 1.11")
par(mfrow = c(2, 2))
plot(ERAModel1.11)


#ERAModel2 - GLM with introducing 94+ indicator variable
ERAModel2.1 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_avg_speed_std + 
                     breaking_range_speed_std + offspeed_avg_speed_std + offspeed_range_speed_std + 
                     fastball94plus + fastball94plus*(fastball_avg_speed_std + fastball_range_speed_std + 
                     breaking_avg_speed_std + breaking_range_speed_std + offspeed_avg_speed_std + 
                     offspeed_range_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.1)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.1)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.1")
par(mfrow = c(2, 2))
plot(ERAModel2.1)
#Remove offspeed range both slope and cross effect
ERAModel2.2 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_avg_speed_std + 
                     breaking_range_speed_std + offspeed_avg_speed_std + 
                     fastball94plus + fastball94plus*(fastball_avg_speed_std + fastball_range_speed_std + 
                     breaking_avg_speed_std + breaking_range_speed_std + offspeed_avg_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.2)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.2)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.2")
par(mfrow = c(2, 2))
plot(ERAModel2.2)
#Remove offspeed avg speed cross effect
ERAModel2.3 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_avg_speed_std + 
                     breaking_range_speed_std + offspeed_avg_speed_std + 
                     fastball94plus + fastball94plus*(fastball_avg_speed_std + fastball_range_speed_std + 
                     breaking_avg_speed_std + breaking_range_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.3)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.3)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.3")
par(mfrow = c(2, 2))
plot(ERAModel2.3)
#remove breaking avg speed cross effect
ERAModel2.4 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_avg_speed_std + 
                     breaking_range_speed_std + offspeed_avg_speed_std + 
                     fastball94plus + fastball94plus*(fastball_avg_speed_std + fastball_range_speed_std + 
                     breaking_range_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.4)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.4)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.4")
par(mfrow = c(2, 2))
plot(ERAModel2.4)
#remove breaking avg speed
ERAModel2.5 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_range_speed_std + 
                     offspeed_avg_speed_std + 
                     fastball94plus + fastball94plus*(fastball_avg_speed_std + fastball_range_speed_std + 
                     breaking_range_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.5)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.5)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.5")
par(mfrow = c(2, 2))
plot(ERAModel2.5)
#remove fastball range speed cross effect
ERAModel2.6 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_range_speed_std + 
                     offspeed_avg_speed_std + 
                     fastball94plus + fastball94plus*(fastball_avg_speed_std + breaking_range_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.6)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.6)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.6")
par(mfrow = c(2, 2))
plot(ERAModel2.6)
#remove breaking range cross effect
ERAModel2.7 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_range_speed_std + 
                     offspeed_avg_speed_std + 
                     fastball94plus + fastball94plus*(fastball_avg_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.7)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.7)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.7")
par(mfrow = c(2, 2))
plot(ERAModel2.7)
#remove breaking range
ERAModel2.8 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + offspeed_avg_speed_std + 
                     fastball94plus + fastball94plus*(fastball_avg_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.8)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.8)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.8")
par(mfrow = c(2, 2))
plot(ERAModel2.8)
#remove offspeed avg speed
ERAModel2.9 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + 
                     fastball94plus + fastball94plus*(fastball_avg_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.9)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.9)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.9")
par(mfrow = c(2, 2))
plot(ERAModel2.9)
#remove fastball cross effect
ERAModel2.10 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + fastball94plus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.10)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.10)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.10")
par(mfrow = c(2, 2))
plot(ERAModel2.10)
#go back to 2.2 and remove fastball avg speed fixed and cross effect
ERAModel2.11 <- glm(p_era ~ fastball_range_speed_std + breaking_avg_speed_std + breaking_range_speed_std + 
                     offspeed_avg_speed_std + 
                     fastball94plus + fastball94plus*(fastball_range_speed_std + breaking_avg_speed_std + 
                     breaking_range_speed_std + offspeed_avg_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.11)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.11)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.11")
par(mfrow = c(2, 2))
plot(ERAModel2.11)
#Remove offspeed avg speed cross effect
ERAModel2.12 <- glm(p_era ~ fastball_range_speed_std + breaking_avg_speed_std + breaking_range_speed_std + 
                      offspeed_avg_speed_std + 
                      fastball94plus + fastball94plus*(fastball_range_speed_std + breaking_avg_speed_std + 
                      breaking_range_speed_std),
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.12)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.12)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.12")
par(mfrow = c(2, 2))
plot(ERAModel2.12)
#Remove fastball range cross effect
ERAModel2.13 <- glm(p_era ~ fastball_range_speed_std + breaking_avg_speed_std + breaking_range_speed_std + 
                      offspeed_avg_speed_std + 
                      fastball94plus + fastball94plus*(breaking_avg_speed_std + breaking_range_speed_std),
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.13)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.13)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.13")
par(mfrow = c(2, 2))
plot(ERAModel2.13)
#Remove breaking range cross effect
ERAModel2.14 <- glm(p_era ~ fastball_range_speed_std + breaking_avg_speed_std + breaking_range_speed_std + 
                      offspeed_avg_speed_std + 
                      fastball94plus + fastball94plus*breaking_avg_speed_std,
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.14)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.14)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.14")
par(mfrow = c(2, 2))
plot(ERAModel2.14)
#Remove last cross effect
ERAModel2.15 <- glm(p_era ~ fastball_range_speed_std + breaking_avg_speed_std + breaking_range_speed_std + 
                      offspeed_avg_speed_std + fastball94plus,
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.15)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.15)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.15")
par(mfrow = c(2, 2))
plot(ERAModel2.15)
#remove breaking avg speed
ERAModel2.16 <- glm(p_era ~ fastball_range_speed_std + breaking_range_speed_std + offspeed_avg_speed_std + 
                      fastball94plus,
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.16)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.16)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.16")
par(mfrow = c(2, 2))
plot(ERAModel2.16)
#Remove offspeed avg speed
ERAModel2.17 <- glm(p_era ~ fastball_range_speed_std + breaking_range_speed_std + fastball94plus,
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.17)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.17)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.17")
par(mfrow = c(2, 2))
plot(ERAModel2.17)
#Remove breaking range
ERAModel2.18 <- glm(p_era ~ fastball_range_speed_std + fastball94plus,
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel2.18)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel2.18)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 2.18")
par(mfrow = c(2, 2))
plot(ERAModel2.18)


#ERAModel3 - Check 94+ indicator with 96+ and potentially 94 to 96
ERAModel3.1 <- glm(p_era ~ fastball_range_speed_std + fastball96plus,
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel3.1)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel3.1)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 3.1")
par(mfrow = c(2, 2))
plot(ERAModel3.1)
#add 94 to 96 intercept
ERAModel3.2 <- glm(p_era ~ fastball_range_speed_std + fastball94to96 + fastball96plus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel3.2)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel3.2)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 3.2")
par(mfrow = c(2, 2))
plot(ERAModel3.2)

#ERAModel4 - Check fastball range 3.0 plus indicator
#Start with 1.8 then add indicator and cross terms
ERAModel4.1 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_avg_speed_std +  
                     breaking_range_speed_std + offspeed_avg_speed_std +
                     fastball_range3.0plus + fastball_range3.0plus*(fastball_avg_speed_std + 
                     fastball_range_speed_std + breaking_avg_speed_std + breaking_range_speed_std + 
                     offspeed_avg_speed_std),
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel4.1)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel4.1)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 4.1")
par(mfrow = c(2, 2))
plot(ERAModel4.1)
#remove all cross effects except fastball range
ERAModel4.2 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + breaking_avg_speed_std +  
                     breaking_range_speed_std + offspeed_avg_speed_std +
                     fastball_range3.0plus + fastball_range3.0plus*fastball_range_speed_std,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel4.2)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel4.2)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 4.2")
par(mfrow = c(2, 2))
plot(ERAModel4.2)
#remove fastball range fix and cross
ERAModel4.3 <- glm(p_era ~ fastball_avg_speed_std + breaking_avg_speed_std + breaking_range_speed_std + 
                     offspeed_avg_speed_std + fastball_range3.0plus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel4.3)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel4.3)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 4.3")
par(mfrow = c(2, 2))
plot(ERAModel4.3)
#remove breaking avg speed
ERAModel4.4 <- glm(p_era ~ fastball_avg_speed_std + breaking_range_speed_std + offspeed_avg_speed_std + 
                     fastball_range3.0plus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel4.4)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel4.4)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 4.4")
par(mfrow = c(2, 2))
plot(ERAModel4.4)
#remove offspeed avg speed
ERAModel4.5 <- glm(p_era ~ fastball_avg_speed_std + breaking_range_speed_std + fastball_range3.0plus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel4.5)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel4.5)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 4.5")
par(mfrow = c(2, 2))
plot(ERAModel4.5)
#remove breaking range speed
ERAModel4.6 <- glm(p_era ~ fastball_avg_speed_std + fastball_range3.0plus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel4.6)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel4.6)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 4.6")
par(mfrow = c(2, 2))
plot(ERAModel4.6)
#switch fastball to intercepts
ERAModel4.7 <- glm(p_era ~ fastball_range3.0plus + fastball94to96 + fastball96plus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel4.7)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel4.7)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 4.7")
par(mfrow = c(2, 2))
plot(ERAModel4.7)
#Compare to 3.5 plus
ERAModel4.8 <- glm(p_era ~ fastball_range3.5plus + fastball94to96 + fastball96plus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel4.8)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel4.8)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 4.8")
par(mfrow = c(2, 2))
plot(ERAModel4.8)

#ERAModel5 - All intercept model
ERAModel5.1 <- glm(p_era ~ fastball_range3.5plus + fastball94to96 + fastball96plus + breaking75minus + 
                     offspeed90plus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel5.1)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel5.1)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 5.1")
par(mfrow = c(2, 2))
plot(ERAModel5.1)
#remove offspeed
ERAModel5.2 <- glm(p_era ~ fastball_range3.5plus + fastball94to96 + fastball96plus + breaking75minus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel5.2)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel5.2)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 5.2")
par(mfrow = c(2, 2))
plot(ERAModel5.2)
#remove 94to96
ERAModel5.3 <- glm(p_era ~ fastball_range3.5plus + fastball96plus + breaking75minus,
                   family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel5.3)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel5.3)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 5.3")
par(mfrow = c(2, 2))
plot(ERAModel5.3)

#ERAModel6 - Reintroduce difference effects
#start with 1.11 and add two fastball diff effects
ERAModel6.1 <- glm(p_era ~ fastball_avg_speed_std + fastball_range_speed_std + fastball_breaking_diff + fastball_offspeed_diff,
                    family = gaussian(link = "log"), data = PitchStats)
summary(ERAModel6.1)
par(mfrow = c(1, 1))
plot(exp(predict(ERAModel6.1)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = "ERA Model 6.1")
par(mfrow = c(2, 2))
plot(ERAModel6.1)




#Compare to Test
#Set up test file
TestStats <- read.csv("2019_Pitcher_Stats.csv")
TestStats <- na.omit(TestStats)
TestStats$fastball_breaking_diff <- TestStats$fastball_avg_speed - TestStats$breaking_avg_speed
TestStats$fastball_offspeed_diff <- TestStats$fastball_avg_speed - TestStats$offspeed_avg_speed
TestStats$offspeed_breaking_diff <- TestStats$offspeed_avg_speed - TestStats$breaking_avg_speed
TestYear <- "2019"
TestStats$fastball_avg_speed_std <- scale(TestStats$fastball_avg_speed)
TestStats$fastball_range_speed_std <- scale(TestStats$fastball_range_speed)
TestStats$breaking_avg_speed_std <- scale(TestStats$breaking_avg_speed)
TestStats$breaking_range_speed_std <- scale(TestStats$breaking_range_speed)
TestStats$offspeed_avg_speed_std <- scale(TestStats$offspeed_avg_speed)
TestStats$offspeed_range_speed_std <- scale(TestStats$offspeed_range_speed)
TestStats$fastball_breaking_diff_std <- scale(TestStats$fastball_breaking_diff)
TestStats$fastball_offspeed_diff_std <- scale(TestStats$fastball_offspeed_diff)
TestStats$offspeed_breaking_diff_std <- scale(TestStats$offspeed_breaking_diff)
k = 1
while (k <= nrow(TestStats)) {
  if(TestStats$fastball_avg_speed[k] > 94){
    TestStats$fastball94plus[k] <- 1
  } else {
    TestStats$fastball94plus[k] <-0
  }
  if(TestStats$fastball_avg_speed[k] > 96){
    TestStats$fastball96plus[k] <- 1
  } else {
    TestStats$fastball96plus[k] <- 0
  }
  if(TestStats$fastball_avg_speed[k] > 94 & TestStats$fastball_avg_speed[k] <= 96){
    TestStats$fastball94to96[k] <- 1
  } else {
    TestStats$fastball94to96[k] <- 0
  }
  if(TestStats$fastball_range_speed[k] > 3){
    TestStats$fastball_range3.0plus[k] <- 1
  } else {
    TestStats$fastball_range3.0plus[k] <- 0
  }
  if(TestStats$fastball_range_speed[k] > 3.5){
    TestStats$fastball_range3.5plus[k] <- 1
  } else {
    TestStats$fastball_range3.5plus[k] <- 0
  }
  if(TestStats$breaking_avg_speed[k] < 75){
    TestStats$breaking75minus[k] <- 1
  } else {
    TestStats$breaking75minus[k] <- 0
  }
  if(TestStats$offspeed_avg_speed[k] > 90){
    TestStats$offspeed90plus[k] <- 1
  } else {
    TestStats$offspeed90plus[k] <- 0
  }
  k = k + 1
}
#Create test comparisons

par(mfrow = c(1,2))
#install.packages("MLmetrics")
library("MLmetrics")
plot(exp(predict(ERAModel1.6)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = paste("ERA Model 1.6 ", Year))
plot(exp(predict(ERAModel1.6, TestStats)), TestStats$p_era, xlab = "Predicted", ylab = "Actual", main = paste("ERA Model 1.6 ", TestYear))
MSE(exp(predict(ERAModel1.6)), PitchStats$p_era)
MSE(exp(predict(ERAModel1.6, TestStats)), TestStats$p_era)
plot(exp(predict(ERAModel2.18)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = paste("ERA Model 2.18 ", Year))
plot(exp(predict(ERAModel2.18, TestStats)), TestStats$p_era, xlab = "Predicted", ylab = "Actual", main = paste("ERA Model 2.18 ", TestYear))
MSE(exp(predict(ERAModel2.18)), PitchStats$p_era)
MSE(exp(predict(ERAModel2.18, TestStats)), TestStats$p_era)
plot(exp(predict(ERAModel3.1)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = paste("ERA Model 3.1 ", Year))
plot(exp(predict(ERAModel3.1, TestStats)), TestStats$p_era, xlab = "Predicted", ylab = "Actual", main = paste("ERA Model 3.1 ", TestYear))
MSE(exp(predict(ERAModel3.1)), PitchStats$p_era)
MSE(exp(predict(ERAModel3.1, TestStats)), TestStats$p_era)
plot(exp(predict(ERAModel3.2)), PitchStats$p_era, xlab = "Predicted", ylab = "Actual", main = paste("ERA Model 3.2 ", Year))
plot(exp(predict(ERAModel3.2, TestStats)), TestStats$p_era, xlab = "Predicted", ylab = "Actual", main = paste("ERA Model 3.2 ", TestYear))
MSE(exp(predict(ERAModel3.2)), PitchStats$p_era)
MSE(exp(predict(ERAModel3.2, TestStats)), TestStats$p_era)

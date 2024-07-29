library(tidyverse)
library(DescTools) # for getting mode
data <- readRDS("data_factors.rds")
fit <- glm(Diabetes_binaryF ~ HvyAlcoholConsumpF + PhysHlthF + MentHlthF + SmokerF + HighCholF + BMI, data = data, family = "binomial")
####################################### adding API stuff below ###################################

# #*Analyzing Predictors
AlcMode <- as.character(Mode(data[["HvyAlcoholConsumpF"]]))[[1]]
PhysMode <- as.character(Mode(data[["PhysHlthF"]]))[[1]]
MentMode <- as.character(Mode(data[["MentHlthF"]]))[[1]]
SmokMode <- as.character(Mode(data[["SmokerF"]]))[[1]]
HighCMode <- as.character(Mode(data[["HighCholF"]]))[[1]]
BMIMean <- mean(data[["BMI"]])


#* @serializer json
#* @param HvyAlcoholConsumpF 
#* @param PhysHlthF
#* @param MentHlthF
#* @param SmokerF
#* @param HighCholF
#* @param BMI
#* @get /pred
function(HvyAlcoholConsumpF = AlcMode, PhysHlthF = PhysMode, MentHlthF = MentMode, 
         SmokerF = SmokMode, HighCholF = HighCMode, BMI = BMIMean){
BMI <- as.numeric(BMI)
pred_obs <- data.frame(HvyAlcoholConsumpF = HvyAlcoholConsumpF,
                      PhysHlthF = PhysHlthF,
                      MentHlthF = MentHlthF,
                      SmokerF = SmokerF,
                      HighCholF = HighCholF,
                      BMI = BMI)  
prediction <- predict(fit, newdata = pred_obs)
value <- exp(prediction)
if (value > 0.5){
return("Predicted to have diabetes")
}
else if (value <= 0.5){
  return("Predicted to NOT have diabetes")
  
}
}

#http://localhost:PORT/pred?HvyAlcoholConsumpF=Is%20a%20heavy%20drinker&PhysHlthF=0%20Days&MentHlthF=0%20Days&SmokerF=Has%20smoked%20at%20least%20100%20cigarettes&HighCholF=No%20High%20Cholesterol&BMI=60
#http://localhost:PORT/pred?HvyAlcoholConsumpF=Is%20a%20heavy%20drinker&PhysHlthF=10%20Days&MentHlthF=10%20Days&SmokerF=Has%20not%20smoked%20at%20least%20100%20cigarettes&HighCholF=No%20High%20Cholesterol&BMI=30
#http://localhost:PORT/pred?HvyAlcoholConsumpF=Is%20not%20a%20heavy%20drinker&PhysHlthF=30%20Days&MentHlthF=30%20Days&SmokerF=Has%20smoked%20at%20least%20100%20cigarettes&HighCholF=No%20High%20Cholesterol&BMI=20



#*Returning Info
#* @get /info
function(){
  "Samuel O'Ferrell - https://samoferrell.github.io/FinalProject/DiabetesDataEDA.html"
}
#http://localhost:PORT/info

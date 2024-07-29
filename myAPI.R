library(tidyverse)
library(leaflet)
library(DescTools) # for getting mode
data <- readRDS("data_factors.rds")
#fit <- glm(Diabetes_binaryF ~ HvyAlcoholConsumpF + PhysHlthF + MentHlthF + SmokerF + HighCholF + BMI, data = data, family = "binomial")
####################################### adding API stuff below ###################################

#*Analyzing Predictors
#* @get /pred
#* @param predictor
function(predictor, data = data){
  if (is.factor(data[[predictor]])){
    mode <- Mode(data[[predictor]])
    value <- as.character(mode[[1]])
    return(paste("The most prevalent value for", predictor, "is", value))
  }
  if (!is.factor(data[[predictor]])){
    mean_value <- mean(data[[predictor]])
    return(paste("The mean of", predictor, "is", mean_value))  
  }
}
#http://localhost:PORT/pred?predictor=PhysHlthF
#http://localhost:PORT/pred?predictor=BMI
#http://localhost:PORT/pred?predictor=SmokerF



#*Returning Info
#* @get /info
function(){
  "Samuel O'Ferrell - "
}
#http://localhost:PORT/info

library(tidyverse)
library(leaflet)
library(DescTools) # for getting mode
data <- readRDS("data_factors.rds")
fit <- glm(Diabetes_binaryF ~ HvyAlcoholConsumpF + PhysHlthF + MentHlthF + SmokerF + HighCholF + BMI, data = data, family = "binomial")
####################################### adding API stuff below ###################################

# #*Analyzing Predictors

#* @get /pred
#* 

#http://localhost:PORT/pred?predictor=PhysHlthF
#http://localhost:PORT/pred?predictor=BMI
#http://localhost:PORT/pred?predictor=SmokerF



#*Returning Info
#* @get /info
function(){
  "Samuel O'Ferrell - "
}
#http://localhost:PORT/info

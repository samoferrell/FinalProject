library(tidyverse)
library(DescTools) # for getting mode
library(plumber)
data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
data <- data |>
  mutate(
    Diabetes_binaryF = factor(Diabetes_binary, 
                              levels = c("0","1"), 
                              labels = c("No_Diabetes", "Diabetes")),
    HighBPF = factor(HighBP, 
                     levels = c("0","1"), 
                     labels = c("No High Blood Pressure", "High Blood Pressure")),
    HighCholF = factor(HighChol, 
                       levels = c("0","1"), 
                       labels = c("No High Cholesterol", "High Cholesterol")),
    CholCheckF = factor(CholCheck, 
                        levels = c("0","1"), 
                        labels = c("No Cholesterol Check in 5 Years", 
                                   "Yes Cholesterol Check in 5 Years")),
    SmokerF = factor(Smoker, 
                     levels = c("0","1"), 
                     labels = c("Has not smoked at least 100 cigarettes", 
                                "Has smoked at least 100 cigarettes")),
    StrokeF = factor(Stroke, 
                     levels = c("0","1"), 
                     labels = c("Has had stroke", 
                                "Has not had stroke")),
    HeartDiseaseorAttackF = factor(HeartDiseaseorAttack, 
                                   levels = c("0","1"), 
                                   labels = c("Has not had coronary heart disease or myocardial infarction", 
                                              "Has had coronary heart disease or myocardial infarction")),
    PhysActivityF = factor(PhysActivity, 
                           levels = c("0","1"), 
                           labels = c("Was not physically active within past 30 days")), 
    FruitsF = factor(Fruits, 
                     levels = c("0","1"), 
                     labels = c("Does not consume fruit 1 or more times per day", 
                                "Does consume fruit 1 or more times per day")),
    VeggiesF = factor(Veggies, 
                      levels = c("0","1"), 
                      labels = c("Does not consume vegetables 1 or more times per day", 
                                 "Does consume vegetables 1 or more times per day")),
    HvyAlcoholConsumpF = factor(HvyAlcoholConsump, 
                                levels = c("0","1"), 
                                labels = c("Is a heavy drinker", 
                                           "Is not a heavy drinker")),
    AnyHealthcareF = factor(AnyHealthcare, 
                            levels = c("0","1"), 
                            labels = c("Has healthcare", 
                                       "Does not have healthcare")),
    NoDocbcCostF = factor(NoDocbcCost, 
                          levels = c("0","1"), 
                          labels = c("Could afford doctor within past year", 
                                     "Could not afford doctor within past year")),
    GenHlthF = factor(GenHlth, 
                      levels = c("1","2","3","4","5"), 
                      labels = c("Excellent",
                                 "Very Good",
                                 "Good",
                                 "Fair",
                                 "Poor")),
    MentHlthF = factor(MentHlth,
                       levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13",
                                  "14","15","16","17","18","19","20","21","22","23","24","25",
                                  "26","27","28","29","30"),
                       labels = c("0 Days", "1 Day", "2 Days", "3 Days", "4 Days", "5 Days", "6 Days",
                                  "7 Days", "8 Days", "9 Days", "10 Days", "11 Days", 
                                  "12 Days", "13 Days", "14 Days", "15 Days", "16 Days", 
                                  "17 Days", "18 Days", "19 Days", "20 Days", "21 Days", 
                                  "22 Days", "23 Days", "24 Days", "25 Days", "26 Days", 
                                  "27 Days", "28 Days", "29 Days", "30 Days")),
    PhysHlthF = factor(PhysHlth,
                       levels = c("0", "1","2","3","4","5","6","7","8","9","10","11","12","13",
                                  "14","15","16","17","18","19","20","21","22","23","24","25",
                                  "26","27","28","29","30"),
                       labels = c("0 Days","1 Day", "2 Days", "3 Days", "4 Days", "5 Days", "6 Days",
                                  "7 Days", "8 Days", "9 Days", "10 Days", "11 Days", 
                                  "12 Days", "13 Days", "14 Days", "15 Days", "16 Days", 
                                  "17 Days", "18 Days", "19 Days", "20 Days", "21 Days", 
                                  "22 Days", "23 Days", "24 Days", "25 Days", "26 Days", 
                                  "27 Days", "28 Days", "29 Days", "30 Days")),
    DiffWalkF = factor(DiffWalk, 
                       levels = c("0","1"), 
                       labels = c("Does not have difficulty walking or climbing stairs", 
                                  "Does have difficulty walking or climbing stairs")),
    SexF = factor(Sex, 
                  levels = c("0","1"), 
                  labels = c("Female", 
                             "Male")),
    AgeF = factor(Age, 
                  levels = c("1","2","3","4","5","6","7","8","9","10",
                             "11","12","13"), 
                  labels = c("Aged 18-24", "Aged 25-29", "Aged 30-34",
                             "Aged 35-39", "Aged 40-44", "Aged 45-49",
                             "Aged 50-54", "Aged 55-59", "Aged 60-64",
                             "Aged 65-69", "Aged 70-74", "Aged 75-79",
                             "Aged 80 or older")),
    EducationF = factor(Education, 
                        levels = c("1","2","3","4","5", "6"), 
                        labels = c("Never attended school or only kindergarten",
                                   "Grades 1 through 8 (Elementary)",
                                   "Grades 9 through 11 (Some high school)",
                                   "Grade 12 or GED (High school graduate)",
                                   "College 1 year to 3 years (Some college or technical school)",
                                   "College 4 years or more (College graduate)")),    
    
    
    IncomeF = factor(Income, 
                     levels = c("1","2","3","4","5","6","7","8"), 
                     labels = c("Less than $10,000",
                                "Less than $15,000",
                                "Less than $20,000",
                                "Less than $25,000",
                                "Less than $35,000",
                                "Less than $50,000",
                                "Less than $75,000",
                                "More than $75,000")))
fit <- glm(Diabetes_binaryF ~ HvyAlcoholConsumpF + PhysHlthF + MentHlthF + SmokerF + HighCholF + BMI, data = data, family = "binomial")
####################################### adding API stuff below ###################################

# #*Analyzing Predictors
AlcMode <- as.character(Mode(data[["HvyAlcoholConsumpF"]]))[[1]]
PhysMode <- as.character(Mode(data[["PhysHlthF"]]))[[1]]
MentMode <- as.character(Mode(data[["MentHlthF"]]))[[1]]
SmokMode <- as.character(Mode(data[["SmokerF"]]))[[1]]
HighCMode <- as.character(Mode(data[["HighCholF"]]))[[1]]
BMIMean <- mean(data[["BMI"]])


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

prediction_data = pop %>% filter(year > 2020  & year < 2031)
prediction_data$log_pop = NULL
#To give a confidence level of estimates, modify the code by making population as pop * 0.95, pop*1.05 
#for 10% confidence

#Prediction for rice
prediction_data$predicted_allotment = predict(rice_model, prediction_data)
prediction_data$Population = NULL

#Making the years into column wise. A better shorter code could be written using spread()
# and tidyr but note getting desired result because the columns where spread
#year wise with multiple rows for a state. Each state should be in a row.
rice_prediction = prediction_data %>% filter(year == 2021)
names(rice_prediction)[which(names(rice_prediction) == "predicted_allotment")] = 2021
rice_prediction$year = NULL
for(ye in c(2022:2030))
{
  temp = prediction_data %>% filter(year == ye)
  temp$year = NULL
  names(temp)[which(names(temp) == "predicted_allotment")] = ye
  rice_prediction = inner_join(rice_prediction, temp, by=c('State.UT'))  
}

all_india = c("All India")
for(i in seq(1:10))
{
  year = names(rice_prediction)[i+1]
  all_india = c(all_india, sum(rice_prediction[year]))
}
rice_prediction = rbind(rice_prediction, all_india)
write.xlsx(rice_prediction, "rice_prediction.xlsx")

#Making predictions for wheat
prediction_data = pop %>% filter(year > 2020  & year < 2031)
prediction_data$log_pop = NULL

prediction_data$predicted_allotment = predict(wheat_model, prediction_data)
prediction_data$Population = NULL

wheat_prediction = prediction_data %>% filter(year == 2021)
names(wheat_prediction)[which(names(wheat_prediction) == "predicted_allotment")] = 2021
wheat_prediction$year = NULL
for(ye in c(2022:2030))
{
  temp = prediction_data %>% filter(year == ye)
  temp$year = NULL
  names(temp)[which(names(temp) == "predicted_allotment")] = ye
  wheat_prediction = inner_join(wheat_prediction, temp, by=c('State.UT'))  
}

all_india = c("All India")
for(i in seq(1:10))
{
  year = names(wheat_prediction)[i+1]
  all_india = c(all_india, sum(wheat_prediction[year]))
}
wheat_prediction = rbind(wheat_prediction, all_india)

write.xlsx(wheat_prediction, "wheat_prediction.xlsx")

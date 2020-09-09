rice_oa <- read.csv("rice 2003-20.csv")
wheat_oa <- read.csv("wheat 2003-20.csv")
#Dataframe in column format is being made to rows.
col_names_off = col_names_all = c("State.UT")
for(i in c(2003:2019))
{
  col_names_off = c(col_names_off, paste('offtake', paste(i, substr(i+1, 3, 4), sep='.'), sep='.'))
  col_names_all = c(col_names_all, paste('allotment', paste(i, substr(i+1, 3, 4), sep='.'), sep='.'))
}

rice_o = rice_oa[col_names_off]
rice_o = gather(rice_o, key='year', value='offtake', -one_of('State.UT'))
rice_o$year = as.numeric(substr(rice_o$year, 9, 12))
rice_o$log_offtake = log(rice_o$offtake)
rice_o = rice_o[complete.cases(rice_o), ]
rice_a = rice_oa[col_names_all]
rice_a = gather(rice_a, key='year', value='allotment', -one_of('State.UT'))
rice_a$year = as.numeric(substr(rice_a$year, 11, 14))
rice_a$log_allotment = log(rice_a$allotment)
rice_a = rice_a[complete.cases(rice_a),]
rice_a$allotment = as.numeric(rice_a$allotment)

wheat_o = wheat_oa[col_names_off]
wheat_o = gather(wheat_o, key='year', value='offtake', -one_of('State.UT'))
wheat_o$year = as.numeric(substr(wheat_o$year, 9, 12))
wheat_o$log_offtake = log(wheat_o$offtake)
wheat_o = wheat_o[complete.cases(wheat_o), ]
wheat_a = wheat_oa[col_names_all]
wheat_a = gather(wheat_a, key='year', value='allotment', -one_of('State.UT'))
wheat_a$year = as.numeric(substr(wheat_a$year, 11, 14))
wheat_a$log_allotment = log(wheat_a$allotment)
wheat_a = wheat_a[complete.cases(wheat_a),]
wheat_a$allotment = as.numeric(wheat_a$allotment)

total_all = merge(rice_a, wheat_a, by=c('State.UT', 'year'))
total_all$allotment = total_all$allotment.x + total_all$allotment.y
total_all$log_allotment.x = total_all$log_allotment.y = total_all$allotment.x = total_all$allotment.y = NULL
total_all$log_allotment = log(total_all$allotment)
total_all = total_all[which(is.finite(total_all$allotment)), ]
total_off = merge(rice_o, wheat_o, by=c('State.UT', 'year'))
total_off$offtake = total_off$Offtake.x + total_off$Offtake.y
total_off$Offtake.x = total_off$Offtake.y = NULL
total_off$log_offtake.x = total_off$log_offtake.y = NULL
total_off$log_offtake = log(total_off$offtake)
total_off = total_off[which(is.finite(total_off$offtake)), ]

all_off = merge(total_all, total_off, by=c('State.UT', 'year'))
all_off$year = as.numeric(all_off$year)
all_off = all_off[complete.cases(all_off),]
all_off$log_allotment = log(all_off$allotment)
all_off$log_offtake = log(all_off$offtake)
all_off$utilisation_ratio = all_off$offtake/all_off$allotment
all_off = all_off[-which(all_off$utilisation_ratio > 1),]
all_off <- remove_outliers(all_off, names(all_off)[-1:-2])


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
for(year in c(2021:2030))
{
  year = as.character(year)
  rice_prediction[year] = round(rice_prediction[year], 2)
  all_india = c(all_india, sum(rice_prediction[year]))
}
rice_prediction = rbind(rice_prediction, all_india)
rice_prediction[, 2:11] = sapply(rice_prediction[, 2:11], as.numeric)
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
for(year in c(2021:2030))
{
  year = as.character(year)
  wheat_prediction[year] = round(wheat_prediction[year], 2)
  all_india = c(all_india, sum(wheat_prediction[year]))
}
wheat_prediction = rbind(wheat_prediction, all_india)
wheat_prediction[, 2:11] = sapply(wheat_prediction[, 2:11], as.numeric)
write.xlsx(wheat_prediction, "wheat_prediction.xlsx")

#Forecasting expenditure.
#Forecast for rice price
#Units are in 1000 MTs = 1000 000 kgs = 1000 0 quintals
#Rs. 1868 / Qtl
quintal_mt_conversion = 10000
price = 1868 * quintal_mt_conversion
all_india = rice_prediction[which(rice_prediction$State.UT == "All India"),]
crore = 10000000
temp = all_india[, 2:11]*price / crore
rice_expenditure = gather(temp, key="year", "rice expenditure")

#Forecast for wheat price
#Rs. 1925 per quintal
price = 1925 * quintal_mt_conversion
all_india = wheat_prediction[which(wheat_prediction$State.UT == "All India"),]
crore = 10000000
temp = all_india[, 2:11]*price / crore
wheat_expenditure = gather(temp, key="year", "wheat expenditure")

rice_wheat_expenditure = inner_join(rice_expenditure, wheat_expenditure, by=c("year"))

write.xlsx(rice_wheat_expenditure, "rice_wheat_expenditure.xlsx")

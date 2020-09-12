#Population or BPL Population
total_all_pop = inner_join(all_off, pop, by=c('State.UT', 'year'))
total_all_pop$utilisation_ratio = total_all_pop$zone = NULL
total_all_pop$offtake = total_all_pop$log_pop = NULL

pop_fit <- lm(allotment ~ Population, total_all_pop)
summary(pop_fit)
summary(pop_fit)$r.squared

bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 3, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  tbp = inner_join(all_off, bpl, on=c("State.UT", "year"))
  fit <- lm(allotment ~ bpl_pop, tbp)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}

total_bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
plot(bpl_change_rate, ssr)
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
tbp = inner_join(all_off, bpl, on=c("State.UT", "year"))

tbp$percent = tbp$log_bplpop = tbp$log_pop = tbp$Population = NULL
tbp$utilisation_ratio = tbp$offtake = tbp$zone = NULL

bpl_fit = lm(allotment ~ bpl_pop, tbp)
summary(bpl_fit)$r.squared
summary(bpl_fit)

################################################################################################
r = rice
w = wheat
r$zone = r$offtake = w$offtake = w$zone = NULL
names(r)[which(names(r) == "allotment")] = "rice_allotment"
names(r)
names(w)[which(names(w) == "allotment")] = "wheat_allotment"
names(w)
rw = inner_join(r, w, by=c('State.UT', 'year'))

rw$rice_perc = rw$rice_allotment / (rw$rice_allotment + rw$wheat_allotment)
rw$wheat_perc = rw$wheat_allotment / (rw$wheat_allotment + rw$rice_allotment)
rw$rice_moving_perc = rw$wheat_moving_perc = 0
for(year in c(2006:2019))
{
  past_years = c((year - 3): (year - 1))
  for(state in unique(rw$State.UT))
  {
    idx1 = which(rw$State.UT == state & rw$year == past_years[1])
    idx2 = which(rw$State.UT == state & rw$year == past_years[2])
    idx3 = which(rw$State.UT == state & rw$year == past_years[3])
    idx4 = which(rw$State.UT == state & rw$year == year)
    if(length(idx1) > 0 & length(idx2) > 0 & length(idx3) > 0 & length(idx4) > 0)
    {
      rw[idx4, ]$rice_moving_perc = mean(rw[c(idx1, idx2, idx3),]$rice_perc)
      rw[idx4, ]$wheat_moving_perc = mean(rw[c(idx1, idx2, idx3),]$wheat_perc)
    }
  }
}

#rw contains rice allotment and wheat allotment, state wise and year wise
rw = rw[which(rw$wheat_moving_perc > 0 & rw$rice_moving_perc > 0),]
rw$rice_perc = rw$wheat_perc = NULL

#Making of datasets for rice models
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 3, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  rbp = inner_join(bpl, rw, on=c("State.UT", "year"))
  fit <- lm(rice_allotment ~ bpl_pop, rbp)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}

rice_bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
plot(bpl_change_rate, ssr)
bpl = generate_bpl_data(pop, bpl_perc2011, rice_bpl_cr)
rbp = inner_join(bpl, rw, on=c("State.UT", "year"))
rbp$Population = rbp$log_pop = rbp$log_bplpop = NULL
rbp$wheat_moving_perc = rbp$wheat_allotment = NULL
rbp = remove_outliers(rbp, c("rice_allotment", "rice_moving_perc", "bpl_pop"))

rp = inner_join(rw, pop, by=c('State.UT', 'year'))
rp$wheat_allotment = rp$log_pop = NULL
rp = remove_outliers(rp, c("Population", "rice_allotment", "rice_moving_perc", "wheat_moving_perc"))

#Rice allotment with population, percentage taken as moving average of past 3 years
rice_pop_fit <- lm(rice_allotment ~ Population + rice_moving_perc, rp)
summary(rice_pop_fit)$r.squared
rice_pop_fit$coefficients

rice_bpl_pop_fit <- lm(rice_allotment ~ bpl_pop + rice_moving_perc, rbp)
summary(rice_bpl_pop_fit)$r.squared
rice_bpl_pop_fit$coefficients

#Other models built
fit <- lm(rice_allotment ~ Population, rp)
summary(fit)$r.squared

fit <- lm(rice_allotment ~ bpl_pop, rbp)
summary(fit)$r.squared

#Wheat with bpl population
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 3, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  wbp = inner_join(bpl, rw, on=c("State.UT", "year"))
  fit <- lm(wheat_allotment ~ bpl_pop, wbp)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}

wheat_bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
plot(bpl_change_rate, ssr)
bpl = generate_bpl_data(pop, bpl_perc2011, wheat_bpl_cr)
wbp = inner_join(bpl, rw, on=c("State.UT", "year"))
wbp$Population = wbp$log_pop = wbp$log_bplpop = NULL
wbp$rice_allotment = wbp$rice_moving_perc = NULL

wp = inner_join(rw, pop, by=c('State.UT', 'year'))
wp$rice_allotment = wp$rice_moving_perc = wp$log_pop = NULL

#Wheat allotment with population, percentage as past 3 years moving average
wheat_pop_fit <- lm(wheat_allotment ~ Population + wheat_moving_perc, wp)
summary(wheat_pop_fit)$r.squared

#Wheat allotment with bpl population, percentage taken as moving average of past 3 years
wheat_bpl_pop_fit <- lm(wheat_allotment ~ bpl_pop + wheat_moving_perc, wbp)
summary(wheat_bpl_pop_fit)$r.squared

#Other models built
fit <- lm(wheat_allotment ~ Population, wp)
summary(fit)$r.squared

fit <- lm(wheat_allotment ~ bpl_pop, wbp)
summary(fit)$r.squared

#MODEL FORECASTING
prediction_data = pop %>% filter(year > 2020  & year < 2031)
prediction_data$log_pop = NULL

#To give a confidence level of estimates, modify the code by making population as pop * 0.95, pop*1.05 
#for 10% confidence

#Prediction for rice
#rice_wheat contains average proportion of rice and wheat consumption of states over years 2003-2019
prediction_data = inner_join(prediction_data, rice_wheat, by=c('State.UT'))
names(prediction_data)[names(prediction_data) == "perc_rice"] = "rice_moving_perc"
names(prediction_data)[names(prediction_data) == "perc_wheat"] = "wheat_moving_perc"
#Byproducts are joining from rice_wheat which are not needed
prediction_data$rice_allotment = prediction_data$wheat_allotment = NULL
prediction_data$predicted_allotment = predict(rice_pop_fit, prediction_data)
prediction_data$Population = prediction_data$rice_moving_perc = prediction_data$wheat_moving_perc = NULL

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
write.xlsx(rice_prediction, "Data/rice_prediction.xlsx")

#Making predictions for wheat
prediction_data = pop %>% filter(year > 2020  & year < 2031)
prediction_data$log_pop = NULL

prediction_data = inner_join(prediction_data, rice_wheat, by=c('State.UT'))
names(prediction_data)[names(prediction_data) == "perc_rice"] = "rice_moving_perc"
names(prediction_data)[names(prediction_data) == "perc_wheat"] = "wheat_moving_perc"
#Byproducts are joining from rice_wheat which are not needed
prediction_data$rice_allotment = prediction_data$wheat_allotment = NULL
prediction_data$predicted_allotment = predict(wheat_pop_fit, prediction_data)
prediction_data$Population = prediction_data$rice_moving_perc = prediction_data$wheat_moving_perc = NULL

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
write.xlsx(wheat_prediction, "Data/wheat_prediction.xlsx")

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

write.xlsx(rice_wheat_expenditure, "Data/rice_wheat_expenditure.xlsx")

#Analysis with total allotment
ao_pop = inner_join(all_off, pop, by=c('State.UT', 'year'))
fit <- lm(ao_pop$allotment ~ ao_pop$Population)
summary(fit)$r.squared

## Prediction with previous year offtake
ri = rice
ri$allotment_prev_year = 0
for(state in unique(ri$State.UT))
{
  for(year in c(2004:2019))
  {
    idx = which(ri$State.UT == state & ri$year == year)
    idx2 = which(ri$State.UT == state & ri$year == year-1)
    if(length(idx) > 0 & length(idx2) > 0)
    {
      ri[idx, ]$allotment_prev_year = ri[idx2, ]$allotment
    }
  }
}
ri = ri %>% filter(year > 2003, allotment > 0, allotment_prev_year > 0)
fit <- lm(ri$allotment ~ ri$allotment_prev_year)
fit
summary(fit)$r.squared

#Make a model using log allotment and log population
#Section 5
# Rice all. Vs GSDP

ra_gdp = inner_join(rice_a, gsdp, on=c('State.UT', 'year'))
cor(ra_gdp$Allotment, ra_gdp$gsdp)
cor(ra_gdp$Allotment, ra_gdp$log_gsdp)
cor(ra_gdp$log_allotment, ra_gdp$log_gsdp)
cor(ra_gdp$log_allotment, ra_gdp$gsdp)
summary(lm(Allotment ~ gsdp, ra_gdp))$r.squared
summary(lm(Allotment ~ log_gsdp, ra_gdp))$r.squared
summary(lm(log_allotment ~ log_gsdp, ra_gdp))$r.squared
summary(lm(log_allotment ~ gsdp, ra_gdp))$r.squared

wa_gdp = inner_join(wheat_a, gsdp, on=c('State.UT', 'year'))
wa_gdp = wa_gdp[is.finite(wa_gdp$log_allotment), ]
cor(wa_gdp$Allotment, wa_gdp$gsdp)
cor(wa_gdp$Allotment, wa_gdp$log_gsdp)
cor(wa_gdp$log_allotment, wa_gdp$log_gsdp)
cor(wa_gdp$log_allotment, wa_gdp$gsdp)
summary(lm(Allotment ~ gsdp, wa_gdp))$r.squared
summary(lm(Allotment ~ log_gsdp, wa_gdp))$r.squared
summary(lm(log_allotment ~ log_gsdp, wa_gdp))$r.squared
summary(lm(log_allotment ~ gsdp, wa_gdp))$r.squared

ta_gdp = inner_join(total_all, gsdp, on=c('State.UT', 'year')) 
cor(ta_gdp$allotment, ta_gdp$gsdp)
cor(ta_gdp$allotment, ta_gdp$log_gsdp)
cor(ta_gdp$log_allotment, ta_gdp$log_gsdp)
cor(ta_gdp$log_allotment, ta_gdp$gsdp)
summary(lm(allotment ~ gsdp, ta_gdp))$r.squared
summary(lm(allotment ~ log_gsdp, ta_gdp))$r.squared
summary(lm(log_allotment ~ log_gsdp, ta_gdp))$r.squared
summary(lm(log_allotment ~ gsdp, ta_gdp))$r.squared

ra_p = inner_join(rice_a, pop, on=c('State.UT', 'year'))
cor(ra_p$Allotment, ra_p$Population)
cor(ra_p$Allotment, ra_p$log_pop)
cor(ra_p$log_allotment, ra_p$log_pop)
cor(ra_p$log_allotment, ra_p$Population)
summary(lm(Allotment ~ Population, ra_p))$r.squared
summary(lm(Allotment ~ log_pop, ra_p))$r.squared
summary(lm(log_allotment ~ log_pop, ra_p))$r.squared
summary(lm(log_allotment ~ Population, ra_p))$r.squared

wa_p = inner_join(wheat_a, pop, on=c('State.UT', 'year'))
wa_p = wa_p[is.finite(wa_p$log_allotment),]
cor(wa_p$Allotment, wa_p$Population)
cor(wa_p$Allotment, wa_p$log_pop)
cor(wa_p$log_allotment, wa_p$log_pop)
cor(wa_p$log_allotment, wa_p$Population)
summary(lm(Allotment ~ Population, wa_p))$r.squared
summary(lm(Allotment ~ log_pop, wa_p))$r.squared
summary(lm(log_allotment ~ log_pop, wa_p))$r.squared
summary(lm(log_allotment ~ Population, wa_p))$r.squared

ta_p = inner_join(total_all, pop, on=c('State.UT', 'year'))
ta_p = ta_p[is.finite(ta_p$allotment),]
cor(ta_p$allotment, ta_p$Population)
cor(ta_p$allotment, ta_p$log_pop)
cor(ta_p$log_allotment, ta_p$log_pop)
cor(ta_p$log_allotment, ta_p$Population)
summary(lm(allotment ~ Population, ta_p))$r.squared
summary(lm(allotment ~ log_pop, ta_p))$r.squared
summary(lm(log_allotment ~ log_pop, ta_p))$r.squared
summary(lm(log_allotment ~ Population, ta_p))$r.squared

bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 2, 0.01))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  ra_bpl = inner_join(bpl, rice_a, on=c("State.UT", "year"))
  fit <- lm(ra_bpl$Allotment ~ ra_bpl$bpl_pop)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}
plot(bpl_change_rate, ssr)
bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
ra_bpl = inner_join(bpl, rice_a, on=c('State.UT', 'year'))
cor(ra_bpl$Allotment, ra_bpl$bpl_pop)
cor(ra_bpl$Allotment, ra_bpl$log_bplpop)
cor(ra_bpl$log_allotment, ra_bpl$log_bplpop)
cor(ra_bpl$log_allotment, ra_bpl$bpl_pop)
summary(lm(Allotment ~ bpl_pop, ra_bpl))$r.squared
summary(lm(Allotment ~ log_bplpop, ra_bpl))$r.squared
summary(lm(log_allotment ~ log_bplpop, ra_bpl))$r.squared
summary(lm(log_allotment ~ bpl_pop, ra_bpl))$r.squared

bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-0.5, 1, 0.01))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  wa_bpl = inner_join(bpl, wheat_a, on=c("State.UT", "year"))
  fit <- lm(Allotment ~ bpl_pop, wa_bpl)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}
plot(bpl_change_rate, ssr)
bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
wa_bpl = inner_join(bpl, wheat_a, on=c('State.UT', 'year'))
wa_bpl = wa_bpl[is.finite(wa_bpl$log_allotment),]
cor(wa_bpl$Allotment, wa_bpl$bpl_pop)
cor(wa_bpl$Allotment, wa_bpl$log_bplpop)
cor(wa_bpl$log_allotment, wa_bpl$log_bplpop)
cor(wa_bpl$log_allotment, wa_bpl$bpl_pop)
summary(lm(Allotment ~ bpl_pop, wa_bpl))$r.squared
summary(lm(Allotment ~ log_bplpop, wa_bpl))$r.squared
summary(lm(log_allotment ~ log_bplpop, wa_bpl))$r.squared
summary(lm(log_allotment ~ bpl_pop, wa_bpl))$r.squared

bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(0, 1, 0.01))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  ta_bpl = inner_join(bpl, total_all, on=c("State.UT", "year"))
  fit <- lm(allotment ~ bpl_pop, ta_bpl)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}
plot(bpl_change_rate, ssr)
bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
ta_bpl = inner_join(bpl, total_all, on=c('State.UT', 'year'))
cor(ta_bpl$allotment, ta_bpl$bpl_pop)
cor(ta_bpl$allotment, ta_bpl$log_bplpop)
cor(ta_bpl$log_allotment, ta_bpl$log_bplpop)
cor(ta_bpl$log_allotment, ta_bpl$bpl_pop)
summary(lm(allotment ~ bpl_pop, ta_bpl))$r.squared
summary(lm(allotment ~ log_bplpop, ta_bpl))$r.squared
summary(lm(log_allotment ~ log_bplpop, ta_bpl))$r.squared
summary(lm(log_allotment ~ bpl_pop, ta_bpl))$r.squared

bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-2, -1, 0.01))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  ra_bpl = inner_join(bpl, rice_a, on=c("State.UT", "year"))
  fit <- lm(ra_bpl$log_allotment ~ ra_bpl$log_bplpop)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}
plot(bpl_change_rate, ssr)
bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, 1)
ra_bpl = inner_join(bpl, rice_a, on=c('State.UT', 'year'))
cor(ra_bpl$Allotment, ra_bpl$bpl_pop)
cor(ra_bpl$Allotment, ra_bpl$log_bplpop)
cor(ra_bpl$log_allotment, ra_bpl$log_bplpop)
cor(ra_bpl$log_allotment, ra_bpl$bpl_pop)
summary(lm(Allotment ~ bpl_pop, ra_bpl))$r.squared
summary(lm(Allotment ~ log_bplpop, ra_bpl))$r.squared
summary(lm(log_allotment ~ log_bplpop, ra_bpl))$r.squared
summary(lm(log_allotment ~ bpl_pop, ra_bpl))$r.squared

bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-2, -1, 0.01))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  wa_bpl = inner_join(bpl, wheat_a, on=c("State.UT", "year"))
  wa_bpl = wa_bpl[is.finite(wa_bpl$log_allotment),]
  fit <- lm(log_allotment ~ log_bplpop, wa_bpl)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}
plot(bpl_change_rate, ssr)
bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
wa_bpl = inner_join(bpl, wheat_a, on=c('State.UT', 'year'))
wa_bpl = wa_bpl[is.finite(wa_bpl$log_allotment),]
cor(wa_bpl$Allotment, wa_bpl$bpl_pop)
cor(wa_bpl$Allotment, wa_bpl$log_bplpop)
cor(wa_bpl$log_allotment, wa_bpl$log_bplpop)
cor(wa_bpl$log_allotment, wa_bpl$bpl_pop)
summary(lm(Allotment ~ bpl_pop, wa_bpl))$r.squared
summary(lm(Allotment ~ log_bplpop, wa_bpl))$r.squared
summary(lm(log_allotment ~ log_bplpop, wa_bpl))$r.squared
summary(lm(log_allotment ~ bpl_pop, wa_bpl))$r.squared

bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-2, -1, 0.01))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  ta_bpl = inner_join(bpl, total_all, on=c("State.UT", "year"))
  fit <- lm(log_allotment ~ log_bplpop, ta_bpl)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}
plot(bpl_change_rate, ssr)
bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
ta_bpl = inner_join(bpl, total_all, on=c('State.UT', 'year'))
cor(ta_bpl$allotment, ta_bpl$bpl_pop)
cor(ta_bpl$allotment, ta_bpl$log_bplpop)
cor(ta_bpl$log_allotment, ta_bpl$log_bplpop)
cor(ta_bpl$log_allotment, ta_bpl$bpl_pop)
summary(lm(allotment ~ bpl_pop, ta_bpl))$r.squared
summary(lm(allotment ~ log_bplpop, ta_bpl))$r.squared
summary(lm(log_allotment ~ log_bplpop, ta_bpl))$r.squared
summary(lm(log_allotment ~ bpl_pop, ta_bpl))$r.squared

#Multilinear regressions
# rice_a, pop, bpl_perc2011
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 2, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  ra_bpl = inner_join(bpl, rice_a, on=c("State.UT", "year"))
  fit <- lm(Allotment ~ Population + bpl_pop, ra_bpl)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}

plot(bpl_change_rate, ssr, xlab="BPL Change rate", ylab="SSR", main="Plot for rice")
(ssr[2:length(ssr)] - ssr[1:length(ssr)-1]) * 100 / ssr[1:length(ssr)-1]
#bpl_cr = -0.15
bpl_cr = -0.15
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
ra_bpl = inner_join(bpl, rice_a, on=c("State.UT", "year"))
fit <- lm(Allotment ~ Population + bpl_pop, ra_bpl)
summary(fit)$r.squared

#wheat_a, pop, bpl_perc2011
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-2, 7, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  wa_bpl = inner_join(bpl, wheat_a, on=c("State.UT", "year"))
  #fit <- lm(allotment ~ Population + bpl_pop, ra_bpl)
  fit <- lm(allotment ~ Population + bpl_pop, wa_bpl)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}

plot(bpl_change_rate, ssr, xlab="BPL Change rate", ylab="SSR")
(ssr[2:length(ssr)] - ssr[1:length(ssr)-1]) * 100 / ssr[1:length(ssr)-1]
bpl_cr = 5.85
wa_bpl = inner_join(bpl, wheat_a, on=c("State.UT", "year"))
fit <- lm(allotment ~ Population + bpl_pop, wa_bpl)
summary(fit)$r.squared

#total_all, pop, bpl_perc2011
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-2, 7, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  ta_bpl = inner_join(bpl, total_all, on=c("State.UT", "year"))
  #fit <- lm(allotment ~ Population + bpl_pop, ra_bpl)
  fit <- lm(allotment ~ Population + bpl_pop, ta_bpl)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}

plot(bpl_change_rate, ssr, xlab="BPL Change rate", ylab="SSR")
(ssr[2:length(ssr)] - ssr[1:length(ssr)-1]) * 100 / ssr[1:length(ssr)-1]
bpl_cr = 3.75
ta_bpl = inner_join(bpl, total_all, on=c("State.UT", "year"))
fit <- lm(allotment ~ Population + bpl_pop, ta_bpl)
summary(fit)$r.squared


# 
# 
# 
# 
# gsdp_bpl_ao = inner_join(bpl_ao, gsdp, by=c("State.UT", "year"))
# summary(lm(gsdp_bpl_ao$allotment ~ gsdp_bpl_ao$bpl_pop + gsdp_bpl_ao$gsdp))
# 
# model = lm(allotment ~ Population, pop_ao)
# prediction_data = pop %>% filter(year > 2019  & year < 2025)
# prediction_data$Population = prediction_data$Population * 1.05
# prediction_data$allotment = predict(model, prediction_data)
# 
# prediction_data$log_pop = NULL
# prediction_data$Population = NULL
# 
# predictions = prediction_data %>% 
#   
# summarise(total_allotment = sum(allotment))
# 
# write.xlsx(predictions, "predictions.xlsx")
# 
# 
# 
# #Ao gap of states and their gdps
# ao_gsdp$ao_gap = ao_gsdp$allotment - ao_gsdp$offtake
# cor(ao_gsdp$ao_gap, ao_gsdp$gsdp)
# cor(log(ao_gsdp$ao_gap), log(ao_gsdp$gsdp))
# ao_gsdp$log_aogap = log(ao_gsdp$ao_gap)
# ggplot(ao_gsdp, aes(gsdp, ao_gap)) + geom_jitter() + geom_smooth(method=lm)
# #The line should have negative slope here because of the rational that state with 
# #high gsdp have better infrastructure and hence there will be less ao_gap. Hence,
# #roled out.
# 
# cor(ao_gsdp$utilisation_ratio, ao_gsdp$offtake)
# #We can make two predictors. One is the population as a predictor for nect
# #years allotment amount, the other is offtake as predictor for next years allotment
# #amount.
# #The code below uses previous years offtake as predictor for next years allotment.
# #A prediction using population has been done above.
# 
# ao = all_off %>% select(State.UT, year, allotment, offtake)
# View(ao)
# ao$offtake_prev_year = 0
# for(state in unique(ao$State.UT))
# {
#   for(year in c(2012:2019))
#   {
#     idx = which(ao$State.UT == state & ao$year == year)
#     idx2 = which(ao$State.UT == state & ao$year == year-1)
#     if(length(idx) > 0 & length(idx2) > 0){
#       ao[idx, ]$offtake_prev_year = ao[idx2, ]$offtake
#     }
#   }
# }
# ao = ao[-which(ao$year == 2011),]
# cor(ao$offtake_prev_year, ao$allotment)
# summary(lm(ao$allotment ~ ao$offtake_prev_year))
#As individual

r = rice
w = wheat
r$zone = r$offtake = w$offtake = w$zone = NULL
names(r)[which(names(r) == "allotment")] = "rice_allotment"
names(r)
names(w)[which(names(w) == "allotment")] = "wheat_allotment"
names(w)
rw = inner_join(r, w, by=c('State.UT', 'year'))
View(rw)
rw$rice_perc = rw$rice_allotment / (rw$rice_allotment + rw$wheat_allotment)
rw$wheat_perc = rw$wheat_allotment / (rw$wheat_allotment + rw$rice_allotment)
#Rice with population
rwp = inner_join(rw, pop, by=c('State.UT', 'year'))
View(rwp)
fit <- lm(rice_allotment ~ Population + rice_perc + wheat_perc, rwp)
summary(fit)$r.squared

fit <- lm(rice_allotment ~ Population, rwp)
summary(fit)$r.squared

fit <- lm(wheat_allotment ~ Population + rice_perc + wheat_perc, rwp)
summary(fit)$r.squared

#Rice with bpl population

bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 3, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  rsp = inner_join(bpl, rw, on=c("State.UT", "year"))
  fit <- lm(rsp$rice_allotment ~ rsp$bpl_pop)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}

bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
rsp = inner_join(bpl, rw, on=c("State.UT", "year"))
fit <- lm(rice_allotment ~ bpl_pop + rice_perc + wheat_perc, rsp)
summary(fit)$r.squared

fit <- lm(rice_allotment ~ bpl_pop, rsp)
summary(fit)$r.squared

#Wheat with bpl population
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 3, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  rsp = inner_join(bpl, rw, on=c("State.UT", "year"))
  fit <- lm(rsp$wheat_allotment ~ rsp$bpl_pop)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}

bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
rsp = inner_join(bpl, rw, on=c("State.UT", "year"))

fit <- lm(wheat_allotment ~ bpl_pop + rice_perc + wheat_perc, rsp)
summary(fit)$r.squared

fit <- lm(wheat_allotment ~ bpl_pop, rsp)
summary(fit)$r.squared

#RICE

#Rice and population
#As average
rsp = inner_join(r_sw, pop, by=c('State.UT', 'year'))
rsp = inner_join(rsp, state_rice_wheat_perc, by=c('State.UT'))

fit <- lm(allotment ~ Population + perc_rice + perc_wheat, rsp)
summary(fit)$r.squared

fit <- lm(allotment ~ Population, rsp)
summary(fit)$r.squared

#Log population ~ log allotment - 0.611
#Rice and bpl population
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 3, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  rsp = inner_join(bpl, r_sw, on=c("State.UT", "year"))
  rsp <- remove_outliers(rsp, c("allotment", "bpl_pop"))
  fit <- lm(rsp$allotment ~ rsp$bpl_pop)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}
plot(bpl_change_rate, ssr, xlab="BPL Change rate", ylab="SSR", main="BPL Pop & Allotment")
bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
rsp = inner_join(bpl, r_sw, on=c("State.UT", "year"))
rsp$log_allotment = log(rsp$allotment)
rsp <- remove_outliers(rsp, c("allotment", "bpl_pop"))
fit <- lm(rsp$log_allotment ~ rsp$log_bplpop)
summary(fit)$r.squared

#Rice model used for prediction
rice_model <- lm(allotment ~ Population, rsp)

#WHEAT
wheat_state_list = rice_wheat[which(rice_wheat$perc_wheat > 0),]$State.UT
w_sw = wheat[which(wheat$State.UT %in% wheat_state_list), ]

#Wheat and population
wsp = inner_join(w_sw, pop, by=c('State.UT', 'year'))
wsp = inner_join(wsp, state_rice_wheat_perc, by=c('State.UT'))

fit <- lm(allotment ~ Population + perc_rice + perc_wheat, wsp)
summary(fit)$r.squared

fit <- lm(allotment ~ Population, wsp)
summary(fit)$r.squared

#Wheat and bpl population
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 2, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  wsp = inner_join(bpl, w_sw, on=c("State.UT", "year"))
  wsp <- remove_outliers(wsp, c("allotment", "bpl_pop"))
  fit <- lm(wsp$allotment ~ wsp$bpl_pop)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}
plot(bpl_change_rate, ssr, xlab="BPL Change rate", ylab="SSR", main="BPL Pop & Allotment")
bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
wsp = inner_join(bpl, w_sw, on=c("State.UT", "year"))
wsp <- remove_outliers(wsp, c("allotment", "bpl_pop"))
fit <- lm(wsp$allotment ~ wsp$bpl_pop)
summary(fit)

#Wheat model used for prediction
wheat_model <- lm(allotment ~ Population, wsp)
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
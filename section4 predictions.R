
#RICE
rice_state_list = rice_wheat[which(rice_wheat$perc_rice > 0.4),]$State.UT
r_sw = rice[which(rice$State.UT %in% rice_state_list), ]

#Rice and population
rsp = inner_join(r_sw, pop, by=c('State.UT', 'year'))
cor(rsp$Population, rsp$allotment)
fit <- lm(allotment ~ Population, rsp)
summary(fit)$r.squared

#Rice and bpl population
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 3, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  rsp = inner_join(bpl, r_sw, on=c("State.UT", "year"))
  fit <- lm(rsp$allotment ~ rsp$bpl_pop)
  ssr_ = sum(fit$residuals ^ 2)
  bpl_change_rate = c(bpl_change_rate, bpl_cr)
  ssr = c(ssr, ssr_)
}
plot(bpl_change_rate, ssr, xlab="BPL Change rate", ylab="SSR", main="BPL Pop & Allotment")
bpl_cr = bpl_change_rate[which(ssr == min(ssr))]
bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
rsp = inner_join(bpl, r_sw, on=c("State.UT", "year"))
rsp <- remove_outliers(rsp, c("allotment", "bpl_pop"))
fit <- lm(rsp$allotment ~ rsp$bpl_pop)
summary(fit)$r.squared

#WHEAT
wheat_state_list = rice_wheat[which(rice_wheat$perc_wheat > 0.6),]$State.UT
w_sw = wheat[which(wheat$State.UT %in% wheat_state_list), ]

#Wheat and population
wsp = inner_join(w_sw, pop, by=c('State.UT', 'year'))
fit <- lm(allotment ~ Population, wsp)
summary(fit)$r.squared

#Wheat and bpl population
bpl_change_rate = c()
ssr = c()
for(bpl_cr in seq(-1, 2, 0.05))
{
  bpl = generate_bpl_data(pop, bpl_perc2011, bpl_cr)
  wsp = inner_join(bpl, w_sw, on=c("State.UT", "year"))
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
summary(fit)$r.squared


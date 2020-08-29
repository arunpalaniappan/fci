state_rice_wheat_perc = rice_wheat
state_rice_wheat_perc$rice_allotment = state_rice_wheat_perc$wheat_allotment = NULL

ao = all_off
ao$zone = ao$utilisation_ratio = NULL

df = inner_join(ao, state_rice_wheat_perc, by=c('State.UT'))
df = inner_join(df, pop, by=c('State.UT', 'year'))

fit <- lm(allotment ~ Population + perc_rice + perc_wheat, df)
summary(fit)$r.squared

fit <- lm(allotment ~ Population, df)
summary(fit)$r.squared


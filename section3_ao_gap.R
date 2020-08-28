source("make_datasets.R")

print (paste('Utilisation ratio for nation 2003-2019',sum(state_ao$offtake) / sum(state_ao$allotment)))
print (paste('Foodgrains wasted is 2003-2019', sum(state_ao$allotment) - sum(state_ao$offtake)))

zone_ao = state_ao[c('allotment', 'offtake')] %>% aggregate(by=list(state_ao$zone), sum)
zone_ao$utilisation_ratio = zone_ao$offtake / zone_ao$allotment
zone_ao$gap = zone_ao$allotment - zone_ao$offtake

year_total_ao$utilisation_ratio = year_total_ao$offtake / year_total_ao$allotment
View(zone_ao)

#Analysis 1 - with road length
ao_roadl = inner_join(all_off, road_l, by=c('State.UT', 'year'))
ao_roadl = ao_roadl[which(ao_roadl$utilisation_ratio < 1 & ao_roadl$utilisation_ratio > 0),]
cor(ao_roadl$utilisation_ratio, ao_roadl$length)
cor(ao_roadl$utilisation_ratio, ao_roadl$log_length)

#Analysis 2 - with road density
ao_roadd = inner_join(all_off, roadd_sqkm, by=c('State.UT', 'year'))
ao_roadd = ao_roadd[which(ao_roadd$utilisation_ratio < 1 & ao_roadd$utilisation_ratio > 0),]
cor(ao_roadd$utilisation_ratio, ao_roadd$length)
cor(ao_roadd$utilisation_ratio, ao_roadd$log_density)

#Analysis 3 - with state highway length
ao_shl = inner_join(all_off, sh_l, by = c('State.UT', 'year'))
ao_shl = ao_shl[which(ao_shl$utilisation_ratio < 1 & ao_shl$utilisation_ratio > 0),]
cor(ao_shl$utilisation_ratio, ao_shl$length)
cor(ao_shl$utilisation_ratio, ao_shl$log_length)

#Analysis 4 - with SH density
ao_shd = inner_join(all_off, sh_d, by=c("State.UT", "year"))
ao_shd = ao_shd[which(ao_shd$utilisation_ratio < 1 & ao_shd$utilisation_ratio > 0),]
cor(ao_shd$utilisation_ratio, ao_shd$density)
cor(ao_shd$utilisation_ratio, ao_shd$log_density)

#Analysis 5 - with railway length
ao_rwl = inner_join(all_off, rw_l, by=c('State.UT', 'year'))
ao_rwl = ao_rwl[which(ao_rwl$utilisation_ratio < 1 & ao_rwl$utilisation_ratio > 0),]
cor(ao_rwl$utilisation_ratio, ao_rwl$length)
cor(ao_rwl$utilisation_ratio, ao_rwl$log_length)

#Analysis 6 - with railway density
ao_rwd = inner_join(all_off, rw_d, by=c('State.UT', 'year'))
cor(ao_rwd$utilisation_ratio, ao_rwd$density)
cor(ao_rwd$utilisation_ratio, ao_rwd$log_density)

#Analysis 7 - with state GSDP
ao_gsdp = inner_join(all_off, gsdp, by=c('State.UT', 'year'))
ao_gsdp = ao_gsdp[which(ao_gsdp$utilisation_ratio < 1 & ao_gsdp$utilisation_ratio > 0),]
cor(ao_gsdp$log_gsdp, ao_gsdp$utilisation_ratio)
cor(ao_gsdp$gsdp, ao_gsdp$utilisation_ratio)

#Analysis 8 - With DO Count
sao = sao[which(sao$utilisation_ratio < 1 & sao$utilisation_ratio > 0),]
cor(sao$utilisation_ratio, sao$do_count)

#Analysis 9 - with DCP
cor(df_dcp$dcp, df_dcp$utilisation_ratio)
fit <- lm(df_dcp$utilisation_ratio ~ df_dcp$dcp)
fit

#Performing t-test for DCP [Resource : https://statistics.berkeley.edu/computing/r-t-tests]
dcp1 = df_dcp %>% filter(dcp == 1) %>% select(dcp, utilisation_ratio)
dcp0 = df_dcp %>% filter(dcp == 0) %>% select(dcp, utilisation_ratio)
t.test(dcp1$utilisation_ratio, dcp0$utilisation_ratio)



#Child mortality rate and ao gap
#t = inf_mr %>% group_by(year) %>% summarise(mean_inf = mean(mortality.rate))
#ggplot(t, aes(year, mean_inf)) + geom_point()
imr_ao = inner_join(inf_mr, all_off, by=c('State.UT', 'year'))
imr_ao$log_offtake = log(imr_ao$offtake)
imr_ao$utilisation_ratio = imr_ao$offtake / imr_ao$allotment
imr_ao = imr_ao[which(is.finite(imr_ao$log_offtake) ),]
imr_ao = remove_outliers(imr_ao, c('offtake', 'mortality.rate', 'log_offtake', 'utilisation_ratio'))
ggplot(imr_ao, aes(utilisation_ratio, mortality.rate)) + geom_point() + geom_smooth(method=lm)
cor(imr_ao$mortality.rate, imr_ao$offtake) #Not as expected. To be -ve
cor(imr_ao$mortality.rate, imr_ao$utilisation_ratio) #As expected but not significant
fit <- lm(imr_ao$mortality.rate ~ imr_ao$utilisation_ratio)
summary(fit)$r.squared
source("C:/Users/ARUN PALANIAPPAN/Desktop/FCI/Final Work/make_datasets.R")

#The objective here is to find states which are consistently good in utilising, 
#states which are over utilising, states which are under utilising.
#Finding over-utilising states:
states = ur$State.UT
over_util_count = under_util_count = optimal_util_count = c()
for(state in states)
{
  data_points = ur[which(ur$State.UT == state),]
  data_points = data_points[names(data_points)[-1]]
  over_util_count = c(over_util_count, length(which(data_points > 1)))
  under_util_count = c(under_util_count, length(which(data_points < 0.7)))
  optimal_util_count = c(optimal_util_count, length(which(data_points <= 1 & data_points >= 0.7)))
}
ur$over_util_count = over_util_count
ur$under_util_count = under_util_count
ur$optimal_util_count = optimal_util_count

year_count = c("Count")
for(col in names(ur)[2:19])
{
  year_count = c(year_count, length(which(ur[col] > 1)))
}
year_count = c(year_count, 0, 0, 0)
ur = rbind(ur, year_count)

make_financial_year <- function(year)
{
  current_year = as.character(year)
  next_year = as.character(year + 1)
  financial_year = paste(current_year, substr(next_year, 3, 4), sep="-")
  return (financial_year)
}

year_total_ao$financial_year = make_financial_year(year_total_ao$year)

ggplot(year_total_ao, mapping = aes(x=financial_year)) +
  geom_line(aes(y=allotment, group=1), size=2, color="green", labels="Total Allotment") + 
  geom_line(aes(y=offtake, group=1), size=2, color="blue", labels="Total Offtake") + 
  labs(title="Allotmnt and offtake over years", x="Financial Year", y="Weigth in '000 MT") + 
  theme(axis.text.x = element_text(angle=45))

print (paste('Utilisation ratio for nation 2003-2019',sum(state_ao$offtake) / sum(state_ao$allotment)))
print (paste('Foodgrains wasted is 2003-2019', sum(state_ao$allotment) - sum(state_ao$offtake)))

zone_ao = state_ao[c('allotment', 'offtake')] %>% aggregate(by=list(state_ao$zone), sum)
zone_ao$utilisation_ratio = zone_ao$offtake / zone_ao$allotment
zone_ao$gap = zone_ao$allotment - zone_ao$offtake

year_total_ao$utilisation_ratio = year_total_ao$offtake / year_total_ao$allotment

ao_road = inner_join(all_off, road_l, by=c('State.UT', 'year'))
#ao_road$offtake = ao_road$allotment = ao_road$zone = NULL
ao_road = inner_join(ao_road, roadd_sqkm, by=c('State.UT', 'year'))
ao_road = ao_road[which(ao_road$utilisation_ratio < 1 & ao_road$utilisation_ratio > 0),]
fit <- lm(allotment ~ ro_length, ao_road)
summary(fit)
fit
summary(fit)

ao_sh = inner_join(all_off, sh_l, by=c('State.UT', 'year'))
ao_sh$offtake = ao_sh$allotment = ao_sh$zone = NULL
ao_sh = inner_join(ao_sh, sh_d, by=c('State.UT', 'year'))
ao_sh = ao_sh[which(ao_sh$utilisation_ratio < 1 & ao_sh$utilisation_ratio > 0),]
fit <- lm(utilisation_ratio ~ sh_log_length, ao_sh)
summary(fit)
fit
dim(ao_sh)


ao_rail = inner_join(all_off, rw_l, by=c('State.UT', 'year'))
ao_rail$offtake = ao_rail$allotment = ao_rail$zone = NULL
ao_rail = inner_join(ao_rail, rw_d, by=c('State.UT', 'year'))
ao_rail = ao_rail[which(ao_rail$utilisation_ratio < 1 & ao_rail$utilisation_ratio > 0),]
fit <- lm(utilisation_ratio ~ rw_log_length, ao_rail)
summary(fit)
fit
dim(ao_rail)


#Analysis 7 - with state GSDP
ao_gsdp = inner_join(all_off, gsdp, by=c('State.UT', 'year'))
ao_gsdp = ao_gsdp[which(ao_gsdp$utilisation_ratio < 1 & ao_gsdp$utilisation_ratio > 0),]
cor(ao_gsdp$log_gsdp, ao_gsdp$utilisation_ratio)
cor(ao_gsdp$gsdp, ao_gsdp$utilisation_ratio)
fit <- lm(utilisation_ratio ~ log_gsdp, ao_gsdp)
summary(fit)
fit

ao_factors = inner_join(ao_road, ao_sh, by=c('State.UT', 'year', 'utilisation_ratio'))
ao_factors = inner_join(ao_rail, ao_factors, by=c('State.UT', 'year', 'utilisation_ratio'))
ao_factors = inner_join(ao_gsdp, ao_factors, by=c('State.UT', 'year', 'utilisation_ratio'))
fit <- lm(utilisation_ratio ~ ro_log_length + sh_log_length + rw_log_length + log_gsdp, ao_factors)
summary(fit)
#Analysis 8 - With DO Count
sao = sao[which(sao$utilisation_ratio < 1 & sao$utilisation_ratio > 0),]
fit <- lm(utilisation_ratio ~ log(do_count), sao)
summary(fit)
fit

#Analysis 9 - with DCP
cor(df_dcp$dcp, df_dcp$utilisation_ratio)
fit <- lm(df_dcp$utilisation_ratio ~ df_dcp$dcp)
fit

#Performing t-test for DCP [Resource : https://statistics.berkeley.edu/computing/r-t-tests]
dcp1 = df_dcp %>% filter(dcp == 1) %>% select(dcp, utilisation_ratio)
dcp0 = df_dcp %>% filter(dcp == 0) %>% select(dcp, utilisation_ratio)
t.test(dcp1$utilisation_ratio, dcp0$utilisation_ratio)

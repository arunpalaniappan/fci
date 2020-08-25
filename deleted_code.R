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

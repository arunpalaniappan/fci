setwd("C:/Users/ARUN PALANIAPPAN/Desktop/FCI/Final Work")
#This program is used to generate all the required datasets.
rm(list=ls())

#Notes: Orissa should be Odisha, Pondicherry should be puducherry.
suppressPackageStartupMessages(library(openxlsx))
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(e1071)

remove_outliers <- function(data_set, cols)
{
  #cols = names(all_off)[-1:-2]
  #data_set = all_off
  #Columns is a list of columns for which outlier is to be removed
  idxs = c()
  # col = "allotment" - recently removed. Probably this line is useless
  for(col in cols)
  {
    mn = lapply(data_set[col], mean)
    std = sd(data_set[[col]])
    #Outliers are points where are away from mean by 3*std
    idxs = c(idxs, which(data_set[col] > (mn[[1]] + 3*std) | data_set[col] < (mn[[1]] - 3*std)))
  }
  idxs = unique(idxs)
  if(length(idxs) > 0)
    data_set = data_set[-idxs, ]
  return (data_set)
}

# #Stricter removal of outliers
# remove_outliers_strict <- function(data_set, cols)
# {
#   #cols = names(all_off)[-1:-2]
#   #data_set = all_off
#   #Columns is a list of columns for which outlier is to be removed
#   idxs = c()
#   #col = "allotment"
#   for(col in cols)
#   {
#     lb = quantile(data_set[[col]], 0.1)
#     ub = quantile(data_set[[col]], 0.9)
#     #Outliers are points where are away from mean by 3*std
#     idxs = c(idxs, which(data_set[col] > ub | data_set[col] < lb))
#   }
#   idxs = unique(idxs)
#   if(length(idxs) > 0)
#     data_set = data_set[-idxs, ]
#   return (data_set)
# }


#Follows zonal rankings as per FCI
assign_zones <- function(df)
{
  df$zone = 0
  df$zone[which(df$State.UT %in% c("BIHAR", "JHARKHAND", "ORISSA", 
                                   "WEST BENGAL", "SIKKIM"))] = "EAST ZONE"
  df$zone[which(df$State.UT %in% c("ASSAM", "ARUNACHAL", "MEGHALAYA",
                                   "MIZORAM", "TRIPURA", "MANIPUR", "NAGALAND"))] = "N.E ZONE"
  df$zone[which(df$State.UT %in% c("DELHI", "HARYANA", "HIMACHAL", "J & K",
                                   "PUNJAB", "CHANDIGARH", "RAJASTHAN", "UTTAR PR", "UTTRANCHAL"))] = "NORTH ZONE"
  df$zone[which(df$State.UT %in% c("ANDHRA PR", "A&N ISLANDS", "TELANGANA", "KERALA",
                                   "KARNATAKA", "TAMILNADU", "PONDICHERRY", "LAKSHADWEEP"))] = "SOUTH ZONE"
  df$zone[which(df$State.UT %in% c("GUJARAT", "MAHARASHTRA", "GOA", "MADHYA PR",
                                   "CHHATTISGARH", "DAMAN & DIU", "D&N HAVELI"))] = "WEST ZONE"
  return (df)
}

generate_bpl_data <- function(pop, bpl, bpl_cr)
{
  # bpl_cr - bpl change rate
  # bpl_cr > 0 Ex: bpl_cr = 0.3 => bpl(2011, tn) = 19.5%, bpl(2012, tn) = 19.8%
  states = unique(bpl$State.UT)
  for(state in states)
  {
    perc = bpl[which(bpl$State.UT == state & bpl$year == 2011),"percent"]
    for(year in 2012:2019)
    {
      perc = perc + bpl_cr
      new_entry <- data.frame(state, year, perc)
      names(new_entry) <- c("State.UT", "year", "percent")
      bpl <- rbind(bpl, new_entry)
    }
  }
  #Removing entries for andhra pradesh after 2013
  #AP was bifurcated in 2013 and bpl data for 2011 is of united AP
  state = "ANDHRA PR"
  bpl = bpl[-which(bpl$State.UT == state & bpl$year > 2013),]
  #Removing entries where bpl percent is less than 0
  bpl = inner_join(bpl, pop, on=c("State.UT", "year"))
  bpl$bpl_pop = (bpl$percent * bpl$Population / 100)
  bpl = bpl %>% filter(bpl_pop > 0)
  bpl$log_bplpop = log(bpl$bpl_pop)
  return (bpl)
}

rice <- read.xlsx("Data/rice.xlsx")
wheat <- read.xlsx("Data/wheat.xlsx")
rice <- remove_outliers(rice, c("allotment", "offtake"))
wheat <- remove_outliers(wheat, c("allotment", "offtake"))

#Splitting of states into rice consuming and wheat consuming
#rice_state = rice %>% group_by(State.UT) %>% 
 # summarise(allotment = sum(allotment))
rice_summary = rice %>% group_by(State.UT) %>% 
  summarise(allotment = sum(allotment))
names(rice_summary)[2] = "rice_allotment"
#wheat_state = wheat %>% group_by(State.UT) %>% 
  #summarise(allotment = sum(allotment))
wheat_summary = wheat %>% group_by(State.UT) %>% 
  summarise(allotment = sum(allotment))
names(wheat_summary)[2] = "wheat_allotment" 

rice_wheat = inner_join(rice_summary, wheat_summary, by=c('State.UT'))
rice_wheat$perc_rice = rice_wheat$rice_allotment / (rice_wheat$rice_allotment + rice_wheat$wheat_allotment)
rice_wheat$perc_wheat = rice_wheat$wheat_allotment / (rice_wheat$rice_allotment + rice_wheat$wheat_allotment)

#Total allotment offtake
all_off = inner_join(rice, wheat, by=c('State.UT', 'year'))
all_off$offtake = all_off$offtake.x + all_off$offtake.y
all_off$allotment = all_off$allotment.x + all_off$allotment.y
all_off$zone = all_off$zone.x
all_off$offtake.x = all_off$offtake.y = all_off$zone.x = NULL
all_off$allotment.x = all_off$allotment.y = all_off$zone.y = NULL
all_off$utilisation_ratio = all_off$offtake / all_off$allotment
all_off = remove_outliers(all_off, c("allotment", "offtake", "utilisation_ratio"))

#rice$grain = "rice"
#wheat$grain = "wheat"
#rice_wheat = rbind(rice, wheat)

#Summary of total all_off state wise
state_ao = read.xlsx("Data/state_ao.xlsx")

#Getting road length data
road_l <- read.xlsx("Data/Road Length 2010-16.XLSX")
road_l <- gather(road_l, key='year', value='length', -one_of('State.UT'))
road_l <- road_l[complete.cases(road_l),]
road_l$year = as.numeric(road_l$year)
road_l$log_length = log(road_l$length)
road_l <- remove_outliers(road_l, c("length", "log_length"))

#Getting road density data
#Road density was made using dividing length of road and area of state
roadd_sqkm <- read.xlsx("Data/Road Density per sq km 2010-16.xlsx")
roadd_sqkm <- gather(roadd_sqkm, key='year', value='length', -one_of('State.UT'))
roadd_sqkm <- roadd_sqkm[complete.cases(roadd_sqkm),]
roadd_sqkm$log_density = log(roadd_sqkm$length)
roadd_sqkm$year = as.numeric(roadd_sqkm$year)
roadd_sqkm <- remove_outliers(roadd_sqkm, c("length", "log_density"))

#Getting state highway length
sh_l <- read.xlsx("Data/SH Length 2011-16.xlsx")
sh_l <- gather(sh_l, key='year', value='length', -one_of('State.UT'))
sh_l <- sh_l[complete.cases(sh_l),]
sh_l$log_length = log(sh_l$length)
sh_l$year = as.numeric(sh_l$year)
sh_l <- remove_outliers(sh_l, names(sh_l)[-1:-2])

#Getting state highway density
sh_d <- read.xlsx("Data/SH Density 2011-16.xlsx")
sh_d <- gather(sh_d, key='year', value='density', -one_of('State.UT'))
sh_d <- sh_d[complete.cases(sh_d),]
sh_d$log_density = log(sh_d$density)
sh_d$year = as.numeric(sh_d$year)
sh_d <- remove_outliers(sh_d, names(sh_d)[-1:-2])

#Getting railway length data
rw_l <- read.xlsx("Data/Railway Length 2011-17.xlsx")
rw_l <- gather(rw_l, key='year', value='length', -one_of('State.UT'))
rw_l <- rw_l[complete.cases(rw_l),]
rw_l$log_length = log(rw_l$length)
rw_l$year = as.numeric(rw_l$year)
rw_l <- remove_outliers(rw_l, names(rw_l)[-1:-2])

#Getting railway density data
rw_d <- read.xlsx("Data/Railway Density per sq km 2011-17.xlsx")
rw_d <- gather(rw_d, key='year', value='density', -one_of('State.UT'))
rw_d <- rw_d[complete.cases(rw_d),]
rw_d$log_density = log(rw_d$density)
rw_d$year = as.numeric(rw_d$year)

#Getting GSDP data
gsdp <- read.xlsx("Data/GSDP Current Price 2011-20.xlsx")
gsdp = gather(gsdp, key='year', value='gsdp', -one_of('State.UT'))
gsdp$year = substr(gsdp$year, 1, 4)
gsdp = gsdp[complete.cases(gsdp),]
gsdp$log_gsdp = log(gsdp$gsdp)
gsdp$year = as.numeric(gsdp$year)

#Aggregating of years to get state wise data
state_total_ao = all_off %>% group_by(State.UT) %>% 
  summarise(allotment = sum(allotment), offtake = sum(offtake))

#Aggreagating of states to get year wise data
year_total_ao = all_off %>% group_by(year) %>% 
  summarise(allotment = sum(allotment), offtake = sum(offtake))

## Making district office count
#The below code is for making district wise offtake allotment
add_row_entries_sao <- function(sao, idx1, idx2)
{
  sao[idx1,]$allotment = sao[idx1, ]$allotment + sao[idx2, ]$allotment
  sao[idx1,]$offtake = sao[idx1, ]$offtake + sao[idx2, ]$offtake
  sao = sao[-idx2, ]
  return (sao)
}

make_district_office_count <- function(sao, fci_do_count)
{
  sao = add_row_entries_sao(sao, which(sao$State.UT == "PUNJAB"), which(sao$State.UT == "CHANDIGARH"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "TAMILNADU"), which(sao$State.UT == "PONDICHERRY"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "ANDHRA PR"), which(sao$State.UT == "TELANGANA"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "ANDHRA PR"), which(sao$State.UT == "A&N ISLANDS"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "KARNATAKA"), which(sao$State.UT == "LAKSHADWEEP"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "WEST BENGAL"), which(sao$State.UT == "SIKKIM"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "MAHARASHTRA"), which(sao$State.UT == "GOA"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "GUJARAT"), which(sao$State.UT == "D&N HAVELI"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "GUJARAT"), which(sao$State.UT == "DAMAN & DIU"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "MEGHALAYA"), which(sao$State.UT == "MIZORAM"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "MEGHALAYA"), which(sao$State.UT == "TRIPURA"))
  sao = add_row_entries_sao(sao, which(sao$State.UT == "NAGALAND"), which(sao$State.UT == "MANIPUR"))
  sao = inner_join(sao, fci_do_count, by=c("State.UT"))
  return (sao)
}

fci_do_count = read.xlsx("Data/FCI District Office Count.xlsx")
sao <- make_district_office_count(state_total_ao, fci_do_count)
sao$ao_gap = sao$allotment - sao$offtake
sao$utilisation_ratio = sao$offtake / sao$allotment
sao <- remove_outliers(sao, c("utilisation_ratio", "do_count"))

#DCP Analysis
rice$grain = "rice"
wheat$grain = "wheat"
df_dcp = rbind(rice, wheat)
rice$grain = wheat$grain = NULL
df_dcp$dcp = 0
dcp_status <- read.xlsx("Data/DCP Status.xlsx")
dcp_status = dcp_status[complete.cases(dcp_status),]
#wef - with effect from. The year from which dcp is with effect
for(i in 1:dim(dcp_status)[1])
{
  year = dcp_status[i, ]$wef
  state = dcp_status[i, ]$State.UT  
  grain = dcp_status[i, ]$grain
  idxs = which(df_dcp$year >= year & df_dcp$State.UT == state & df_dcp$grain == grain)
  df_dcp[idxs, ]$dcp = 1
}

df_dcp$utilisation_ratio = df_dcp$offtake / df_dcp$allotment
df_dcp <- df_dcp[complete.cases(df_dcp), ]
df_dcp <- df_dcp[is.finite(df_dcp$utilisation_ratio),]
df_dcp <- df_dcp[which(df_dcp$utilisation_ratio <= 1), ]
df_dcp <- remove_outliers(df_dcp, c("utilisation_ratio"))


#Reading population data
pop <- read.xlsx("Data/projected_population_by_state_2012_2036.xlsx")
pop$log_pop = log(pop$Population)

#Reading BPL Population data 
bpl_perc2011 <- read.xlsx("Data/BPL data.xlsx")
names(bpl_perc2011)[2] = "percent"
bpl_perc2011$year = 2011
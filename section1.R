setwd("C:/Users/ARUN PALANIAPPAN/Desktop/FCI/Final Work")

suppressPackageStartupMessages(library(openxlsx))
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(e1071)

subsidy_data <- read.xlsx("data/Food Subsidy.xlsx")

names(subsidy_data)
names(subsidy_data)[which(names(subsidy_data) == "subsidy.released.for.current.year")] = "sr_cy"
names(subsidy_data)[which(names(subsidy_data) == "subsidy.incurred.current.year")] = "si_cy"

#Extract two columns from the data, make two new data frames with a factor and combine them and plot in them
ggplot(subsidy_data, mapping = aes(x=year)) +
  geom_line(aes(y=sr_cy, group=1), size=2, color="green", labels="Subsidy Released") + 
  geom_line(aes(y=si_cy, group=1), size=2, color="blue", labels="Subsidy Incurred") + 
  labs(title="Subsidy incurred and subsidy released", x="Financial Year", y="Rupees in Crores") + 
  theme(axis.text.x = element_text(angle=45))


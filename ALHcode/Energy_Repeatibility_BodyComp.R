### Energy Density replicates

rm(list = ls())
Data_lower <- read.csv("ALHcode/AIC/lowerriver_bodycomp_all.csv")
Data_upper <- read.csv("ALHcode/AIC/upperriver_bodycomp_all.csv")

str(Data_lower)
str(Data_upper)


#subset duplicates
# Lower dataset
duplicates_lower <- Data_lower[rowSums(!is.na(Data_lower[, c("EnergyPDry_1","EnergyPDry_2","EnergyPDry_3")])) > 1, ]

# Upper dataset
duplicates_upper <- Data_upper[rowSums(!is.na(Data_upper[, c("EnergyPDry_1","EnergyPDry_2","EnergyPDry_3")])) > 1, ]


#pairwise correlations
cor(duplicates_lower[, c("EnergyPDry_1","EnergyPDry_2","EnergyPDry_3")], use="pairwise.complete.obs") #all above 0.9

cor(duplicates_upper[, c("EnergyPDry_1","EnergyPDry_2","EnergyPDry_3")], use="pairwise.complete.obs") #all above 0.9


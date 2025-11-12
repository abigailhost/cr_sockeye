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




#Calculating Coefficient of Variation
# Lower dataset
duplicates_lower$CV_EnergyPDry <- apply(
  duplicates_lower[, c("EnergyPDry_1", "EnergyPDry_2", "EnergyPDry_3")],
  1,
  function(x) {
    x <- x[!is.na(x)]             # remove NAs
    if (length(x) > 1) {
      (sd(x) / mean(x))    # CV 
    } else {
      NA
    }
  }
)

# Upper dataset
duplicates_upper$CV_EnergyPDry <- apply(
  duplicates_upper[, c("EnergyPDry_1", "EnergyPDry_2", "EnergyPDry_3")],
  1,
  function(x) {
    x <- x[!is.na(x)]
    if (length(x) > 1) {
      (sd(x) / mean(x))
    } else {
      NA
    }
  }
)


summary(duplicates_lower$CV_EnergyPDry) #summary, plus min/mix
summary(duplicates_upper$CV_EnergyPDry)

#Mean Coefficients for data sets (upper vs lower)
mean(duplicates_lower$CV_EnergyPDry, na.rm = TRUE) #0.0197 or 1.97%
mean(duplicates_upper$CV_EnergyPDry, na.rm = TRUE) #0.0395 or 3.95%



#histogram of distribution
hist(duplicates_lower$CV_EnergyPDry * 100,
     main = "EnergyPDry Triplicate CVs (%)",
     xlab = "Coefficient of Variation (%)",
     col = "steelblue")

hist(duplicates_upper$CV_EnergyPDry * 100,
     main = "EnergyPDry Triplicate CVs (%)",
     xlab = "Coefficient of Variation (%)",
     col = "steelblue")



### Using Benzoic Acid Standards
rm(list = ls())
standards <- read.csv("ALHcode/odata/bodycomp_benzoicacidstandards_2019-2021.csv")
str(standards)

standards$EnergyDensity_mJ.g <- standards$EnergyDensity_mJ.kg_1 / 1000


cv_energy_density <- sd(standards$EnergyDensity_mJ.g, na.rm = TRUE) / 
  mean(standards$EnergyDensity_mJ.g, na.rm = TRUE)

cv_energy_density

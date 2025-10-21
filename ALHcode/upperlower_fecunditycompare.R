#Fecundity comparison between group assignments in lower river and group assignments in upper river

## parameter estimates from lower river, absolute estimates from upper river

rm(list = ls())

Upper <- read.csv("ALHcode/AIC/upperriver_bodycomp_all.csv") #total data set
Lower <- read.csv("ALHcode/AIC/fecundity_GSI/FecundityEstimates_GroupAssignments.csv")[-1]
str(Lower)


### Absolute average calculations for upper river ###
str(Upper)

Upper <- Upper %>%
  filter(!Collection_Location %in% c("Power Creek Cordova", "Eyak Weir Cordova", "Bone Creek", "Copper Lake", "Klutina River", "Upper Klutina"))
UpperF <- subset(Upper, Sex == "F")
UpperF$Group_Assignment <- as.factor(UpperF$Group_Assignment)
levels(UpperF$Group_Assignment)

# convert gonad_wt to grams from kilograms
UpperF$Gonad_Wt_g <- UpperF$Gonad_Wt * 1000

# filter out rows where gonad weight is zero or missing
UpperF <- subset(UpperF, Gonad_Wt_g > 0 & !is.na(Gonad_Wt_g))

# now calculate fecundity
UpperF$Fecun_num_by_wt1 <- (UpperF$Fem_Fecun_1_num * UpperF$Gonad_Wt_g) / UpperF$Fem_Fecun_1_wt
str(UpperF)

## Table of absolute averages for upper river fecundity according to group assignment
library(dplyr)

fecundity_means <- tapply(UpperF$Fecun_num_by_wt1,
                          UpperF$Group_Assignment,
                          mean,
                          na.rm = TRUE)

UpperF_means_df <- data.frame(
  Group_Assignment = names(fecundity_means),
  Upper_mean_fecundity = as.numeric(fecundity_means)
)

UpperF_means_df


install.packages("dplyr")
## Upper Lower Difference! 
Lower <- dplyr::rename(Lower, Group_Assignment = GSI)

#join data sets
fecundity_compare <- full_join(Lower, UpperF_means_df, by = "Group_Assignment")
fecundity_compare <- dplyr::select(fecundity_compare, Group_Assignment, Estimated_Fecundity, Upper_mean_fecundity)
fecundity_compare <- dplyr::rename(fecundity_compare, Lower_estimated_Fecundity = Estimated_Fecundity)
fecundity_compare

#comparison: 
fecundity_compare <- fecundity_compare %>%
  mutate(
  difference = Upper_mean_fecundity - Lower_estimated_Fecundity,
  percent_difference = (Upper_mean_fecundity - Lower_estimated_Fecundity) / Lower_estimated_Fecundity * 100
)

fecundity_compare


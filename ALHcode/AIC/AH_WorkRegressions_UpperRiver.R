### Linear Regression between body size ~ Work AND energy density ~ Work to see if there is a linear relationship between these


rm(list = ls())
DataSet <- read.csv("ALHcode/AIC/upperriver_bodycomp_PCscores.csv")

library(ggplot2)

DataSet$Work_km2 <- (DataSet$Collection_RiverMile_m * DataSet$Elevation_m) / (1000*1000) #so work is in km^2 
library(dplyr)

DataSet_filtered <- DataSet %>%
  filter(Collection_Location != "Long Lake")

str(DataSet_filtered)

###### WORK ########

#Body Size Question
model <- lm(bodysize_pc_g_mm ~ Work_km2, data = DataSet_filtered)
summary(model) #not significant

ggplot(DataSet_filtered, aes(x = Work_km2, y = bodysize_pc_g_mm)) + 
  geom_point(aes(color = Group_Assignment)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()




#EnergyPDry_1 Question
model1 <- lm(EnergyPDry_1 ~ Work_km2, data = DataSet_filtered)
summary(model1) #### SIGNIFICANT!!!!!! p = 1.04e-09

ggplot(DataSet_filtered, aes(x = Work_km2, y = EnergyPDry_1)) + 
  geom_point(aes(color = Collection_Location)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()

ggplot(DataSet_filtered, aes(x = Work_km2, y = EnergyPDry_1)) + 
  geom_point(aes(color = Group_Assignment)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()


#TotalEnergy Question
DataSet_filtered$EnergyPDry_1_mJ_g <- DataSet_filtered$EnergyPDry_1 / 1000
DataSet_filtered$Fish_Wt_g <- DataSet_filtered$Fish_Wt * 1000
DataSet_filtered$TotalEnergy_mJ <- DataSet_filtered$EnergyPDry_1_mJ_g * DataSet_filtered$Fish_Wt_g
str(DataSet_filtered)

model2 <- lm(TotalEnergy_mJ ~ Work_km2, data = DataSet_filtered)
summary(model2) #Significant!!!!!! p = 1.65e-05 ***

ggplot(DataSet_filtered, aes(x = Work_km2, y = TotalEnergy_mJ)) + 
  geom_point(aes(color = Collection_Location)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()

ggplot(DataSet_filtered, aes(x = Work_km2, y = TotalEnergy_mJ)) + 
  geom_point(aes(color = Group_Assignment)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()


##### Group_Assignment #####


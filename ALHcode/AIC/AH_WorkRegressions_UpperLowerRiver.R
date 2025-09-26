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




### LOWER RIVER SAME ANALYSIS ###
rm(list = ls())
DataSet <- read.csv("ALHcode/AIC/lowerriver_GSI_PCscores.csv")

DataSet$Group_Assignment <- factor(DataSet$Group_Assignment)

# Clean up levels manually
levels(DataSet$Group_Assignment)[levels(DataSet$Group_Assignment) == "Gulkana "] <- "Gulkana"
levels(DataSet$Group_Assignment)[levels(DataSet$Group_Assignment) == "Gulkanahatchery"] <- "GulkanaHatchery"
levels(DataSet$Group_Assignment)[levels(DataSet$Group_Assignment) == "KlutinaTonsinaOutlets"] <- "KlutinaTonsinaOutlet"
levels(DataSet$Group_Assignment)[levels(DataSet$Group_Assignment) == "insufficient genotypes"] <- "insufficient_genotypes"

# Then drop the insufficient genotypes
DataSet <- subset(DataSet, Group_Assignment != "insufficient_genotypes")
DataSet$Group_Assignment <- droplevels(DataSet$Group_Assignment)

levels(DataSet$Group_Assignment)

DataSet_filtered <- DataSet %>%
  filter(!Group_Assignment  %in% c("Chitina", "Bremner", "Tazlina", "LowerGroups"))
DataSet_filtered$Group_Assignment <- droplevels(DataSet_filtered$Group_Assignment)
levels(DataSet_filtered$Group_Assignment)


DataSet_filtered$Group_Assignment <- factor(DataSet_filtered$Group_Assignment, 
                                            levels = c("KlutinaTonsinaOutlet",
                                                       "KlutinaLake",
                                                       "Slana",
                                                       "TanadaCopperLakes",
                                                       "GulkanaHatchery",
                                                       "Gulkana"))
                                            

#Body Size Question
model3 <- lm(bodysize_pc_g_mm ~ Group_Assignment, data = DataSet_filtered)
summary(model3) #not significant

ggplot(DataSet_filtered, aes(x = Group_Assignment, y = bodysize_pc_g_mm, color = Group_Assignment)) + 
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, aes(group = Group_Assignment), color = "black") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),                          # Increase all font sizes
    axis.text.x = element_text(angle = 45, hjust = 1)        # Slant x-axis labels
  )
#### NOT technically proper graphing as the x-axis is categorical. but at least visualizes a trend




#EnergyPDry_1 Question
model4 <- lm(EnergyPDry_1 ~ Group_Assignment, data = DataSet_filtered)
summary(model4) #### SIGNIFICANT!!!!!! each group_assignment is significant for energy density

ggplot(DataSet_filtered, aes(x = Group_Assignment, y = EnergyPDry_1, color = Group_Assignment)) + 
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, aes(group = Group_Assignment), color = "black") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),                          # Increase all font sizes
    axis.text.x = element_text(angle = 30, hjust = 1)        # Slant x-axis labels
  )


#TotalEnergy Question
DataSet_filtered$EnergyPDry_1_mJ_g <- DataSet_filtered$EnergyPDry_1 / 1000
DataSet_filtered$Fish_Wt_g <- DataSet_filtered$Fish_Wt * 1000
DataSet_filtered$TotalEnergy_mJ <- DataSet_filtered$EnergyPDry_1_mJ_g * DataSet_filtered$Fish_Wt_g
str(DataSet_filtered)

model5 <- lm(TotalEnergy_mJ ~ Group_Assignment, data = DataSet_filtered)
summary(model5) #TCL and GH have significantly larger total energy, compared to intercept and significant p-values

ggplot(DataSet_filtered, aes(x = Group_Assignment, y = TotalEnergy_mJ, color = Group_Assignment)) + 
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, aes(group = Group_Assignment), color = "black") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),                          # Increase all font sizes
    axis.text.x = element_text(angle = 30, hjust = 1)        # Slant x-axis labels
  )


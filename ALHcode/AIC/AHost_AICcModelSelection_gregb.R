#' =====================================================
#' Section 1: AICc code from Greg's Class, Fish_Leng_1~RunTimingGroup test
#' =====================================================

# AICmodavg way of doing things
install.packages("AICcmodavg")
library(AICcmodavg)
library(lubridate)

#data loading
bc19 <- read.csv("ALHcode/AIC/bodycomp_2019.csv")
bc20 <- read.csv("ALHcode/AIC/bodycomp_2020.csv")
bc21 <- read.csv("ALHcode/AIC/bodycomp_2021.csv")
#need to bind all together for total body comp dataset, lower river only

bc_LR_total <- rbind(bc19[1:60,], bc20[1:60,], bc21[1:60,])
write.csv(bc_LR_total, "ALHcode/AIC/lowerriver_bodycomp_all.csv")
lowerriver_bodycomp_all<-read.csv("ALHcode/AIC/lowerriver_bodycomp_all.csv")[,-1]
#all data is loaded now

#need to add year to dataset
lowerriver_bodycomp_all$Collection_Date <- as.Date(lowerriver_bodycomp_all$Collection_Date)

# Create a new column 'year' from the collection_date
lowerriver_bodycomp_all$Year <- year(lowerriver_bodycomp_all$Collection_Date)

write.csv(lowerriver_bodycomp_all, "ALHcode/AIC/lowerriver_bodycomp_all.csv")


lowerriver_bodycomp_all <- "ALHcode/AIC/lowerriver_bodycomp_all.csv"
Fish_Leng_1.RunTimingGroup.modelset <- "ALHcode/AIC/Fish_Leng_1~RunTimingGroup.csv"

# Import the data. Be sure to check the structure of the data and that R is reading continuous and categorical variables appropriately.
DataSet<-read.csv(lowerriver_bodycomp_all)
#DataSet<-read.csv("ADPE.SSD.IsoVarSex.DataSet.Sub3.csv")

# View DataSet.
head(DataSet)
summary(DataSet)
nrow(DataSet)
str(DataSet)

# Change parameter structures if needed.
DataSet$Collection_Location <- as.factor(DataSet$Collection_Location)
DataSet$RunTimingGroup <- as.factor(DataSet$RunTimingGroup)
DataSet$Year<- as.factor(DataSet$Year)
DataSet$Sex<- as.factor(DataSet$Sex)
str(DataSet)
DataSet

# Import candidate model set and convert to a character vector.
ModelSet.df<-read.csv(Fish_Leng_1.RunTimingGroup.modelset)  
ModelSet<-as.character(ModelSet.df$Model)
str(ModelSet)
ModelSet

# Making Models
model1 <- lm(Fish_Leng_1~1, data=DataSet)
model2 <- lm(Fish_Leng_1~Collection_Location, data = DataSet)
model3 <- lm(Fish_Leng_1~Collection_Location + RunTimingGroup, data = DataSet)
model4 <- lm(Fish_Leng_1~Collection_Location + Sex, data = DataSet)
model5 <- lm(Fish_Leng_1~Collection_Location + Year, data = DataSet)
model6 <- lm(Fish_Leng_1~Collection_Location + RunTimingGroup + Sex, data = DataSet)
model7 <- lm(Fish_Leng_1~Collection_Location + RunTimingGroup + Year, data = DataSet)
model8 <- lm(Fish_Leng_1~Collection_Location + Sex + Year, data = DataSet)
model9 <- lm(Fish_Leng_1~Collection_Location + RunTimingGroup + Sex + RunTimingGroup:Sex, data = DataSet)
model10 <- lm(Fish_Leng_1~Collection_Location + RunTimingGroup + Year + RunTimingGroup:Year, data = DataSet)
model11 <- lm(Fish_Leng_1~Collection_Location + Sex + Year + Sex:Year, data = DataSet)
model12 <- lm(Fish_Leng_1~Collection_Location + RunTimingGroup + Sex + Year, data = DataSet)


AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12)
#table of AIC results
mynames <- paste("model", as.character(1:12), sep = "")
myaicc <- aictab(list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12), modnames = mynames)
print(myaicc, LL = FALSE)

#when the response variable is fish_leng_1, the best model is the model8 which uses main factors sex and year, but has no interactions



#' =====================================================
#' Section 2: AICc code from Greg's Class, BodySizePC_1~RunTimingGroup test
#' =====================================================
lowerriver_bodycomp_all<-read.csv("ALHcode/AIC/lowerriver_bodycomp_all.csv")[,-1]

any(is.na(lowerriver_bodycomp_all$Fish_Leng_2))
any(is.na(lowerriver_bodycomp_all$Fish_Ht))
any(is.na(lowerriver_bodycomp_all$Fish_Grth)) #Na's present, will need to omit them for PC score
lowerriver_bodycomp_all[is.na(lowerriver_bodycomp_all$Fish_Grth), ] #row 64 has NA, remove for PC

lowerriver_bodycomp_all_pc <- lowerriver_bodycomp_all[-64,] #new dataframe with no NA's for bodysize metrics

bc_pca1 <- princomp(lowerriver_bodycomp_all_pc[,c(12,14,16,19)], cor=T, scores=T, covmat = NULL) #should work now without any NAs
summary(bc_pca1, loadings=T, cutoff=0.0001) #summary of PC analysis
bc_pca1$scores
screeplot(bc_pca1, type=c('lines'))
bodysize_pc <- bc_pca1$scores[,1]

#combine dataframe with column for body size PC scores
lowerriver_bodycomp_pc_new <- cbind(lowerriver_bodycomp_all_pc, bodysize_pc) 

#Now, run AIC again but PC1 scores are the response variable / numeric
write.csv(lowerriver_bodycomp_pc_new, "ALHcode/AIC/lowerriver_bodycomp_PCscores.csv")
lowerriver_bodycomp_PCscores <- "ALHcode/AIC/lowerriver_bodycomp_PCscores.csv"

# Import the data. Be sure to check the structure of the data and that R is reading continuous and categorical variables appropriately.
DataSet<-read.csv(lowerriver_bodycomp_PCscores)[,-1]

# View DataSet.
head(DataSet)
summary(DataSet)
nrow(DataSet)
str(DataSet)

DataSet$Collection_Location <- as.factor(DataSet$Collection_Location)
DataSet$RunTimingGroup <- as.factor(DataSet$RunTimingGroup)
DataSet$Year<- as.factor(DataSet$Year)
DataSet$Sex<- as.factor(DataSet$Sex)
str(DataSet)

# Making Models for bodysize_pc~RunTimingGroup
model1 <- lm(bodysize_pc~1, data=DataSet)
model2 <- lm(bodysize_pc~Collection_Location, data = DataSet)
model3 <- lm(bodysize_pc~Collection_Location + RunTimingGroup, data = DataSet)
model4 <- lm(bodysize_pc~Collection_Location + Sex, data = DataSet)
model5 <- lm(bodysize_pc~Collection_Location + Year, data = DataSet)
model6 <- lm(bodysize_pc~Collection_Location + RunTimingGroup + Sex, data = DataSet)
model7 <- lm(bodysize_pc~Collection_Location + RunTimingGroup + Year, data = DataSet)
model8 <- lm(bodysize_pc~Collection_Location + Sex + Year, data = DataSet)
model9 <- lm(bodysize_pc~Collection_Location + RunTimingGroup + Sex + RunTimingGroup:Sex, data = DataSet)
model10 <- lm(bodysize_pc~Collection_Location + RunTimingGroup + Year + RunTimingGroup:Year, data = DataSet)
model11 <- lm(bodysize_pc~Collection_Location + Sex + Year + Sex:Year, data = DataSet)
model12 <- lm(bodysize_pc~Collection_Location + RunTimingGroup + Sex + Year, data = DataSet)


AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12)

#table of AIC results
mynames1 <- paste("model", as.character(1:12), sep = "")
models <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12)

# Generate AIC table
myaicc1 <- aictab(models, modnames = mynames1)
print(myaicc1)
# Convert AIC table to a data frame for easier manipulation
aic_df <- as.data.frame(myaicc1)
aic_df$ModelName <- c("bodysize_pc~Collection_Location + RunTimingGroup + Sex + Year","bodysize_pc~Collection_Location + RunTimingGroup + Sex","bodysize_pc~Collection_Location + RunTimingGroup + Sex + RunTimingGroup:Sex","bodysize_pc~Collection_Location + Sex + Year","bodysize_pc~Collection_Location + Sex","bodysize_pc~Collection_Location + RunTimingGroup + Year","bodysize_pc~Collection_Location + RunTimingGroup","bodysize_pc~Collection_Location + Sex + Year + Sex:Year","bodysize_pc~Collection_Location + RunTimingGroup + Year + RunTimingGroup:Year","bodysize_pc~Collection_Location","bodysize_pc~Collection_Location + Year","bodysize_pc~1")
colnames(aic_df)[colnames(aic_df) == "Modnames"] <- "Model #"

# Write the data frame to a CSV file
write.csv(aic_df, file = "ALHcode/AIC/AICresults_bodysize~RunTimingGroup.csv", row.names = FALSE)


#for body size, when the variable is a PC score of all body metrics, the best model as chosen by AIC is the global model including all main factors but no interactions

#### CHECK DIRECTION OF PC SCORE ###
library(ggplot2)
ggplot(DataSet, aes(x=Fish_Leng_1, y=bodysize_pc)) +
  geom_point()
#so direction of PC score is positive, check with other variables

ggplot(DataSet, aes(x=Fish_Grth, y=bodysize_pc)) +
  geom_point()

ggplot(DataSet, aes(x=Fish_Ht, y=bodysize_pc)) +
  geom_point()
# direction of PC score is all positive according to these plots








#' =====================================================
#' Section 3: AICc code from Greg's Class, scBodyMass_1~RunTimingGroup test
#' =====================================================

#re-load data: 
DataSet<-read.csv("ALHcode/AIC/lowerriver_bodycomp_all.csv")[,-1]
str(DataSet)
DataSet$Collection_Location <- as.factor(DataSet$Collection_Location)
DataSet$RunTimingGroup <- as.factor(DataSet$RunTimingGroup)
DataSet$Year<- as.factor(DataSet$Year)
DataSet$Sex<- as.factor(DataSet$Sex)
str(DataSet)

#calculated size corrected body mass from DataSet ----- residuals of a linear regression of weight ~ length
DataSet$Fish_Leng_Total <- DataSet$Fish_Leng_1 + DataSet$Fish_Leng_2
#row 80 seems incorrect-- length_2 snout to eye is 41.0 cm, too long-- eliminate for this analysis
DataSet <- DataSet[-57,]
sizecorrected_bodymass_lm <- lm(DataSet$Fish_Wt ~ DataSet$Fish_Leng_Total)
sizecorrected_bodymass_residuals <- residuals(sizecorrected_bodymass_lm)
plot(sizecorrected_bodymass_residuals, DataSet$Fish_Leng_Total)
sizecorrected_bodymass <- as.data.frame(sizecorrected_bodymass_residuals)
DataSet <- cbind(DataSet, sizecorrected_bodymass) #now I have size corrected residuals assigned as new variable in the DataSet

#Set up Models
scModel1 <- lm(sizecorrected_bodymass_residuals ~ 1, data = DataSet)
scModel2 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location, data = DataSet)
scModel3 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup, data = DataSet)
scModel4 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + Sex, data = DataSet)
scModel5 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + Year, data = DataSet)
scModel6 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Sex, data = DataSet)
scModel7 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Year, data = DataSet)
scModel8 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + Sex + Year, data = DataSet)
scModel9 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Sex + RunTimingGroup:Sex, data = DataSet)
scModel10 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Year + RunTimingGroup:Year, data = DataSet)
scModel11 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + Sex + Year + Sex:Year, data = DataSet)
scModel12 <- lm(sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Sex + Year, data = DataSet)

AIC(scModel1, scModel2, scModel3, scModel4, scModel5, scModel6, scModel7, scModel8, scModel9, scModel10, scModel11, scModel12)
scModels <- list(scModel1, scModel2, scModel3, scModel4, scModel5, scModel6, scModel7, scModel8, scModel9, scModel10, scModel11, scModel12)

# Generate AIC table
mynames2 <- paste("model", as.character(1:12), sep = "")
myaicc2 <- aictab(scModels, modnames = mynames2)
print(myaicc2) #so model 3 is the best for size corrected bodymass
# Convert AIC table to a data frame for easier manipulation
aic_df <- as.data.frame(myaicc2)
aic_df$ModelName <- c("sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup", "sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Year", "sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Sex + RunTimingGroup:Sex", "sizecorrected_bodymass_residuals ~ Collection_Location", "sizecorrected_bodymass_residuals ~ Collection_Location + Year", "sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Sex", "sizecorrected_bodymass_residuals ~ Collection_Location + Sex + Year + Sex:Year","sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Sex + Year", "sizecorrected_bodymass_residuals ~ Collection_Location + Sex", "sizecorrected_bodymass_residuals ~ Collection_Location + Sex + Year", "sizecorrected_bodymass_residuals ~ Collection_Location + RunTimingGroup + Year + RunTimingGroup:Year", "sizecorrected_bodymass_residuals ~ 1")
colnames(aic_df)[colnames(aic_df) == "Modnames"] <- "Model #"

# Write the data frame to a CSV file
write.csv(aic_df, file = "ALHcode/AIC/AICresults_SizeCorrectedBodyMass~RunTimingGroup.csv", row.names = FALSE)













#' =====================================================
#' Section 4: AICc code from Greg's Class, EnergyDensity~RunTimingGroup test
#' =====================================================
DataSet<-read.csv("ALHcode/AIC/lowerriver_bodycomp_all.csv")[,-1]
str(DataSet)
DataSet$Collection_Location <- as.factor(DataSet$Collection_Location)
DataSet$RunTimingGroup <- as.factor(DataSet$RunTimingGroup)
DataSet$Year<- as.factor(DataSet$Year)
DataSet$Sex<- as.factor(DataSet$Sex)
str(DataSet)


edModel1 <- lm(EnergyDensity_mJ.kg_1 ~ 1, data = DataSet)
edModel2 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location, data = DataSet)
edModel3 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup, data = DataSet)
edModel4 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + Sex, data = DataSet)
edModel5 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + Year, data = DataSet)
edModel6 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Sex, data = DataSet)
edModel7 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Year, data = DataSet)
edModel8 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + Sex + Year, data = DataSet)
edModel9 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Sex + RunTimingGroup:Sex, data = DataSet)
edModel10 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Year + RunTimingGroup:Year, data = DataSet)
edModel11 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + Sex + Year + Sex:Year, data = DataSet)
edModel12 <- lm(EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Sex + Year, data = DataSet)

AIC(edModel1, edModel2, edModel3, edModel4, edModel5, edModel6, edModel7, edModel8, edModel9, edModel10, edModel11, edModel12)
edModels <- list(edModel1, edModel2, edModel3, edModel4, edModel5, edModel6, edModel7, edModel8, edModel9, edModel10, edModel11, edModel12)

# Generate AIC table
mynames3 <- paste("model", as.character(1:12), sep = "")
myaicc3 <- aictab(edModels, modnames = mynames3)
print(myaicc3) #so model 10 is the best, followed closely by model 3 for energy density as response variable
# Convert AIC table to a data frame for easier manipulation
aic_df <- as.data.frame(myaicc3)
aic_df$ModelName <- c("EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Year + RunTimingGroup:Year", "EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup", "EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Year", "EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Sex", "EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Sex + RunTimingGroup:Sex", "EnergyDensity_mJ.kg_1 ~ Collection_Location + RunTimingGroup + Sex + Year", "EnergyDensity_mJ.kg_1 ~ Collection_Location", "EnergyDensity_mJ.kg_1 ~ Collection_Location + Year", "EnergyDensity_mJ.kg_1 ~ Collection_Location + Sex", "EnergyDensity_mJ.kg_1 ~ Collection_Location + Sex + Year", "EnergyDensity_mJ.kg_1 ~ Collection_Location + Sex + Year + Sex:Year", "EnergyDensity_mJ.kg_1 ~ 1")
colnames(aic_df)[colnames(aic_df) == "Modnames"] <- "Model #"

write.csv(aic_df, file = "ALHcode/AIC/AICresults_EnergyDensity~RunTimingGroup.csv", row.names = FALSE)














#' =====================================================
#' Section 5: AICc code from Greg's Class, Total Energy~RunTimingGroup test
#' =====================================================
DataSet<-read.csv("ALHcode/AIC/lowerriver_bodycomp_all.csv")[,-1]
str(DataSet)
DataSet$Collection_Location <- as.factor(DataSet$Collection_Location)
DataSet$RunTimingGroup <- as.factor(DataSet$RunTimingGroup)
DataSet$Year<- as.factor(DataSet$Year)
DataSet$Sex<- as.factor(DataSet$Sex)
str(DataSet)

#make total energy metric (Energy Density * Weight)
DataSet$TotalEnergy <- (DataSet$EnergyDensity_mJ.kg_1) * (DataSet$Fish_Wt)

#Models
teModel1 <- lm(TotalEnergy ~ 1, data = DataSet)
teModel2 <- lm(TotalEnergy ~ Collection_Location, data = DataSet)
teModel3 <- lm(TotalEnergy ~ Collection_Location + RunTimingGroup, data = DataSet)
teModel4 <- lm(TotalEnergy ~ Collection_Location + Sex, data = DataSet)
teModel5 <- lm(TotalEnergy ~ Collection_Location + Year, data = DataSet)
teModel6 <- lm(TotalEnergy ~ Collection_Location + RunTimingGroup + Sex, data = DataSet)
teModel7 <- lm(TotalEnergy ~ Collection_Location + RunTimingGroup + Year, data = DataSet)
teModel8 <- lm(TotalEnergy ~ Collection_Location + Sex + Year, data = DataSet)
teModel9 <- lm(TotalEnergy ~ Collection_Location + RunTimingGroup + Sex + RunTimingGroup:Sex, data = DataSet)
teModel10 <- lm(TotalEnergy ~ Collection_Location + RunTimingGroup + Year + RunTimingGroup:Year, data = DataSet)
teModel11 <- lm(TotalEnergy ~ Collection_Location + Sex + Year + Sex:Year, data = DataSet)
teModel12 <- lm(TotalEnergy ~ Collection_Location + RunTimingGroup + Sex + Year, data = DataSet)

AIC(teModel1, teModel2, teModel3, teModel4, teModel5, teModel6, teModel7, teModel8, teModel9, teModel10, teModel11, teModel12)
teModels <- list(teModel1, teModel2, teModel3, teModel4, teModel5, teModel6, teModel7, teModel8, teModel9, teModel10, teModel11, teModel12)

mynames4 <- paste("model", as.character(1:12), sep = "")
myaicc4 <- aictab(teModels, modnames = mynames4)
print(myaicc4)

aic_df <- as.data.frame(myaicc4)
aic_df$ModelName <- c("TotalEnergy ~ Collection_Location + RunTimingGroup + Sex + Year", "TotalEnergy ~ Collection_Location + RunTimingGroup + Sex", "TotalEnergy ~ Collection_Location + Sex", "TotalEnergy ~ Collection_Location + Sex + Year", "TotalEnergy ~ Collection_Location + RunTimingGroup + Sex + RunTimingGroup:Sex", "TotalEnergy ~ Collection_Location + Sex + Year + Sex:Year", "TotalEnergy ~ Collection_Location + RunTimingGroup + Year", "TotalEnergy ~ Collection_Location + RunTimingGroup", "TotalEnergy ~ Collection_Location", "TotalEnergy ~ Collection_Location + Year", "TotalEnergy ~ Collection_Location + RunTimingGroup + Year + RunTimingGroup:Year", "TotalEnergy ~ 1")
colnames(aic_df)[colnames(aic_df) == "Modnames"] <- "Model #"

write.csv(aic_df, file = "ALHcode/AIC/AICresults_TotalEnergy~RunTimingGroup.csv", row.names = FALSE)














#' =====================================================
#' Section 6: AICc code from Greg's Class, GonadMass~RunTimingGroup test
#' =====================================================
DataSet<-read.csv("ALHcode/AIC/lowerriver_bodycomp_PCscores.csv")[,-1]
str(DataSet)
DataSet$Collection_Location <- as.factor(DataSet$Collection_Location)
DataSet$RunTimingGroup <- as.factor(DataSet$RunTimingGroup)
DataSet$Year<- as.factor(DataSet$Year)
DataSet$Sex<- as.factor(DataSet$Sex)
str(DataSet)


gmModel1 <- lm(Gonad_Wt~1, data = DataSet)
gmModel2 <- lm(Gonad_Wt~Collection_Location, data = DataSet)
gmModel3 <- lm(Gonad_Wt~Collection_Location + bodysize_pc, data = DataSet)
gmModel4 <- lm(Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1, data = DataSet)
gmModel5 <- lm(Gonad_Wt~Collection_Location + RunTimingGroup, data = DataSet)
gmModel6 <- lm(Gonad_Wt~Collection_Location + Sex, data = DataSet)
gmModel7 <- lm(Gonad_Wt~Collection_Location + Year, data = DataSet)
gmModel8 <- lm(Gonad_Wt~Collection_Location + bodysize_pc + EnergyDensity_mJ.kg_1, data = DataSet)
gmModel9 <- lm(Gonad_Wt~Collection_Location + bodysize_pc*EnergyDensity_mJ.kg_1, data = DataSet)
gmModel10 <- lm(Gonad_Wt~Collection_Location + bodysize_pc + RunTimingGroup, data = DataSet)
gmModel11 <- lm(Gonad_Wt~Collection_Location + bodysize_pc*RunTimingGroup, data = DataSet)
gmModel12 <- lm(Gonad_Wt~Collection_Location + bodysize_pc + Sex, data = DataSet)
gmModel13 <- lm(Gonad_Wt~Collection_Location + bodysize_pc*Sex, data = DataSet)
gmModel14 <- lm(Gonad_Wt~Collection_Location + bodysize_pc + Year, data = DataSet)
gmModel15 <- lm(Gonad_Wt~Collection_Location + bodysize_pc*Year, data = DataSet)
gmModel16 <- lm(Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1 + RunTimingGroup, data = DataSet)
gmModel17 <- lm(Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1*RunTimingGroup, data = DataSet)
gmModel18 <- lm(Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1 + Sex, data = DataSet)
gmModel19 <- lm(Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1*Sex, data = DataSet)
gmModel20 <- lm(Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1 + Year, data = DataSet)
gmModel21 <- lm(Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1*Year, data = DataSet)
gmModel22 <- lm(Gonad_Wt~Collection_Location + RunTimingGroup + Sex, data = DataSet)
gmModel23 <- lm(Gonad_Wt~Collection_Location + RunTimingGroup*Sex, data = DataSet)
gmModel24 <- lm(Gonad_Wt~Collection_Location + RunTimingGroup + Year, data = DataSet)
gmModel25 <- lm(Gonad_Wt~Collection_Location + RunTimingGroup*Year, data = DataSet)
gmModel26 <- lm(Gonad_Wt~Collection_Location + Sex + Year, data = DataSet)
gmModel27 <- lm(Gonad_Wt~Collection_Location + Sex*Year, data = DataSet)
gmModel28 <- lm(Gonad_Wt~Collection_Location + (bodysize_pc + EnergyDensity_mJ.kg_1 + RunTimingGroup + Sex + Year)^2, data = DataSet)
gmModel29 <- lm(Gonad_Wt~Collection_Location + bodysize_pc + EnergyDensity_mJ.kg_1 + RunTimingGroup + Sex + Year, data = DataSet)


AIC(gmModel1, gmModel2, gmModel3, gmModel4, gmModel5, gmModel6, gmModel7, gmModel8, gmModel9, gmModel10, gmModel11, gmModel12, gmModel13, gmModel14, gmModel15, gmModel16, gmModel17, gmModel18, gmModel19, gmModel20, gmModel21, gmModel22, gmModel23, gmModel24, gmModel25, gmModel26, gmModel27, gmModel28, gmModel29)
gmModels <- list(gmModel1, gmModel2, gmModel3, gmModel4, gmModel5, gmModel6, gmModel7, gmModel8, gmModel9, gmModel10, gmModel11, gmModel12, gmModel13, gmModel14, gmModel15, gmModel16, gmModel17, gmModel18, gmModel19, gmModel20, gmModel21, gmModel22, gmModel23, gmModel24, gmModel25, gmModel26, gmModel27, gmModel28, gmModel29)

mynames5 <- paste("model", as.character(1:29), sep = "")
myaicc5 <- aictab(gmModels, modnames = mynames5)
print(myaicc5)


aic_df <- as.data.frame(myaicc5)
aic_df$ModelName <- c("Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1 + Sex",
                      "Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1*Sex",
                      "Gonad_Wt~Collection_Location + bodysize_pc + EnergyDensity_mJ.kg_1 + RunTimingGroup + Sex + Year",
                      "Gonad_Wt~Collection_Location + bodysize_pc + Sex",
                      "Gonad_Wt~Collection_Location + Sex",
                      "Gonad_Wt~Collection_Location + bodysize_pc*Sex",
                      "Gonad_Wt~Collection_Location + RunTimingGroup + Sex",
                      "Gonad_Wt~Collection_Location + Sex + Year",
                      "Gonad_Wt~Collection_Location + Sex*Year",
                      "Gonad_Wt~Collection_Location + RunTimingGroup*Sex",
                      "Gonad_Wt~Collection_Location + (bodysize_pc + EnergyDensity_mJ.kg_1 + RunTimingGroup + Sex + Year)^2",
                      "Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1",
                      "Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1*Year",
                      "Gonad_Wt~Collection_Location + bodysize_pc*EnergyDensity_mJ.kg_1",
                      "Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1 + Year",
                      "Gonad_Wt~Collection_Location + bodysize_pc + EnergyDensity_mJ.kg_1",
                      "Gonad_Wt~Collection_Location",
                      "Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1 + RunTimingGroup",
                      "Gonad_Wt~Collection_Location + bodysize_pc",
                      "Gonad_Wt~Collection_Location + Year",
                      "Gonad_Wt~Collection_Location + EnergyDensity_mJ.kg_1*RunTimingGroup",
                      "Gonad_Wt~Collection_Location + RunTimingGroup",
                      "Gonad_Wt~Collection_Location + bodysize_pc + Year",
                      "Gonad_Wt~Collection_Location + RunTimingGroup + Year",
                      "Gonad_Wt~Collection_Location + bodysize_pc + RunTimingGroup",
                      "Gonad_Wt~Collection_Location + bodysize_pc*RunTimingGroup",
                      "Gonad_Wt~1",
                      "Gonad_Wt~Collection_Location + RunTimingGroup*Year",
                      "Gonad_Wt~Collection_Location + bodysize_pc*Year")
colnames(aic_df)[colnames(aic_df) == "Modnames"] <- "Model #"

write.csv(aic_df, file = "ALHcode/AIC/AICresults_GonadMass~RunTimingGroup.csv", row.names = FALSE)












#' =====================================================
#' Section 7: AICc code from Greg's Class, Fecundity~RunTimingGroup test
#' =====================================================
DataSet<-read.csv("ALHcode/AIC/lowerriver_bodycomp_PCscores.csv")[,-1]
str(DataSet)
DataSet$Collection_Location <- as.factor(DataSet$Collection_Location)
DataSet$RunTimingGroup <- as.factor(DataSet$RunTimingGroup)
DataSet$Year<- as.factor(DataSet$Year)
DataSet$Sex<- as.factor(DataSet$Sex)
str(DataSet)

#need to filter out all females to one dataset
DataSet <- subset(DataSet, Sex == "F")

#fecundity is egg #, averaged across 3 different samples
DataSet$avgFecundity <- rowMeans(DataSet[, c("Fem_Fecun_1_num", "Fem_Fecun_2_num", "Fem_Fecun_3_num")], na.rm = TRUE)

#models
fecModel1 <- lm(avgFecundity~1, data = DataSet)
fecModel2 <- lm(avgFecundity~Collection_Location, data = DataSet)
fecModel3 <- lm(avgFecundity~Collection_Location + bodysize_pc, data = DataSet)
fecModel4 <- lm(avgFecundity~Collection_Location + EnergyDensity_mJ.kg_1, data = DataSet)
fecModel5 <- lm(avgFecundity~Collection_Location + RunTimingGroup, data = DataSet)
fecModel6 <- lm(avgFecundity~Collection_Location + Year, data = DataSet)
fecModel7 <- lm(avgFecundity~Collection_Location + bodysize_pc + EnergyDensity_mJ.kg_1, data = DataSet)
fecModel8 <- lm(avgFecundity~Collection_Location + bodysize_pc*EnergyDensity_mJ.kg_1, data = DataSet)
fecModel9 <- lm(avgFecundity~Collection_Location + bodysize_pc + RunTimingGroup, data = DataSet)
fecModel10 <- lm(avgFecundity~Collection_Location + bodysize_pc*RunTimingGroup, data = DataSet)
fecModel11 <- lm(avgFecundity~Collection_Location + bodysize_pc + Year, data = DataSet)
fecModel12 <- lm(avgFecundity~Collection_Location + bodysize_pc*Year, data = DataSet)
fecModel13 <- lm(avgFecundity~Collection_Location + EnergyDensity_mJ.kg_1 + RunTimingGroup, data = DataSet)
fecModel14 <- lm(avgFecundity~Collection_Location + EnergyDensity_mJ.kg_1*RunTimingGroup, data = DataSet)
fecModel15 <- lm(avgFecundity~Collection_Location + EnergyDensity_mJ.kg_1 + Year, data = DataSet)
fecModel16 <- lm(avgFecundity~Collection_Location + EnergyDensity_mJ.kg_1*Year, data = DataSet)
fecModel17 <- lm(avgFecundity~Collection_Location + RunTimingGroup + Year, data = DataSet)
fecModel18 <- lm(avgFecundity~Collection_Location + RunTimingGroup*Year, data = DataSet)
fecModel19 <- lm(avgFecundity~Collection_Location + bodysize_pc + EnergyDensity_mJ.kg_1 + RunTimingGroup, data = DataSet)
fecModel20 <- lm(avgFecundity~Collection_Location + bodysize_pc + EnergyDensity_mJ.kg_1 + Year, data = DataSet)
fecModel21 <- lm(avgFecundity~Collection_Location + bodysize_pc + RunTimingGroup + Year, data = DataSet)
fecModel22 <- lm(avgFecundity~Collection_Location + EnergyDensity_mJ.kg_1 + RunTimingGroup + Year, data = DataSet)
fecModel23 <- lm(avgFecundity~Collection_Location + (bodysize_pc + EnergyDensity_mJ.kg_1 + RunTimingGroup + Year)^2, data = DataSet)
fecModel24 <- lm(avgFecundity~Collection_Location + bodysize_pc + EnergyDensity_mJ.kg_1 + RunTimingGroup + Year, data = DataSet)

#AIC
AIC(fecModel1, fecModel2, fecModel3, fecModel4, fecModel5, fecModel6, fecModel7, fecModel8, fecModel9, fecModel10, fecModel11, fecModel12, fecModel13, fecModel14, fecModel15, fecModel16, fecModel17, fecModel18, fecModel19, fecModel20, fecModel21, fecModel22, fecModel23, fecModel24)
fecModels <- list(fecModel1, fecModel2, fecModel3, fecModel4, fecModel5, fecModel6, fecModel7, fecModel8, fecModel9, fecModel10, fecModel11, fecModel12, fecModel13, fecModel14, fecModel15, fecModel16, fecModel17, fecModel18, fecModel19, fecModel20, fecModel21, fecModel22, fecModel23, fecModel24)

mynames6 <- paste("model", as.character(1:24), sep = "")
myaicc6 <- aictab(fecModels, modnames = mynames6)
print(myaicc6)



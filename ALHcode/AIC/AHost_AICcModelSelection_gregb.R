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
#' Section 2: AICc code from Greg's Class, scBodyMass_1~RunTimingGroup test
#' =====================================================













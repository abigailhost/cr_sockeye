#
# KB Gorman
# CR Sockeye Body Comp, Abby Cand Models
# Dec 18, 2024
#
#-----

# Reset R, clear all objects
rm(list=ls()) 

# Set WD
setwd("/Users/kristengorman/Documents/R Data Abby CR Sockeye BC")
getwd()
list.files()

# Input files
# Updated Oct 2024 .csv files to change 2019 datapt for CC-57-2019 to 4.1, add Year variable to each dataset, 
# and remove the Benzoic Acid lines from each dataset
DS19<- read.csv("CRSockeye_BodyComp_2019FINAL_NPRB_Dec2024.csv")
DS20<- read.csv("CRSockeye_BodyComp_2020FINAL_NPRB_Dec2024.csv")
DS21<- read.csv("CRSockeye_BodyComp_2021FINAL_AKSSF_Dec2024.csv")

# Combine datasets
DS.BCall<- rbind(DS19, DS20, DS21)
nrow(DS.BCall) #474
write.csv(DS.BCall, file="CRSockeye_BodyComp_2019-21FINAL_Dec2024.csv")

# Explore DS.BCall for complete data for body size from lower vs upper river:
# Fish_Wt,	Fish_Leng_1,	Fish_Leng_1_tape,	Fish_Leng_2,	Fish_Leng_2_tape,
# Fish_Ht,	Fish_Ht_tape,	Fish_Wdth,	Fish_Grth

nrow(subset(DS.BCall, Fish_Wt!="NA")) #474-474 = 0 missing values
nrow(subset(DS.BCall, Fish_Leng_1!="NA")) #474-462 = 12 missing values
nrow(subset(DS.BCall, Fish_Leng_1_tape!="NA")) #474-179 = 295 missing values, DNU
nrow(subset(DS.BCall, Fish_Leng_2!="NA")) #474-462 = 12 missing values
nrow(subset(DS.BCall, Fish_Leng_2_tape!="NA")) #474-179 = 295 missing values, DNU
nrow(subset(DS.BCall, Fish_Ht!="NA")) #474-462 = 12 missing values
nrow(subset(DS.BCall, Fish_Ht_tape!="NA")) #474-179 = 295 missing values, DNU
nrow(subset(DS.BCall, Fish_Wdth!="NA")) #474-432 = 42 missing values, DNU?
nrow(subset(DS.BCall, Fish_Grth!="NA")) #474-467 = 7 missing values

# Use and estimate the following metrics:
# Fish_Wt,	Fish_Leng_1,	Fish_Leng_2,
# Fish_Ht,	Fish_Grth

#Subset for lower river fish only
library(dplyr)

DS.BCall.LR<- DS.BCall |> filter(Collection_Location %in% c("Canyon Creek","Baird Canyon", "Chitina Airport"))
nrow(DS.BCall.LR) #180

write.csv(DS.BCall.LR,file="DS.BCall.LR.csv",row.names=FALSE)

# Estimate missing values for each metric above, base it on sex-specific regressions
DS.BCall.LR.Fish_Leng_1<- subset(DS.BCall.LR, Fish_Leng_1!="NA")
nrow(DS.BCall.LR.Fish_Leng_1) #180, HAVE ALL
DS.BCall.LR.Fish_Leng_1_tape<- subset(DS.BCall.LR, Fish_Leng_1_tape!="NA")
nrow(DS.BCall.LR.Fish_Leng_1_tape) #60
DS.BCall.LR.Fish_Leng_2<- subset(DS.BCall.LR, Fish_Leng_2!="NA")
nrow(DS.BCall.LR.Fish_Leng_2) #180, HAVE ALL
DS.BCall.LR.Fish_Leng_2_tape<- subset(DS.BCall.LR, Fish_Leng_2_tape!="NA")
nrow(DS.BCall.LR.Fish_Leng_2_tape) #60
DS.BCall.LR.Fish_Ht<- subset(DS.BCall.LR, Fish_Ht!="NA")
nrow(DS.BCall.LR.Fish_Ht) #180, HAVE ALL
DS.BCall.LR.Fish_Ht_tape<- subset(DS.BCall.LR, Fish_Ht_tape!="NA")
nrow(DS.BCall.LR.Fish_Ht_tape) #60
DS.BCall.LR.Fish_Wdth<- subset(DS.BCall.LR, Fish_Wdth!="NA")
nrow(DS.BCall.LR.Fish_Wdth) #158
DS.BCall.LR.Fish_Grth<- subset(DS.BCall.LR, Fish_Grth!="NA")
nrow(DS.BCall.LR.Fish_Grth) #179, Need 1
#Should use Leng_1, Leng_2, Fish_Ht, Fish_Grth for PCscore

DS.BCall.LR.Fish.M<- subset(DS.BCall.LR.Fish_Leng_1, Sex=="M")
nrow(DS.BCall.LR.Fish.M) #91
DS.BCall.LR.Fish.M_Leng_1<- subset(DS.BCall.LR.Fish.M, Fish_Leng_1!="NA")
nrow(DS.BCall.LR.Fish.M_Leng_1) #91
DS.BCall.LR.Fish.M_Leng_2<- subset(DS.BCall.LR.Fish.M, Fish_Leng_2!="NA")
nrow(DS.BCall.LR.Fish.M_Leng_2) #91
DS.BCall.LR.Fish.M_Ht<- subset(DS.BCall.LR.Fish.M, Fish_Ht!="NA")
nrow(DS.BCall.LR.Fish.M_Ht) #91
DS.BCall.LR.Fish.M_Grth<- subset(DS.BCall.LR.Fish.M, Fish_Grth!="NA")
nrow(DS.BCall.LR.Fish.M_Grth) #91

DS.BCall.LR.Fish.F<- subset(DS.BCall.LR.Fish_Leng_1, Sex=="F")
nrow(DS.BCall.LR.Fish.F) #88
DS.BCall.LR.Fish.F_Leng_1<- subset(DS.BCall.LR.Fish.F, Fish_Leng_1!="NA")
nrow(DS.BCall.LR.Fish.F_Leng_1) #88
DS.BCall.LR.Fish.F_Leng_2<- subset(DS.BCall.LR.Fish.F, Fish_Leng_2!="NA")
nrow(DS.BCall.LR.Fish.F_Leng_2) #88
DS.BCall.LR.Fish.F_Ht<- subset(DS.BCall.LR.Fish.F, Fish_Ht!="NA")
nrow(DS.BCall.LR.Fish.F_Ht) #88
DS.BCall.LR.Fish.F_Grth<- subset(DS.BCall.LR.Fish.F, Fish_Grth!="NA")
nrow(DS.BCall.LR.Fish.F_Grth) #87

write.csv(DS.BCall.LR.Fish.F,file="DS.BCall.LR.Fish.F.csv",row.names=FALSE)

#-----
#Est Fish.F_Grth using PCscore for Leng_1, Leng_2, Fish_Ht for females only

nrow(DS.BCall.LR.Fish.F)

VarPCScore<-DS.BCall.LR.Fish.F[1:88,c(12,14,16)]
VarPCScore
nrow(VarPCScore)

Mod1<-prcomp(VarPCScore,scale=TRUE)
Mod1

summary(Mod1)

Importance of components:
  PC1    PC2    PC3
Standard deviation     1.419 0.8056 0.5815
Proportion of Variance 0.671 0.2163 0.1127
Cumulative Proportion  0.671 0.8873 1.0000

Mod2<-prcomp(VarPCScore,scale=TRUE)$x
Mod2

DS.BCall.LR.Fish.F2<-cbind(DS.BCall.LR.Fish.F,Mod2=Mod2)

write.csv(DS.BCall.LR.Fish.F2,file="DS.BCall.LR.Fish.F2.PCscore.csv",row.names=FALSE)

#Pred Grth from PCscore for Females
#Check PCscore is correct sign

plot(Mod2.PC1~Fish_Leng_1, data=DS.BCall.LR.Fish.F2) #This needs to by *-1
DataSet2<- read.csv("DS.BCall.LR.Fish.F2.PCscore.csv")
plot(Mod2.PC1.2~Fish_Leng_1, data=DataSet2)

mod3<- lm(Fish_Grth~Mod2.PC1.2, data=DataSet2)
summary(mod3)
Call:
  lm(formula = Fish_Grth ~ Mod2.PC1.2, data = DataSet2)

Residuals:
  Min       1Q   Median       3Q      Max 
-2.28663 -0.98209  0.01101  0.70231  2.57802 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 29.12852    0.12194  238.88   <2e-16 ***
  Mod2.PC1.2   1.20585    0.08607   14.01   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.137 on 85 degrees of freedom
(1 observation deleted due to missingness)
Multiple R-squared:  0.6978,	Adjusted R-squared:  0.6942 
F-statistic: 196.3 on 1 and 85 DF,  p-value: < 2.2e-16

mod4<- lm(Fish_Grth~Fish_Leng_1, data=DataSet2)
summary(mod4)
Call:
  lm(formula = Fish_Grth ~ Fish_Leng_1, data = DataSet2)

Residuals:
  Min      1Q  Median      3Q     Max 
-3.3511 -1.0333 -0.0648  0.8908  4.0435 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  7.21966    2.40438   3.003  0.00351 ** 
  Fish_Leng_1  0.49756    0.05451   9.127 2.98e-14 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.47 on 85 degrees of freedom
(1 observation deleted due to missingness)
Multiple R-squared:  0.495,	Adjusted R-squared:  0.489 
F-statistic: 83.31 on 1 and 85 DF,  p-value: 2.98e-14

#Better to use PCscore for females, this has a R2 of 69% vs 49% for Leng_1 only

#Now estimate missing Grth value from intercept and PCscore est
Grth = 29.12852 + 0.730469567 #29.85899, this was added to LR dataset

#----
#Now create PCscores for entire LR dataset to include males and females together

DataSet.LR<- read.csv("DS.BCall.LR.2.csv")
nrow(DataSet.LR) #180

VarPCScore<-DataSet.LR[1:180,c(12,14,16,19)]
VarPCScore
nrow(VarPCScore) #180

Mod1<-prcomp(VarPCScore,scale=TRUE)
Mod1

summary(Mod1)

Importance of components:
  PC1    PC2     PC3     PC4
Standard deviation     1.6615 0.7900 0.61932 0.48119
Proportion of Variance 0.6902 0.1560 0.09589 0.05789
Cumulative Proportion  0.6902 0.8462 0.94211 1.00000

Mod2<-prcomp(VarPCScore,scale=TRUE)$x
Mod2

DataSet.LR2<-cbind(DataSet.LR,Mod2=Mod2)

#Pred Grth from PCscore for Females
#Check PCscore is correct sign

plot(Mod2.PC1~Fish_Leng_1, data=DataSet.LR2) #This does not need to be corrected

write.csv(DataSet.LR2,file="DataSet.LR2.PCscore.csv",row.names=FALSE)

#-----
#Estimate river mile for Baird, Canyon, and Chitina for modeling, try using Pete's code
# Reset R, clear all objects
rm(list=ls()) 

# Set WD
setwd("/Users/kristengorman/Documents/R Data Abby CR Sockeye BC")
getwd()
list.files()

DataSet<- read.csv("DS.BCall.LR.2.csv")
unique(DataSet$Col_Location_Lat) #61.40097 60.73831 61.58650
unique(DataSet$Col_Location_Long) #-144.5177 -144.5687 -144.4276

#So confirms we only have 3 lat/longs for the 3 locations

#Required packages
library(sf)
install.packages("elevatr")
library(elevatr)
install.packages("tmap")
library(tmap)
library(raster)
library(plyr)
library(readxl)
library(lubridate)
library(tmap)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(dplyr)
library(classInt)

setwd("/Users/kristengorman/Documents/R Data Abby CR Sockeye BC/CR_ShapeFiles")
getwd()
list.files()

#Read in shape files
CopperT<-sf::read_sf("CopperRTribs.shp")
class(CopperT)
#watershed boundary
CopperWB<-sf::read_sf("CopperRiverWBD.shp")

#convert to coordinates
CopperT = st_transform(CopperT, "+init=epsg:4326")
CopperWB = st_transform(CopperWB, "+init=epsg:4326")

#Read in elevation data
dem_Copper<-get_elev_raster(CopperWB,,z=7)
elev_crop = crop(dem_Copper, CopperWB)

slope = terrain(elev_crop, opt = "slope")
aspect = terrain(elev_crop, opt = "aspect")

hill = hillShade(slope, aspect, angle = 40, direction = 270)
masked_CR <- mask(hill, CopperWB)

#River network
river_sf = st_as_sf(CopperT)

# Ensure CRS consistency between CopperT and elev_crop
CT <- st_transform(CopperT, crs(elev_crop))  # Transform CopperT to the CRS of the raster

# Extract coordinates (longitude and latitude) from CopperT
coordinates_xy <- st_coordinates(CT)[, 1:2]  # Keep only the first two columns (longitude and latitude)

# Extract elevation values for each point from the raster
elevation_values <- extract(elev_crop, coordinates_xy)

# Create a data frame with x, y coordinates and corresponding elevation values (in meters ASL)
elevation_df <- data.frame(
  x = coordinates_xy[, 1],  # Longitude
  y = coordinates_xy[, 2],  # Latitude
  elevation = unlist(elevation_values)  # Elevation, unlisted in case it's a list
)

# View the result
head(elevation_df)

#setwd("C:/Users/PeteRand/OneDrive - Prince William Sound Science Center/RWorkingDirectory/CopperSalmonGitHub/CopperSalmon/InputFiles/PlaceNames/")

Copper_PlaceNames<-read.csv(file="Copper_PlaceNames.csv")
Copper_PlaceNames<-Copper_PlaceNames[c(-5,-6),]
placenames_sf = st_as_sf (Copper_PlaceNames,coords = c("Longitude","Latitude"),crs = 4326, agr = "constant")

#Read in receiver station positions
Receiver_Loc<-read.csv(file="C:/Users/PeteRand/OneDrive - Prince William Sound Science Center/RWorkingDirectory/CopperSalmonGitHub/CopperSalmon/InputFiles/GroundReceiverStations/Receiver_Loc_For2005.csv")
Receiver_Loc<-read.csv("Receiver_Loc_For2005.csv")
receivers_sf = st_as_sf (Receiver_Loc,coords = c("Longitude","Latitude"),crs = 4326, agr = "constant")

#This produces map with ground receiver telemetry stations as red points.

map_CR = tm_shape(masked_CR) +
  tm_raster(palette = gray(7:10 / 10), style = "cont", legend.show = FALSE) +tm_shape(river_sf)+tm_lines(col = "blue") +
  tm_compass(position = c("right", "top"), size = 2) +tm_graticules(x=c(-146,-142),y=c(61,62,63),labels.size=1.2)+
  tm_scale_bar(position = c("right", "bottom"), width = 0.15, text.size =.6)+
  tm_shape(placenames_sf)+ tm_symbols(col = "black", size = .7, scale = .8,border.lwd=NA)+
  tm_text("Site",just="left",scale=.8,xmod=.1,ymod=-.3)+
  tm_shape(receivers_sf)+ tm_symbols(col = "red", size = .7, scale = .8,border.lwd=NA)+
  tm_text("Site.",just="left",scale=.8,xmod=.1,ymod=-.3)+
  tm_xlab("Longitude", size = 1.5, rotation = 0, space = 0)+
  tm_ylab("Latitude", size = 1.5, rotation = 90, space = 0)

map_CR

#Create the above with just the lower river collection locations
LRColl_Loc<-read.csv("LR.CollLocations.csv")
LRColl_Loc_sf = st_as_sf (LRColl_Loc,coords = c("Longitude","Latitude"),crs = 4326, agr = "constant")

map_CR2 = tm_shape(masked_CR) +
  tm_raster(palette = gray(7:10 / 10), style = "cont", legend.show = FALSE) +tm_shape(river_sf)+tm_lines(col = "blue") +
  tm_compass(position = c("right", "top"), size = 2) +tm_graticules(x=c(-146,-142),y=c(61,62,63),labels.size=1.2)+
  tm_scale_bar(position = c("right", "bottom"), width = 0.15, text.size =.6)+
  tm_shape(placenames_sf)+ tm_symbols(col = "black", size = .7, scale = .8,border.lwd=NA)+
  tm_text("Site",just="left",scale=.8,xmod=.1,ymod=-.3)+
  tm_shape(LRColl_Loc_sf)+ tm_symbols(col = "orange", size = .7, scale = .8,border.lwd=NA)+
  tm_text("Site.",just="left",scale=.8,xmod=.1,ymod=-.3)+
  tm_xlab("Longitude", size = 1.5, rotation = 0, space = 0)+
  tm_ylab("Latitude", size = 1.5, rotation = 90, space = 0)

map_CR2

#Estimate distance traveled for each fish that are located in tributaries
# Step 1: Create the river mouth point using the provided coordinates (location here is Baird tagging site - might want to use lower river site, tagging site is about 70 km from flats)
LRCollLoc<- read.csv("LR.CollLocations.csv")

river_mouth_coords <- st_sfc(st_point(c(-144.513210, 60.816209)), crs = 4326)
river_mouth_coords <- st_sfc(st_point(c(-145.071545, 60.273234)), crs = 4326) #Uses entrance of CR at flats

# Step 2: Assign this as the river mouth for distance calculations
mouth_of_river <- st_sf(geometry = river_mouth_coords)

# Verify the river mouth point
print(mouth_of_river)

# Step 3: Calculate the river distance for each fish
Coll_distances <- LRColl_Loc_sf %>%
  mutate(
    # Calculate distance from fish location to the river mouth point
    river_distance = st_distance(geometry, mouth_of_river$geometry)
  )

# View the calculated distances
head(Coll_distances)
#Simple feature collection with 3 features and 4 fields
#Attribute-geometry relationships: constant (3), NA's (1)
#Geometry type: POINT
#Dimension:     XY
#Bounding box:  xmin: -144.5687 ymin: 60.73831 xmax: -144.4276 ymax: 61.5865
#Geodetic CRS:  WGS 84
#  Site. Site_Description Short_Name                   geometry river_distance
#1     1     Baird Canyon      Baird POINT (-144.5687 60.73831)   58584.32 [m]
#2     2     Canyon Creek     Canyon POINT (-144.5177 61.40097)  128939.07 [m]
#3     3  Chitina Airport    Chitina  POINT (-144.4276 61.5865)  150114.06 [m]

#-----
#AIC calcs for LR body size using PCscore

# Open the file just created, "ADPE.SSD.IsoVarSex.DataSet.Sub3.csv" and be sure to discard column, 1 and change all contunous variables to a num.

# Start here...
# Reset R, clear all objects.
rm(list=ls())

#-----
# Specify working directory and see what files are available.
setwd("/Users/kristengorman/Documents/R Data Abby CR Sockeye BC")
getwd()
list.files()

# Identify the data to be used.
DataFileName<- "DataSet.LR2.PCscore2.csv"

# Identify the models to be run.
ModelFileName<-"CRSockeyeLR_BS_CandSet.csv"

#-----
# Import the data. Be sure to check the structure of the data and that R is reading continuous and categorical variables appropriately.
DataSet<-read.csv(DataFileName)

# View DataSet.
head(DataSet)
summary(DataSet)
nrow(DataSet)
str(DataSet)

# Change parameter structures if needed.
DataSet$Collection_Year<- as.factor(DataSet$Collection_Year)
DataSet$RunTimingGroup<- as.factor(DataSet$RunTimingGroup)
DataSet$Sex<- as.factor(DataSet$Sex)
str(DataSet)
DataSet

# Import candidate model set and convert to a character vector.
ModelSet.df<-read.csv(ModelFileName)  
ModelSet<-as.character(ModelSet.df$Model)
str(ModelSet)

# View candidate models.
ModelSet

#-----
# Calculate all combinations of predictor variables to check candidate model set. Note, any candidate set may not include all combos of variables, but this will allow for checking that all combos have at least been considered (i.e., didnt forget some combo).
# Not necessary unless you want R to tell you all potential combos of predictors so that you dont have
globalmodel<- lm(Mod2.PC1~CollLoc_Distance.m*RunTimingGroup*Sex*Collection_Year*RunTimingGroup:Sex*Collection_Year:Sex, data=DataSet)

AllCombCoefName<-names(globalmodel$coef)

# View all combinations of predictor variables.
AllCombCoefName

#-----
# Specify names of main explanatory parameters and associated SEs in the ModelSet (including interactions). Need to use : for interactions. !!Make sure that when creating any subsequent versions of CoefName in the code below that it follows the same order as is listed here. Further, when a candidate set has interaction models where (a) both main parameters are not included in the model, or where (b) the main effect is a categorical variable, you must include columns labeled here for all appropriate output!!
CoefName<- c("Intercept", "Intercept.SE", 
             "CollLoc_Distance.m", "CollLoc_Distance.m.SE", 
             "RunTimingGroupLate", "RunTimingGroupLate.SE", 
             "RunTimingGroupMiddle", "RunTimingGroupMiddle.SE", 
             "Collection_Year2020", "Collection_Year2020.SE", 
             "Collection_Year2021", "Collection_Year2021.SE",
             "SexM", "SexM.SE",
             "RunTimingGroupLate:SexM", "RunTimingGroupLate:SexM.SE", 
             "RunTimingGroupMiddle:SexM", "RunTimingGroupMiddle:SexM.SE",
             "Collection_Year2020:SexM", "Collection_Year2020:SexM.SE",
             "Collection_Year2021:SexM", "Collection_Year2021:SexM.SE")

# View predictor variables and associated SEs.
CoefName

#-----
# AIC function produces calculations for AIC Model Matrix that is used in conjunction with the ModelSet.
calculate.AIC<- function(AIC.Table, ModelSet) {
  
  deltaAIC.c<- AIC.Table$AIC.c-min(AIC.Table$AIC.c)
  lik.dAIC.c<- exp(-deltaAIC.c/2)
  AIC.c.W<- lik.dAIC.c/(sum(lik.dAIC.c))
  AICFinalMatrix<- data.frame(AICModelMatrix, deltaAIC.c, lik.dAIC.c, AIC.c.W)
}

#-----
# Create matrix to hold output from models.

# I. AIC Model Matrix, ncol=29 includes models, all coef names listed above, and column headings specified in the matrix below.
AICModelMatrix<- as.data.frame(matrix(NA, nrow=length(ModelSet), ncol=length(CoefName)+11, dimnames=list(c(1:length(ModelSet)), c("Models", CoefName, "N.Obs", "k", "EDF", "RMSE", "SSE", "logLik", "-2logLik", "mul.r.squared", "AIC", "AIC.c"))))

# View AIC Model Matrix.
head(AICModelMatrix)

#-----
# Loop for calculating model output and filling AIC Model Matrix. According to Burnham and Anderson, for least squares model fitting, K = total number of estimated regression parameters, including the intercept, and residual variation. In the case of R output, coef(model) includes an estimate of the intercept, thus, length(m$coef)+1 = K.

# !!Need to check exactly how R is pulling the output and make sure that it is specified accordingly in loop below. For example, for a Year parameter specified as Year A and B, R will pull out the reference value and list as "YearB" in output. When specifying an interaction, R output for the interaction will follow how it is specified in the model orginially. For example, if you have a model Slipid~Rlipid + Year + Year:Rlipid, R output will specify the interaction as Rlipid:Year because Rlipid main effect was specified first. So in loop below you must follow this format or else it wont pull the output properly!!

ModelOutput<- list()

for(i in 1:length(ModelSet)){
  
  ModelOutput[[i]]<- lm(as.formula(ModelSet[i]), data=DataSet)
  m<- ModelOutput[[i]]
  N.Obs<- nrow(DataSet)
  k<- length(m$coef)+1
  EDF<- N.Obs-k
  AIC<- AIC(m)
  AIC.c<- AIC+(2*k*(k+1))/(N.Obs-k-1)
  AICModelMatrix[i,"Models"]<- ModelSet[i]
  AICModelMatrix[i,"Intercept"]<- coef(m)["(Intercept)"]
  AICModelMatrix[i,"Intercept.SE"]<- summary(m)$coef["(Intercept)",2]
  AICModelMatrix[i,"CollLoc_Distance.m"]<- coef(m)["CollLoc_Distance.m"]
  AICModelMatrix[i,"CollLoc_Distance.m.SE"]<- summary(m)$coef[,2]["CollLoc_Distance.m"]
  AICModelMatrix[i,"RunTimingGroupLate"]<- coef(m)["RunTimingGroupLate"]
  AICModelMatrix[i,"RunTimingGroupLate.SE"]<- summary(m)$coef[,2]["RunTimingGroupLate"]
  AICModelMatrix[i,"RunTimingGroupMiddle"]<- coef(m)["RunTimingGroupMiddle"]
  AICModelMatrix[i,"RunTimingGroupMiddle.SE"]<- summary(m)$coef[,2]["RunTimingGroupMiddle"]
  AICModelMatrix[i,"Collection_Year2020"]<- coef(m)["Collection_Year2020"]
  AICModelMatrix[i,"Collection_Year2020.SE"]<- summary(m)$coef[,2]["Collection_Year2020"]
  AICModelMatrix[i,"Collection_Year2021"]<- coef(m)["Collection_Year2021"]
  AICModelMatrix[i,"Collection_Year2021.SE"]<- summary(m)$coef[,2]["Collection_Year2021"]
  AICModelMatrix[i,"SexM"]<- coef(m)["SexM"]
  AICModelMatrix[i,"SexM.SE"]<- summary(m)$coef[,2]["SexM"]
  AICModelMatrix[i,"RunTimingGroupLate:SexM"]<- coef(m)["RunTimingGroupLate:SexM"]
  AICModelMatrix[i,"RunTimingGroupLate:SexM.SE"]<- summary(m)$coef[,2]["RunTimingGroupLate:SexM"]
  AICModelMatrix[i,"RunTimingGroupMiddle:SexM"]<- coef(m)["RunTimingGroupMiddle:SexM"]
  AICModelMatrix[i,"RunTimingGroupMiddle:SexM.SE"]<- summary(m)$coef[,2]["RunTimingGroupMiddle:SexM"]
  AICModelMatrix[i,"Collection_Year2020:SexM"]<- coef(m)["Collection_Year2020:SexM"]
  AICModelMatrix[i,"Collection_Year2020:SexM.SE"]<- summary(m)$coef[,2]["Collection_Year2020:SexM"]
  AICModelMatrix[i,"Collection_Year2021:SexM"]<- coef(m)["Collection_Year2021:SexM"]
  AICModelMatrix[i,"Collection_Year2021:SexM.SE"]<- summary(m)$coef[,2]["Collection_Year2021:SexM"]
  AICModelMatrix[i,"N.Obs"]<- N.Obs
  AICModelMatrix[i,"k"]<- k 
  AICModelMatrix[i,"EDF"]<- EDF
  AICModelMatrix[i,"RMSE"]<- summary(m)$sigma
  AICModelMatrix[i,"SSE"]<- anova(m)["Residuals","Sum Sq"]
  AICModelMatrix[i,"logLik"]<- logLik(m)
  AICModelMatrix[i,"-2logLik"]<- -2*logLik(m)
  AICModelMatrix[i,"mul.r.squared"]<- summary(m)$r.squared
  AICModelMatrix[i,"AIC"]<- AIC
  AICModelMatrix[i,"AIC.c"]<- AIC.c
}

#-----
# Calculate deltaAICc, likdAICc, and AIC weights.
AIC.Output<-calculate.AIC(AICModelMatrix,as.character(ModelSet))

#-----
# View AIC Model Matrix.
print(AIC.Output)

# Write AIC Final Matrix output to .csv files.
write.table(AIC.Output, file="AICFinalMatrix.csv", col.names=NA, sep=",")

# Write model output to .csv files.
sink(paste("Model Summaries_",out="",".doc",sep=""))

for (i in 1:length(ModelSet)) {
  
  print(paste("MODEL ", i, sep = ""))
  print(ModelSet[[i]])
  print(summary(ModelOutput[[i]]))
  print(paste("-------------------------------------------------------------------"))
}

sink()

#-----
#Running individual models
mod1<- lm(Mod2.PC1~1, data=DataSet)
summary (mod1)
mod2<- lm(Mod2.PC1~CollLoc_Distance.m, data=DataSet)
summary(mod2)
mod3<- lm(Mod2.PC1~CollLoc_Distance.m + RunTimingGroup, data=DataSet)
summary(mod3)
mod4<- lm(Mod2.PC1~CollLoc_Distance.m + Sex, data=DataSet)
summary(mod4)
mod5<- lm(Mod2.PC1~CollLoc_Distance.m + Collection_Year, data=DataSet)
summary(mod5)
mod6<- lm(Mod2.PC1~CollLoc_Distance.m + RunTimingGroup + Sex, data=DataSet)
summary(mod6)
mod7<- lm(Mod2.PC1~CollLoc_Distance.m + RunTimingGroup + Collection_Year, data=DataSet)
summary(mod7)
mod8<- lm(Mod2.PC1~CollLoc_Distance.m + Sex + Collection_Year, data=DataSet)
summary(mod8)
mod9<- lm(Mod2.PC1~CollLoc_Distance.m + RunTimingGroup + Sex + RunTimingGroup*Sex, data=DataSet)
summary(mod9)
mod10<- lm(Mod2.PC1~CollLoc_Distance.m + RunTimingGroup + Collection_Year + RunTimingGroup*Collection_Year, data=DataSet)
summary(mod10)
mod11<- lm(Mod2.PC1~CollLoc_Distance.m + Sex + Collection_Year + Sex*Collection_Year, data=DataSet)
summary(mod11)
mod12<- lm(Mod2.PC1~CollLoc_Distance.m + RunTimingGroup + Sex + Collection_Year, data=DataSet)
summary(mod12)

str(DataSet)
DataSet.new<- DataSet[1:179,c(5,7,9,22,80)]
cor(DataSet.new)


#-----
# Begin weighted and summed AIC calcs. Save previous file with AICFinalMatrix output, excluding column A (Model #) and N.Obs through the lik.dAIC.c columns, as weighted AIC Model Matrix Worksheet (W.AICFinalMatrixWS.csv). For example, the scaup dataset, be sure to cut and paste the parameter output for "RFGinit:Rlipid", "RFGinit:Rlipid.SE" and paste into the column for "Rlipid:RFGinit", "Rlipid:RFGinit.SE".

#-----
# Reset R, clear all objects.
rm(list=ls())

#-----
# Specify working directory and see whats available. Make sure that new file created above is listed.
setwd("/Users/kristengorman/Documents/R Data Abby CR Sockeye BC")
getwd()
list.files()

# Identify the new data WS to be used.
DataFileName2<- "W.AICFinalMatrixWS.csv"

# Import data.
DataSet2<-read.csv(DataFileName2)

# View DataSet. Note that all columns, with the exception of Models, should be listed as num.
head(DataSet2)
str(DataSet2)

#-----
# Specify a vector for DataSet2 Parameters. These will be used to define the parameters that will used to calculate Parameter likelihoods and W.ParaEsts that are multiplied by the model W. !!For interactions, be sure to specify them with . and not : as this is out they will be uploaded in DataSet2!!

# First recall CoefName and just exclude the .SE names in CoefName2 below.
CoefName<- c("Intercept", "Intercept.SE", 
             "CollLoc_Distance.m", "CollLoc_Distance.m.SE", 
             "RunTimingGroupLate", "RunTimingGroupLate.SE", 
             "RunTimingGroupMiddle", "RunTimingGroupMiddle.SE", 
             "Collection_Year2020", "Collection_Year2020.SE", 
             "Collection_Year2021", "Collection_Year2021.SE",
             "SexM", "SexM.SE",
             "RunTimingGroupLate.SexM", "RunTimingGroupLate.SexM.SE", 
             "RunTimingGroupMiddle.SexM", "RunTimingGroupMiddle.SexM.SE",
             "Collection_Year2020.SexM", "Collection_Year2020.SexM.SE",
             "Collection_Year2021.SexM", "Collection_Year2021.SexM.SE")

# View CoefName.
CoefName

CoefName2<- c("Intercept", 
             "CollLoc_Distance.m", 
             "RunTimingGroupLate", 
             "RunTimingGroupMiddle", 
             "Collection_Year2020", 
             "Collection_Year2021",
             "SexM",
             "RunTimingGroupLate.SexM", 
             "RunTimingGroupMiddle.SexM",
             "Collection_Year2020.SexM",
             "Collection_Year2021.SexM")

# View CoefName2.
CoefName2

#-----
# III. Calculate parameter likelihoods. Do this before W.ParaEst step below where DataSet2 NAs are turned into 0s.

# Create a vector to hold output.
Paralik<- vector()

# Create a matrix to hold output.
ParalikMatrix<- as.data.frame(matrix(NA, nrow=1, ncol=length(CoefName2), dimnames=list(c(1:length(1)), c(CoefName2))))

# View ParalikMatrix.
ParalikMatrix

# Fill ParalikMatrix manually, b/c loop doesnt work with the subset function!

sub1<- subset(DataSet2, !is.na(Intercept))
nrow(sub1)
ParalikIntercept<- sum(sub1$AIC.c.W)
ParalikMatrix[1]<- signif(ParalikIntercept, digits=12)

sub2<- subset(DataSet2, !is.na(CollLoc_Distance.m))
nrow(sub2)
ParalikCollLoc_Distance.m<- sum(sub2$AIC.c.W)
ParalikMatrix[2]<- signif(ParalikCollLoc_Distance.m, digits=12)

sub3<- subset(DataSet2, !is.na(RunTimingGroupLate))
nrow(sub3)
ParalikRunTimingGroupLate<- sum(sub3$AIC.c.W)
ParalikMatrix[3]<- signif(ParalikRunTimingGroupLate, digits=12)

sub4<- subset(DataSet2, !is.na(RunTimingGroupMiddle))
nrow(sub4)
ParalikRunTimingGroupMiddle<- sum(sub4$AIC.c.W)
ParalikMatrix[4]<- signif(ParalikRunTimingGroupMiddle, digits=12)

sub5<- subset(DataSet2, !is.na(Collection_Year2020))
nrow(sub5)
ParalikCollection_Year2020<- sum(sub5$AIC.c.W)
ParalikMatrix[5]<- signif(ParalikCollection_Year2020, digits=12)

sub6<- subset(DataSet2, !is.na(Collection_Year2021))
nrow(sub6)
ParalikCollection_Year2021<- sum(sub6$AIC.c.W)
ParalikMatrix[6]<- signif(ParalikCollection_Year2021, digits=12)

sub7<- subset(DataSet2, !is.na(SexM))
nrow(sub7)
ParalikSexM<- sum(sub7$AIC.c.W)
ParalikMatrix[7]<- signif(ParalikSexM, digits=12)

sub8<- subset(DataSet2, !is.na(RunTimingGroupLate.SexM))
nrow(sub8)
ParalikRunTimingGroupLate.SexM<- sum(sub8$AIC.c.W)
ParalikMatrix[8]<- signif(ParalikRunTimingGroupLate.SexM, digits=12)

sub9<- subset(DataSet2, !is.na(RunTimingGroupMiddle.SexM))
nrow(sub9)
ParalikRunTimingGroupMiddle.SexM<- sum(sub9$AIC.c.W)
ParalikMatrix[9]<- signif(ParalikRunTimingGroupMiddle.SexM, digits=12)

sub10<- subset(DataSet2, !is.na(Collection_Year2020.SexM))
nrow(sub9)
ParalikCollection_Year2020.SexM<- sum(sub9$AIC.c.W)
ParalikMatrix[10]<- signif(ParalikCollection_Year2020.SexM, digits=12)

sub11<- subset(DataSet2, !is.na(Collection_Year2021.SexM))
nrow(sub9)
ParalikCollection_Year2021.SexM<- sum(sub9$AIC.c.W)
ParalikMatrix[11]<- signif(ParalikCollection_Year2021.SexM, digits=12)

#-----
# View ParalikMatrix
ParalikMatrix

#-----
# Transpose ParalikMatrix
transpose.ParalikMatrix<- t(ParalikMatrix)

# View transposed ParalikMatrix
transpose.ParalikMatrix

#-----
# Calculate Weighted Parameter Estimates (W.ParaEst).

# Turns NAs into 0s for calculating W.ParaEsts.
DataSet2[is.na(DataSet2)]<- 0

# View DataSet2 to check that NAs have have been replaced.
head(DataSet2)

# Specify a vector for new W.ParaEst.
W.CoefName2<- c("W.Intercept", 
              "W.CollLoc_Distance.m", 
              "W.RunTimingGroupLate", 
              "W.RunTimingGroupMiddle", 
              "W.Collection_Year2020", 
              "W.Collection_Year2021",
              "W.SexM",
              "W.RunTimingGroupLate.SexM", 
              "W.RunTimingGroupMiddle.SexM",
              "W.Collection_Year2020.SexM",
              "W.Collection_Year2021.SexM")

# View W.CoefName2.
W.CoefName2

# II. Create matrix to hold outputs for W.ParaEsts.

# Weighted ParaEst Matrix, ncol=8 includes all W.CoefName2 listed above.
W.ParaEstMatrix<- as.data.frame(matrix(NA, nrow=nrow(DataSet2), ncol=length(W.CoefName2), dimnames=list(c(1:nrow(DataSet2)), c(W.CoefName2))))

# View W.ParaMatrix.
head(W.ParaEstMatrix)

# Run W.ParaEst loop to create W.ParaEsts by mulitplying the Para Ests for each model by the AIC.c weight of each model.

for(i in 1:length(CoefName2)) {
  
  W.ParaEst<- DataSet2[CoefName2[i]]*DataSet2["AIC.c.W"]
  W.ParaEstMatrix[,i]<- W.ParaEst
}

# View filled W.ParaEstMatrix.
W.ParaEstMatrix

#-----
# Summed W.ParaEsts.
# Specify a vector for summed W.ParaEsts.
s.W.CoefName2<- c("s.W.Intercept", 
                "s.W.CollLoc_Distance.m", 
                "s.W.RunTimingGroupLate", 
                "s.W.RunTimingGroupMiddle", 
                "s.W.Collection_Year2020", 
                "s.W.Collection_Year2021",
                "s.W.SexM",
                "s.W.RunTimingGroupLate.SexM", 
                "s.W.RunTimingGroupMiddle.SexM",
                "s.W.Collection_Year2020.SexM",
                "s.W.Collection_Year2021.SexM")

# View summed W.ParaEsts.
s.W.CoefName2

# Create 2 matrices to hold same output for summed W.ParaEsts

# II. 1-Summed W.ParaEsts Matrix, ncol=8 includes all s.W.CoefName2 listed above and nrow(DataSet2) to be used in the W.ParaEst.SE calcs.
s.W.ParaEstMatrix1<- as.data.frame(matrix(NA, nrow=nrow(DataSet2), ncol=length(s.W.CoefName2), dimnames=list(c(1:nrow(DataSet2)), c(s.W.CoefName2))))

# 1-View s.W.ParaEstMatrix
s.W.ParaEstMatrix1

# 1-Run s.W.ParaEst loop. Calculates summed W.ParaEsts by summing all W.ParaEsts.

for(i in 1:length(W.CoefName2)) {
  
  s.W.ParaEst<- sum(W.ParaEstMatrix[W.CoefName2[i]])
  s.W.ParaEst[i]<- s.W.ParaEst
  s.W.ParaEstMatrix1[,i]<- s.W.ParaEst[i]
}

# View filled s.W.ParaEstMatrix1
s.W.ParaEstMatrix1

# III. 2-Summed W.ParaEsts Matrix, ncol=8 includes all s.W.CoefName2 listed above and nrow(1) to be used in final binding of all summed matrices.
s.W.ParaEstMatrix2<- as.data.frame(matrix(NA, nrow=1, ncol=length(s.W.CoefName2), dimnames=list(c(1:length(1)), c(s.W.CoefName2))))

# 2-View s.W.ParaEstMatrix
s.W.ParaEstMatrix2

# 2-Run s.W.ParaEst loop. Calculates summed W.ParaEsts by summing all

for(i in 1:length(W.CoefName2)) {
  
  s.W.ParaEst<- sum(W.ParaEstMatrix[W.CoefName2[i]])
  s.W.ParaEst[i]<- s.W.ParaEst
  s.W.ParaEstMatrix2[,i]<- s.W.ParaEst[i]
}

# 2-View filled s.W.ParaEstMatrix2
s.W.ParaEstMatrix2

# 2-Transpose s.W.ParaEstMatrix2
transpose.s.W.ParaEstMatrix2<- t(s.W.ParaEstMatrix2)

# 2-View transposed s.W.ParaEstMatrix2
transpose.s.W.ParaEstMatrix2

#-----
# Calculate Weighted Parameter Estimate SEs (W.ParaEst.SE).

# Specify a vector for DataSet2 Parameter SEs. These will be used to define the parameter SEs that will used to calculate W.ParaEst.SE that are caculated below in the loop. !!Don't forget to use . instead of : for interactions as this is how they are uploaded in DataSet2.
CoefName3<- c("Intercept.SE", 
              "CollLoc_Distance.m.SE", 
              "RunTimingGroupLate.SE", 
              "RunTimingGroupMiddle.SE", 
              "Collection_Year2020.SE", 
              "Collection_Year2021.SE",
              "SexM.SE",
              "RunTimingGroupLate.SexM.SE", 
              "RunTimingGroupMiddle.SexM.SE",
              "Collection_Year2020.SexM.SE",
              "Collection_Year2021.SexM.SE")

# View CoefName3
CoefName3

# Specify a vector for new W.ParaEst.SE.
W.CoefName3.SE<- c("W.Intercept.SE", 
              "W.CollLoc_Distance.m.SE", 
              "W.RunTimingGroupLate.SE", 
              "W.RunTimingGroupMiddle.SE", 
              "W.Collection_Year2020.SE", 
              "W.Collection_Year2021.SE",
              "W.SexM.SE",
              "W.RunTimingGroupLate.SexM.SE", 
              "W.RunTimingGroupMiddle.SexM.SE",
              "W.Collection_Year2020.SexM.SE",
              "W.Collection_Year2021.SexM.SE")

# View W.CoefName3.SE
W.CoefName3.SE

# Create matrix to hold outputs for W.ParaEst.SE.

# II. Weighted ParaEst.SE Matrix, ncol=8 includes all W.CoefName3.SE listed above.
W.ParaEst.SEMatrix<- as.data.frame(matrix(NA, nrow=nrow(DataSet2), ncol=length(W.CoefName3.SE), dimnames=list(c(1:nrow(DataSet2)), c(W.CoefName3.SE))))

# View W.ParaMatrix.
head(W.ParaEst.SEMatrix)

# Run W.ParaEst.SE loop to create W.ParaEstSEs using the following equation W.SE = (AIC.c.W*(sqrt((ParaEst SE^2)+(ParaEst-summedWParaEst)^2)))

for(j in 1:nrow(DataSet2)) {
  for(i in 1:length(CoefName3)) {
    
    if (DataSet2[j,CoefName3[i]] !=0) {W.ParaEst.SEMatrix[j,i]<- DataSet2$AIC.c.W[j]*(sqrt((DataSet2[j,CoefName3[i]])^2+(DataSet2[j,CoefName2[i]]-s.W.ParaEstMatrix1[j,i])^2))} else { 
      W.ParaEst.SEMatrix[j,i]<- 0}
  }
}

# View filled W.ParaEst.SEMatrix.
W.ParaEst.SEMatrix

#-----
# Summed W.ParaEst.SEs (Unconditional SEs)
# Specify a vector for summed W.ParaEsts.SEs.
s.W.CoefName3.SE<- c("s.W.Intercept.SE", 
                   "s.W.CollLoc_Distance.m.SE", 
                   "s.W.RunTimingGroupLate.SE", 
                   "s.W.RunTimingGroupMiddle.SE", 
                   "s.W.Collection_Year2020.SE", 
                   "s.W.Collection_Year2021.SE",
                   "s.W.SexM.SE",
                   "s.W.RunTimingGroupLate.SexM.SE", 
                   "s.W.RunTimingGroupMiddle.SexM.SE",
                   "s.W.Collection_Year2020.SexM.SE",
                   "s.W.Collection_Year2021.SexM.SE")

# View summed W.ParaEst.SEs.
s.W.CoefName3.SE

# Create matrix to hold outputs for summed W.ParaEst.SEs

# III. Summed W.ParaEst.SE Matrix, ncol=8 includes all s.W.CoefNames2.SE listed above
s.W.ParaEst.SEMatrix<- as.data.frame(matrix(NA, nrow=1, ncol=length(s.W.CoefName3.SE), dimnames=list(c(1:length(1)), c(s.W.CoefName3.SE))))

# View s.W.ParaEst.SEMatrix
s.W.ParaEst.SEMatrix

#-----
# Run s.W.ParaEst.SE loop. Calculates summed W.ParaEsts by summing all

for(i in 1:length(s.W.CoefName3.SE)) {
  
  s.W.ParaEst.SE<- sum(W.ParaEst.SEMatrix[W.CoefName3.SE[i]])
  s.W.ParaEst.SE[i]<- s.W.ParaEst.SE
  s.W.ParaEst.SEMatrix[,i]<- s.W.ParaEst.SE[i]
}

# View filled s.W.ParaEst.SEMatrix
s.W.ParaEst.SEMatrix

# Transpose s.W.ParaEst.SEMatrix
transpose.s.W.ParaEst.SEMatrix<- t(s.W.ParaEst.SEMatrix)

# View transposed s.W.ParaEst.SEMatrix
transpose.s.W.ParaEst.SEMatrix

#-----
# Unconditional CIs
# Specify a vector for summed W.ParaEst.CIs.
s.W.CoefName3.CI<- c("s.W.Intercept.CI", 
                     "s.W.CollLoc_Distance.m.CI", 
                     "s.W.RunTimingGroupLate.CI", 
                     "s.W.RunTimingGroupMiddle.CI", 
                     "s.W.Collection_Year2020.CI", 
                     "s.W.Collection_Year2021.CI",
                     "s.W.SexM.CI",
                     "s.W.RunTimingGroupLate.SexM.CI", 
                     "s.W.RunTimingGroupMiddle.SexM.CI",
                     "s.W.Collection_Year2020.SexM.CI",
                     "s.W.Collection_Year2021.SexM.CI")

# View summed W.ParaEst.CIs.
s.W.CoefName3.CI

# Create matrix to hold outputs for summed W.ParaEst.CIs

# III. Summed W.ParaEst.CI Matrix, ncol=8 includes all s.W.CoefNames2.CI listed above
s.W.ParaEst.CIMatrix<- as.data.frame(matrix(NA, nrow=1, ncol=length(s.W.CoefName3.CI), dimnames=list(c(1:length(1)), c(s.W.CoefName3.CI))))

# View s.W.ParaEst.CIMatrix
s.W.ParaEst.CIMatrix

# Run s.W.ParaEst.CI loop. Calculates summed W.ParaEst.CIs by multiplying 1.96*summed W.ParaEst.Ses

for(i in 1:length(s.W.CoefName3.SE)) {
  
  s.W.ParaEst.CI<- s.W.ParaEst.SEMatrix[s.W.CoefName3.SE[i]]*1.96
  s.W.ParaEst.CIMatrix[,i]<- s.W.ParaEst.CI
}

# View filled s.W.ParaEst.CIMatrix
s.W.ParaEst.CIMatrix

# Transpose s.W.ParaEst.CIMatrix
transpose.s.W.ParaEst.CIMatrix<- t(s.W.ParaEst.CIMatrix)

# View transposed s.W.ParaEst.CIMatrix
transpose.s.W.ParaEst.CIMatrix

#-----
# II. Bind DataSet2, W.ParaEstMatrix, W.ParaEst.SEMatrix

W.AICFinalMatrix<- cbind(DataSet2,W.ParaEstMatrix,W.ParaEst.SEMatrix)

# View W.AICFinaMatrix
W.AICFinalMatrix

# Write W.AICFinaMatrix to a .cvs file
write.table(W.AICFinalMatrix, file="W.AICFinalMatrix.csv", col.names=NA, sep=",")

#-----
# III. Bind ParalikMatrix, s.W.ParaEst.SEMatrix, s.W.ParaEst.CIMatrix
s.W.AICFinalMatrix<- cbind(transpose.ParalikMatrix,transpose.s.W.ParaEstMatrix2,transpose.s.W.ParaEst.SEMatrix,transpose.s.W.ParaEst.CIMatrix)

# View s.W.AICFinalMatrix
s.W.AICFinalMatrix

# Create vector for s.W.AICFinalMatrix column names.
s.W.AICFinalMatrixColName<- c("Paralik","s.W.ParaEst","UncondSE","UncondCI")

s.W.AICFinalMatrixColName

# Write s.W.AICFinaMatrix to a .cvs file. Note will have to move Column names over 1 column to be correct because R is weird.
write.table(s.W.AICFinalMatrix, file="s.W.AICFinalMatrix.csv", col.names=(s.W.AICFinalMatrixColName), sep=",")

#-----
# End code. You should have now produced the following files with this code: 1-AICFinalMatrix.csv; use this file to sort models by deltaAIC.c and in excel calculate manually the cumAIC.c.W values. 2-Model Summaries_.doc; this is a word doc that holds the output from all models in the Candidate Set. 3-W.AICFinalMatrixWS.csv; this file is used as DataSet2 to run weighted calcs. 4-W.AICFinalMatrix.csv; this file contains all Models, Para Ests and associated SEs, AIC.c.Ws for each model, calculated W.Para Ests and associated W.SEs. These are the data that are used to create summed weighted estimates. 5-s.W.AICFinalMatrix.csv; this file holds summed weighted estimates including Parameter Likelihoods, s.W.ParaEsts, unconditional SEs, and unconditional CIs. !!When all these files have been created, cut and paste them into 1 excel file with 4 sheets for each part of the analysis and label this excel file the same name as that for the Model Summaries_.doc file so that things can be referenced easily within the same analysis.

# Yahoo! The End.



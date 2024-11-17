#
# KB Gorman
# AICc code to Abby, penguin SSD example
# November 2024
#
#-----

# AIC with Year as only environmental variable, modeled categorically.
#-----
# Reset R, clear all objects

rm(list=ls()) 

#-----
# Load Data
# You can skip this part if you want...

setwd("/Users/kristengorman/Documents/R Data Penguins")
getwd()
list.files()

#-----
# Testing for normality of delta.15.N, delta.13.C. and R.PC1.2

list.files()
SSD.IsoVarSex.DataSet<-read.csv("ADPE.SSD.IsoVarSex.DataSet.Sub3.csv")
SSD.IsoVarSex.DataSet
nrow(SSD.IsoVarSex.DataSet)

jpeg(file="Norm.Plot.ADPE.Ads.delta.15.N.jpg")
qqnorm(SSD.IsoVarSex.DataSet$delta.15.N)
qqline(SSD.IsoVarSex.DataSet$delta.15.N)
dev.off()

jpeg(file="HistPlot.ADPE.Ads.delta.15.N.jpg")
hist(SSD.IsoVarSex.DataSet$delta.15.N)
dev.off()

shapiro.test(SSD.IsoVarSex.DataSet$delta.15.N)

jpeg(file="Norm.Plot.ADPE.Ads.delta.13.C.jpg")
qqnorm(SSD.IsoVarSex.DataSet$delta.13.C)
qqline(SSD.IsoVarSex.DataSet$delta.13.C)
dev.off()

jpeg(file="HistPlot.ADPE.Ads.delta.13.C.jpg")
hist(SSD.IsoVarSex.DataSet$delta.13.C)
dev.off()

shapiro.test(SSD.IsoVarSex.DataSet$delta.13.C)

jpeg(file="Norm.Plot.ADPE.Ads.R.PC1.2.jpg")
qqnorm(SSD.IsoVarSex.DataSet$R.PC1.2)
qqline(SSD.IsoVarSex.DataSet$R.PC1.2)
dev.off()

jpeg(file="HistPlot.ADPE.Ads.R.PC1.2.jpg")
hist(SSD.IsoVarSex.DataSet$R.PC1.2)
dev.off()

shapiro.test(SSD.IsoVarSex.DataSet$R.PC1.2)

#-----
# Open the file just created, "ADPE.SSD.IsoVarSex.DataSet.Sub3.csv" and be sure to discard column, 1 and change all contunous variables to a num.

# Start here...
# Reset R, clear all objects.
rm(list=ls())

#-----
# Specify working directory and see what files are available.
setwd("/Users/kristengorman/Documents/R Data Penguins")
getwd()
list.files()

# Identify the data to be used.
DataFileName<- "ADPE.SSD.IsoVarSex.DataSet.Sub3.csv"

# Identify the models to be run.
ModelFileName<-"ADPE.SSD.IsoVarSex.DataSet.CandSet.csv"

#-----
# Import the data. Be sure to check the structure of the data and that R is reading continuous and categorical variables appropriately.
DataSet<-read.csv(DataFileName)
#DataSet<-read.csv("ADPE.SSD.IsoVarSex.DataSet.Sub3.csv")

# View DataSet.
head(DataSet)
summary(DataSet)
nrow(DataSet)
str(DataSet)

# Change parameter structures if needed.
DataSet$Year.2<- as.factor(DataSet$Year.2)
DataSet$Sex.2.M.F<- as.factor(DataSet$Sex.2.M.F)
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
globalmodel<- lm(delta.15.N~Sex.2.M.F*R.PC1.2*Year.2*Sex.2.M.F:Year.2*R.PC1.2:Year.2, data=DataSet)

AllCombCoefName<-names(globalmodel$coef)

# View all combinations of predictor variables.
AllCombCoefName

#-----
# Specify names of main explanatory parameters and associated SEs in the ModelSet (including interactions). Need to use : for interactions. !!Make sure that when creating any subsequent versions of CoefName in the code below that it follows the same order as is listed here. Further, when a candidate set has interaction models where (a) both main parameters are not included in the model, or where (b) the main effect is a categorical variable, you must include columns labeled here for all appropriate output!!
CoefName<- c("Intercept", "Intercept.SE", "Sex.2.M.F1", "Sex.2.M.F1.SE", "R.PC1.2", "R.PC1.2.SE", "Year.22", "Year.22.SE", "Year.23", "Year.23.SE", "Sex.2.M.F1:Year.22", "Sex.2.M.F1:Year.22.SE", "Sex.2.M.F1:Year.23", "Sex.2.M.F1:Year.23.SE", "R.PC1.2:Year.22", "R.PC1.2:Year.22.SE", "R.PC1.2:Year.23", "R.PC1.2:Year.23.SE")

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
  AICModelMatrix[i,"Sex.2.M.F1"]<- coef(m)["Sex.2.M.F1"]
  AICModelMatrix[i,"Sex.2.M.F1.SE"]<- summary(m)$coef[,2]["Sex.2.M.F1"]
  AICModelMatrix[i,"R.PC1.2"]<- coef(m)["R.PC1.2"]
  AICModelMatrix[i,"R.PC1.2.SE"]<- summary(m)$coef[,2]["R.PC1.2"]
  AICModelMatrix[i,"Year.22"]<- coef(m)["Year.22"]
  AICModelMatrix[i,"Year.22.SE"]<- summary(m)$coef[,2]["Year.22"]
  AICModelMatrix[i,"Year.23"]<- coef(m)["Year.23"]
  AICModelMatrix[i,"Year.23.SE"]<- summary(m)$coef[,2]["Year.23"]
  AICModelMatrix[i,"Sex.2.M.F1:Year.22"]<- coef(m)["Sex.2.M.F1:Year.22"]
  AICModelMatrix[i,"Sex.2.M.F1:Year.22.SE"]<- summary(m)$coef[,2]["Sex.2.M.F1:Year.22"]
  AICModelMatrix[i,"Sex.2.M.F1:Year.23"]<- coef(m)["Sex.2.M.F1:Year.23"]
  AICModelMatrix[i,"Sex.2.M.F1:Year.23.SE"]<- summary(m)$coef[,2]["Sex.2.M.F1:Year.23"]
  AICModelMatrix[i,"R.PC1.2:Year.22"]<- coef(m)["R.PC1.2:Year.22"]
  AICModelMatrix[i,"R.PC1.2:Year.22.SE"]<- summary(m)$coef[,2]["R.PC1.2:Year.22"]
  AICModelMatrix[i,"R.PC1.2:Year.23"]<- coef(m)["R.PC1.2:Year.23"]
  AICModelMatrix[i,"R.PC1.2:Year.23.SE"]<- summary(m)$coef[,2]["R.PC1.2:Year.23"]
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
# Begin weighted and summed AIC calcs. Save previous file with AICFinalMatrix output, excluding column A (Model #) and N.Obs through the lik.dAIC.c columns, as weighted AIC Model Matrix Worksheet (W.AICFinalMatrixWS.csv). For example, the scaup dataset, be sure to cut and paste the parameter output for "RFGinit:Rlipid", "RFGinit:Rlipid.SE" and paste into the column for "Rlipid:RFGinit", "Rlipid:RFGinit.SE".

#-----
# Reset R, clear all objects.
rm(list=ls())

#-----
# Specify working directory and see whats available. Make sure that new file created above is listed.
setwd("/Users/kristengorman/Documents/R Data Penguins")
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
CoefName<- c("Intercept", "Intercept.SE", "Sex.2.M.F1", "Sex.2.M.F1.SE", "R.PC1.2", "R.PC1.2.SE", "Year.22", "Year.22.SE", "Year.23", "Year.23.SE", "Sex.2.M.F1.Year.22", "Sex.2.M.F1.Year.22.SE", "Sex.2.M.F1.Year.23", "Sex.2.M.F1.Year.23.SE", "R.PC1.2.Year.22", "R.PC1.2.Year.22.SE", "R.PC1.2.Year.23", "R.PC1.2.Year.23.SE")

# View CoefName.
CoefName

CoefName2<- c("Intercept", "Sex.2.M.F1", "R.PC1.2", "Year.22", "Year.23", "Sex.2.M.F1.Year.22", "Sex.2.M.F1.Year.23", "R.PC1.2.Year.22", "R.PC1.2.Year.23")

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

sub2<- subset(DataSet2, !is.na(Sex.2.M.F1))
nrow(sub2)
ParalikSex.2.M.F1<- sum(sub2$AIC.c.W)
ParalikMatrix[2]<- signif(ParalikSex.2.M.F1, digits=12)

sub3<- subset(DataSet2, !is.na(R.PC1.2))
nrow(sub3)
ParalikR.PC1.2<- sum(sub3$AIC.c.W)
ParalikMatrix[3]<- signif(ParalikR.PC1.2, digits=12)

sub4<- subset(DataSet2, !is.na(Year.22))
nrow(sub4)
ParalikYear.22<- sum(sub4$AIC.c.W)
ParalikMatrix[4]<- signif(ParalikYear.22, digits=12)

sub5<- subset(DataSet2, !is.na(Year.23))
nrow(sub5)
ParalikYear.23<- sum(sub5$AIC.c.W)
ParalikMatrix[5]<- signif(ParalikYear.23, digits=12)

sub6<- subset(DataSet2, !is.na(Sex.2.M.F1.Year.22))
nrow(sub6)
ParalikSex.2.M.F1.Year.22<- sum(sub6$AIC.c.W)
ParalikMatrix[6]<- signif(ParalikSex.2.M.F1.Year.22, digits=12)

sub7<- subset(DataSet2, !is.na(Sex.2.M.F1.Year.23))
nrow(sub7)
ParalikSex.2.M.F1.Year.23<- sum(sub7$AIC.c.W)
ParalikMatrix[7]<- signif(ParalikSex.2.M.F1.Year.23, digits=12)

sub8<- subset(DataSet2, !is.na(R.PC1.2.Year.22))
nrow(sub8)
ParalikR.PC1.2.Year.22<- sum(sub8$AIC.c.W)
ParalikMatrix[8]<- signif(ParalikR.PC1.2.Year.22, digits=12)

sub9<- subset(DataSet2, !is.na(R.PC1.2.Year.23))
nrow(sub9)
ParalikR.PC1.2.Year.23<- sum(sub9$AIC.c.W)
ParalikMatrix[9]<- signif(ParalikR.PC1.2.Year.23, digits=12)


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
W.CoefName2<- c("W.Intercept", "W.Sex.2.M.F1", "W.R.PC1.2", "W.Year.22", "W.Year.23", "W.Sex.2.M.F1.Year.22", "W.Sex.2.M.F1.Year.23", "W.R.PC1.2.Year.22", "W.R.PC1.2.Year.23")

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
s.W.CoefName2<- c("s.W.Intercept", "s.W.Sex.2.M.F1", "s.W.R.PC1.2", "s.W.Year.22", "s.W.Year.23", "s.W.Sex.2.M.F1.Year.22", "s.W.Sex.2.M.F1.Year.23", "s.W.R.PC1.2.Year.22", "s.W.R.PC1.2.Year.23")

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
CoefName3<- c("Intercept.SE", "Sex.2.M.F1.SE", "R.PC1.2.SE", "Year.22.SE", "Year.23.SE", "Sex.2.M.F1.Year.22.SE", "Sex.2.M.F1.Year.23.SE", "R.PC1.2.Year.22.SE", "R.PC1.2.Year.23.SE")

# View CoefName3
CoefName3

# Specify a vector for new W.ParaEst.SE.
W.CoefName3.SE<- c("W.Intercept.SE", "W.Sex.2.M.F1.SE", "W.R.PC1.2.SE", "W.Year.22.SE", "W.Year.23.SE", "W.Sex.2.M.F1.Year.22.SE", "W.Sex.2.M.F1.Year.23.SE", "W.R.PC1.2.Year.22.SE", "W.R.PC1.2.Year.23.SE")

# View W.CoefName2.SE
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
s.W.CoefName3.SE<- c("s.W.Intercept.SE", "s.W.Sex.2.M.F1.SE", "s.W.R.PC1.2.SE", "s.W.Year.22.SE", "s.W.Year.23.SE", "s.W.Sex.2.M.F1.Year.22.SE", "s.W.Sex.2.M.F1.Year.23.SE", "s.W.R.PC1.2.Year.22.SE", "s.W.R.PC1.2.Year.23.SE")

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
s.W.CoefName3.CI<- c("s.W.Intercept.CI", "s.W.Sex.2.M.F1.CI", "s.W.R.PC1.2.CI", "s.W.Year.22.CI", "s.W.Year.23.CI", "s.W.Sex.2.M.F1.Year.22.CI", "s.W.Sex.2.M.F1.Year.23.CI", "s.W.R.PC1.2.Year.22.CI", "s.W.R.PC1.2.Year.23.CI")

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
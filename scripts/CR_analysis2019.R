#2019 Data Analysis Script for Body Composition Data & Pathogen Synthesis

#loading packages for donwloading data from gsheets
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)

write.csv(read.csv(text=gsheet2text("https://docs.google.com/spreadsheets/d/1PuZgbr8eBlCNh1ygfhuv2W_xsryhhezn/edit#gid=1548316807",format="csv"),stringsAsFactors = F),
          row.names = FALSE,
          "odata/crbody_2019.csv") #downloads 2019 body comp data from google sheets, make sure shareable by link in google

write.csv(read.csv(text=gsheet2text("https://docs.google.com/spreadsheets/d/1zOiVbDwnPme8mJdAlvX1QJ30etYJGBwA/edit#gid=1261110748",format="csv"),stringsAsFactors = F),
          row.names = FALSE,
          "odata/crpathogens_2019.csv") 
#notes on this file: shows the copy number as determined by the Biomark software using the MGL APC standards.	
#Assay specific LOD limits have been applied	
# 0 = negative result	
# 999 = duplicate results did not match. This is most often due to the high CT (low load) samples on the edge of the detection limits (or LOD limit in LOD worksheet) and can generally be converted to a negative (0)	

bodycomp <- read.csv("odata/crbody_2019.csv")
path <- read.csv("odata/crpathogens_2019.csv")
str(path)

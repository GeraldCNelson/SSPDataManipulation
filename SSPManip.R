# Intro -------------------------------------------------------------------
#This script reads in the Shared Socioeconomic Profiles information, does some manipulations of the data,
#and writes out results to an excel spreadsheet

#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.

setwd("~/Documents/workspace/SSPDataManipulation")
library(openxlsx) 
library(reshape2)
library(splitstackshape)
#library(plotrix)
#userName is reported in the basic info worksheet
userName <- "Gerald Nelson"

# file names
IMPACTregionsFileName <- "../nutrient-modeling/IMPACTRegionsJan2015.xlsx"
SSPdataZipFileLocation <- c("data/SspDb_country_data_2013-06-12.csv.zip")
SSPdataZipFileName <- c("SspDb_country_data_2013-06-12.csv")

# Read in and manipulate the SSP data -------------------------------------

SSPDat <- read.csv(unz(description = SSPdataZipFileLocation, file=SSPdataZipFileName), stringsAsFactors=FALSE)
#remove years 1950 to 1995 and 2105 to 2150 because they are all NAs. In addition, NCAR has NA for 2000 and 2005.
#Remove years X2000 and X2005 later because they are NA for pop values
keepCols <- c("MODEL", "SCENARIO", "REGION", "VARIABLE","UNIT","X2000","X2005","X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050","X2055","X2060","X2065","X2070","X2075","X2080","X2085","X2090","X2095","X2100")
SSPDat <- SSPDat[,keepCols]

# Extract lists of the scenarios, regions, and data variables
scenarios <- unique(SSPDat$SCENARIO)
regions <- unique(SSPDat$REGION)
vNames <- unique(SSPDat$VARIABLE)

# Create population-only data set by removing rows that include education breakdown and GDP
popList <- "Population"
ageList <- c("Aged0-4", "Aged5-9","Aged10-14", "Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34", "Aged35-39", "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64", "Aged65-69", "Aged70-74", "Aged75-79", "Aged80-84", "Aged85-89", "Aged90-94", "Aged95-99", "Aged100+")
edList <- c("No Education", "Primary Education", "Secondary Education", "Tertiary Education")
genderList <-c("Male","Female")
incomeList <- c("GDP|PPP")
modelList.income <- c("OECD Env-Growth", "PIK GDP-32", "IIASA GDP")
modelList.population <- c("IIASA-WiC POP","NCAR") 
urbanList <- c("Population|Urban|Share")
# NCAR data are for 10 year periods, starting in 2010 and ending in 2100 and are broken down by Urban and Rural
# IIASA population data are for 5 year periods, starting in 2010 and ending in 2100 and 
# are broken down by age, education, and gender

# Subset the full data by model
SSPDat.IIASA <- subset(SSPDat,(MODEL %in% modelList.population[1]))
SSPDat.NCAR <- subset(SSPDat,(MODEL %in% modelList.population[2]))
SSPDat.OECD <- subset(SSPDat,(MODEL %in% modelList.income[2]))

#fill in every fifth year for NCAR with linear interpolation
SSPDat.NCAR$X2015 <- (SSPDat.NCAR$X2010 + SSPDat.NCAR$X2020)/2
SSPDat.NCAR$X2025 <- (SSPDat.NCAR$X2020 + SSPDat.NCAR$X2030)/2
SSPDat.NCAR$X2035 <- (SSPDat.NCAR$X2030 + SSPDat.NCAR$X2040)/2
SSPDat.NCAR$X2045 <- (SSPDat.NCAR$X2040 + SSPDat.NCAR$X2050)/2
SSPDat.NCAR$X2055 <- (SSPDat.NCAR$X2050 + SSPDat.NCAR$X2060)/2
SSPDat.NCAR$X2065 <- (SSPDat.NCAR$X2060 + SSPDat.NCAR$X2070)/2
SSPDat.NCAR$X2075 <- (SSPDat.NCAR$X2070 + SSPDat.NCAR$X2080)/2
SSPDat.NCAR$X2085 <- (SSPDat.NCAR$X2080 + SSPDat.NCAR$X2090)/2
SSPDat.NCAR$X2095 <- (SSPDat.NCAR$X2090 + SSPDat.NCAR$X2100)/2

# Remove GDP data, NCAR population data, and the aggregates of 
# "Population", "Population|Female" and "Population|Male"
removeList <- c("GDP|PPP", "NCAR","Population", "Population|Female", "Population|Male")
pop.IIASA <-SSPDat[!SSPDat$VARIABLE %in% removeList,]
pop.IIASA <- cSplit(pop.IIASA, 'VARIABLE', sep="|", type.convert=FALSE)
setnames(pop.IIASA, "VARIABLE_2", "gender")
setnames(pop.IIASA, "VARIABLE_3", "age")
pop.IIASA$age<- gsub("Aged","",pop.IIASA$age)
setnames(pop.IIASA, "VARIABLE_4", "education")
mpop.IIASA <- melt(pop.IIASA, id.vars=c("MODEL","SCENARIO","REGION","UNIT","gender","age","education"),
             measure.vars =c("X2010","X2015","X2020","X2025", "X2030","X2035","X2040","X2045",
                             "X2050","X2055","X2060","X2065","X2070","X2075", "X2080","X2085",
                             "X2090","X2095","X2100"))
mpop.IIASA.sort <- mpop.IIASA[with( mpop.IIASA, order(SCENARIO, REGION, variable, age)), ]
#remove rows with NA in the education column. These are the totals from summing across all the education possibilities.
mpop.IIASA.sort.complete <- mpop.IIASA.sort[complete.cases(mpop.IIASA.sort[,education]),]

# have women and men be separate columns
mpop.IIASA.sort.complete <- dcast.data.table(mpop.IIASA.sort.complete, MODEL + SCENARIO + UNIT + REGION + age + education + variable ~ gender, mean, value.var = "value")

# use the inverse to get the pop totals, then have men and women in separate columns, or have columns for each year
mpop.IIASA.sort.tot <- mpop.IIASA.sort[!complete.cases(mpop.IIASA.sort[,education]),]
mpop.IIASA.sort.gender <- dcast.data.table(mpop.IIASA.sort.tot, 
                                           MODEL + SCENARIO + UNIT + REGION + age + education + variable ~ gender, mean, value.var = "value")
pop.IIASA.tot.years <- dcast.data.table(mpop.IIASA.sort.tot, 
                                  MODEL + SCENARIO + UNIT + REGION + age + gender ~ variable, mean, value.var = "value")

#pop.wide.sort <- pop.wide.sort[order(c(variable,age))]

income <- subset(SSPDat,(VARIABLE %in% incomeList))
#income.PIK <- subset(income,(MODEL %in% modelList.income[2])) #Note: income.PIK only has data for the US
income.OECD <- subset(income,(MODEL %in% modelList.income[1]))
#income.IIASA <- subset(income,(MODEL %in% modelList.income[3]))
urbanShare <-subset(SSPDat.NCAR,(VARIABLE %in% urbanList)) #Model is NCAR; only has values every 10 years

#list all the regions used by each model in the SSP data
regions.IIASA <- sort(unique(SSPDat.IIASA$REGION))
regions.NCAR <- sort(unique(SSPDat.NCAR$REGION))
regions.NCAR <- as.data.frame(regions.NCAR); colnames(regions.NCAR)[1]<-"CTY"; regions.NCAR$CTY <- sort(regions.NCAR$CTY)
regions.IIASA <- as.data.frame(regions.IIASA); colnames(regions.IIASA)[1]<-"CTY"; regions.IIASA$CTY <- sort(regions.IIASA$CTY)

# get the regions used by IMPACT
regions.IMPACT <- read.xlsx(IMPACTregionsFileName, colNames = TRUE, sheet = 1)

# make sure that the regions used by both IMPACT and IIASA are include
regions.combined <- merge(regions.IMPACT,regions.IIASA, by="CTY", all = TRUE)

# reorder IIASA population data so the country codes are the columns
pop.IIASA.wide <-    dcast(mpop.IIASA, MODEL + SCENARIO + VARIABLE + UNIT + variable ~ REGION, 
                           mean, value.var = "value")

#set up income for IMPACT regions
mincome.IIASA <- melt(income.IIASA, id=c("MODEL","SCENARIO","REGION","VARIABLE","UNIT"),
                      measure.vars =c("X2010","X2015","X2020","X2025", "X2030","X2035","X2040","X2045",
                                  "X2050","X2055","X2060","X2065","X2070","X2075", "X2080","X2085",
                                  "X2090","X2095","X2100"))

# reorder IIASA income data so the country codes are the columns
income.IIASA.wide <- dcast(mincome.IIASA, MODEL + SCENARIO + VARIABLE + UNIT + variable ~ REGION, 
                           mean, value.var = "value")

#now add the new IFPRI regions. The source command runs the RegionsAlignment script
source(file = "RegionsAlignment.R")

# IMPACT calls regions that are groupings of individual countries 'plus regions', as in FRA is FrancePlus
# loop through all the plus regions in IMPACT, generated by RegionsAlignment
for (j in 1:length(plusRegions)) { 
  ctyList <- eval(parse(text = plusRegions[j]))
  ctyListNew <- character()
  for (i in 1:length(ctyList)) { #look at all the country names in a plus region to make sure they are a IIASA country
    if(!(ctyList[i] %in% colnames(pop.IIASA.wide))) { #identify countries not in IIASA list
      print(paste(ctyList[i],"from", plusRegions[j], "is not in IIASA countries"))
    }
    else {
      ctyListNew <- c(ctyListNew,ctyList[i])
    }
  }
  print(paste(ctyListNew, " is in ctyListNew for", plusRegions[j]))
  #construct column names by adding the df name to the front of the cty name.
  ctyList <- gsub("^","pop.IIASA.wide$",ctyListNew)
  
  #convert the list into an expression that sums across the cty columns that are in the IIASA data, 
  # and that make up the IMPACT region
  pop.IIASA.wide$tmp <- eval(parse(text = paste(ctyList, sep = "", collapse=" + ")))
  income.IIASA.wide$tmp <- eval(parse(text = paste(ctyList, sep = "", collapse=" + ")))
  #Give the tmp column its correct name
  names(pop.IIASA.wide)[names(pop.IIASA.wide)=="tmp"] <- plusRegions[j]
  names(income.IIASA.wide)[names(income.IIASA.wide)=="tmp"] <- plusRegions[j]
  #Now remove the countries that make up the IMPACT plus region
  #ctyListNew.as.c <- c(paste(ctyListNew, sep = "\", collapse=" , "
  pop.IIASA.wide <- pop.IIASA.wide[,!(names(pop.IIASA.wide) %in% ctyListNew)]
  income.IIASA.wide <- income.IIASA.wide[,!(names(income.IIASA.wide) %in% ctyListNew)]

}



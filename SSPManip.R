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

setwd("~/Documents/workspace/SSP")
library(openxlsx) 
library(reshape2)
library(splitstackshape)
#library(plotrix)
#userName is reported in the basic info worksheet
userName <- "Gerald Nelson"
SSPDat <- read.csv("~/Documents/workspace/nutrient-modeling/data/SSPData/SspDb_country_data_2013-06-12.csv", stringsAsFactors=FALSE)
#remove years 1950 to 1995 and 2105 to 2150 because they are all NAs. 
#Remove years X2000 and X2005 later because they are NA for pop values
keepCols <- c("MODEL", "SCENARIO", "REGION", "VARIABLE","UNIT","X2000","X2005","X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050","X2055","X2060","X2065","X2070","X2075","X2080","X2085","X2090","X2095","X2100")
SSPDat <- SSPDat[,keepCols]

scenarios <- unique(SSPDat$SCENARIO)
regions <- unique(SSPDat$REGION)
vNames <- unique(SSPDat$VARIABLE)
#create population-only data set by removing rows that include education breakdown and GDP
popList <- "Population"
ageList <- c("Aged0-4", "Aged5-9","Aged10-14", "Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34", "Aged35-39", "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64", "Aged65-69", "Aged70-74", "Aged75-79", "Aged80-84", "Aged85-89", "Aged90-94", "Aged95-99", "Aged100+")
edList <- c("No Education", "Primary Education", "Secondary Education", "Tertiary Education")
genderList <-c("Male","Female")
incomeList <- c("GDP|PPP")
modelList.income <- c("OECD Env-Growth", "PIK GDP-32", "IIASA GDP")
modelList.population <- c("IIASA-WiC POP","NCAR") 
#NCAR data are for 10 year periods, starting in 2010 and ending in 2100
#IIASA population data are for 5 year periods, starting in 2010 and ending in 2100
urbanList <- c("Population|Urban|Share")
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

removeList <- c("GDP|PPP", "NCAR","Population", "Population|Female", "Population|Male")
pop <-SSPDat[!SSPDat$VARIABLE %in% removeList,]
pop <- cSplit(pop, 'VARIABLE', sep="|", type.convert=FALSE)
setnames(pop, "VARIABLE_2", "gender")
setnames(pop, "VARIABLE_3", "age")
pop$age<- gsub("Aged","",pop$age)
setnames(pop, "VARIABLE_4", "education")
mpop <- melt(pop, id.vars=c("MODEL","SCENARIO","REGION","UNIT","gender","age","education"),
             measure.vars =c("X2010","X2015","X2020","X2025", "X2030","X2035","X2040","X2045",
                             "X2050","X2055","X2060","X2065","X2070","X2075", "X2080","X2085",
                             "X2090","X2095","X2100"))
mpop.sort <- mpop[with( mpop, order(SCENARIO, REGION, variable, age)), ]
#remove rows with NA in the education column. These are the totals from summing across all the education possibilities.
mpop.sort.complete <- mpop.sort[complete.cases(mpop.sort[,education]),]

# have women and men be separate columns
mpop.sort.complete.wide <- dcast.data.table(mpop.sort.complete, MODEL + SCENARIO + UNIT + REGION + age + education + variable ~ gender, mean, value.var = "value")

# use the inverse to get the pop totals, then have men and women in separate columns, or have columns for each year
mpop.sort.tot <- mpop.sort[!complete.cases(mpop.sort[,education]),]
mpop.sort.gender <- dcast.data.table(mpop.sort.tot, MODEL + SCENARIO + UNIT + REGION + age + education + variable ~ gender, mean, value.var = "value")
pop.tot.years <- dcast.data.table(mpop.sort.tot, MODEL + SCENARIO + UNIT + REGION + age + gender ~ variable, mean, value.var = "value")


pop.wide.sort <- pop.wide.sort[order(c(variable,age))]

pop.IIASA <- subset(SSPDat.IIASA,(VARIABLE %in% ageList))
pop.NCAR <- subset(SSPDat.NCAR,(VARIABLE %in% ageList)) #Note: Pop.NCAR only has values every 10 years
income <- subset(SSPDat,(VARIABLE %in% incomeList))
income.PIK <- subset(income,(MODEL %in% modelList.income[2])) #Note: income.PIK only has data for the US
income.OECD <- subset(income,(MODEL %in% modelList.income[1]))
income.IIASA <- subset(income,(MODEL %in% modelList.income[3]))
urbanShare <-subset(SSPDat.NCAR,(VARIABLE %in% urbanList)) #Model is NCAR; only has values every 10 years
regions.IIASA <- sort(unique(SSPDat.IIASA$REGION))
regions.NCAR <- sort(unique(SSPDat.NCAR$REGION))
regions.NCAR <- as.data.frame(regions.NCAR); colnames(regions.NCAR)[1]<-"CTY"; regions.NCAR$CTY <- sort(regions.NCAR$CTY)
regions.IIASA <- as.data.frame(regions.IIASA); colnames(regions.IIASA)[1]<-"CTY"; regions.IIASA$CTY <- sort(regions.IIASA$CTY)
IMPACTregionsFileName <- "../nutrient-modeling/IMPACTRegionsJan2015.xlsx"
regions.IMPACT <- read.xlsx(IMPACTregionsFileName, colNames = TRUE, sheet = 1)
regions.combined <- merge(regions.IMPACT,regions.IIASA, by="CTY", all = TRUE)

#set up pop for IMPACT regions
mpop.IIASA <- melt(pop.IIASA, id=c("MODEL","SCENARIO","REGION","VARIABLE","UNIT"),
                   measured =c("X2010","X2015","X2020","X2025", "X2030","X2035","X2040","X2045",
                               "X2050","X2055","X2060","X2065","X2070","X2075", "X2080","X2085",
                               "X2090","X2095","X2100"))
# reorders IIASA data so the country codes are the columns
pop.IIASA.wide <- dcast(mpop.IIASA,MODEL + SCENARIO + VARIABLE + UNIT + variable ~ REGION, mean, value.var = "value")

#set up income for IMPACT regions
mincome.IIASA <- melt(income.IIASA, id=c("MODEL","SCENARIO","REGION","VARIABLE","UNIT"),
                   measured =c("X2010","X2015","X2020","X2025", "X2030","X2035","X2040","X2045",
                               "X2050","X2055","X2060","X2065","X2070","X2075", "X2080","X2085",
                               "X2090","X2095","X2100"))
# reorders IIASA data so the country codes are the columns
income.IIASA.wide <- dcast(mincome.IIASA,MODEL + SCENARIO + VARIABLE + UNIT + variable ~ REGION, mean, value.var = "value")

#now add the new IFPRI regions
source(file = "RegionsAlignment.R")

for (j in 1:length(plusRegions)) { # loop through all the plus regions in IMPACT
  ctyList <- eval(parse(text = plusRegions[j]))
  ctyListNew <- character()
  for (i in 1:length(ctyList)) { #look at all the country names in a plus region to make sure they are a IIASA country
    if(!(ctyList[i] %in% colnames(pop.IIASA.wide))) { #identify countries not is IIASA list
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



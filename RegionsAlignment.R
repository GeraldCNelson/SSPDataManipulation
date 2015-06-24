# Intro -------------------------------------------------------------------
#This script aligns the IMPACT model regions (countries and a few country aggregates) information with the regions used in the 
#Shared Socioeconomic Pathways data

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

#Note that these 3 digit country codes are based on the ISO 3166 standard. See http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
#First do IMPACT regions
#For small countries and other political units, IMPACT has created regions that are essentially the largest political unit
#plusRegions are all the regions larger than a single political unit and what political units are included
plusRegions <- list()
plusRegionsText <- list()
BLT <- c("EST", "LTU", "LVA") 
BLTtext <- c("BLT Baltic States is Estonia EST, Lithuania LTU and Latvia LVA")
plusRegions <- c(plusRegions,"BLT")
plusRegionsText <- c(plusRegionsText,"BLTtext")

BLX <- c("BEL", "LUX")
BLXtext <- c("BLX Belgium-Luxembourg is Belgium BEL and Luxembourg LUX")
plusRegions <- c(plusRegions,"BLX")
plusRegionsText <- c(plusRegionsText,"BLXtext")

CHM <- c("CHN", "HKG", "MAC","TWN") 
CHMtext <- c("CHM China Plus is China CHN, Hong Kong HKG, Macao MAC and Taiwan TWN")
plusRegions <- c(plusRegions,"CHM")
plusRegionsText <- c(plusRegionsText,"CHMtext")

CHP <- c("CHE","LIE") 
CHPtext <- c("CHP Switzerland Plus is Switzerland CHE and Liechtenstein LIE")
plusRegions <- c(plusRegions,"CHP")
plusRegionsText <- c(plusRegionsText,"CHPtext")

CRB <- c("ABW", "AIA", "ANT", "ATG", "BES", "BHS", "BLM", "BRB", "CUW", "CYM", "DMA", "GLP", 
         "GRD", "KNA", "LCA", "MAF", "MSR", "MTQ", "PRI", "SXM", "TCA", "TTO", "VCT", "VGB", "VIR") 
CRBtext <- c("CRB Other Caribbean is Aruba ABW, Anguilla AIA, Netherlands Antilles (obsolete) ANT, Antigua ATG
 Bonaire, Sint Eustatius, and Saba BES, Bahamas BHS, St. Barthélemy BLM, Barbados BRB, Curacao CUW, Cayman Islands CYM
 Dominica DMA, Guadeloupe GLP, Grenada GRD, St. Kitts and Nevis KNA, St. Lucia LCA, Saint Martin MAF
 Montserrat MSR, Martinique MTQ, Puerto Rico PRI, Sint Maarten SXM, Turks and Caicos Islands TCA
Trinidad and Tobago TTO, St. Vincent and Grenadines VCT, British Virgin Islands VGB and U.S. Virgin Islands VIR")
plusRegions <- c(plusRegions,"CRB")
plusRegionsText <- c(plusRegionsText,"CRBtext")

DNP <- c("DNK","GRL") 
DNPtext <- c("DNP Denmark plus is DNK Denmark and GRL Greenland")
plusRegions <- c(plusRegions,"DNP")
plusRegionsText <- c(plusRegionsText,"DNKtext")

FNP <- c("ALA", "FIN") 
FNPtext <- c("FNP Finland Plus is Aland Islands ALA and Finland FIN")
plusRegions <- c(plusRegions,"FNP")
plusRegionsText <- c(plusRegionsText,"FNPtext")

FRP <- c("FRA", "MCO") 
FRPtext <- c("FRP France plus is France FRA and Monaco MCO")
plusRegions <- c(plusRegions,"FRP")
plusRegionsText <- c(plusRegionsText,"FRPtext")

GSA <- c("GUF", "GUY", "SUR") 
GSAtext <- c("GSA Guyanas is South America French Guiana GUF, Guyana GUY and Suriname SUR")
plusRegions <- c(plusRegions,"GSA")
plusRegionsText <- c(plusRegionsText,"GSAtext")

ITP <- c("ITA", "MLT", "SMR", "VAT") 
ITPtext <- c("ITP Italy plus is Italy ITA, Malta MLT, San Marino SMR and Vatican City VAT")
plusRegions <- c(plusRegions,"ITP")
plusRegionsText <- c(plusRegionsText,"ITPtext")

MOR <- c("MAR","ESH") 
MORtext <- c("MOR Morocco plus is Morocco MAR and Western Sahara ESH")
plusRegions <- c(plusRegions,"MOR")
plusRegionsText <- c(plusRegionsText,"MORtext")

OAO <- c("BMU", "BVT", "CPV", "FLK", "FRO", "SGS", "SHN", "SJM", "SPM", "STP", "BIH")
OAOtext <- c("OAO Other is Atlantic Ocean Bermuda BMU, Bouvet Island BVT, Cape Verde CPV, Falkland Islands FLK,
Faroe Islands FRO, South Georgia and South Sandwich Islands SGS, Saint Helena, Ascension, and Tristan de Cunha SHN,
Svalbard and Jan Mayen SJM, Saint Pierre and Miquelon SPM and Sao Tome and Principe STP")
plusRegions <- c(plusRegions,"OAO")
plusRegionsText <- c(plusRegionsText,"OAOtext")

OBN <- c("BIH", "MKD", "MNE", "SRB") 
OBNtext <- c("OBN Other is Balkans Bosnia-Herzegovina BIH, Macedonia (FYR) MKD, Montenegro MNE,  and Serbia SRB")
plusRegions <- c(plusRegions,"OBN")
plusRegionsText <- c(plusRegionsText,"OBNtext")

OIO <- c("ATF", "CCK", "COM", "CXR", "HMD", "IOT", "MDV", "MUS", "MYT", "REU", "SYC")
OIOtext <- c("OIO Other is Indian Ocean Southern Territories ATF Keeling Islands CCK Comoros COM Christmas Island CXR
 Heard and McDonald Islands HMD British Indian Ocean Territory IOT Maldives MDV Mauritius MUS
 Mayotte MYT Réunion REU Seychelles SYC")
plusRegions <- c(plusRegions,"OIO")
plusRegionsText <- c(plusRegionsText,"OIOtext")

OPO <- c("ASM", "COK", "FSM", "GUM", "KIR", "MHL", "MNP", "NCL", "NFK", "NIU", "NRU", "PCN", "PLW", "PYF", "TKL", 
         "TON", "TUV", "UMI", "WLF", "WSM")
OPOtext <- c("OPO Other is Pacific Ocean American Samoa ASM, Cook Islands COK, Micronesia FSM, Guam GUM,
 Kiribati KIR, Marshall Islands MHL, Northern Mariana Islands MNP, New Caledonia NCL, Norfolk Island NFK,
 Niue NIU, Nauru NRU, Pitcairn PCN, Palau PLW, French Polynesia PYF, Tokelau TKL, Tonga TON, Tuvalu TUV,
 Minor Outlying Islands UMI, Wallis and Futuna WLF, and Samoa WSM")
plusRegions <- c(plusRegions,"OPO")
plusRegionsText <- c(plusRegionsText,"OPOtext")

OSA <- c("BRN", "SGP") 
OSAtext <- c("OSA Other is Southeast Asia Brunei BRN Singapore SGP")
plusRegions <- c(plusRegions,"OSA")
plusRegionsText <- c(plusRegionsText,"OSAtext")

RAP <- c("ARE", "BHR", "KWT", "OMN", "QAT") 
RAPtext <- c("RAP  Rest of Arab is Peninsula	United Arab Emirates	ARE, 
 Bahrain	BHR, Kuwait	KWT, Oman	OMN, and Qatar	QAT")
plusRegions <- c(plusRegions,"RAP")
plusRegionsText <- c(plusRegionsText,"RAPtext")

SDP <- c("SSD","SDN") 
SDPtext <- c("SDP Sudan plus is Sudan SSD and South Sudan SDN")
plusRegions <- c(plusRegions,"SDP")
plusRegionsText <- c(plusRegionsText,"SDPtext")

SPP <- c("AND", "ESP", "GIB") 
SPPtext <- c("SPP  Spain plus	is Andorra	AND, Spain	ESP, and Gibraltar	GIB")
plusRegions <- c(plusRegions,"SPP")
plusRegionsText <- c(plusRegionsText,"SPPtext")

UKP <- c("GBR", "GGY", "IMN") 
UKPtext <- c("UKP Great Britain plus is	Great Britain	GBR,  Guernsey	GGY, and Isle of Man	IMN")
plusRegions <- c(plusRegions,"UKP")
plusRegionsText <- c(plusRegionsText,"UKPtext")

#plusRegions <- c("BLT", "BLX", "CHM", "CHP", "CRB", "FNP", 
#"FRP", "GSA","ITP", "MOR","OAO", "OBN", "OIO", "OPO", "OSA", "RAP", "SPP", "UKP")

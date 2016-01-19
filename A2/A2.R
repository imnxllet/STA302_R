#Building Permit#
#A1times.csv#
#activepermits.csv#
setwd("/Users/chris/Desktop/year2/class/STA302/A2")

#1a,b
permits <- read.csv("activepermits.csv", head=T)
require(lubridate)
permits <- within(permits, {
  APPLICATION_DATE <- as.Date(APPLICATION_DATE)
  ISSUED_DATE <- as.Date(ISSUED_DATE)
  Year <- factor(year(APPLICATION_DATE))
  Month <- factor(month.abb[month(APPLICATION_DATE)], levels = month.abb)
})

#1c
permits <- within(permits,{
  DATE_DIFF <-  ISSUED_DATE - APPLICATION_DATE
})

#1d
permits <- subset(permits, DATE_DIFF >= 0, select=c(Year, Month, DATE_DIFF, STATUS, POSTAL, CURRENT_USE,
                                                    PROPOSED_USE, DWELLING_UNITS_CREATED, DWELLING_UNITS_LOST))


#2a(i),(iii)
#Get us a table with property numbers(>30) in each type
permits$license = 1 
permits$CURRENT_USE <- toupper(permits$CURRENT_USE)
permits.tab1 <- with(permits, tapply(license,CURRENT_USE, sum)); 
subset(permits.tab1, permits.tab1>30)

#2a
#Modify and update CURRENT_USED NAMES.
#Update Apartment(IV)
permits <- within(permits, {
  
  CURRENT_USE <- factor(gsub("APARTMENT BUILDING", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APARTMENT BLDG", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APARTMENT UNIT", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APT. BLDG", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APT BLDG", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APT.", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APT", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APT BUILDING", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONDOMINIUM", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONDO", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APARTMENT ", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APARTMENTBUILDING", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APPARTMENT BUILDING", "APARTMENT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("APPARTMENTLDG", "APARTMENT", CURRENT_USE))


#Education, Restaurant, ParkingLot, Retail, Office

  #Education
  CURRENT_USE <- factor(gsub("ELEMENTARY SCHOOL", "EDUCATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SECONDARY SCHOOL", "EDUCATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SCHOOL", "EDUCATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("UNIVERSITY OF TORONTO", "EDUCATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("UNIVERSITY", "EDUCATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("COLLEGE", "EDUCATIONAL", CURRENT_USE))
  #Restaurant
  CURRENT_USE <- factor(gsub("RESTAURANT > 30 SEATS", "RESTAURANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RESTAURANT>30 SEATS", "RESTAURANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RESTAURANT GREATER THAN 30 SEATS", "RESTAURANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RESTAURANT LESS THAN 30 SEATS", "RESTAURANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RESTAURANT UNDER 30 SEATS", "RESTAURANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RESTAURANT, GREATER THAN 30 SEATS", "RESTAURANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("TAKE OUT RESTAURANT", "RESTAURANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("TAKEOUT RESTAURANT", "RESTAURANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RESTAURANT ", "RESTAURANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RESTAURANT", "BBBBB", CURRENT_USE))
  CURRENT_USE <- factor(gsub("REST", "BBBBB", CURRENT_USE))
  CURRENT_USE <- factor(gsub("BBBBB", "RESTAURANT", CURRENT_USE))
  


  #PARKINGLOT
  CURRENT_USE <- factor(gsub("PARKING GARAGE", "PARKING LOT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SURFACE PARKING LOT/EXISTING COMMERCIAL BUILDING", "PARKING LOT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("GARAGE", "PARKING LOT", CURRENT_USE))
  #RETAIL
  CURRENT_USE <- factor(gsub("MIXED USE (RETAIL)", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("OFFICE/RETAIL", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED USE NON RES (RETAIL)", "RETAIL", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("RETAIL PLAZA", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RETAIL PLAZ", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RETAIL STORE", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RETAIL MALL", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RETAIL STORE", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RETAIL/RESIDENTIAL", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RETAIL ", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RETAIL", "CCCCC", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RETAIL ", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RET", "RETAIL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CCCCC", "RETAIL", CURRENT_USE))
  
  #OFFICE
  CURRENT_USE <- factor(gsub("MEDICAL OFFICE", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("DENTAL OFFICE", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MEDICAL OFFICE", "OFFICE", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("OFFICE BLD", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("OFFICE BUILDING", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("OFFICE SPACE", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("OFFICES", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("PROF. OFFICE", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("WAREHOUSE/OFFICE", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("OFFICE/WAREHOUSE", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RETAIL/OFFICE", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("OFFICE ", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("OFFICE", "HHHHH", CURRENT_USE))
  CURRENT_USE <- factor(gsub("OFF", "HHHHH", CURRENT_USE))
  CURRENT_USE <- factor(gsub("HHHHH", "OFFICE", CURRENT_USE))
  #MIXED
  CURRENT_USE <- factor(gsub("MIXEDRESIDENTIAL & NON RESIDENTIAL", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED USE NON RES", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED USE (RETAIL)", "MIXED", CURRENT_USE))

  CURRENT_USE <- factor(gsub("MIXED USE BUILDING/NON RESIDENTIAL", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED USE BUILDING", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED USE (COMMERCIAL)", "MIXED", CURRENT_USE))

  CURRENT_USE <- factor(gsub("MIXED USE ", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED-USE", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXEDIDENTIAL (RETAIL)", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXEDIDENTIALIDENTIAL", "MIXED", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("MIXED USE", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTI USE, NON RES", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTI USE/NON RES", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTI USE/NON RESIDENTIAL", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTI USE", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXEDIDENTIAL", "MIXED", CURRENT_USE))

  
  CURRENT_USE <- factor(gsub("MIXED (RETAIL)", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED BUILDING", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED USE", "MIXED", CURRENT_USE))

  
  #RESIDENTIAL
  CURRENT_USE <- factor(gsub("RESIDENTIAL", "RES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RESTAURANT", "NNNNN", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RES", "RESIDENTIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub( "NNNNN", "RESTAURANT",CURRENT_USE))
  
  #PlaceOfWorship
  CURRENT_USE <- factor(gsub("CHURCH", "PLACE OF WORSHIP", CURRENT_USE))
  #LAB
  CURRENT_USE <- factor(gsub("LABORATORY", "LAB", CURRENT_USE))
  CURRENT_USE <- factor(gsub("LAB", "LABORATORY", CURRENT_USE))

  #INDUSTRIAL
  CURRENT_USE <- factor(gsub("VACANT INDUSTRIAL", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTRIAL", "IND", CURRENT_USE))
  CURRENT_USE <- factor(gsub("IND", "INDUSTRIAL", CURRENT_USE))
  #TRANSIT STATION
  CURRENT_USE <- factor(gsub("SUBWAY STATION", "TRANSIT STATION", CURRENT_USE))
  CURRENT_USE <- factor(gsub("UNION STATION", "TRANSIT STATION", CURRENT_USE))
  #VACANT
  CURRENT_USE <- factor(gsub("VACNT", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT UNIT", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT SPACE", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT SINGLE FAMILY DWELLING ", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT RETAIL UNIT", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT RETAIL", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT LOT", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT LAND", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT OFFICE", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANTUNIT", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT INDUSTRIAL", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT COMMERCIAL UNIT", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT COMMERCIAL", "VACANT", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("VACANT ", "ZZZZ", CURRENT_USE))
  CURRENT_USE <- factor(gsub("VACANT", "ZZZZ", CURRENT_USE))
  CURRENT_USE <- factor(gsub("ZZZZ", "VACANT", CURRENT_USE))
  
  #N/A
  CURRENT_USE <- factor(gsub("N/A", "", CURRENT_USE))
  CURRENT_USE <- factor(gsub("NOT KNOWN", "", CURRENT_USE))
  CURRENT_USE <- factor(gsub("NONE", "", CURRENT_USE))
  #SFD DETACHED
  CURRENT_USE <- factor(gsub("SINGLE FAMILY DETACHED HOUSE", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SINGLE-FAMILY DETACHED HOUSE", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SINGLE FAMILY DETACHED", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("DETACHED - SINGLE FAMILY DWELLI", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI DETACHED - SFD", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD-DETACHED HOUSE", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACHED HOUSE", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACHED HOU", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACHEDDWELLING", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD - DETACHED", "SFD DETACHED", CURRENT_USE))
 
  
  CURRENT_USE <- factor(gsub("SFD-DETACHED", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACEHD", "SFD DETACHED", CURRENT_USE))


  
  CURRENT_USE <- factor(gsub("SFD DETACHED ", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETATCHED", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD - DETCAHED", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("DETACHED SFD", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("DETACHED - SFD", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACHEDDWELLING", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACHEDN", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACHEDG", "SFD DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACHED ", "SFD DETACHED", CURRENT_USE))
  
  
  
  #SFDSEMI
  CURRENT_USE <- factor(gsub("SEMI DETACHED - SINGLE FAMILY DWELLING", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI-DETACHED SINGLE FAMILY DWELLING", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI-DETACHED DWELLING", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SINGLE FAMILY DWELLING", "NEW SFD/SEMI/DETACHED", CURRENT_USE))


  CURRENT_USE <- factor(gsub("SINGLE-FAMILY SEMI-DETACHED HOUSE", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SINGLE FAMILY SEMI-DETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE)) 
  
  CURRENT_USE <- factor(gsub("SFD - SEMI-DETACHED ", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD - SEMIDETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD - SEMI DETACH", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD SEMI DETACH", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD SEMI", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD- SEMI DETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD-SEMI DETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI DETACHED SFD", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI DETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI-DETACHED - SFD", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  #CURRENT_USE <- factor(gsub("SEMI-DETACHED S", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI-DETACHED HOUSE", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI-DETACH", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD-SEMI-DETACH", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD-SEMI ", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD - SEMI", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("NEW SFD/SEMI/DETACHED-DETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("NEW SFD/SEMI/DETACHEDED HOUSE", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("NEW SFD/SEMI/DETACHEDED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("NEW SFD/SEMI/DETACHEDG", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD - NEW SFD/SEMI/DETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD-NEW SFD/SEMI/DETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI SFD DETACHEDNG", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI SFD DETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI-SFD DETACHED", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD-SEMI", "NEW SFD/SEMI/DETACHED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("NEW SFD/SEMI/DETACHED", "EEEEE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SEMI", "EEEEE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("EEEEE", "SFD SEMI-DETACHED", CURRENT_USE))
  
  



  #SFDTOWNHOUSE
 
  CURRENT_USE <- factor(gsub("SFD - ROW HOUSE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD - ROWHOUSE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD - TOWNHOUSE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD ROWHOUSE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD TOWNHOUSE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD-ROW/TO", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD-ROWHOUSE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD-TOWNHOUSE", "SFD HOUSE", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("TOWNHOUSE - SFD", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("TOWNHOUS", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("ROW HOUSE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("ROWHOUSE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD HOUSEES", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD HOUSEE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD HOUSEWN", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD HOUSE - SFD", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFDSFD HOUSE", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACHEDSE", "SFD DETACHED", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("SFD HOUSE", "FFFFF", CURRENT_USE))
  CURRENT_USE <- factor(gsub("HOUSE", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD HOUS", "SFD HOUSE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("FFFFF", "SFD HOUSE", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("RESIDENTIAL - SFD", "SFD", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SF RESIDENTIAL", "SFD", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("SINGLE FAMILY", "SFD", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SINGLE", "SFD", CURRENT_USE))

  
  CURRENT_USE <- factor(gsub("SFD DETACHED", "AAAAA", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DETACHE", "AAAAA", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD DET", "AAAAA", CURRENT_USE))
  CURRENT_USE <- factor(gsub("AAAAA", "SFD DETACHED", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("SFD DETACHED", "AAAAA", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD HOUSE", "BBBBB", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD SEMI-DETACHED", "CCCCC", CURRENT_USE))
  CURRENT_USE <- factor(gsub("SFD ", "SFD", CURRENT_USE))
  CURRENT_USE <- factor(gsub("AAAAA","SFD DETACHED",  CURRENT_USE))
  CURRENT_USE <- factor(gsub("BBBBB","SFD HOUSE",  CURRENT_USE))
  CURRENT_USE <- factor(gsub("CCCCC","SFD SEMI-DETACHED",  CURRENT_USE))
  

  CURRENT_USE <- factor(gsub("MIXEDRESIDENTIAL & NON RESIDENTIAL", "MIXED", CURRENT_USE))
  
  
  #MULTIUNIT
  CURRENT_USE <- factor(gsub("2 UNIT CONVERTED DWELLING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("2 UNIT DWELLING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("2 UNITS", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("3 UNIT DWELLING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED DWELLING ", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("3 UNIT DWELLING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("3 UNIT DWELLING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("3 UNIT DWELLING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("3 UNIT DWELLING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("DUPLEX", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("DUPLEX", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTI UNIT BUILDING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTI-UNIT BUILDING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTIPLE UNIT", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTIPLE UNIT BUILDING", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTI UNIT", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("TRIPLEX", "MULTI-UNIT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("DUPLEX", "MULTI-UNIT", CURRENT_USE))
  
  #INDUSTRIAL AGAIN
  CURRENT_USE <- factor(gsub("NDUSTRIAL BUILDING", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("LUMBER YARD", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTIAL MANUFACTURING", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MANUFACTURING", "INDUSTRIAL", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("INDUSTIAL BUILDING", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTIAL PROCESSING", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTIAL WAREHOUSE", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("WATER TREATMENT PLANT", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTRIAL PLANT", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSRTIAL ", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUTIAL", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTIAL ", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTRIAL ", "INDUSTRIAL", CURRENT_USE))
  
  #NURSINGHOMES
  CURRENT_USE <- factor(gsub("LONG TERM CARE FACILITY", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES (2 UNITS)", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES - 2 UNITS", "NURSING HOMES", CURRENT_USE))
 
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES 2 UNITS", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES, 2 UNITS", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("ROOMING NURSING HOMES", "NURSING HOMES", CURRENT_USE))
  
  
  CURRENT_USE <- factor(gsub("NURSING HOME", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("HOTEL", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("HOSPITAL", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("HOME FOR THE AGED", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("GROUP HOME", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("FUNERAL HOME", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("HOME FOR THE AGED", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("DAYCARE", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("HOME FOR THE AGED", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("HOME FOR THE AGED", "NURSING HOMES", CURRENT_USE))
  
  #RECREATIONAL
  CURRENT_USE <- factor(gsub("THEATRE", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("STADIUM", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("PARK", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MUSEUM", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("LIBRARY", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("FITNESS CENTRE", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("COMMUNITY HALL", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("COMMUNITY CENTRE", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("BOWLING ALLEY", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CLUB", "RECREATIONAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("ARENA", "RECREATIONAL", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("STUDENT RESIDENTIALIDENCE", "RESIDENTIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("RESIDENTIAL APARTMENT", "APARTMENT", CURRENT_USE))
  
  
 

  
  CURRENT_USE <- factor(gsub("NURSING HOMESS", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES - MULTI-UNIT", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES (MULTI-UNIT)", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES, MULTI-UNIT", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES MULTI-UNIT", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES - 3 UNITS", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES, 3 UNITS", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("CONVERTED NURSING HOMES", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("DETACHED NURSING HOMES", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("WARENURSING HOMES", "NURSING HOMES", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("NURSING HOMES (MULTI-UNIT)", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MULTI-UNIT BUILDING", "MULTI-UNIT", CURRENT_USE))

  

 })

permits <- within(permits, {
  
  CURRENT_USE <- factor(gsub("VACANT(AFTER DEMO)", "VACANT", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED (RETAIL)", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED(COMMERCIAL)", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("MIXED(RETAIL)", "MIXED", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTRIAL/OFFICE", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTRIALINDUSTRIAL", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTRIAL/OFFICE", "OFFICE", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTRIALNURSING HOMES", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("INDUSTRIALPROCESSING", "INDUSTRIAL", CURRENT_USE))
  CURRENT_USE <- factor(gsub("IINDUSTRIAL", "INDUSTRIAL", CURRENT_USE))
  
  CURRENT_USE <- factor(gsub("NURSING HOMES (MULTI-UNIT)", "NURSING HOMES", CURRENT_USE))
  CURRENT_USE <- factor(gsub("NURSING HOMES ", "NURSING HOMES", CURRENT_USE))
  
  
})

permits.tab1 <- with(permits, tapply(license,CURRENT_USE, sum)); 
subset(permits.tab1, permits.tab1>30)


str(permits)

#B Histogram
require(histogram)

permits$DATE_DIFF <- as.numeric(permits$DATE_DIFF, units = "days")
max(permits$DATE_DIFF)
hist(permits$DATE_DIFF, xlim=c(0,5500))

#C
permits$DATE_DIFF <- permits$DATE_DIFF + 1
permits$DATE_DIFF <- log(permits$DATE_DIFF)
hist(permits$DATE_DIFF, main="Histogram of # of days to make permits", xlab = "Adjusted # of days to make permits")

#d
gain = subset(permits, permits$DWELLING_UNITS_CREATED > 0)
lost = subset(permits, permits$DWELLING_UNITS_LOST > 0)
no_gain = subset(permits, permits$DWELLING_UNITS_CREATED == 0)

hist(no_gain$DATE_DIFF, col="blue")

hist(gain$DATE_DIFF,col="yellow",add=T)
hist(lost$DATE_DIFF, add=T, col="red")

#e
changing = subset(permits, permits$STATUS == "Inspection")
Issued = subset(permits, permits$STATUS == "Permit Issued")

boxplot(changing$DATE_DIFF,Issued$DATE_DIFF)

#f
permits.tab1 <- with(permits, tapply(DATE_DIFF, STATUS, mean)); 
permits.tab1 <- permits.tab1[order(permits.tab1, decreasing=T)]
permits.tab1 <- subset(permits.tab1, permits.tab1>21)
permits.tab1


#3
require(wordcloud)
require(tm)
my_postal <- subset(permits,permits$POSTAL == "M1S")
wordcloud(my_postal$CURRENT_USE,scale=c(2,1))

#4
biz <- read.csv("businessLicences.csv", head=T, stringsAsFactors = F)
wed <- read.csv("marriage.csv", head=T)

wed <- within(wed, {
  Year <- factor(gsub("-.*", "", TIME_PERIOD))
  Month <- factor(month.abb[as.numeric(gsub(".*-", "", TIME_PERIOD))], levels = month.abb)
})

require(lubridate) # Nice package for dealing with dates, although you don't need it
biz <- within(biz, {
  #   Date <- as.Date(Issued, format = ifelse(nchar(Issued) > 8, "%d/%m/%Y", "%d/%m/%y"))
  Date <- as.Date(Issued, format = "%d/%m/%y")
  Year <- factor(year(Date))
  Month <- factor(month.abb[month(Date)], levels = month.abb)
})
biz <- subset(biz, Year %in% unique(wed$Year), select=c(Month, Year))
biz <- subset(biz, Year %in% unique(permits$Year), select=c(Month, Year))# Doing it here to save RAM, but could be done in join instead
biz <- droplevels(biz) # Gets rid of unused factor levels
require(plyr)
biz.agg <- ddply(biz, .(Year, Month), summarize, BusinessLic = length(Month))
permits.agg <- ddply(permits, .(Year, Month), summarize, BusinessPer = length(Month))
licenses <- join(permits.agg, biz.agg, by = c("Year", "Month"))


with(licenses, plot(BusinessPer, BusinessLic))
fit1 <- lm(BusinessLic ~ BusinessPer, data=licenses)
abline(fit1)
res1 = resid(fit1)
plot(licenses$BusinessPer,res1)
qqplot(licenses$BusinessPer, licenses$BusinessLic)

#PartB
times <- read.csv("A1times.csv", head=T)

#a
hist(times$time)
grid(nx=2,ny=1)
hist(times$a1)
grid(nx=2,ny=1)

#b
with(times, plot(time, a1))
fit <- lm(a1 ~ time, data=times)
summary(fit)
abline(fit)

#c
res = resid(fit)
plot(times$time, res)
qqplot(times$time,times$a1)

#d
times$adjusted_time = log(times$time + 1)
times$adjusted_grade = times$a1 ** 2

with(times, plot(adjusted_time,adjusted_grade))
ad_fit <- lm(adjusted_grade ~ adjusted_time, data=times)
abline(ad_fit)
ad_res = resid(ad_fit)
plot(times$adjusted_time, ad_res)

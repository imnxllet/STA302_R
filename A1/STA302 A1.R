#read.csv(marriage.csv)
setwd("/Users/chris/Desktop/year2/class/STA302")
getwd()
mydata = read.csv("marriage.csv")
mydata
str(mydata)
#1b
mydata$Year <- substring(mydata$TIME_PERIOD, 1, 4)
#mydata$Year = factor(Year)
Year
#1c
mydata$Month <- substring(mydata$TIME_PERIOD, 6, 7)
#mydata$Month <- factor(Month)
#1d
mydata$Month <- month.abb[as.numeric(mydata$Month)]

#1e

CC = substring(mydata$CIVIC_CENTRE, 1)
CC
issued = mydata$MARRIAGE_LICENSES
issued
df1 = data.frame(mydata$Year, mydata$Month, mydata$MARRIAGE_LICENSES)
df1
#2a
df = data.frame(CC, issued)
SC = df[df$CC == 'SC',]
ET = df[df$CC == 'ET',]
NY = df[df$CC == 'NY',]
TO = df[df$CC == 'TO',]
License_issued = c(sum(SC$issued),sum(ET$issued),sum(NY$issued),sum(TO$issued))

CivicCentre = levels(factor(mydata$CIVIC_CENTRE))
CivicCentre
t1 = data.frame(License_issued,CivicCentre)
#table(t1)
t1

#2b
sort(License_issued)
barplot(License_issued,names.arg=CivicCentre)
#2c
t2 <- with(mydata, tapply(mydata$MARRIAGE_LICENSES, list(Year, Month), sum))
t2

#2d
t3 = with(mydata, tapply(mydata$MARRIAGE_LICENSES, list(mydata$CIVIC_CENTRE, Month), sum))
t3
t3_ordered = t3[, month.abb]
t3_ordered
barplot(t3_ordered)
#barplot(mydata$MARRIAGE_LICENSES, names.arg = mydata$CIVIC_CENTRE)
Month

#3a
business= read.csv("businessLicences.csv")
business
str(business)
business <- read.csv("businessLicences.csv", head=T, stringsAsFactors = F)
business


business$Issued <- as.Date(business$Issued, format = "%d/%m/%y")
#3b
Year <- substring(business$Issued, 1, 4)
business$Year = factor(Year)
business$Year
Month <- substring(business$Issued, 6, 7)
business$Month = factor(Month)

#3c
YM = data.frame(business$Year, business$Month)

YM
Subset_YM <- subset(YM, Year > "2010" & Year < "2016")
Subset_YM$Year <- business$Year[business$Year > "2010"]
Subset_YM = droplevels(Subset_YM)
Subset_YM

#3d

Subset_YM$count <- 1
t4 <- with(Subset_YM, tapply(count, list(Subset_YM$business.Year, Subset_YM$business.Month), sum))
t4
Subset_YM  

#PART C
#1a
library(plyr)
combined_data <- join(t2, t4)
Year_df = factor(c(rep("2011", 12), rep("2012", 12), rep("2013", 12),rep("2014", 12),rep("2015", 12)))
Month_df = factor(rep(c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12), 5))
Marriagelicense = c(742, 796, 1210, 1376, 1885, 1824, 1943, 1933, 1321, 1013, 816, 785, 902, 879, 1227, 1232, 1650, 1843, 2015, 1930, 1143, 1065, 826, 785, 763, 725,  990, 1360, 1581, 1579, 1999, 1821, 1229,  940, 709, 679,785, 717, 1062, 1294, 1682, 1806, 1962, 1845, 1280, 1001, 673, 785, 739, 730, 1160, 1344, 1663, 1927, 2184, 1855, NA, NA, NA,NA)
Business_license = c(468, 478, 572, 578, 563, 520, 386, 467, 414, 480, 494, 451, 417, 405, 490, 529, 530, 524, 435, 495, 394, 591, 485, 354, 491, 383, 455, 558, 590, 527, 425, 455, 526, 471, 464, 343, 435, 353, 673, 514, 630, 579, 583, 477, 590, 532, 554, 434, 462, 416, 632, 647, 616, 577, 707, 594, 165, 8,  3,NA)

combined_df = data.frame(Year_df, Month_df, factor(Marriagelicense), factor(Business_license))
combined_df

#2b

with(combined_df,plot( Business_license, Marriagelicense))
#3a

model = lm(Marriagelicense ~ Business_license)
model

# (Intercept)  Business_license  
#126.250             2.302 

#3c
summary(model)




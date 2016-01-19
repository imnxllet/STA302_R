setwd("/Users/chris/Desktop/year2/class/STA302/A3")
states <- as.data.frame(state.x77)
names(states)[names(states) == "Life Exp"] <- "Life.Exp"
names(states)[names(states) == "HS Grad"] <- "HS.Grad"
states$regions <- state.division
names(states)[names(states) == "Life Exp"] <- "Life.Exp"

#1a
states$density <- 1000 * (states$Population/states$Area)

#1b
df <- data.frame(states$Life.Exp, states$density, states$Income, states$Murder, states$HS.Grad, states$Frost)
cor(df)

#1c
grid(3, 2)
hist(states$Life.Exp)
grid(3, 2)
hist(states$density)
grid(3, 2)
hist(states$Income)
grid(3, 2)
hist(states$Murder)
grid(3, 2)
hist(states$HS.Grad)
grid(3, 2)
hist(states$Frost)
grid(3, 2)
#1d
plot(states$Life.Exp, states$density)
plot(states$Life.Exp, states$Income)
plot(states$Life.Exp, states$Murder)
plot(states$Life.Exp, states$HS.Grad)
plot(states$Life.Exp, states$Frost)
with(states, boxplot(Life.Exp ~ Area)) 

#1e
plot(states$Life.Exp, log(states$density), main="life exp vs density")
plot(states$Life.Exp, states$Income, main="life exp vs income")
plot(states$Life.Exp, states$Murder, main="life exp vs murder")
plot(states$Life.Exp, states$HS.Grad, main="life exp vs HsGrad")
plot(states$Life.Exp, states$Frost, main="life exp vs Frost")
with(states, boxplot(Life.Exp ~ regions)) 

#1f(indicator variable)
states$esc <- as.numeric(states$regions == "East South Central")
states$pac <- as.numeric(states$regions == "Pacific")
states$mou <- as.numeric(states$regions == "Mountain")
states$wnc <- as.numeric(states$regions == "West North Central")
  states$sat <- as.numeric(states$regions == "South Atlantic")
states$mat <- as.numeric(states$regions == "Middle Atlantic")
states$new <- as.numeric(states$regions == "New England")
states$enc <- as.numeric(states$regions == "East North Central")
states$wsc <- as.numeric(states$regions == "West South Central")




#2a
fit <- lm(Life.Exp ~ log(density) + Income + Murder + HS.Grad + Frost + esc + pac + mou + wnc + sat + mat + new + enc + wsc, data=states)

#2b
fit <- lm(Life.Exp ~ log(density) + Income + Murder + HS.Grad + Frost + esc + pac + mou + wnc + sat + mat + new + enc + wsc - 1, data=states)
summary.lm(fit)

fit.red <- lm(Life.Exp ~ log(density) + Income + Murder + HS.Grad + Frost + wnc, data=states)
summary.lm(fit.red)

#2c 
anova(fit, fit.red)
summary.lm(fit.red)

#2d
fit.red1 <- lm(Life.Exp ~ Population + Murder + HS.Grad, data=states)
summary(fit.red1)anova(fit.red, fit.red1)
#2e type up equation
#2f interpretation
#2g interpretation



#3a,b
plot(fit.red1)



#3c which stage high DEFFIT
#3d which stage high leverage



#PARTB
#1a
Man <- read.fwf("100mM.txt", width= c(12, 9, 13, 31, 3, 13, 11, 25, 10), head= F, 
                    col.names= c("ID", "time", "wind", "Name", "country", "birth", "heat", "Location", "date"), 
                    stringsAsFactors = F)
Man
Woman <- read.fwf("100mW.txt", width= c(12, 9, 13, 31, 3, 13, 11, 25, 10), head= F, 
                    col.names= c("ID", "time", "wind", "Name", "country", "birth", "heat", "Location", "date"), 
                    stringsAsFactors = F)
Man["gender"] <- NA
Woman["gender"] <- NA
Man$gender <- "M"
Woman$gender<- "W"
Woman
require(plyr)
tnf100M <- rbind(Man, Woman)
tnf100M$gender

#1b
tnf100M <- tnf100M[! tnf100M$time %in% c('A'),]
tnf100M$time <- as.numeric(tnf100M$time)

#1c
tnf100M <- subset(tnf100M, select=c(country, date, birth, wind, time, gender))
str(tnf100M)

#1d
require(lubridate)
tnf100M$date <- as.Date(tnf100M$date, format="%d.%m.%Y")
tnf100M$birth <- as.Date(tnf100M$birth, format="%d.%m.%y")

year(tnf100M$birth)[year(tnf100M$birth)>2015 & !is.na(year(tnf100M$birth))] <- year(tnf100M$birth)[year(tnf100M$birth)>2015 & !is.na(year(tnf100M$birth))] - 100
tnf100M$birth

str(tnf100M)
tnf100M$birth

#1e
str(tnf100M)
tnf100M <- within(tnf100M, {
  Year <- factor(year(date))
  Month <- factor(month.abb[month(date)], levels = month.abb)
})


#1f
tnf100M$current <- as.Date("2015-12-05")
tnf100M$current
year
tnf100M$age <- as.numeric(tnf100M$current - tnf100M$birth)
tnf100M$age <- tnf100M$age/365.25
tnf100M <- tnf100M[!(tnf100M$age == ""), ]
tnf100M$age

#1g
tnf100M$wind <- gsub(",", ".", tnf100M$wind)
tnf100M$wind

#1h
tnf100M$wind <- gsub("0.0", "0", tnf100M$wind)
tnf100M <- tnf100M[!(tnf100M$wind == "" & is.na(tnf100M$wind)), ]
tnf100M$wind 

#1i
tnf100M$time <- as.numeric(tnf100M$time)
tnf100M.M<- subset(tnf100M, gender == "M",
                select=c(Year, Month, country, date, birth, wind, time, gender))
tnf100M.W<- subset(tnf100M, gender == "W",
                  select=c(Year, Month, country, date, birth, wind, time, gender))
tnf100M$wind <- as.numeric(tnf100M$wind)
tnf100M.M$time <- tnf100M.M$time + tnf100M.W$wind * 0.05
tnf100M.W$time <- tnf100M.W$time + tnf100M.W$wind * 0.06
tnf100M <- rbind(tnf100M.M, tnf100M.W)
tnf100M
tnf100M <- tnf100M[!(is.na(tnf100M$time)), ]

#2a
tnf1500M.M <- read.fwf("1500mM.txt", width= c(15, 11, 31, 3, 13, 11, 25, 10), head= F, 
                     col.names= c("ID", "time", "Name", "country", "birth", "heat", "Location", "date"), 
                     stringsAsFactors = F)
tnf1500M.W <- read.fwf("1500mW.txt", width= c(15, 11, 31, 3, 13, 11, 25, 10), head= F, 
                       col.names= c("ID", "time", "Name", "country", "birth", "heat", "Location", "date"), 
                       stringsAsFactors = F)
tnf1500M.M$gender <- "M"
tnf1500M.W$gender <- "W"
tnf1500M <- rbind(tnf1500M.M, tnf1500M.W) 
tnf1500M

#2b
options(digits.secs = 2);
tnf1500M <- tnf1500M[!(tnf1500M$time == "A"), ]
tnf1500M$time
tnf1500M$time <- strptime(tnf1500M$time, "%M:%OS")
tnf1500M <- tnf1500M[!is.na(tnf1500M$time), ]
tnf1500M$time <- format(tnf1500M$time, "%M:%OS")

#2c same as 1d(date),e(YM),f(age), delete woman, delete missing age
str(tnf1500M)
require(lubridate)
tnf1500M$date <- as.Date(tnf1500M$date, format="%d.%m.%Y")
tnf1500M$birth <- as.Date(tnf1500M$birth, format="%d.%m.%y")
year(tnf1500M$birth)[year(tnf1500M$birth)>2015 & !is.na(year(tnf1500M$birth))] <- year(tnf1500M$birth)[year(tnf1500M$birth)>2015 & !is.na(year(tnf100M$birth))] - 100

tnf1500M <- within(tnf1500M, {
  Year <- factor(year(date))
  Month <- factor(month.abb[month(date)], levels = month.abb)
})

tnf1500M$current <- as.Date("2015-12-05")
tnf1500M$age <- as.numeric(tnf1500M$current - tnf1500M$birth)
tnf1500M$age <- tnf1500M$age/365.25
tnf1500M <- tnf1500M[!(tnf1500M$age == ""), ]
tnf1500M <- tnf1500M[(tnf1500M$date > tnf1500M$birth), ]
tnf1500M


#2d convert the time to second
str(tnf1500M$time)
minute <- substr(tnf1500M$time, 0,2)
minute <- as.numeric(minute)
minute
second <- substr(tnf1500M$time, 4,8)
second
second <- as.numeric(second)
second
tnf1500M$time <- minute * 60 + second
tnf1500M <- tnf1500M[!(is.na(tnf1500M$time)), ]

#3
#bind 1, 2 datefram tgt
#create column called race before in each DF, to label which race
#Present summary statistics , summary()
tnf100M$race <- "100M"
tnf1500M$race <- "1500M"
tnf100M <- subset(tnf100M, select=c(Year, Month, country, date, birth, time, gender, race))
tnf1500M <- subset(tnf1500M, select=c(Year, Month, country, date, birth, time, gender, race))
#str(tnf1500M)
tournament <- rbind(tnf100M, tnf1500M)
summary(tournament)


#4
#barplot for each race(grouped by sex), showing the counts of top times by month , aggregated over all yrs
#all months appear on x-axis, ddply()
tnf1500M.W$date 
min(tnf1500M$time)
tnf100M$count <- 1
tnf1500M$count <- 1
tnf100M.bar <- ddply(tnf100M, c("Month", "gender"), summarize, toptime = sum(count), .drop = F)
tnf1500M.bar <- ddply(tnf1500M, c("Month", "gender"), summarize, toptime = sum(count), .drop = F)
tnf1500M.bar
require(ggplot2)
ggplot(tnf1500M.bar, aes(x= Month, y= toptime, fill= gender)) + geom_bar(stat="identity") + labs(y= "Top Times Count")
ggplot(tnf100M.bar, aes(x= Month, y= toptime, fill= gender)) + geom_bar(stat="identity") + labs(y= "Top Times Count")


#5
#Present a separate two-way table for each race with Country along the rows and Sex across the columns, showing the counts
#of top times.
#Show only the top 25 countries for each race.
table1 <- table(tnf100M$country, tnf100M$gender, tnf100M$count)
table1
sort(rowSums(table1), decreasing = T)
 
table2 <- table(tnf1500M$country, tnf1500M$gender, tnf1500M$count)
table2
sort(rowSums(table2), decreasing = T)

tnf100M.bar1 <- with(tnf100M, tapply(count, list(country, gender), sum))
tnf1500M.bar1 <- with(tnf1500M, tapply(count, list(country, gender), sum))
tnf100M.bar1 
tnf1500M.bar1

a <- tnf100M.bar1[order(tnf100M.bar1)]

tnf1500M.bar1
#6a
#make a new data_frame containing countries, sex, and two columns with the total 
#counts of top times for each race
#subset it, by country/sex that has at least one count for both races. (45 rows)

table1 <- table(tnf100M$country, tnf100M$gender, tnf100M$count)
df_100M <- as.data.frame(table1)
names(df_100M)[names(df_100M) == "Freq"] <- "R.one"
names(df_100M)[names(df_100M) == "Var1"] <- "country"
names(df_100M)[names(df_100M) == "Var2"] <- "sex"
table2 <- table(tnf1500M$country, tnf1500M$gender, tnf1500M$count)
df_1500M <- as.data.frame(table2)
names(df_1500M)[names(df_1500M) == "Freq"] <- "R.two"
names(df_1500M)[names(df_1500M) == "Var1"] <- "country"
names(df_1500M)[names(df_1500M) == "Var2"] <- "sex"
df <- join(df_100M, df_1500M, by = c("country", "sex"))

df_both <- subset(df, !(R.one %in% c(NA, 0) | R.two %in% c(NA, 0)), select=c(country, sex, R.one, R.two))
str(df_both)


#6b
df_m <- df
df_m$R.one <- log(df_m$R.one)
df_m$R.two <- log(df_m$R.two)
df_m <- df_m[!df_m$R.one==-Inf,]

df_m <- df_m[!df_m$R.two==-Inf,]
df_m <- df_m[!is.na(df_m$R.one),]
df_m <- df_m[!is.na(df_m$R.two),]
df_m
log100MM <- df_m$R.one[df_m$sex=="M"]
log100MW <- df_m$R.one[df_m$sex=="W"]
log1500MM <- df_m$R.two[df_m$sex=="M"]
log1500MW <- df_m$R.two[df_m$sex=="W"]

plot(log100MM, log1500MM, col="black",main="100M vs 1500M", xlab="100m top time ", ylab="1500m top time")
par(new=TRUE)
plot(log100MW, log1500MW,col="grey",main="100M vs 1500M", xlab="100m top time ", ylab="1500m top time")

fit <- lm(log1500MM ~log100MM)
abline(fit, col="black")
fit1 <- lm(log1500MW ~log100MW)
abline(fit1, col="grey")

#6c
#Finally, fit a model regressing the log of 1500m counts on the log
#of 100m counts and sex. Start o??? with a model with both main
#e???ects and an interaction, and test the interaction with a partial Ftest.
#If it is not significant, remove it and test the additive model,
#removing anything that is not significant. Give the fitted equation
#for your final model and interpret the parameter estimate(s) in
#plain English. 
fit2 <- lm(df_m$R.two~ df_m$R.one)
summary(fit2)

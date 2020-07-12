library(lubridate)
library(timeDate)
setwd("\\Users\\Francis\\Desktop\\NIkkei")
date<-dmy_hms("01–12–2018")
date
date<-ymd_hms("2017-06-01 12:34:56")
date
x<-as.Date("2013-11-15")
x
date<-"2013-11-12"
as.Date(data1[1,1],format='%m/%d/%Y')  
dateGMT<-timeDate(date, FinC = "GMT")
day<-dayOfWeek(dateGMT)
date
day
getwd()
Whenyear=1996
WhenMonth=c(3,6,9,12)
day<-dayOfWeek(dateGMT)
names(day)=NULL
day=="Tue"

library(microbenchmark)
f <- function() 3**3
fds <- microbenchmark(f())
g <- function() 3^3
gds <- microbenchmark(g())
## Print results:
print(fds)
print(gds)
------------------
setwd("C:\\Users\\Francis\\Desktop")
data1<-read.csv("BusinessDate.csv", header=T, sep=",",encoding = "UTF-8")  
data1[1,1]=="12/31/2014"
data1[2,1]

data1<-as.list(data1)
...  
data1
length(data1)
data2 <- df(x = 1,df1 = "Date")  #y = rnorm(6), # z = runif(6) > 0.5)
x.data.frame
year(as.Date(data1[1,1],format='%m/%d/%Y'))
month(as.Date(data1[1,1],format='%m/%d/%Y'))
day(as.Date(data1[1,1],format='%m/%d/%Y'))

(as.Date(data1[1,1],format='%m/%d/%Y'))
dayOfWeek(as.Date(data1[1,1],format='%m/%d/%Y'))
?year
for (i in nrow(data1)){
  year(data1[1,1])
}

date<-"2013-11-14"
dateGMeek.of.month(2014,12,T<-timeDate(date, FinC = "GMT")
day<-dayOfWeek(dateGMT)

x <- c(3, 6, 9, 12)
starty=1996
weekdays(as.Date(data1[1,1],format='%m/%d/%Y'))
month(as.Date(data1[1,1],format='%m/%d/%Y')-7)

paste("Hello","world",collapse = "",sep = " ")

library(lubridate)

monthweeks <- function(x) {
  ceiling(as.numeric(day(x))/7)
}
as.numeric(day(as.Date(data1[1,1],format='%m/%d/%Y')))/7
as.numeric(day(as.Date(data1[1,1],format='%m/%d/%Y')))
#====================================================
#Generate rolling date for OSE Nikkei Futures at second Friday of Mar Jun Sep Dec 
out=structure(list(character()), class = "data.frame")
k=0
i<-1
mo <- c(3, 6, 9, 12)
year=1996
while (year<2016){
  Y=as.character(year)
  t=i%%4
  if (t==0){t=4}
  i=i+1
  m=mo[t]
  M=as.character(m)
    if(TRUE){
      DateH=paste(Y,"-",M,"-",sep = "")
      for (j in c(1:14)){
        D=as.character(j)
        datet=as.Date(paste(DateH,D,sep = ""),format='%Y-%m-%d')
        if (month(datet)==month(datet-7) & weekdays(datet)=="Friday"){
          k=1+k
          datess=as.character(datet)
          out[k,1]=datess
          break
        }else{
          next
        }}}
  year=1996+ceiling(i/4)-1
}


#==============================================
setwd("C:\\Users\\Francis\\Desktop\\Nikkei")
data1<-read.csv("BusinessDate.csv", header=T, sep=",",encoding = "UTF-8")  
outt=structure(list(character()), class = "data.frame")
for (i in 1:nrow(data1))
{
  YY=as.numeric(year(as.Date(data1[i,1],format='%m/%d/%Y')))
  Y=as.character(YY)
  #if (year(as.Date(data1[i,1],format='%m/%d/%Y'))==YY){
  if(as.numeric(month(as.Date(data1[i,1],format='%m/%d/%Y')))<3){
    for (j in 1:nrow(out))
      {
       if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))=="3"){
        outt[i,1]=as.character(out[j,1])
        break;
      }else{
        next;
        }
      }
    }else if(as.numeric(month(as.Date(data1[i,1],format='%m/%d/%Y')))<6){
      for (j in 1:nrow(out))
        {
        if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))=="6"){
          outt[i,1]=as.character(out[j,1])
          break;
        }else{
          next;
          }
        }
      }else if (as.numeric(month(as.Date(data1[i,1],format='%m/%d/%Y')))<9){
      for (j in 1:nrow(out))
        {
        if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))=="9"){
          outt[i,1]=as.character(out[j,1])
          break;
        }else{
          next;
          }
        }
      }else if (as.numeric(month(as.Date(data1[i,1],format='%m/%d/%Y')))<12){
      for (j in 1:nrow(out))
        {
        if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))=="12"){
          outt[i,1]=as.character(out[j,1])
          break;
        }else{
          next;
          }
        }
      }else if(as.numeric(month(as.Date(data1[i,1],format='%m/%d/%Y')))==12){
      for (j in 1:nrow(out))
        {
        Y1=YY+1
        Y1=as.character(Y1)
        if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))== Y1 && month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))=="3"){
          outt[i,1]=as.character(out[j,1])
          break;
        }else{
          next;
          }
        }
      }
}
#===================================================================
write.csv(outt,file = "Maturity.csv",row.names=F) 
#===================================================================
set.seed(456)
## list description for AR(1) model with small coef
AR.sm <- list(order = c(1, 0, 0), ar = 0.1, sd = 0.1)
## list description for AR(1) model with large coef
AR.lg <- list(order = c(1, 0, 0), ar = 0.9, sd = 0.1)
## simulate AR(1)
AR1.sm <- arima.sim(n = 50, model = AR.sm)
AR1.lg <- arima.sim(n = 50, model = AR.lg)
## setup plot region
par(mfrow = c(1, 2))
## get y-limits for common plots
ylm <- c(min(AR1.sm, AR1.lg), max(AR1.sm, AR1.lg))
## plot the ts
plot.ts(AR1.sm, ylim = ylm, ylab = expression(italic(x)[italic(t)]), 
        main = expression(paste(phi, " = 0.1")))
plot.ts(AR1.lg, ylim = ylm, ylab = expression(italic(x)[italic(t)]), 
        main = expression(paste(phi, " = 0.9")))

set.seed(123)
## list description for AR(1) model with small coef
AR.pos <- list(order = c(1, 0, 0), ar = 0.5, sd = 0.1)
## list description for AR(1) model with large coef
AR.neg <- list(order = c(1, 0, 0), ar = -0.5, sd = 0.1)
## simulate AR(1)
AR1.pos <- arima.sim(n = 50, model = AR.pos)
AR1.neg <- arima.sim(n = 50, model = AR.neg)
#=================================================
# subset data
GDPGRSub <- GDPGrowth["1962::2012"]

# estimate the model
ar.ols(GDPGRSub, 
       order.max = 1, 
       demean = F, 
       intercept = T)
#++++++++++++++++++

require(stats)

env <- new.env(hash = FALSE) # so the order is fixed
env$a <- 1:10
env$beta <- exp(-3:3)
env$logic <- c(TRUE, FALSE, FALSE, TRUE)
# what have we there?
utils::ls.str(env)

# compute the mean for each list element
eapply(env, mean)
unlist(eapply(env, mean, USE.NAMES = FALSE))

# median and quartiles for each element (making use of "..." passing):
eapply(env, quantile, probs = 1:3/4)
eapply(env, quantile)
# }

# Same as get("*"):
match.fun("*")
# Overwrite outer with a vector
outer <- 1:5
try(match.fun(outer, descend = FALSE)) #-> Error:  not a function
match.fun(outer) # finds it anyway
is.function(match.fun("outer"))
#========================About SGX NIKKEI Futures Data codes
setwd("C://Users//Francis//Desktop//Python")
datasgx<-read.csv("SGX .csv", header=T, sep=",",encoding = "UTF-8")  
syfu=structure(list(character()), class = "data.frame")
datasgx[1,1]
month(as.Date(datasgx[1,1],format='%m/%d/%Y'))

#++++++++++++++++++++++create rolling date series for SGX futures series
out=structure(list(character()), class = "data.frame")
k=0
i<-1
mo <- c(1,2,3,4,5,6,7,8,9,10,11,12)
year=1996
while (year<2016){
  Y=as.character(year)
  t=i%%12
  if (t==0){t=12}
  i=i+1
  m=mo[t]
  M=as.character(m)
  if(TRUE){
    DateH=paste(Y,"-",M,"-",sep = "")
    for (j in c(1:14)){
      D=as.character(j)
      datet=as.Date(paste(DateH,D,sep = ""),format='%Y-%m-%d')
      if (month(datet)==month(datet-7) & weekdays(datet)=="Friday"){
        k=1+k
        datess=as.character(datet)
        out[k,1]=datess
        break
      }else{
        next
      }}}
  year=1996+ceiling(i/12)-1
}
#+++++++++++++++++++++++Create synthetic Futures Data series for SGX Nikkei Futures
 
outt=structure(list(character()), class = "data.frame")
for (i in 1:nrow(datasgx))
{
  YY=as.numeric(year(as.Date(datasgx[i,1],format='%m/%d/%Y')))
  Y=as.character(YY)
  #if (year(as.Date(data1[i,1],format='%m/%d/%Y'))==YY){
  if(as.numeric(month(as.Date(datasgx[i,1],format='%m/%d/%Y')))%%3==0){
    for (j in 1:nrow(out))
    {
      if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==month(as.Date(datasgx[i,1],format='%m/%d/%Y')) & as.Date(datasgx[i,1],format='%m/%d/%Y')<as.Date(as.character(out[j,1]),format='%Y-%m-%d')){
        outt[i,1]=as.character(datasgx[i,5])
      }else if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==month(as.Date(datasgx[i,1],format='%m/%d/%Y'))){
        outt[i,1]=as.character(datasgx[i,4])
        break;
      }else{
        next;
      }
    }
  }else if(as.numeric(month(as.Date(datasgx[i,1],format='%m/%d/%Y')))%%3==1){
    for (j in 1:nrow(out))
    {
      if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==month(as.Date(datasgx[i,1],format='%m/%d/%Y')) & as.Date(datasgx[i,1],format='%m/%d/%Y')<as.Date(as.character(out[j,1]),format='%Y-%m-%d')){
        outt[i,1]=as.character(datasgx[i,4])
      }else if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==month(as.Date(datasgx[i,1],format='%m/%d/%Y'))){
        outt[i,1]=as.character(datasgx[i,3])
        break;
      }else{
        next;
      }
    }
  }else if (as.numeric(month(as.Date(datasgx[i,1],format='%m/%d/%Y')))%%3==2){
    for (j in 1:nrow(out))
    {
      if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==month(as.Date(as.character(datasgx[i,1]),format='%m/%d/%Y')) & as.Date(as.character(datasgx[i,1]),format='%m/%d/%Y')<as.Date(as.character(out[j,1]),format='%Y-%m-%d')){
        outt[i,1]=as.character(datasgx[i,3])
      }else if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==month(as.Date(as.character(datasgx[i,1]),format='%m/%d/%Y'))){
        outt[i,1]=as.character(datasgx[i,2])
        break;
      }else{
        next;
      }
    }
  }
}
#+++++++++++++++++++++++Create synthetic Futures Data series for OSE Nikkei Futures
#Update 02/07/2020

outt=structure(list(character()), class = "data.frame")
for (i in 1:nrow(dataose))
{
  YY=as.numeric(year(as.Date(dataose[i,1],format='%m/%d/%Y')))
  Y=as.character(YY)
  #if (year(as.Date(data1[i,1],format='%m/%d/%Y'))==YY){
  if(as.numeric(month(as.Date(dataose[i,1],format='%m/%d/%Y')))%%3==0){
    for (j in 1:nrow(out))
    {
      if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==month(as.Date(dataose[i,1],format='%m/%d/%Y')) & as.Date(dataose[i,1],format='%m/%d/%Y')<as.Date(as.character(out[j,1]),format='%Y-%m-%d')){
        outt[i,1]=as.character(dataose[i,3])
      }else if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==month(as.Date(dataose[i,1],format='%m/%d/%Y'))){
        outt[i,1]=as.character(dataose[i,2])
        break;
      }else{
        next;
      }
    }
  }else if(as.numeric(month(as.Date(dataose[i,1],format='%m/%d/%Y')))%%3==1){
    for (j in 1:nrow(out))
    {
      if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & (month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))-2)==month(as.Date(dataose[i,1],format='%m/%d/%Y')) & as.Date(dataose[i,1],format='%m/%d/%Y')<as.Date(as.character(out[j,1]),format='%Y-%m-%d')){
        outt[i,1]=as.character(dataose[i,2])
      }else if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & (month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))-2)==month(as.Date(dataose[i,1],format='%m/%d/%Y'))){
        outt[i,1]=as.character(dataose[i,2])
        break;
      }else{
        next;
      }
    }
  }else if (as.numeric(month(as.Date(dataose[i,1],format='%m/%d/%Y')))%%3==2){
    for (j in 1:nrow(out))
    {
      if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & (month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))-1)==month(as.Date(as.character(dataose[i,1]),format='%m/%d/%Y')) & as.Date(as.character(dataose[i,1]),format='%m/%d/%Y')<as.Date(as.character(out[j,1]),format='%Y-%m-%d')){
        outt[i,1]=as.character(dataose[i,2])
      }else if(year(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))==Y & (month(as.Date(as.character(out[j,1]),format='%Y-%m-%d'))-1)==month(as.Date(as.character(dataose[i,1]),format='%m/%d/%Y'))){
        outt[i,1]=as.character(dataose[i,2])
        break;
      }else{
        next;
      }
    }
  }
}
View(outt)
osetest=outt
osetest[which(osetest==0)] = NA
OSER=structure(numeric(), class = "numeric")

for (i in 1:(nrow(outt)-1)){
  OSER[i]=(log(as.numeric(outt[i,1])/as.numeric(outt[i+1,1]),exp(1)))
}
View(OSER)
OSER[which(OSER==0)] = NA
options(digits=7)
mean(na.omit(OSER))
skewness(na.omit(OSER))
sd(na.omit(na.omit(OSER)))
kurtosis(na.omit(OSER))

#=============================Write CSV
write.table(outt,file="C://Users//Francis//Desktop//series.csv",sep=",",row.names=F, na = "NA")
setwd("\\Users\\Francis\\Desktop\\NIkkei")
dataose<-read.csv("OSE.csv", header=T, sep="\t",encoding = "UTF-8")

View(dataose)
dataose$OSE[which(dataose$OSE==0)] = NA
options(digits=7)
mean(na.omit(dataose$OSE))
skewness(na.omit(dataose$OSE))
sd(na.omit(dataose$OSE))
kurtosis(na.omit(dataose$OSE))

# ====================================================Regression Model Test 1
# ctrl+shift+c automatically cite #
# Reference: 

# set the working directory
# setwd("~/Desktop/Rstatistics")
setwd("C://Users//Francis//Desktop/Rstatistics")
getwd() # where am I?
list.files("dataSets") # files in the dataSets folder
# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# summary of expense and csat columns, all rows
# correlation between expense and csat
cor(sts.ex.sat) 
# scatter plot of expense vs csat
plot(sts.ex.sat)
# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

summary(lm(csat ~ expense + percent, data = states.data))
class(sat.mod)
names(sat.mod)

methods(class = class(sat.mod))[1:9]
confint(sat.mod)
hist(residuals(sat.mod))
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)

coef(summary(sat.voting.mod))

#Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
coef(summary(sat.expense.by.percent)) # show regression coefficients table

# make sure R knows region is categorical
str(states.data$region)

states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table
# print default contrasts
contrasts(states.data$region)


# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++Regression Variable with dummy

library(tidyverse)
# Load the data
install.packages('car')
install.packages('carData')
library(car)
library(carData)
data("Salaries", package = "carData")

library(dplyr)
# Inspect the data
sample_n(Salaries, 3)

# Compute the model
model <- lm(salary ~ sex, data = Salaries)
summary(model)$coef

contrasts(Salaries$sex)

Salaries <- Salaries %>%
  mutate(sex = relevel(sex, ref = "Male"))

model <- lm(salary ~ sex, data = Salaries)
summary(model)$coef

res <- model.matrix(~rank, data = Salaries)
head(res[, -1])


model2 <- lm(salary ~ yrs.service + rank + discipline + sex,
             data = Salaries)
install.packages('nnet')
library(nnet)
Anova(model2)
library(MASS)
library(survival)
install.packages('coxme')
library(coxme)

summary(model2)







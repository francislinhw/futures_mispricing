library(lubridate)
library(timeDate)
library(dplyr)
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
data1=SESE
for (i in 1:nrow(data1)){
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
#Nikkei Project Objects  
#=================================Function of Rolling business day f(Today,How many business day we like to add, after n business day date)
#可以給出幾個非例行假日後的日期
RollBD_Forward<-function(datedef='06/01/2020',n=10){
  library(lubridate)
  library(timeDate)
  #未來可以加上日期格式判斷'%m/%d/%Y'
  N=as.numeric(n)-1
  datedef=as.character(datedef)
  D=as.Date(datedef,format='%m/%d/%Y')
  i=0
  
  while (i<=N) {
    if (weekdays(D)!="Saturday"& weekdays(D)!="Sunday"&i!=N){
      
        i = i + 1
        D = 1 + D
      
      }else if(weekdays(D)!="Saturday"&weekdays(D)!="Sunday"&i==N){
        
        DF=D
        i = i + 1
        
      }else{
        
        D=D+1
        
      }
  }
  return(DF)
}
#===========================================================
RollBD_Backward<-function(datedef='12/31/2019',n=10){
  library(lubridate)
  library(timeDate)
  #未來可以加上日期格式判斷'%m/%d/%Y'
  N=as.numeric(n)-1
  datedef=as.character(datedef)
  D=as.Date(datedef,format='%m/%d/%Y')
  i=0
  
  while (i<=N) {
    if (weekdays(D)!="Saturday"& weekdays(D)!="Sunday"&i!=N){
      
      i = i + 1
      D = D - 1
      
    }else if(weekdays(D)!="Saturday"&weekdays(D)!="Sunday"&i==N){
      
      DF=D
      i = i + 1
      
    }else{
      
      D = D - 1
      
    }
  }
  return(DF)
}
#架構就是要先生出對於每個日期都可以判斷 我就只是想知道頭尾 如果是business day 我就加上1直到加上10就會跳到下一天
#分成從Target Maturity Date > date series 小於上面函數弄出來的日期就可以加上股利並且
#=======================判斷是否為周一周五或周末
IFBD_bool<-function(datedef='12/31/2019'){
  library(lubridate)
  library(timeDate)
  #未來可以加上日期格式判斷'%m/%d/%Y'
  datedef=as.character(datedef)
  if(weekdays(D)!="Saturday"& weekdays(D)!="Sunday"){i=1}
  else{i=0}
  return(i)
}
#=========================================================刪除資料中非Business Day 的日子

#=========================================================
#找出六月份前十天 十二月後十天
FindJune_Howmany_Forward<-function(Month=6,N=10,datelist=dataose$Date){
  
  datelist=as.array(datelist)
  datelist=as.Date(datelist,format='%m/%d/%Y')
  StartYear=year(as.Date(datelist[1],format='%m/%d/%Y'))
  EndYear=year(as.Date(datelist[length(datelist)],format='%m/%d/%Y'))
  M=abs(StartYear-EndYear)+2
  SS=1
  TargetDate=structure(character(), class = "data.frame")
  for (i in (1:length(datelist))) {
    if (month(datelist[i])==Month & month(datelist[i+N-1])==Month & month(datelist[i+N])!=Month){
      SS=SS+1
      TargetDate[SS-1,1] = as.character(datelist[i])
      if(SS==M){break}
    }else{next}
  }
  return(TargetDate)
}
#---------------SAVE
saveRDS(JUNEDAY,'JUNEDAY.rds')
#==========================================================
#找出六月份前十天 十二月後十天
FindJune_Howmany_Backward<-function(Month=12,N=10,datelist=dataose$Date,ifaddone=1){
  datelist=as.array(datelist)
  datelist=as.Date(datelist,format='%m/%d/%Y')
  StartYear=year(as.Date(datelist[1],format='%m/%d/%Y'))
  EndYear=year(as.Date(datelist[length(datelist)],format='%m/%d/%Y'))
  M=abs(StartYear-EndYear)+1+ifaddone
  SS=1
  TargetDate=structure(character(), class = "data.frame")
  for (i in (1:length(datelist))){
    if(i<=N){
      if(i==1 & month(as.Date(datelist[i],'%m/%d/%Y'))==Month){
        SS=SS+1
        TargetDate[SS-1,1] = as.character(datelist[i])
      }else{next}
    }else{
      if(month(datelist[i])==Month & month(datelist[i-1])!=Month & month(datelist[i+N-1])==Month){
          SS=SS+1
          TargetDate[SS-1,1] = as.character(datelist[i])
          if(SS==M){break}
        }else{next}
       }
      }
  return(TargetDate)
}
#------------------------------SAVE
saveRDS(DECDATE,'DECDATE.rds')
#=========================================================
#對於每個日期計算股利 先檢查該到期日是否在股利發放日中 6/1-6/10  12/31 -12/20 roughly 
UnderlyingDiv<-function(datedef=c('12/30/2019','12/31/2019'),divlist=c(200,200)){
  library(lubridate)
  library(timeDate)
  #未來可以加上日期格式判斷'%m/%d/%Y'
  for (i in (1:length(datedef))) {
    maturity=
    for (j in length(datedef)) {
            
    }
  }
  datedef=as.character(datedef)
  if(weekdays(D)!="Saturday"& weekdays(D)!="Sunday"){i=1}
  else{i=0}
  return(i)
}
#=====================================================判斷Nikkei的放假日，剔除放假的資料
BDseries<-function(dataseries=OSE0715){
  #資料的第一個column是日期 第二個是市價資料 當前後兩天市價資料一樣我們當作是假日
  p=0
  deletarray<-structure(numeric(), class = "numeric")
  N=nrow(dataseries)-1
  for (i in (1:N)){
    HD=log(as.numeric(dataseries[i,2])/as.numeric(dataseries[i+1,2]))
    HDF=log(as.numeric(dataseries[i,3])/as.numeric(dataseries[i+1,3]))
    dataseries[i,5]<-HD
    dataseries[i,6]<-HDF
    if(HD==0){
      p = p + 1
      deletarray[p]=-i
    }else{next}
  }
  colnames(dataseries)[1]='Date'
  colnames(dataseries)[5]='SPOTRET'
  colnames(dataseries)[6]='FUTURESRET'
  dataseries1=dataseries[deletarray,]
  return(dataseries1)
}

#======================================2020/7/15 OSE
setwd("C:\\Users\\Francis\\Desktop\\Nikkei")
OSE0715<-read.csv("715OSESPOT.csv", header=T, sep=",",encoding = "UTF-8") 

mean(tata$SPOTRET,na.rm =TRUE)
sd(tata$SPOTRET,na.rm = TRUE)
skewness(tata$SPOTRET,na.rm = TRUE)
kurtosis(tata$SPOTRET,na.rm = TRUE)

mean(tata$FUTURESRET,na.rm =TRUE)
sd(tata$FUTURESRET,na.rm = TRUE)
skewness(tata$FUTURESRET,na.rm = TRUE)
kurtosis(tata$FUTURESRET,na.rm = TRUE)


#R定義的kurtosis好像與論文中的不大一樣 所以這邊的就當作參考就好了 ，驗證資料還是放在excel中
#Dividend Payment Appened=====================================================================
#tata$AVEDIV=0
tata$AVEDIV=0
#--------------7/24
SET1$AVEDIV=0
View(SET1)
DIV=c(258.2364945,
      223.0606165,
      203.2569545,
      191.9618111,
      174.7508693,
      161.4132642,
      221.1700558,
      209.578816,
      170.9319111,
      133.2415261,
      104.4098509,
      90.5912904,
      81.74881455,
      87.70405578,
      87.70455978,
      79.6188997,
      86.36129863,
      91.20148898,
      86.9324615
)
APPENDIV<-function(When=JUNEDATE,Main=tata,howmuch=DIV,N=10){
  S=0
  
  for (i in (1:nrow(Main))){
    
    for(j in (1:nrow(When))){
      if(as.Date(Main[i,1],'%m/%d/%Y')==as.Date(When[j,1],'%Y-%m-%d')){
        S=S+1
        for(k in (0:(N-1))){Main$AVEDIV[i+k]=howmuch[S]}
      }else{next}
    }
  }
  return(Main)
}
#====================================
JUNEDATE=FindJune_Howmany_Backward()
#==================================
SESE$Maturity=outt
SESE$SPOTRET[4554]=0.003121999220976130
SESE$FUTURESRET[4554]=0.00056495
SESE$AVEDIV=SESE$AVEDIV/20
write.csv(SESE,file = "Current2.csv",row.names=T,sep="\t") 
write.table(SESE,file="ir.txt",sep=",",row.names = F,col.names = T)
saveRDS(SESE,"SESE017.rds")
SE=readRDS("SESE017.rds")

as.Date(SESE$Date[1],'%m/%d/%Y')

#=================================================================
#Extracting the payout dividends date from series
#=================================================================
# Usage: variables : series = your data with div and time
# d = Which column is Dividend e.g 3.34 ($)omitted 
# T = Which column is Date     e.g 2012/3/3
# Output will be a date serie of all dividends paying date

EXTRACTDIV<-function(SERIES=SESE,d=7,t=1){
  DIVSE<-structure(list(character()), class = "data.frame")
  j=0
  for (i in (1:nrow(SERIES))) {
    if(SERIES[i,d]!=0){
     j <- j + 1
     DIVSE[j,1] <- as.character(SERIES[i,t])
    }
  }
colnames(DIVSE)<-"DIVDATE"
return(DIVSE)
}
#=================================================================
# CrossDay Function to calculate how much dividends days passed
#=================================================================
#
#
#Previous Setting in case error
SESE$Divs=0
#------------------
CrossDay=function(Main=SESE,d=7,Start = SESE$Date, End=SESE$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y'){
  COC=0
  for(i in (1:nrow(Main))){
    for(j in (1:nrow(Divdata))){
      if((as.Date(Start[i],startformat)<=as.Date(Divdata[j,1],Divformat)) & (as.Date(as.character(End[i]),endformat)>=as.Date(Divdata[j,1],Divformat))){
        COC <- 1 + COC
        #print(COC)
        if(j==nrow(Divdata)){Main$Divs[i] <- COC}
      }else if(j==nrow(Divdata)){
        Main$Divs[i] <- COC
        }else{next}
      
    }
    COC<-0
  }
  return(Main)  
}
#------------------Implementing code
SET=CrossDay()
#================================================================
# Outt to SESE
#================================================================
for (i in (1:nrow(outt))) {
  SESE$Maturity[i]=outt[i,1]
}
#================================================================
#SAVE RDS 7/18 
#================================================================
saveRDS(SET,"NIKKEI0726.rds")

#================================================================
# Day Count Convention & Day Count faction
#================================================================
#calculate accrual day Convention : ACT/360, 30/360,and BD/360
#Main Data contain accrual start and end dates
#S(E)=indicate at which column is start(End) date in Main data

DCF=function(Convention='ACT/365',maindata=SET,S=1,E=8,SC='%m/%d/%Y',EC='%Y-%m-%d'){
  maindata[,S]<- as.character(maindata[,S])
  maindata[,E]<- as.character(maindata[,E])
  switch (Convention,
    'ACT/365' = for (i in (1:nrow(maindata))){maindata$'T-t'[i]<-(as.Date(maindata[i,E],EC)-as.Date(maindata[i,S],SC))/365}
  )
  return(maindata)
}


#----------save
saveRDS(SET,"NIKKEI0719.rds")
write.csv(SET,file = "Current progress.csv",row.names=F)
#----------------To start from this day's Data
dat <- read.csv("ACS.csv", header=TRUE)
SET <- readRDS("NIKKEI0726.rds")
View(SET)
SET$AVEDIV<-SET$AVEDIV/20

#================================================================
# COC1 : FTt = St * e^((rt-dt)*(T-t))
#================================================================
#Divs is DIV data like DIV <- c(134,334,445 ... )
#Main is a dataset including date column to which dividends are expected to append.
#T is at which column is date(in this case is 1)
#DIV adjustments
DIVRATE=function(Divs=DIV,Main=SET,T=1){
  StartYear=year(as.Date(Main[1,1],format='%m/%d/%Y'))
  
  d=length(Divs)
  for (i in (1:nrow(Main))) {
    if(i==1){
      Main$DIVR[i] <- DIV[d]/Main$SPOT[i]
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))==year(as.Date(Main[i-1,T],format='%m/%d/%Y'))){
      Main$DIVR[i] <- DIV[d]/Main$SPOT[i]
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))!=year(as.Date(Main[i-1,t],format='%m/%d/%Y'))){
      d = d - 1
      Main$DIVR[i] <- DIV[d]/Main$SPOT[i]
    }
  }
  return(Main)
}
#----------COC1
COC1=function(Main=SET2,S=2,F=1,M=8,R=4,D=11,T=10,FP=3){
  Main$COC1=0
  Main$MisCOC1=0
  for (i in (1:nrow(Main))) {
    Main$COC1[i] <- as.numeric(Main[i,S]*exp((Main[i,R]-Main[i,D])*Main[i,T]))
    Main$MisCOC1[i] <- as.numeric((Main$COC1[i]-Main[i,FP])/Main[i,S])
  }
  return(Main)
}

#--------------------------------------COC1 Revised Version------------------------------------
#Prepare Div from Bloomberg Nikkei 225 Yield 
DIVR<-c(1.4798 #From 2014...
,1.3692
,1.9553
,2.2703
,1.7084
,1.5305
,2.4964
,1.3691
,0.9923
,0.827
,0.9088
,0.8485
,0.9529
,0.8319
,0.6362
,0.4205
,0.6239
,0.5977
,0.449)#To 1996 #7/24 update divdends data

DIVR<-DIVR*0.01 # Convert to Percentage

View(DIVR)
#--------lateast version
#
#
#建議可以加上順或是反14-96 or 96-14可以用+-1來表示
#
RevisedCOC1=function(Divs=DIVR,Main=SET,T=1,R=4,FP=3,DU=10,S=2){
  StartYear=year(as.Date(Main[1,1],format='%m/%d/%Y'))
  Main$DIVR <-0 
  Main$COC1 <-0 
  Main$MisCOC1 <-0 
  d=1
  for (i in (1:nrow(Main))) {
    if(i==1){
      Main$DIVR[i] <- Divs[d]
      Main$COC1[i] <- as.numeric(Main[i,S]*exp((0.01*Main[i,R]-Main$DIVR[i])*Main[i,DU]))
      Main$MisCOC1[i] <- as.numeric(-(Main$COC1[i]-Main[i,FP])/Main[i,S])
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))==year(as.Date(Main[i-1,T],format='%m/%d/%Y'))){
      Main$DIVR[i] <- Divs[d]
      Main$COC1[i] <- as.numeric(Main[i,S]*exp((0.01*Main[i,R]-Main$DIVR[i])*Main[i,DU]))
      Main$MisCOC1[i] <- as.numeric(-(Main$COC1[i]-Main[i,FP])/Main[i,S])
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))!=year(as.Date(Main[i-1,T],format='%m/%d/%Y'))){
      d = d + 1
      Main$DIVR[i] <- Divs[d]
      Main$COC1[i] <- as.numeric(Main[i,S]*exp((0.01*Main[i,R]-Main$DIVR[i])*Main[i,DU]))
      Main$MisCOC1[i] <- as.numeric(-(Main$COC1[i]-Main[i,FP])/Main[i,S])
    }
  }
  return(Main)
}

#------------------------------------------------------SAVE
saveRDS(SET1,"COC1_REVISED.rds")
#====================Statistic Summary
D2014=filter(SET1,Date=='12/30/2014')
write.csv(SET1,'COC1REVISED.csv')


SET1[SET1["Divs"]>1]
#================================================================
# Use Dividend Yield to generate DIV series 
#================================================================
#
#
#closing price method
DivYieldDIV <- function(Divs=DIVR,Main=SET1,T=1,S=2){
  StartYear <- year(as.Date(Main[1,1],format='%m/%d/%Y'))
  Result <- structure(array(numeric()), class = "array")
  d <- 0
  for (i in (1:nrow(Main))) {
    if(i==1){
      Stock <- Main[i,S]
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))==year(as.Date(Main[i-1,T],format='%m/%d/%Y'))){
      next
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))!=year(as.Date(Main[i-1,T],format='%m/%d/%Y'))){
      d = d + 1
      print(Stock)
      Result[d] <- Divs[d]*Stock
      Stock <- Main[i,S]
      print(Result[d])
    }
  }
  return(Result)
}
#--------------------------------
ReDIV_closing<-DivYieldDIV()
saveRDS(ReDIV_closing,"ReDIV_closing.rds")
readRDS("ReDIV_closing.rds")
#-------------Average Price Method----------------------------------
DivYieldDIV_AVE <- function(Divs=DIVR,Main=SET1,T=1,S=2){
  StartYear <- year(as.Date(Main[1,1],format='%m/%d/%Y'))
  Result <- structure(array(numeric()), class = "array")
  d <- 0
  j <- 0
  for (i in (1:nrow(Main))) {
    if(i==1){
      Stocksum <- Main[i,S]
      j <- j + 1
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))==year(as.Date(Main[i-1,T],format='%m/%d/%Y'))){
      Stocksum <- Stocksum + Main[i,S]
      j <- j + 1
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))!=year(as.Date(Main[i-1,T],format='%m/%d/%Y'))){
      d = d + 1
      print(Stocksum)
      Result[d] <- Divs[d]*Stocksum/(j)
      Stocksum <- Main[i,S]
      j <- 1
      print(Result[d])
    }
  }
  return(Result)
}
#----------------------------------------------------------------
ReDIV_AVE <- DivYieldDIV_AVE()
#================================================================
# COC2 : FTt = St * e^(rt*(T-t)) - /Sigma^T_t {Dp*e^((rp)*(T-p))} 
#================================================================
# 
# 
# 
#Previous CrossDay for reference

#------------------
COC2 <- function(Main=SET1){
  
  Main$MisCOC2 <- 0
  for (i in (1:nrow(Main))) {
    sumDiv = 0
    k=0
    j=1
    if(Main[i,9] != 0){
      while (j < Main[i,9]+1) {
        k = k + 1
        if(Main[i-k+1,7]!=0){
          j = j + 1
          DCF = abs(as.numeric(as.Date(Main[i,8],'%Y-%m-%d') - as.Date(Main[i-k+1,1],'%m/%d/%Y')))/360
          sumDiv = Main[i-k+1,7]*exp(Main[i-k+1,4]*DCF) + sumDiv
          #print(sumDiv)
        }else{next}
      }
      
      Main$COC2[i] <- Main[i,2]*exp(Main[i,4]*as.numeric(Main[i,10]))-sumDiv
      Main$MisCOC2[i] <- as.numeric((Main$COC2[i]-Main[i,3])/Main[i,2])
    }else{
      Main$COC2[i] <- Main[i,2]*exp(Main[i,4]*as.numeric(Main[i,10]))
      Main$MisCOC2[i] <- as.numeric((Main$COC2[i]-Main[i,3])/Main[i,2])
    }
  }
  return(Main)
}
#----------------------------------------Generalized-----------------------------------
COC2G <- function(Main=SET1,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1){
  Main$COC2T1 <- 0
  Main$COC2T2 <- 0
  Main$COC2 <- 0
  Main$MisCOC2 <- 0
  for (i in (1:nrow(Main))) {
    sumDiv = 0
    k=0
    j=1
    if(Main[i,Divs] != 0){
      while (j < Main[i,Divs]+1) {
        k = k + 1
        if(Main[i-k+1,AVEDIV]!=0){
          j = j + 1
          DCF = abs(as.numeric(as.Date(Main[i,Ma],'%Y-%m-%d') - as.Date(Main[i-k+1,T],'%m/%d/%Y')))/365
          sumDiv = Main[i-k+1,AVEDIV]*exp(0.01*Main[i-k+1,rf]*DCF) + sumDiv
          #print(sumDiv)
        }else{next}
      }
      Main$COC2T1[i]<- Main[i,S]*exp(0.01*Main[i,rf]*as.numeric(Main[i,DF]))
      Main$COC2T2[i]<- sumDiv
      Main$COC2[i] <- Main[i,S]*exp(0.01*Main[i,rf]*as.numeric(Main[i,DF]))-sumDiv
      Main$MisCOC2[i] <- as.numeric(-(Main$COC2[i]-Main[i,Fu])/Main[i,S])
    }else{
      Main$COC2T1[i]<- Main[i,S]*exp(0.01*Main[i,rf]*as.numeric(Main[i,DF]))
      Main$COC2T2[i]<- 0
      Main$COC2[i] <- Main[i,S]*exp(0.01*Main[i,rf]*as.numeric(Main[i,DF]))
      Main$MisCOC2[i] <- as.numeric(-(Main$COC2[i]-Main[i,Fu])/Main[i,S])
    }
  }
  return(Main)
}
#===============False approach============to duplicate paper result=========
for (i in (1:nrow(SET1))){
  if(month(as.Date(SET1$Date[i],'%m/%d/%Y'))==1 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==2 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==3 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==4 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==5 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==7 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==8 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==9 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==10 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==11){
    SET1$Divs[i] <- 0
  }
}
j=0
for(i in (1:nrow(SET1))){
  YY=as.numeric(month(as.Date(SET1$Date[i],'%m/%d/%Y')))
  if(YY==12){
    if(SET1$AVEDIV[i]!=0){
      j=j+1
    }else if(j >= 10){
      SET1$Divs[i]=0
    }else{next}
  
  }else{
      j=0
    }
}

FCOC2G <- function(Main=SET1,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1){
  Main$COC2T1 <- 0
  Main$COC2T2 <- 0
  Main$COC2 <- 0
  Main$MisCOC2 <- 0
  for (i in (1:nrow(Main))) {
    sumDiv = 0
    k=0
    j=1
    if(Main[i,Divs] != 0){
      while (j < Main[i,Divs]+1) {
        k = k + 1
        if(Main[i-k+1,AVEDIV]!=0){
          j = j + 1
          DCF = abs(as.numeric(as.Date(Main[i,Ma],'%Y-%m-%d') - as.Date(Main[i-k+1,T],'%m/%d/%Y')))/360
          sumDiv = Main[i-k+1,AVEDIV]*exp(0.01*Main[i-k+1,rf]*DCF) + sumDiv
          #print(sumDiv)
        }else{next}
      }
      Main$COC2T1[i]<- Main[i,S]*exp(0.01*Main[i,rf]*as.numeric(Main[i,DF]))
      Main$COC2T2[i]<- sumDiv
      Main$COC2[i] <- Main[i,S]*exp(0.01*Main[i,rf]*as.numeric(Main[i,DF]))-sumDiv
      Main$MisCOC2[i] <- as.numeric(-(Main$COC2[i]-Main[i,Fu])/Main[i,S])
    }else{
      Main$COC2T1[i]<- Main[i,S]*exp(0.01*Main[i,rf]*as.numeric(Main[i,DF]))
      Main$COC2T2[i]<- 0
      Main$COC2[i] <- Main[i,S]*exp(0.01*Main[i,rf]*as.numeric(Main[i,DF]))
      Main$MisCOC2[i] <- as.numeric(-(Main$COC2[i]-Main[i,Fu])/Main[i,S])
    }
  }
  return(Main)
}
#---------------------------SAVE--------------------------------------------
saveRDS(SET3,"REVISEDCOC1&2.rds")
write.csv(SET3,file = "RevisedCOC12.csv",row.names=F)
SET3<-readRDS("REVISEDCOC1&2.rds")
#-------------------
saveRDS(SET2,"REVISEDCOC1&2726.rds")
write.csv(SET3,file = "726neg.csv",row.names=F)
#---------Data processing-------------------------------
YEARFUN <- function(Main = SET3){
  Main$YEAR=""
  for (i in (1:nrow(Main))) {
    Main$YEAR[i] <- as.character(year(as.Date(Main$Date[i],'%m/%d/%Y')))
  }
  return(Main)
}
SETYEAR<-YEARFUN(SET2)
SET2014=filter(SETYEAR, YEAR == "2014")

MainSUM=function(Main=SETYEAR){
  N=2014-1996+1
  for (i in (1:N)) {
    YEARR=1996+i-1
    #YEARR=as.character(YEAR)
    NOW=filter(Main,YEAR == YEARR)
    NUMBER=filter(NOW,MisCOC2>0)
    NUMBER2=filter(NOW,MisCOC2<=0)
    E=as.numeric(nrow(NOW))-as.numeric(nrow(NUMBER))
    print(YEARR)
    print('StD MisCOC2:')
    print(sd(NOW$MisCOC2))
    print('Average MisCOC2:')
    print(mean(NOW$MisCOC2))
    print('Number of non negative:')
    print(nrow(NUMBER))
    print('Mean of non negative:')
    print(mean(NUMBER$MisCOC2))
    print('Number negative:')
    print(E)
    print('Mean of negative:')
    print(mean(NUMBER2$MisCOC2))
  }
}
#-----------------------Extract MISCOC summary statistics by year

MISSUMM <- function(Divs=DIVR,Main=SETYEAR,T=1,S=2,MIS2=15){
  Result <- structure(array(numeric()), class = "array")
  d <- 0
  j <- 0
  for (i in (1:nrow(Main))) {
    if(i==1){
      Stocksum <- Main[i,MIS2]
      j <- j + 1
      print(Main[i,T])
    }else if(i==nrow(Main)){
      d <- d + 1
      j <- j + 1
      print(j)
      Result[d] <- Divs[d]*Stocksum/(j)
      print(Result[d])
      
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))==year(as.Date(Main[i-1,T],format='%m/%d/%Y'))){
      Stocksum <- Stocksum + Main[i,MIS2]
      j <- j + 1
    }else if(year(as.Date(Main[i,T],format='%m/%d/%Y'))!=year(as.Date(Main[i-1,T],format='%m/%d/%Y'))){
      d <- d + 1
      
      
      print(j)
      Result[d] <- Divs[d]*Stocksum/(j)
      print(Result[d])
      print(Main[i,T])
      Stocksum <- Main[i,MIS2]
      j <- 1
      
    }
  }
  return(Result)
}
#=================Run below to get log
MISSUMM()
#============7/26 發現錯誤 AVEDIV未更新
write.table(SET4,file="C://Users//Francis//Desktop//726updatecoc1.csv",sep=",",row.names=F, na = "NA")

#----------------draft---------------------------------
appendefalse=c(10,9,8,7,6,5,4,3,2,1)
d = 0
SET1$Divs=0

for (i in (1:nrow(SET1))) {
  if(SET1$AVEDIV[i]!=0){
    d = d + 1
    j = d%%10
    if(j==0){j=10}
    
    SET1$Divs[i]=appendefalse[j]
  }else{next}
  
}

write.csv(SET3,file = "False.csv",row.names=F)
saveRDS(SETYEAR,'731complete.rds')


#===================8/1==================
setwd("C:\\Users\\Francis\\Desktop\\Nikkei")
SET=readRDS('8m1dSETupdate5.rds')
SETupdate=DCF() # which is basis on 30/365
SETupdate$Divs=0
SETupdate$AVEDIV=0
DECDATE<-FindJune_Howmany_Forward(Month = 12,N=10,datelist = SETupdate$Date)
JUNEDATE<-FindJune_Howmany_Backward(Month = 6,N=3,datelist = SETupdate$Date)
#1417
SETupdate2=APPENDIV(When=DECDATE,Main=SETupdate,howmuch=DIV,N=10,excludelast = 0)
SETupdate3=APPENDIV(When=JUNEDATE,Main=SETupdate2,howmuch=DIV,N=10,excludelast = 0)

View(SETupdate3)

for(i in (1)){
  SETupdate3$AVEDIV[4555-i]<-86.93246
}

saveRDS(SETupdate3,'SETupdate3.rds')
DIVSES=EXTRACTDIV(SERIES=SETupdate3,d=7,t=1)
SETupdate4=CrossDay(Main=SETupdate3,d=7,Start = SETupdate3$Date, End=SETupdate3$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
saveRDS(SETupdate4,'8m1dSETupdate4.rds')
DIVR<-c(1.4798 #From 2014...
        ,1.3692
        ,1.9553
        ,2.2703
        ,1.7084
        ,1.5305
        ,2.4964
        ,1.3691
        ,0.9923
        ,0.827
        ,0.9088
        ,0.8485
        ,0.9529
        ,0.8319
        ,0.6362
        ,0.4205
        ,0.6239
        ,0.5977
        ,0.449)#To 1996 #7/24 update divdends data
DIVR<-DIVR*0.01 # Convert to Percentage
View(SETupdate4)

SETupdate4$AVEDIV=SETupdate4$AVEDIV/20
SETupdate5=RevisedCOC1(Divs=DIVR,Main=SETupdate4,T=1,R=4,FP=3,DU=10,S=2)
saveRDS(SETupdate5,'8m1dSETupdate5.rds')
SETupdate5=readRDS('8m1dSETupdate5.rds')
SETupdate6=COC2G(Main=SETupdate5,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
View(SETupdate6)
Part2day3=Setdummy(Direction=1,Main=SETupdate6,Judge=1,dateformat='%m/%d/%Y',whichm=6,lastway=TRUE,Howmany=3,Newbeta='beta3')
Part2day3=Setdummy(Direction=1,Main=Part2day3,Judge=1,dateformat='%m/%d/%Y',whichm=12,lastway=TRUE,Howmany=3,Newbeta='beta9')


View(Part2day3)
regressor_part8d4= lm(MisCOC2 ~ beta3+beta9 ,data = Part2day3)
summary(regressor_part8d4)

SETYEAR2<-YEARFUN(SETupdate6)
MainSUM(SETYEAR2)
write.csv(SETupdate6,file = "correct version.csv")
View(SETupdate5)

SET1=SETupdate5

for (i in (1:nrow(SET1))){
  if(month(as.Date(SET1$Date[i],'%m/%d/%Y'))==1 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==2 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==3 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==4 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==5 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==7 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==8 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==9 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==10 | month(as.Date(SET1$Date[i],'%m/%d/%Y'))==11){
    SET1$Divs[i] <- 0
  }
}
View(SET1)
j=0
for(i in (1:nrow(SET1))){
  YY=as.numeric(month(as.Date(SET1$Date[i],'%m/%d/%Y')))
  if(YY==6){
    if(SET1$AVEDIV[i]!=0){
      j=j+1
    }else if(j >= 10){
      SET1$Divs[i]=0
    }else{next}
    
  }else{
    j=0
  }
}
View(SET1)
SETupdate8=COC2G(Main=SET1,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
SETYEAR<-YEARFUN(SETupdate8)
MainSUM()
saveRDS(SETupdate8,'8m1dSETupdate8.rds')
write.csv(SETupdate8,file = "Qin's Corrected version.csv") 
View(SETupdate8)
#============regresssion 8/1==========
SETupdate8RE<-read.csv("SETupdate8RE.csv", header=T, sep=",",encoding = "UTF-8")
SETupdate8RE<-read.csv("correct version.csv", header=T, sep=",",encoding = "UTF-8")
SETupdate8RE<-read.csv("Qin's Corrected version.csv", header=T, sep=",",encoding = "UTF-8")
View(SETupdate8RE)
regressor = lm(MisCOC2 ~ Beta1+Beta2+Beta3+Beta4+Beta5+Beta6+Beta7+Beta8+Beta9+Beta10+Beta11+Beta12+Beta13+Beta14+Beta15+Beta16+Beta17+Beta18+Beta19+Beta20 ,data = SETupdate8RE)
regressor1 = lm(MisCOC2 ~ Beta1+Beta2+Beta3+Beta4+Beta5+Beta6+Beta7+Beta8+Beta9+Beta10 ,data = SETupdate8RE)
regressor2 = lm(MisCOC2 ~ Beta11+Beta12+Beta13+Beta14+Beta15+Beta16+Beta17+Beta18+Beta19+Beta20 ,data = SETupdate8RE)
summary(regressor) 
summary(regressor1)
summary(regressor2) 
saveRDS(regressor,'regressor.rds')
saveRDS(regressor1,'regressor1.rds')
saveRDS(regressor2,'regressor2')

#===================================================Part 3 -------8/6/2020-----------

MARDATE_10=FindJune_Howmany_Backward(Month=3,N=10,datelist=SETupdate5$Date,ifaddone = 0)
MARDATE_05=FindJune_Howmany_Backward(Month=3,N=5,datelist=SETupdate5$Date,ifaddone = 0)
SEPDATE_10=FindJune_Howmany_Backward(Month=9,N=10,datelist=SETupdate5$Date)
SEPDATE_05=FindJune_Howmany_Backward(Month=9,N=5,datelist=SETupdate5$Date)
DIV=c(258.2364945,
      223.0606165,
      203.2569545,
      191.9618111,
      174.7508693,
      161.4132642,
      221.1700558,
      209.578816,
      170.9319111,
      133.2415261,
      104.4098509,
      90.5912904,
      81.74881455,
      87.70405578,
      87.70455978,
      79.6188997,
      86.36129863,
      91.20148898,
      86.9324615
)
part3SET=SETupdate5
part3SET$AVEDIV=0
part3SET_05=APPENDIV(When = MARDATE_05,Main=part3SET,howmuch = DIV,N=5)
part3SET_05=APPENDIV(When = SEPDATE_05,Main=part3SET_05,howmuch = DIV,N=5)
#---------------------10
part3SET_10=APPENDIV(When = MARDATE_10,Main=part3SET,howmuch = DIV,N=10)
part3SET_10=APPENDIV(When = SEPDATE_10,Main=part3SET_10,howmuch = DIV,N=10)
#-------
part3SET_05$Divs=0
part3SET_10$Divs=0
#----------
DIVSES_05=EXTRACTDIV(SERIES = part3SET_05)
part3SET2_05=CrossDay(Main=part3SET_05,d=7,Start = part3SET_05$Date, End=part3SET_05$Maturity,Divdata=DIVSES_05,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')

DIVSES_10=EXTRACTDIV(SERIES = part3SET_10)
part3SET2_10=CrossDay(Main=part3SET_10,d=7,Start = part3SET_10$Date, End=part3SET_10$Maturity,Divdata=DIVSES_10,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
  
part3SET2_05$AVEDIV=part3SET2_05$AVEDIV/10
part3SET2_10$AVEDIV=part3SET2_10$AVEDIV/20

part3SET3_05=COC2G(Main = part3SET2_05)
part3SET3_10=COC2G(Main = part3SET2_10)
  
saveRDS(part3SET3_05,'part3 05.rds')
saveRDS(part3SET3_10,'part3 10.rds')
write.csv(part3SET3_05,file = "part3 05.csv",row.names=F)
write.csv(part3SET3_10,file = "part3 10.csv",row.names=F) 
#Add dummy from excel and reimport
DUMMpart3_05<-read.csv("part3 05.csv", header=T, sep=",",encoding = "UTF-8") 
DUMMpart3_10<-read.csv("part3 10.csv", header=T, sep=",",encoding = "UTF-8") 
regressor05 = lm(MisCOC2 ~ Beta1+Beta2+Beta3+Beta4+Beta5+Beta6+Beta7+Beta8+Beta9+Beta10 ,data = DUMMpart3_05)
regressor105 = lm(MisCOC2 ~ Beta1+Beta2+Beta3+Beta4+Beta5 ,data = DUMMpart3_05)
regressor205 = lm(MisCOC2 ~ Beta6+Beta7+Beta8+Beta9+Beta10 ,data = DUMMpart3_05)

regressor10 = lm(MisCOC2 ~ Beta1+Beta2+Beta3+Beta4+Beta5+Beta6+Beta7+Beta8+Beta9+Beta10+Beta11+Beta12+Beta13+Beta14+Beta15+Beta16+Beta17+Beta18+Beta19+Beta20 ,data = DUMMpart3_10)
regressor110 = lm(MisCOC2 ~ Beta1+Beta2+Beta3+Beta4+Beta5+Beta6+Beta7+Beta8+Beta9+Beta10 ,data = DUMMpart3_10)
regressor210 = lm(MisCOC2 ~ Beta11+Beta12+Beta13+Beta14+Beta15+Beta16+Beta17+Beta18+Beta19+Beta20 ,data = DUMMpart3_10)
summary(regressor05)
summary(regressor105)
summary(regressor205)
summary(regressor10)
summary(regressor110)
summary(regressor210)

#-----------LOG-------------
SETYEAR_05<-YEARFUN(DUMMpart3_05)
MainSUM(SETYEAR_05)

SETYEAR_10<-YEARFUN(DUMMpart3_10)
MainSUM(SETYEAR_10)
#----------------------------------------------Set dummay function
#Set dummy funciton
Setdummy<-function(Direction=1,Main=DUMMpart3_05,Judge=1,dateformat='%m/%d/%Y',whichmm=3,lastway=TRUE,Howmany=2,Newbeta='Newbeta1'){
  for (l in (1:length(whichmm))) {
  whichm = whichmm[l]
  M = l - 1
  if(lastway==TRUE){
    S=ncol(Main)+1-M
    Main[,S]=0
    for (i in (1:nrow(Main))) {
      if(i<=Howmany & Direction==1){
        Main[i,S]=0
      }else if(i>=nrow(Main)-Howmany & Direction==0){
          Main[i,S]=0
          }else if(month(as.Date(Main[i,Judge],dateformat))==whichm & month(as.Date(Main[i-Howmany,Judge],dateformat))==whichm){
        Main[i,S]=1
      }else{next}
    }
  }else if(lastway==FALSE){
    next
  }else{
    print('Your lastway is not boolin type!!')
  }
  colnames(Main)[ncol(Main)]=Newbeta
  }
  return(Main)
}
#========================================Set dummy 4.1,2
TRY=Setdummy()
TRY3=Setdummy(Howmany = 3)
DUM_05_EX_02=Setdummy(Direction=1,Main=TRY,Judge=1,dateformat='%m/%d/%Y',whichm=9,lastway=TRUE,Howmany=2,Newbeta='Newbeta1')
DUM_05_EX_03=Setdummy(Direction=1,Main=TRY3,Judge=1,dateformat='%m/%d/%Y',whichm=9,lastway=TRUE,Howmany=3,Newbeta='Newbeta1')

regressor_02 = lm(MisCOC1 ~ V28+V29 ,data = DUM_05_EX_02)

regressor_03 = lm(MisCOC1 ~ V28+V29 ,data = DUM_05_EX_03)
summary(regressor_02)
summary(regressor_03)

#========================================Model 5-1 & 2

APPENDIV<-function(When=JUNEDATE,Main=tata,howmuch=DIV,N=3,excludelast=3){
  S=0
  
  for (i in (1:nrow(Main))){
    
    for(j in (1:nrow(When))){
      if(as.Date(Main[i,1],'%m/%d/%Y')==as.Date(When[j,1],'%Y-%m-%d')){
        S=S+1
        while(i+N+excludelast>nrow(Main)){N = N - 1}
        for(k in (0:(N-1))){Main$AVEDIV[i+k+excludelast]=howmuch[S]}
      }else{next}
    }
  }
  return(Main)
}
#=================8/10========================5 model 4days
setwd("C:\\Users\\Francis\\Desktop\\Nikkei")
SETupdate=DCF(maindata=SET1) # which is basis on 30/365
SETupdate$Divs=0
SETupdate$AVEDIV=0
SEPDATE<-FindJune_Howmany_Backward(Month = 9,N=10,datelist = SETupdate$Date)
MARDATE<-FindJune_Howmany_Backward(Month = 3,N=10,datelist = SETupdate$Date)

SETupdate2=APPENDIV(When=SEPDATE,Main=SETupdate,howmuch=DIV,N=4,excludelast = 3)
SETupdate3=APPENDIV(When=MARDATE,Main=SETupdate2,howmuch=DIV,N=4,excludelast = 3)

View(SETupdate3)

saveRDS(SETupdate3,'part5 SETupdate3.rds')
DIVSES=EXTRACTDIV(SERIES=SETupdate3,d=7,t=1)
SETupdate4=CrossDay(Main=SETupdate3,d=7,Start = SETupdate3$Date, End=SETupdate3$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
saveRDS(SETupdate4,'8m1dSETupdate4.rds')
DIVR<-c(1.4798 #From 2014...
        ,1.3692
        ,1.9553
        ,2.2703
        ,1.7084
        ,1.5305
        ,2.4964
        ,1.3691
        ,0.9923
        ,0.827
        ,0.9088
        ,0.8485
        ,0.9529
        ,0.8319
        ,0.6362
        ,0.4205
        ,0.6239
        ,0.5977
        ,0.449)#To 1996 #7/24 update divdends data
DIVR<-DIVR*0.01 # Convert to Percentage
View(SETupdate4)

SETupdate4$AVEDIV=SETupdate4$AVEDIV/8
SETupdate5=RevisedCOC1(Divs=DIVR,Main=SETupdate4,T=1,R=4,FP=3,DU=10,S=2)
saveRDS(SETupdate5,'8m1dSETupdate5.rds')
SETupdate5=readRDS('8m1dSETupdate5.rds')
SETupdate6=COC2G(Main=SETupdate5,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
View(SETupdate6)
SETYEAR2<-YEARFUN(SETupdate6)
MainSUM(SETYEAR2)
write.csv(SETupdate6,file = "part5 4 days correct version.csv")
View(SETupdate5)
part6day4=SETupdate6

#============regresssion 8/10==========
part6day3=Setdummy(Direction=1,Main=SETupdate6,Judge=1,dateformat='%m/%d/%Y',whichm=3,lastway=TRUE,Howmany=3,Newbeta='Newbeta1')
part6day3=Setdummy(Direction=1,Main=part6day3,Judge=1,dateformat='%m/%d/%Y',whichm=9,lastway=TRUE,Howmany=3,Newbeta='Newbeta2')
View(part6day3)
regressor_part6d3= lm(MisCOC2 ~ Newbeta1+Newbeta2 ,data = part6day3)
summary(regressor_part6d3)
write.csv(part6day3,file = "part6 3 days.csv")
#-------------
part6day4=Setdummy(Direction=1,Main=part6day4,Judge=1,dateformat='%m/%d/%Y',whichm=3,lastway=TRUE,Howmany=3,Newbeta='Newbeta1')
part6day4=Setdummy(Direction=1,Main=part6day4,Judge=1,dateformat='%m/%d/%Y',whichm=9,lastway=TRUE,Howmany=3,Newbeta='Newbeta2')
View(part6day4)
regressor_part6d4= lm(MisCOC2 ~ Newbeta1+Newbeta2 ,data = part6day4)
summary(regressor_part6d4)
write.csv(part6day4,file = "part6 4 days.csv")

#=================8/10========================5 model 3days
setwd("C:\\Users\\Francis\\Desktop\\Nikkei")
SETupdate=DCF(maindata=SET1) # which is basis on 30/365
SETupdate$Divs=0
SETupdate$AVEDIV=0
SEPDATE<-FindJune_Howmany_Backward(Month = 9,N=10,datelist = SETupdate$Date)
MARDATE<-FindJune_Howmany_Backward(Month = 3,N=10,datelist = SETupdate$Date)

SETupdate2=APPENDIV(When=SEPDATE,Main=SETupdate,howmuch=DIV,N=3,excludelast = 3)
SETupdate3=APPENDIV(When=MARDATE,Main=SETupdate2,howmuch=DIV,N=3,excludelast = 3)

View(SETupdate3)

saveRDS(SETupdate3,'part5 3days SETupdate3.rds')
DIVSES=EXTRACTDIV(SERIES=SETupdate3,d=7,t=1)
SETupdate4=CrossDay(Main=SETupdate3,d=7,Start = SETupdate3$Date, End=SETupdate3$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
saveRDS(SETupdate4,'part5 day38m1dSETupdate4.rds')
DIVR<-c(1.4798 #From 2014...
        ,1.3692
        ,1.9553
        ,2.2703
        ,1.7084
        ,1.5305
        ,2.4964
        ,1.3691
        ,0.9923
        ,0.827
        ,0.9088
        ,0.8485
        ,0.9529
        ,0.8319
        ,0.6362
        ,0.4205
        ,0.6239
        ,0.5977
        ,0.449)#To 1996 #7/24 update divdends data
DIVR<-DIVR*0.01 # Convert to Percentage
View(SETupdate4)

SETupdate4$AVEDIV=SETupdate4$AVEDIV/6
SETupdate5=RevisedCOC1(Divs=DIVR,Main=SETupdate4,T=1,R=4,FP=3,DU=10,S=2)
saveRDS(SETupdate5,'day 3 8m1dSETupdate5.rds')
SETupdate5=readRDS('8m1dSETupdate5.rds')
SETupdate6=COC2G(Main=SETupdate5,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
View(SETupdate6)
SETYEAR2<-YEARFUN(SETupdate6)
MainSUM(SETYEAR2)
write.csv(SETupdate6,file = "part5 3 days correct version.csv")
View(SETupdate5)

#================================== 8/16 part 8 ====================== 
#3days
DIV=c(258.2364945,
      223.0606165,
      203.2569545,
      191.9618111,
      174.7508693,
      161.4132642,
      221.1700558,
      209.578816,
      170.9319111,
      133.2415261,
      104.4098509,
      90.5912904,
      81.74881455,
      87.70405578,
      87.70455978,
      79.6188997,
      86.36129863,
      91.20148898,
      86.9324615
)
DIV_6_12=0.25*DIV
DIV_3_9=0.75*DIV
SET1=readRDS('8m1dSETupdate5.rds')

SETupdate=DCF(maindata=SET1) # which is basis on 30/365
SETupdate$Divs=0
SETupdate$AVEDIV=0

SEPDATE<-FindJune_Howmany_Backward(Month = 9,N=3,datelist = SETupdate$Date)
MARDATE<-FindJune_Howmany_Backward(Month = 3,N=3,datelist = SETupdate$Date)
JUNEDATE<-FindJune_Howmany_Backward(Month = 6,N=3,datelist = SETupdate$Date)
DECDATE<-FindJune_Howmany_Backward(Month = 12,N=3,datelist = SETupdate$Date)
#1410 lines
SETupdate2=APPENDIV(When=SEPDATE,Main=SETupdate,howmuch=DIV_3_9/6,N=3,excludelast = 3)
SETupdate3=APPENDIV(When=MARDATE,Main=SETupdate2,howmuch=DIV_3_9/6,N=3,excludelast = 3)
SETupdate4=APPENDIV(When=JUNEDATE,Main=SETupdate3,howmuch=DIV_6_12/6,N=3,excludelast = 3)
SETupdate5=APPENDIV(When=DECDATE,Main=SETupdate4,howmuch=DIV_6_12/6,N=3,excludelast = 3)

View(SETupdate5)

saveRDS(SETupdate5,'part8 SETupdate5.rds')
DIVSES=EXTRACTDIV(SERIES=SETupdate5,d=7,t=1)
SETupdate6=CrossDay(Main=SETupdate5,d=7,Start = SETupdate5$Date, End=SETupdate5$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
saveRDS(SETupdate6,'part8 cross day.rds')
DIVR<-c(1.4798 #From 2014...
        ,1.3692
        ,1.9553
        ,2.2703
        ,1.7084
        ,1.5305
        ,2.4964
        ,1.3691
        ,0.9923
        ,0.827
        ,0.9088
        ,0.8485
        ,0.9529
        ,0.8319
        ,0.6362
        ,0.4205
        ,0.6239
        ,0.5977
        ,0.449)#To 1996 #7/24 update divdends data
DIVR<-DIVR*0.01 # Convert to Percentage
View(SETupdate6)


SETupdate5=RevisedCOC1(Divs=DIVR,Main=SETupdate4,T=1,R=4,FP=3,DU=10,S=2)
saveRDS(SETupdate5,'8m1dSETupdate5.rds')
SETupdate5=readRDS('8m1dSETupdate5.rds')

SETupdate7=COC2G(Main=SETupdate6,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
View(SETupdate7)
SETYEAR2<-YEARFUN(SETupdate7)
MainSUM(SETYEAR2)
write.csv(SETupdate7,file = "part8 3 days correct version.csv")

Part8day3=Setdummy(Direction=1,Main=SETupdate7,Judge=1,dateformat='%m/%d/%Y',whichmm=c(3),lastway=TRUE,Howmany=3,Newbeta='beta3and9')
Part8day3=Setdummy(Direction=1,Main=Part8day3,Judge=1,dateformat='%m/%d/%Y',whichm=6,lastway=TRUE,Howmany=3,Newbeta='beta6&12')
Part8day3=Setdummy(Direction=1,Main=Part8day3,Judge=1,dateformat='%m/%d/%Y',whichm=c(9),lastway=TRUE,Howmany=3,Newbeta='beta6and12')
Part8day3=Setdummy(Direction=1,Main=Part8day3,Judge=1,dateformat='%m/%d/%Y',whichm=12,lastway=TRUE,Howmany=3,Newbeta='beta6&12')

View(Part8day3)
regressor_part8d3= lm(MisCOC2 ~ beta3+beta6+beta9+beta12 ,data = Part8day3)
summary(regressor_part8d3)

regressor_part8d3= lm(MisCOC2 ~ beta3and9+beta6and12 ,data = Part8day3)
summary(regressor_part8d3)
#day4================================================

SET1=readRDS('8m1dSETupdate5.rds')

SETupdate=DCF(maindata=SET1) # which is basis on 30/365
SETupdate$Divs=0
SETupdate$AVEDIV=0

SEPDATE<-FindJune_Howmany_Backward(Month = 9,N=3,datelist = SETupdate$Date)
MARDATE<-FindJune_Howmany_Backward(Month = 3,N=3,datelist = SETupdate$Date)
JUNEDATE<-FindJune_Howmany_Backward(Month = 6,N=3,datelist = SETupdate$Date)
DECDATE<-FindJune_Howmany_Backward(Month = 12,N=3,datelist = SETupdate$Date)
#1410 lines
SETupdate2=APPENDIV(When=SEPDATE,Main=SETupdate,howmuch=DIV_3_9/8,N=4,excludelast = 3)
SETupdate3=APPENDIV(When=MARDATE,Main=SETupdate2,howmuch=DIV_3_9/8,N=4,excludelast = 3)
SETupdate4=APPENDIV(When=JUNEDATE,Main=SETupdate3,howmuch=DIV_6_12/8,N=4,excludelast = 3)
SETupdate5=APPENDIV(When=DECDATE,Main=SETupdate4,howmuch=DIV_6_12/8,N=4,excludelast = 3)

View(SETupdate5)

saveRDS(SETupdate5,'part8 day4 SETupdate5.rds')
DIVSES=EXTRACTDIV(SERIES=SETupdate5,d=7,t=1)
SETupdate6=CrossDay(Main=SETupdate5,d=7,Start = SETupdate5$Date, End=SETupdate5$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
saveRDS(SETupdate6,'part8 days4 cross day.rds')

View(SETupdate6)

SETupdate7=COC2G(Main=SETupdate6,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
View(SETupdate7)
SETYEAR2<-YEARFUN(SETupdate7)
MainSUM(SETYEAR2)
write.csv(SETupdate7,file = "part8 4 days correct version.csv")

Part8day4=Setdummy(Direction=1,Main=SETupdate7,Judge=1,dateformat='%m/%d/%Y',whichm=c(6),lastway=TRUE,Howmany=3,Newbeta='beta3and9')
Part8day4=Setdummy(Direction=1,Main=Part8day4,Judge=1,dateformat='%m/%d/%Y',whichm=6,lastway=TRUE,Howmany=3,Newbeta='beta6')
Part8day4=Setdummy(Direction=1,Main=Part8day4,Judge=1,dateformat='%m/%d/%Y',whichm=c(12),lastway=TRUE,Howmany=3,Newbeta='beta6and12')
Part8day4=Setdummy(Direction=1,Main=Part8day4,Judge=1,dateformat='%m/%d/%Y',whichm=12,lastway=TRUE,Howmany=3,Newbeta='beta12')

View(Part8day4)
regressor_part8d4= lm(MisCOC2 ~ beta3+beta6+beta9+beta12 ,data = Part8day4)
summary(regressor_part8d4)

regressor_part8d4= lm(MisCOC2 ~ beta3and9+beta6and12 ,data = Part8day4)
summary(regressor_part8d4)
#=================================================== part 7 regression 
#================================== 8/30 part 8 80% 20% test ====================== 
#3days
DIV=c(258.2364945,
      223.0606165,
      203.2569545,
      191.9618111,
      174.7508693,
      161.4132642,
      221.1700558,
      209.578816,
      170.9319111,
      133.2415261,
      104.4098509,
      90.5912904,
      81.74881455,
      87.70405578,
      87.70455978,
      79.6188997,
      86.36129863,
      91.20148898,
      86.9324615
)
DIV_6_12=0.20*DIV
DIV_3_9=0.80*DIV
SET1=readRDS('8m1dSETupdate5.rds')

SETupdate=DCF(maindata=SET1) # which is basis on 30/365
SETupdate$Divs=0
SETupdate$AVEDIV=0

SEPDATE<-FindJune_Howmany_Backward(Month = 9,N=3,datelist = SETupdate$Date)
MARDATE<-FindJune_Howmany_Backward(Month = 3,N=3,datelist = SETupdate$Date)
JUNEDATE<-FindJune_Howmany_Backward(Month = 6,N=3,datelist = SETupdate$Date)
DECDATE<-FindJune_Howmany_Backward(Month = 12,N=3,datelist = SETupdate$Date)
#1410 lines
SETupdate2=APPENDIV(When=SEPDATE,Main=SETupdate,howmuch=DIV_3_9/6,N=3,excludelast = 3)
SETupdate3=APPENDIV(When=MARDATE,Main=SETupdate2,howmuch=DIV_3_9/6,N=3,excludelast = 3)
SETupdate4=APPENDIV(When=JUNEDATE,Main=SETupdate3,howmuch=DIV_6_12/6,N=3,excludelast = 3)
SETupdate5=APPENDIV(When=DECDATE,Main=SETupdate4,howmuch=DIV_6_12/6,N=3,excludelast = 3)

View(SETupdate5)

saveRDS(SETupdate5,'part8 test 80 20 SETupdate5.rds')
DIVSES=EXTRACTDIV(SERIES=SETupdate5,d=7,t=1)
SETupdate6=CrossDay(Main=SETupdate5,d=7,Start = SETupdate5$Date, End=SETupdate5$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
saveRDS(SETupdate6,'part8 80 20 cross day.rds')
DIVR<-c(1.4798 #From 2014...
        ,1.3692
        ,1.9553
        ,2.2703
        ,1.7084
        ,1.5305
        ,2.4964
        ,1.3691
        ,0.9923
        ,0.827
        ,0.9088
        ,0.8485
        ,0.9529
        ,0.8319
        ,0.6362
        ,0.4205
        ,0.6239
        ,0.5977
        ,0.449)#To 1996 #7/24 update divdends data
DIVR<-DIVR*0.01 # Convert to Percentage
View(SETupdate6)


SETupdate5=RevisedCOC1(Divs=DIVR,Main=SETupdate4,T=1,R=4,FP=3,DU=10,S=2)
saveRDS(SETupdate5,'8m1dSETupdate5.rds')
SETupdate5=readRDS('8m1dSETupdate5.rds')

SETupdate7=COC2G(Main=SETupdate6,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
View(SETupdate7)
SETYEAR2<-YEARFUN(SETupdate7)
MainSUM(SETYEAR2)
write.csv(SETupdate7,file = "part8 3 days correct version.csv")

Part8day3=Setdummy(Direction=1,Main=SETupdate7,Judge=1,dateformat='%m/%d/%Y',whichmm=c(3),lastway=TRUE,Howmany=3,Newbeta='beta3')
Part8day3=Setdummy(Direction=1,Main=Part8day3,Judge=1,dateformat='%m/%d/%Y',whichm=6,lastway=TRUE,Howmany=3,Newbeta='beta6')
Part8day3=Setdummy(Direction=1,Main=Part8day3,Judge=1,dateformat='%m/%d/%Y',whichm=c(9),lastway=TRUE,Howmany=3,Newbeta='beta9')
Part8day3=Setdummy(Direction=1,Main=Part8day3,Judge=1,dateformat='%m/%d/%Y',whichm=12,lastway=TRUE,Howmany=3,Newbeta='beta12')

View(Part8day3)
regressor_part8d3= lm(MisCOC2 ~ beta3+beta6+beta9+beta12 ,data = Part8day3)
summary(regressor_part8d3)

regressor_part8d3= lm(MisCOC2 ~ beta3and9+beta6and12 ,data = Part8day3)
summary(regressor_part8d3)
#day4================================================

SET1=readRDS('8m1dSETupdate5.rds')

SETupdate=DCF(maindata=SET1) # which is basis on 30/365
SETupdate$Divs=0
SETupdate$AVEDIV=0

SEPDATE<-FindJune_Howmany_Backward(Month = 9,N=3,datelist = SETupdate$Date)
MARDATE<-FindJune_Howmany_Backward(Month = 3,N=3,datelist = SETupdate$Date)
JUNEDATE<-FindJune_Howmany_Backward(Month = 6,N=3,datelist = SETupdate$Date)
DECDATE<-FindJune_Howmany_Backward(Month = 12,N=3,datelist = SETupdate$Date)
#1410 lines
SETupdate2=APPENDIV(When=SEPDATE,Main=SETupdate,howmuch=DIV_3_9/8,N=4,excludelast = 3)
SETupdate3=APPENDIV(When=MARDATE,Main=SETupdate2,howmuch=DIV_3_9/8,N=4,excludelast = 3)
SETupdate4=APPENDIV(When=JUNEDATE,Main=SETupdate3,howmuch=DIV_6_12/8,N=4,excludelast = 3)
SETupdate5=APPENDIV(When=DECDATE,Main=SETupdate4,howmuch=DIV_6_12/8,N=4,excludelast = 3)

View(SETupdate5)

saveRDS(SETupdate5,'part8 day4 80 20 SETupdate5.rds')
DIVSES=EXTRACTDIV(SERIES=SETupdate5,d=7,t=1)
SETupdate6=CrossDay(Main=SETupdate5,d=7,Start = SETupdate5$Date, End=SETupdate5$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
saveRDS(SETupdate6,'part8 days4 80 20 cross day.rds')

View(SETupdate6)

SETupdate7=COC2G(Main=SETupdate6,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
View(SETupdate7)
SETYEAR2<-YEARFUN(SETupdate7)
MainSUM(SETYEAR2)
write.csv(SETupdate7,file = "part8 4 days correct version.csv")

Part8day4=Setdummy(Direction=1,Main=SETupdate7,Judge=1,dateformat='%m/%d/%Y',whichm=c(3),lastway=TRUE,Howmany=3,Newbeta='beta3')
Part8day4=Setdummy(Direction=1,Main=Part8day4,Judge=1,dateformat='%m/%d/%Y',whichm=6,lastway=TRUE,Howmany=3,Newbeta='beta6')
Part8day4=Setdummy(Direction=1,Main=Part8day4,Judge=1,dateformat='%m/%d/%Y',whichm=c(9),lastway=TRUE,Howmany=3,Newbeta='beta9')
Part8day4=Setdummy(Direction=1,Main=Part8day4,Judge=1,dateformat='%m/%d/%Y',whichm=12,lastway=TRUE,Howmany=3,Newbeta='beta12')

View(Part8day4)
regressor_part8d4= lm(MisCOC2 ~ beta3+beta6+beta9+beta12 ,data = Part8day4)
summary(regressor_part8d4)

regressor_part8d4= lm(MisCOC2 ~ beta3and9+beta6and12 ,data = Part8day4)
summary(regressor_part8d4)
#================================== 8/30 part 8 85% 15% test ====================== 
#3days
DIV=c(258.2364945,
      223.0606165,
      203.2569545,
      191.9618111,
      174.7508693,
      161.4132642,
      221.1700558,
      209.578816,
      170.9319111,
      133.2415261,
      104.4098509,
      90.5912904,
      81.74881455,
      87.70405578,
      87.70455978,
      79.6188997,
      86.36129863,
      91.20148898,
      86.9324615
)
DIV_6_12=0.15*DIV
DIV_3_9=0.85*DIV
SET1=readRDS('8m1dSETupdate5.rds')

SETupdate=DCF(maindata=SET1) # which is basis on 30/365
SETupdate$Divs=0
SETupdate$AVEDIV=0

SEPDATE<-FindJune_Howmany_Backward(Month = 9,N=3,datelist = SETupdate$Date)
MARDATE<-FindJune_Howmany_Backward(Month = 3,N=3,datelist = SETupdate$Date)
JUNEDATE<-FindJune_Howmany_Backward(Month = 6,N=3,datelist = SETupdate$Date)
DECDATE<-FindJune_Howmany_Backward(Month = 12,N=3,datelist = SETupdate$Date)
#1410 lines
SETupdate2=APPENDIV(When=SEPDATE,Main=SETupdate,howmuch=DIV_3_9/6,N=3,excludelast = 3)
SETupdate3=APPENDIV(When=MARDATE,Main=SETupdate2,howmuch=DIV_3_9/6,N=3,excludelast = 3)
SETupdate4=APPENDIV(When=JUNEDATE,Main=SETupdate3,howmuch=DIV_6_12/6,N=3,excludelast = 3)
SETupdate5=APPENDIV(When=DECDATE,Main=SETupdate4,howmuch=DIV_6_12/6,N=3,excludelast = 3)

View(SETupdate5)

saveRDS(SETupdate5,'part8 test 80 20 SETupdate5.rds')
DIVSES=EXTRACTDIV(SERIES=SETupdate5,d=7,t=1)
SETupdate6=CrossDay(Main=SETupdate5,d=7,Start = SETupdate5$Date, End=SETupdate5$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
saveRDS(SETupdate6,'part8 85 15 cross day.rds')
DIVR<-c(1.4798 #From 2014...
        ,1.3692
        ,1.9553
        ,2.2703
        ,1.7084
        ,1.5305
        ,2.4964
        ,1.3691
        ,0.9923
        ,0.827
        ,0.9088
        ,0.8485
        ,0.9529
        ,0.8319
        ,0.6362
        ,0.4205
        ,0.6239
        ,0.5977
        ,0.449)#To 1996 #7/24 update divdends data
DIVR<-DIVR*0.01 # Convert to Percentage
View(SETupdate6)


SETupdate5=RevisedCOC1(Divs=DIVR,Main=SETupdate4,T=1,R=4,FP=3,DU=10,S=2)
saveRDS(SETupdate5,'8m1dSETupdate5.rds')
SETupdate5=readRDS('8m1dSETupdate5.rds')

SETupdate7=COC2G(Main=SETupdate6,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
View(SETupdate7)
SETYEAR2<-YEARFUN(SETupdate7)
MainSUM(SETYEAR2)
write.csv(SETupdate7,file = "part8 3 days correct version.csv")

Part8day3=Setdummy(Direction=1,Main=SETupdate7,Judge=1,dateformat='%m/%d/%Y',whichmm=c(3,9),lastway=TRUE,Howmany=3,Newbeta='beta3and9')
Part8day3=Setdummy(Direction=1,Main=Part8day3,Judge=1,dateformat='%m/%d/%Y',whichm=6,lastway=TRUE,Howmany=3,Newbeta='beta6')
Part8day3=Setdummy(Direction=1,Main=Part8day3,Judge=1,dateformat='%m/%d/%Y',whichm=c(6,12),lastway=TRUE,Howmany=3,Newbeta='beta6and12')
Part8day3=Setdummy(Direction=1,Main=Part8day3,Judge=1,dateformat='%m/%d/%Y',whichm=12,lastway=TRUE,Howmany=3,Newbeta='beta12')

View(Part8day3)
regressor_part8d3= lm(MisCOC2 ~ beta3+beta6+beta9+beta12 ,data = Part8day3)
summary(regressor_part8d3)

regressor_part8d3= lm(MisCOC2 ~ beta3and9+beta6and12 ,data = Part8day3)
summary(regressor_part8d3)
#day4================================================

SET1=readRDS('8m1dSETupdate5.rds')

SETupdate=DCF(maindata=SET1) # which is basis on 30/365
SETupdate$Divs=0
SETupdate$AVEDIV=0

SEPDATE<-FindJune_Howmany_Backward(Month = 9,N=3,datelist = SETupdate$Date)
MARDATE<-FindJune_Howmany_Backward(Month = 3,N=3,datelist = SETupdate$Date)
JUNEDATE<-FindJune_Howmany_Backward(Month = 6,N=3,datelist = SETupdate$Date)
DECDATE<-FindJune_Howmany_Backward(Month = 12,N=3,datelist = SETupdate$Date)
#1410 lines
SETupdate2=APPENDIV(When=SEPDATE,Main=SETupdate,howmuch=DIV_3_9/8,N=4,excludelast = 3)
SETupdate3=APPENDIV(When=MARDATE,Main=SETupdate2,howmuch=DIV_3_9/8,N=4,excludelast = 3)
SETupdate4=APPENDIV(When=JUNEDATE,Main=SETupdate3,howmuch=DIV_6_12/8,N=4,excludelast = 3)
SETupdate5=APPENDIV(When=DECDATE,Main=SETupdate4,howmuch=DIV_6_12/8,N=4,excludelast = 3)

View(SETupdate5)

saveRDS(SETupdate5,'part8 day4 85 15 SETupdate5.rds')
DIVSES=EXTRACTDIV(SERIES=SETupdate5,d=7,t=1)
SETupdate6=CrossDay(Main=SETupdate5,d=7,Start = SETupdate5$Date, End=SETupdate5$Maturity,Divdata=DIVSES,startformat='%m/%d/%Y',endformat='%Y-%m-%d',Divformat='%m/%d/%Y')
saveRDS(SETupdate6,'part8 days4 85 15 cross day.rds')

View(SETupdate6)

SETupdate7=COC2G(Main=SETupdate6,Divs=9,AVEDIV=7,Ma=8,rf=4,Fu=3,S=2,DF=10,T=1)
View(SETupdate7)
SETYEAR2<-YEARFUN(SETupdate7)
MainSUM(SETYEAR2)
write.csv(SETupdate7,file = "part8 4 days correct version.csv")

Part8day4=Setdummy(Direction=1,Main=SETupdate7,Judge=1,dateformat='%m/%d/%Y',whichm=c(3,9),lastway=TRUE,Howmany=3,Newbeta='beta3and9')
Part8day4=Setdummy(Direction=1,Main=Part8day4,Judge=1,dateformat='%m/%d/%Y',whichm=6,lastway=TRUE,Howmany=3,Newbeta='beta6')
Part8day4=Setdummy(Direction=1,Main=Part8day4,Judge=1,dateformat='%m/%d/%Y',whichm=c(6,12),lastway=TRUE,Howmany=3,Newbeta='beta6and12')
Part8day4=Setdummy(Direction=1,Main=Part8day4,Judge=1,dateformat='%m/%d/%Y',whichm=12,lastway=TRUE,Howmany=3,Newbeta='beta12')

View(Part8day4)
regressor_part8d4= lm(MisCOC2 ~ beta3+beta6+beta9+beta12 ,data = Part8day4)
summary(regressor_part8d4)

regressor_part8d4= lm(MisCOC2 ~ beta3and9+beta6and12 ,data = Part8day4)
summary(regressor_part8d4)

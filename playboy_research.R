library("xlsx")
library("lubridate")

## Read data
df <- read.xlsx("1702_Infoporn_Playmate_Data.xls", sheetIndex=1, startRow=1, endRow=663)
names(df) <- c("Month", "Year", "Bust", "Cup", "Waist", "Hips", "Height", "Weight", "BMI", "WH")

## Convert into numeric
for(i in 3:10) {
  df[,i] <- as.numeric(gsub('[A-Z a-z/]', '', df[,i]))
}

## Convert Year and Month into Date type
tmp <- paste(paste(df$Year, df$Month), "01")
df$Date <- as.Date(tmp, format="%Y %B %d")

## Find complete cases, make new data frames
tmp <- subset(df, complete.cases(df$Bust)==TRUE)
df_Bust <- tmp[!(tmp$Bust == "NOT LISTED"),]
df_Bust$Bust <- as.numeric(gsub('[A-Z a-z/]', '', df_Bust$Bust))

tmp <- subset(df, complete.cases(df$Cup)==TRUE)
df_Cup <- tmp[!(tmp$Cup == "NOT LISTED"),]

tmp <- subset(df, complete.cases(df$Waist)==TRUE)
df_Waist <- tmp[!(tmp$Waist == "NOT LISTED"),]
df_Waist$Waist <- as.numeric(gsub('[A-Z a-z/]', '', df_Waist$Waist))

tmp <- subset(df, complete.cases(df$Hips)==TRUE)
df_Hips <- tmp[!(tmp$Hips == "NOT LISTED"),]
df_Hips$Hips <- as.numeric(gsub('[A-Z a-z/]', '', df_Hips$Hips))

tmp <- subset(df, complete.cases(df$Height)==TRUE)
df_Height <- tmp[!(tmp$Height == "NOT LISTED"),]
df_Height$Height <- as.numeric(gsub('[A-Z a-z/]', '', df_Height$Height))

tmp <- subset(df, complete.cases(df$Weight)==TRUE)
df_Weight <- tmp[!(tmp$Weight == "NOT LISTED"),]
df_Weight$Weight <- as.numeric(gsub('[A-Z a-z/]', '', df_Weight$Weight))

tmp <- subset(df, complete.cases(df$BMI)==TRUE)
df_BMI <- tmp[!(tmp$BMI == 0),]

tmp <- subset(df, complete.cases(df$WH)==TRUE)
df_WH <- tmp[!(tmp$WH == 0),]

par(mfrow=c(3,3))
## Plot Bust size
hist(df_Bust$Bust, main="Частота Bust", xlab="Bust", col="cyan")
abline(v=mean(df_Bust$Bust), lty=2, lwd=3, col="red")
abline(v=median(df_Bust$Bust), lty=2, lwd=3, col="blue")
text(mean(df_Bust$Bust), 100, labels = paste("mean = ", as.character(round(mean(df_Bust$Bust), digits=4))),
     pos = 4, col="red")
text(mean(df_Bust$Bust), 120, labels = paste("median = ", as.character(round(median(df_Bust$Bust), digits=4))),
     pos = 4, col="blue")

## Plot Cup size
hist(df_Cup$Cup, main="Частота Cup", xlab="Cup", col="cyan")
abline(v=mean(df_Cup$Cup), lty=2, lwd=3, col="red")
abline(v=median(df_Cup$Cup), lty=2, lwd=3, col="blue")
text(mean(df_Cup$Cup), 50, labels = paste("mean = ", as.character(round(mean(df_Cup$Cup), digits=4))), 
     pos = 4, col="red")
text(mean(df_Cup$Cup), 60, labels = paste("median = ", as.character(round(median(df_Cup$Cup), digits=4))), 
     pos = 4, col="blue")

## Plot Waist size
hist(df_Waist$Waist, main="Частота Waist", xlab="Waist", col="cyan")
abline(v=mean(df_Waist$Waist), lty=2, lwd=3, col="red")
abline(v=median(df_Waist$Waist), lty=2, lwd=3, col="blue")
text(mean(df_Waist$Waist), 200, labels = paste("mean = ", as.character(round(mean(df_Waist$Waist), digits=4))), 
     pos = 4, col="red")
text(mean(df_Waist$Waist), 230, labels = paste("median = ", as.character(round(median(df_Waist$Waist), digits=4))), 
     pos = 4, col="blue")

## Plot Hips size
hist(df_Hips$Hips, main="Частота Hips", xlab="Hips", col="cyan")
abline(v=mean(df_Hips$Hips), lty=2, lwd=3, col="red")
abline(v=median(df_Hips$Hips), lty=2, lwd=3, col="blue")
text(mean(df_Hips$Hips), 150, labels = paste("mean = ", as.character(round(mean(df_Hips$Hips), digits=4))), 
     pos = 3, col="red")
text(mean(df_Hips$Hips), 170, labels = paste("median = ", as.character(round(median(df_Hips$Hips), digits=4))), 
     pos = 3, col="blue")

## List models whos hips size < 30
df_Hips[df_Hips$Hips < 30,]
## 1. Feb 1991 - Cristy Thom, real hips size: 36" - incorrect, need to fix in the dataset
## 2. Apr 2003 - Carmella DeCesare, real hips size: 27 - correct
## 3. Jan 2005 - Destiny Davis, real hips size: 34" - incorrect, need to fix in the dataset

## Plot Height size
hist(df_Height$Height, main="Частота Height", xlab="Height", col="cyan")
abline(v=mean(df_Height$Height), lty=2, lwd=3, col="red")
abline(v=median(df_Height$Height), lty=2, lwd=3, col="blue")
text(mean(df_Height$Height), 60, labels = paste("mean = ", as.character(round(mean(df_Height$Height), digits=4))),
     pos = 4, col="red")
text(mean(df_Height$Height), 80, labels = paste("median = ", as.character(round(median(df_Height$Height), digits=4))),
     pos = 4, col="blue")

## Plot Weight size
hist(df_Weight$Weight, main="Частота Weight", xlab="Weight", col="cyan")
abline(v=mean(df_Weight$Weight), lty=2, lwd=3, col="red")
abline(v=median(df_Weight$Weight), lty=2, lwd=3, col="blue")
text(mean(df_Weight$Weight), 100, labels = paste("mean = ", as.character(round(mean(df_Weight$Weight), digits=4))),
     pos = 4, col="red")
text(mean(df_Weight$Weight), 120, labels = paste("median = ", as.character(round(median(df_Weight$Weight), digits=4))),
     pos = 4, col="blue")

## Plot BMI size
hist(df_BMI$BMI, main="Частота BMI", xlab="BMI", col="cyan") 
abline(v=mean(df_BMI$BMI), lty=2, lwd=3, col="red")
abline(v=median(df_BMI$BMI), lty=2, lwd=3, col="blue")
text(mean(df_BMI$BMI), 60, labels = paste("mean = ", as.character(round(mean(df_BMI$BMI), digits=4))),
     pos = 4, col="red")
text(mean(df_BMI$BMI), 80, labels = paste("median = ", as.character(round(median(df_BMI$BMI), digits=4))),
     pos = 4, col="blue")

## Plot WH size
hist(df_WH$WH, main="Частота Waist/Hips", xlab="WH", col="cyan")
abline(v=mean(df_WH$WH), lty=2, lwd=3, col="red")
abline(v=median(df_WH$WH), lty=2, lwd=3, col="blue")
text(mean(df_WH$WH), 200, labels = paste("mean = ", as.character(round(mean(df_WH$WH), digits=4))),
     pos = 4, col="red")
text(mean(df_WH$WH), 220, labels = paste("median = ", as.character(round(median(df_WH$WH), digits=4))),
     pos = 4, col="blue")

## Calculate most beautiful model by mean of measurements
prec <- 1
bust <- round(mean(df_Bust$Bust), digits = prec)
cup <- round(mean(df_Cup$Cup), digits = prec)
waist <- round(mean(df_Waist$Waist), digits = prec)
hips <- round(mean(df_Hips$Hips), digits = prec)
height <- round(mean(df_Height$Height), digits = prec)
weight <- round(mean(df_Weight$Weight), digits = prec)
bmi <- round(mean(df_BMI$BMI), digits = prec)
wh <- round(mean(df_WH$WH), digits = prec)

df1 <- subset(df, complete.cases(df)==TRUE)
range <- 1

df1[ 
  df1$Bust < bust+range & df1$Bust > bust-range &
  df1$Cup < cup+range & df1$Cup > cup-range &
  df1$Waist < waist+range & df1$Waist > waist-range &
  df1$Hips < hips+range & df1$Hips > hips-range &
  df1$Height < height+range & df1$Height > height-range &
  df1$Weight < weight+range & df1$Weight > weight-range &
  df1$BMI < bmi+range & df1$BMI > bmi-range &
  df1$WH < wh+range & df1$WH > wh-range 
,]
### We get Stacy Arthur here. She is all time star.
### http://en.wikipedia.org/wiki/Stacy_Arthur
### Month Year Bust Cup Waist Hips Height Weight      BMI        WH       Date
### 446 January 1991   36   4    23   35     67    115 18.00958 0.6571429 1991-01-01

## Calculate most beautiful model by median of measurements
bust <- round(median(df_Bust$Bust), digits = prec)
cup <- round(median(df_Cup$Cup), digits = prec)
waist <- round(median(df_Waist$Waist), digits = prec)
hips <- round(median(df_Hips$Hips), digits = prec)
height <- round(median(df_Height$Height), digits = prec)
weight <- round(median(df_Weight$Weight), digits = prec)
bmi <- round(median(df_BMI$BMI), digits = prec)
wh <- round(median(df_WH$WH), digits = prec)

range <- 1.001

df1[ 
    df1$Bust < bust+range & df1$Bust > bust-range &
    df1$Cup < cup+range & df1$Cup > cup-range &
    df1$Waist < waist+range & df1$Waist > waist-range &
    df1$Hips < hips+range & df1$Hips > hips-range &
    df1$Height < height+range & df1$Height > height-range &
    df1$Weight < weight+range & df1$Weight > weight-range &
    df1$BMI < bmi+range & df1$BMI > bmi-range &
    df1$WH < wh+range & df1$WH > wh-range 
  ,]
### We get many girls here
### 1. http://en.wikipedia.org/wiki/Stacy_Arthur
### 2. http://en.wikipedia.org/wiki/Valerie_Mason-John
### 3. http://en.wikipedia.org/wiki/Kelly_Carrington
### 
### Month Year Bust Cup Waist Hips Height Weight      BMI        WH       Date
### 446   January 1991   36   4    23   35     67    115 18.00958 0.6571429 1991-01-01
### 460     March 1992   34   3    23   34     66    116 18.72084 0.6764706 1992-03-01
### 520     March 1997   36   3    24   34     65    114 18.96852 0.7058824 1997-03-01
### 523      June 1997   34   4    24   34     66    114 18.39807 0.7058824 1997-06-01
### 613  December 2004   34   3    23   35     66    115 18.55946 0.6571429 2004-12-01
### 628     March 2006   36   3    24   36     66    114 18.39807 0.6666667 2006-03-01
### 644      July 2007   34   4    24   34     67    115 18.00958 0.7058824 2007-07-01
### 658 September 2008   34   4    25   36     67    115 18.00958 0.6944444 2008-09-01
### 659   October 2008   34   3    24   34     65    115 19.13491 0.7058824 2008-10-01

## Show trends
par(mfrow=c(3,3))
plot(df_Bust$Date, df_Bust$Bust)
plot(df_Cup$Date, df_Cup$Cup)
plot(df_Height$Date, df_Height$Height)
plot(df_Weight$Date, df_Weight$Weight)
plot(df_Waist$Date, df_Waist$Waist)
plot(df_Hips$Date, df_Hips$Hips)
plot(df_BMI$Date, df_BMI$BMI)
plot(df_WH$Date, df_WH$WH)

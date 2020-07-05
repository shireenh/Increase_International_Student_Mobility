#read original data frame:
originalData = read.csv("sponsor_data_countrywise_backup.csv")
colnames(originalData) = c("Country",seq(2003,2019))

#build country predictor column:
countryCol = rep(originalData$Country,17)
factorCountryCol = factor(countryCol)

#build sponsored student response column:
sponsoredStudentResponse = c()
for (i in seq(2,18)){
  sponsoredStudentResponse = append(sponsoredStudentResponse, originalData[,i])
}

#build year column:
yearCol = rep(colnames(originalData)[-1],each=55)

#build oil price column (Dec of this year, Jun of this year, Dec of the last year):
dailyOilPrice = data.frame(read_excel("basket-oilprice.xlsx"))
colnames(dailyOilPrice) = c("Date", "Price")

monthlyOilPrice = c()
for (i in seq(2003,2019)){
  for (j in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    monthlyOilPrice = rbind(monthlyOilPrice, c(paste(i,j,sep = "-"),mean(dailyOilPrice[grepl(paste(i,j,sep = "-"),dailyOilPrice$Date),]$Price)))
  }
}
monthlyOilPrice = as.data.frame(monthlyOilPrice)
colnames(monthlyOilPrice) = c("Month","Price")
monthlyOilPrice$Price = as.numeric(as.character(monthlyOilPrice$Price))
monthlyOilPrice$Month = as.character(monthlyOilPrice$Month)

semiannuallyOilPrice=c()
for (i in seq(2003,2019)){
  temp = c()
  for (j in c("01","02","03","04","05","06")){
    temp = append(temp, monthlyOilPrice[grepl(paste(i,j,sep="-"),monthlyOilPrice$Month),]$Price)
  }
  semiannuallyOilPrice = rbind(semiannuallyOilPrice, c(paste(i,"H1",sep="-"),mean(temp, na.rm = TRUE)))
  
  temp = c()
  for (j in c("07","08","09","10","11","12")){
    temp = append(temp, monthlyOilPrice[grepl(paste(i,j,sep="-"),monthlyOilPrice$Month),]$Price)
  }
  semiannuallyOilPrice = rbind(semiannuallyOilPrice, c(paste(i,"H2",sep="-"),mean(temp, na.rm = TRUE)))
}
semiannuallyOilPrice = as.data.frame(semiannuallyOilPrice)
colnames(semiannuallyOilPrice) = c("halfYear","Price")
semiannuallyOilPrice$halfYear = as.character(semiannuallyOilPrice$halfYear)
semiannuallyOilPrice$Price = as.numeric(as.character(semiannuallyOilPrice$Price))


#build sponsored student data frame:
sponsoredStudentData = cbind.data.frame(yearCol,countryCol)
head(sponsoredStudentData)
   
#build oil price predictor of second half of this year:
oilPriceThisYearH2 = rep(NaN,935)
sponsoredStudentData = cbind.data.frame(sponsoredStudentData,oilPriceThisYearH2)
for (i in seq(1,length(sponsoredStudentData$yearCol))){
  sponsoredStudentData[i,]$oilPriceThisYearH2 = semiannuallyOilPrice[semiannuallyOilPrice$halfYear == paste(sponsoredStudentData[i,]$yearCol,"H2",sep="-"),]$Price
}

#build oil price predictor of first half of this year:
oilPriceThisYearH1 = rep(NaN,935)
sponsoredStudentData = cbind.data.frame(sponsoredStudentData,oilPriceThisYearH1)
for (i in seq(1,length(sponsoredStudentData$yearCol))){
  sponsoredStudentData[i,]$oilPriceThisYearH1 = semiannuallyOilPrice[semiannuallyOilPrice$halfYear == paste(sponsoredStudentData[i,]$yearCol,"H1",sep="-"),]$Price
}

##build oil price predictor of second half of last year:
oilPriceLastYearH2 = rep(NaN,935)
sponsoredStudentData = cbind.data.frame(sponsoredStudentData,oilPriceLastYearH2)
for (i in seq(1,length(sponsoredStudentData$yearCol))){
  possibleError = tryCatch(
    {sponsoredStudentData[i,]$oilPriceLastYearH2 = semiannuallyOilPrice[semiannuallyOilPrice$halfYear == paste(as.numeric(as.character(sponsoredStudentData[i,]$yearCol))-1,"H2",sep="-"),]$Price},
    error = function(e) e)
  if(inherits(possibleError, "error")) next
}

#build oil price predictor of first half of last year:
oilPriceLastYearH1 = rep(NaN,935)
sponsoredStudentData = cbind.data.frame(sponsoredStudentData,oilPriceLastYearH1)
for (i in seq(1,length(sponsoredStudentData$yearCol))){
  possibleError = tryCatch(
    {sponsoredStudentData[i,]$oilPriceLastYearH1 = semiannuallyOilPrice[semiannuallyOilPrice$halfYear == paste(as.numeric(as.character(sponsoredStudentData[i,]$yearCol))-1,"H1",sep="-"),]$Price},
    error = function(e) e)
  if(inherits(possibleError, "error")) next
}

#add response column:
sponsoredStudentData = cbind.data.frame(sponsoredStudentData, sponsoredStudentResponse)
write.csv(sponsoredStudentData,"sponsored student data & oil price.csv")

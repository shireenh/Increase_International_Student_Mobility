mydata=`sponsored.student.data.&.oil.price`

name<-names(table(mydata$countryCol))  
n<-length(name)
n
for (i in 1:n)      #split data by country
{
  assign(name[i],mydata[which(mydata$countryCol==name[i]),])
}
#Then we can choose country
#Australia
f2= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
        family=poisson(link = log),
        data= Australia)
summary(f2)
#Malaysia
f1= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
      family=poisson(link = log),
         data= Malaysia)
summary(f1)
#plot(predict(f1,type = "response"),residuals(f1,type = "deviance"))

#Saudi Arabia
f3= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
          family=poisson(link = log),
        data= `Saudi Arabia`)
summary(f3)

#Mexico
f4= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
        family=poisson(link = log),
        data= Mexico)
summary(f4)
#Chile
f5= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
        family=poisson(link = log),
        data= Chile)
summary(f5)

#Indonesia
f6= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
        family=poisson(link = log),
        data= Indonesia)
summary(f6)
#singapore
f7= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
        family=poisson(link = log),
        data= Singapore)
summary(f7)
#Japan
f8= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
        family=poisson(link = log),
        data= Japan)
summary(f8)
# Iran
f9= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
        family=poisson(link = log),
        data= Iran)
summary(f9)
#Azerbaijan
f10= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
        # family=poisson(link = log),
        data= Azerbaijan)
summary(f10)
#Viet Nam
f11= glm(sponsoredStudentResponse~ yearCol + oilPriceThisYearH2+
          oilPriceThisYearH1 + oilPriceLastYearH2+ oilPriceLastYearH1,
        # family=poisson(link = log),
        data= `Viet Nam`)
summary(f11)




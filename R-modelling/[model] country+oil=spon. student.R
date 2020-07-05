sponsData = read.csv("sponsored student data & oil price.csv")
M1 = glm(sponsoredStudentResponse ~ countryCol + oilPriceThisYearH2 + oilPriceThisYearH1 + oilPriceLastYearH2 + oilPriceLastYearH1, data = sponsData, family = poisson)
summary(M1)

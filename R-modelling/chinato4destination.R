options(scipen = 200)
#----------china->aus----------
china_aus_data = read.csv("~/Desktop/Data Science Project/code/R/R_data/china-aus.csv")
for (i in 4:10){
  china_aus_data[i] = scale(china_aus_data[i])
  }
M_china_aus = glm(total_students ~ graduates + GNI + ave_wage + cost_of_living + source_top200 + destination_top200, data = china_aus_data, family = poisson)
summary(M_china_aus)

plot(china_aus_data$year[1:9], china_aus_data$total_students[1:9], type='o', main='Student count from China to Australia',, ylab="Student count", xlab="Year")
lines(china_aus_data$year[1:9], M_china_aus$fitted.values,col='red',pch=20,type='o')
legend("topleft", inset=.02, legend=c("Response Data", "Predicted Data"),col=c("black", "red"), pch = c(1,20))


#----------china->can----------
china_can_data = read.csv("~/Desktop/Data Science Project/code/R/R_data/china-can.csv")
#note: we do not have china-> canada student count for 2017, 2018,...
for (i in 4:10){
  china_can_data[i] = scale(china_can_data[i])
}
M_china_can = glm(total_students ~ graduates + GNI + ave_wage + cost_of_living + source_top200 + destination_top200, data = china_can_data, family = poisson)
summary(M_china_can)

plot(china_can_data$year[1:8], china_can_data$total_students[1:8], type='o', main='Student count from China to Canada', ylab="Student count", xlab="Year")
lines(china_can_data$year[1:8], M_china_can$fitted.values,col='red',pch=20,type='b')
legend("topleft", inset=.02, legend=c("Response Data", "Predicted Data"),col=c("black", "red"), pch = c(1,20))

#----------china->UK----------
china_uk_data = read.csv("~/Desktop/Data Science Project/code/R/R_data/china-uk.csv")
for (i in 4:10){
  china_uk_data[i] = scale(china_uk_data[i])
}
M_china_uk = glm(total_students ~ graduates + GNI + ave_wage + cost_of_living + source_top200 + destination_top200, data = china_uk_data, family = poisson)
summary(M_china_uk)

plot(china_uk_data$year[1:9], china_uk_data$total_students[1:9], type='o', main='Student count from China to United Kingdom', ylab="Student count", xlab="Year")
lines(china_uk_data$year[1:9], M_china_uk$fitted.values,col='red',pch=20,type='b')
legend("topleft", inset=.02, legend=c("Response Data", "Predicted Data"),col=c("black", "red"), pch = c(1,20))

#----------china->US----------
china_us_data = read.csv("~/Desktop/Data Science Project/code/R/R_data/china-us.csv")
for (i in 4:10){
  china_us_data[i] = scale(china_us_data[i])
}
M_china_us = glm(total_students ~ graduates + GNI + ave_wage + cost_of_living + source_top200 + destination_top200,
                 data = china_us_data, family = poisson)
summary(M_china_us)

plot(china_us_data$year[1:9], china_us_data$total_students[1:9], type='o', main='Student count from China to United States', ylab="Student count", xlab="Year")
lines(china_us_data$year[1:9], M_china_us$fitted.values,col='red',pch=20,type='b')
legend("topleft", inset=.02, legend=c("Response Data", "Predicted Data"),col=c("black", "red"), pch = c(1,20))

#----------coefficients----------
plot(M_china_aus$coefficients[-1],pch=8,col='yellowgreen', axes = FALSE, ylab="Coefficients", xlab='Predictors', ylim=c(-0.5,0.5))
axis(side=1, at=1:6,tck=0.01,labels=c("source\ngraduates","source\nGNI","destination\nave_wage","destination\ncost_of_living","source\ntop200","destination\ntop200"))
axis(side=2)
box()
points(M_china_can$coefficients[-1], col='red',pch=8)
points(M_china_uk$coefficients[-1], col='blue',pch=8)
points(M_china_us$coefficients[-1], col='orange',pch=8)
legend("bottomleft", inset=.02, legend=c("Australia", "Canada", "United Kingdom", "United States"),col=c("yellowgreen", "red", "blue", "orange"), pch = c(8,8,8,8))
abline(h=0, lty=2)

options(scipen = 200)
library(effects)
library(ggplot2)
library(scales)
library(stringi)
#---------------------------------------------------------------------------------------------------------
# GLM for student go to AUS by year and source contries:
dataset = read.csv("R_data/UOM_count_by_year_and_source.csv")

#select source country:
source = c('China', 'India', "Indonesia", "Malaysia")
i = 1
dataset = dataset[dataset[,"source_country"] == source[i],]
head(dataset)

# standardlize dataset
for (j in c(4,5,6,7,8,9,10,11,12)){
  dataset[,j] <- (dataset[,j]-mean(dataset[,j]))/sd(dataset[,j])
}
na.omit(dataset) 

# create design matrix X and response y
if (i==1) {
  X <- model.matrix( ~ ., dataset[,c(-1,-3,-5)]) #remove 1:source_country, 3:total_student_count, 5:src_qoe_top20
} else if (i==2){
  X <- model.matrix( ~ ., dataset[,c(-1,-3,-5,-6,-7)]) #remove 1:source_country, 3:total_student_count, 5:src_qoe_top20
} else {
  X <- model.matrix( ~ ., dataset[,c(-1,-3,-5,-6,-7,-8)]) #remove 1:source_country, 3:total_student_count, 5:src_qoe_top20
}
y <- dataset$UOM_count


# use cross-validation to choose a lambda value in lasso
lasso_cv <- cv.glmnet(X, y, family = 'poisson', nlambda = 50, alpha = 0) #alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
lasso_cv$lambda.min #the optimal lambda value choosed by CV
coef(lasso_cv, s=lasso_cv$lambda.min) 

#ggplot
coef<-as.data.frame(as.matrix(coef(lasso_cv, s=lasso_cv$lambda.min)))
coef$var<-row.names(coef)
colnames(coef)[1]<-"coef"
coef<-coef[c(-1,-2),] #remove intercept
coef$neg = coef$coef<0
coef <- na.omit(coef)
coef



ggplot(data = coef,
       aes(x = reorder(var,-coef),
           y = coef,
           fill = neg))+
  geom_bar(stat = 'identity',position = 'identity')+
  theme(axis.text.x = element_text(angle = 50,hjust = 1,vjust = 1), 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 1),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(size = 0.3))+
  labs(fill = "Negative",
       x = "Predictors",
       y="Effects",
       subtitle="alpha=0 (Ridge)")+
  scale_y_continuous(limits = c(-0.1, 0.22), breaks=c(0,seq(-0.1,0.22, by=0.1)))+
  ggtitle(paste("Effects on the number of international student go to UOM (", source[i], ")"))

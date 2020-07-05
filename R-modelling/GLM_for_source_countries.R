options(scipen = 200)
#---------------------------------------------------------------------------------------------------------
# GLM for total student count by year and source contries:
dataset = read.csv("R_data/total_count_by_year_and_source.csv")
dataset$source_country <- factor(dataset$source_country)
glm_model = glm(total_student_count 
                ~ source_country + year + src_population 
                + src_qoe_top20 + src_qoe_top100 + src_qoe_top200 + src_qoe_top500 
                + src_gni + src_gdp + src_rate_of_one_usd + src_tertiary_enrolment
                + year:source_country + src_population:source_country 
                + src_qoe_top20:source_country + src_qoe_top100:source_country + src_qoe_top200:source_country + src_qoe_top500:source_country 
                + src_gni:source_country + src_gdp:source_country + src_rate_of_one_usd:source_country + src_tertiary_enrolment:source_country
                , data = dataset, family = poisson)
summary(glm_model)
# plot
source = c('China', 'India', "Indonesia", "Malaysia")
i = 1
plot(dataset[dataset$source_country== source[i],]$year, dataset[dataset$source_country==source[i],]$total_student_count,
     xlab = 'Year', ylab = 'Student Count', main = paste('Number of Student come from', source[i]))
lines(dataset[dataset$source_country==source[i],]$year, glm_model$fitted.values[(1+(i-1)*19):(19+(i-1)*19)],type='o', col='red')

#---------------------------------------------------------------------------------------------------------
# GLM for student go to AUS by year and source contries:
dataset_aus = read.csv("R_data/AUS_count_by_year_and_source.csv")
aus_model = glm(total_student_count 
                ~ source_country + year + src_population 
                + src_qoe_top20 + src_qoe_top100 + src_qoe_top200 + src_qoe_top500 
                + src_gni + src_gdp + src_rate_of_one_usd + src_tertiary_enrolment
                + year:source_country + src_population:source_country 
                + src_qoe_top20:source_country + src_qoe_top100:source_country + src_qoe_top200:source_country + src_qoe_top500:source_country 
                + src_gni:source_country + src_gdp:source_country + src_rate_of_one_usd:source_country + src_tertiary_enrolment:source_country
                , data = dataset_aus, family = poisson)
summary(aus_model)
# plot
source = c('China', 'India', "Indonesia", "Malaysia")
i = 4
plot(dataset_aus[dataset_aus$source_country== source[i],]$year, dataset_aus[dataset_aus$source_country==source[i],]$total_student_count,
     xlab = 'Year', ylab = 'Australia Student Count', main = paste('Number of Student from', source[i], 'to', 'Australia'))
lines(dataset_aus[dataset_aus$source_country==source[i],]$year, aus_model$fitted.values[(1+(i-1)*19):(19+(i-1)*19)],type='o', col='red')

#---------------------------------------------------------------------------------------------------------
# GLM for student go to GO8 by year and source contries:
dataset_go8 = read.csv("R_data/GO8_count_by_year_and_source.csv")
go8_model = glm(GO8_count 
                ~ source_country + year + src_population 
                + src_qoe_top20 + src_qoe_top100 + src_qoe_top200 + src_qoe_top500 
                + src_gni + src_gdp + src_rate_of_one_usd + src_tertiary_enrolment
                + year:source_country + src_population:source_country 
                + src_qoe_top20:source_country + src_qoe_top100:source_country + src_qoe_top200:source_country + src_qoe_top500:source_country 
                + src_gni:source_country + src_gdp:source_country + src_rate_of_one_usd:source_country + src_tertiary_enrolment:source_country
                , data = dataset_go8, family = poisson)
summary(go8_model)
# plot
source = c('China', 'India', "Indonesia", "Malaysia")
i = 4
plot(dataset_go8[dataset_go8$source_country== source[i],]$year, dataset_go8[dataset_go8$source_country==source[i],]$GO8_count,
     xlab = 'Year', ylab = 'GO8 Student Count', main = paste('Number of Student from', source[i], 'to', 'GO8 Uni'))
lines(dataset_go8[dataset_go8$source_country==source[i],]$year, go8_model$fitted.values[(1+(i-1)*11):(11+(i-1)*11)],type='o', col='red')

#---------------------------------------------------------------------------------------------------------
# GLM for student go to UOM by year and source contries:
dataset_uom = read.csv("R_data/UOM_count_by_year_and_source.csv")
uom_model = glm(UOM_count 
                ~ source_country + year + src_population 
                + src_qoe_top20 + src_qoe_top100 + src_qoe_top200 + src_qoe_top500 
                + src_gni + src_gdp + src_rate_of_one_usd + src_tertiary_enrolment
                + year:source_country + src_population:source_country 
                + src_qoe_top20:source_country + src_qoe_top100:source_country + src_qoe_top200:source_country + src_qoe_top500:source_country 
                + src_gni:source_country + src_gdp:source_country + src_rate_of_one_usd:source_country + src_tertiary_enrolment:source_country
                , data = dataset_uom, family = poisson)
summary(uom_model)
# plot
source = c('China', 'India', "Indonesia", "Malaysia")
i = 3
plot(dataset_uom[dataset_uom$source_country== source[i],]$year, dataset_uom[dataset_uom$source_country==source[i],]$UOM_count,
     xlab = 'Year', ylab = 'UOM Student Count', main = paste('Number of Student from', source[i], 'to', 'UOM'))
lines(dataset_uom[dataset_uom$source_country==source[i],]$year, uom_model$fitted.values[(1+(i-1)*11):(11+(i-1)*11)],type='o', col='red')

#---------------------------------------------------------------------------------------------------------

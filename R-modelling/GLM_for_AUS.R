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


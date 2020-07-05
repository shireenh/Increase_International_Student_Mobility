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

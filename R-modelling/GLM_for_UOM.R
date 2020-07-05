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

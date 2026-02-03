#install.packages('dplyr')
#install.packages('readxl')
#install.packages('tidyr')
library(tidyr)
library(readxl)
library(dplyr)

setwd('D:/ClinicalPOC')
CSV_file = './DS/Countries Population.csv'
file2 = './DS/Countries Region Mapping.xlsx'

dfRM = read_excel(file2)
CSV2 = './DS/Countries Indicators.csv'
dfCI = read.csv(CSV2)

#Create data frame "dfCountries", load countries population file in it.
dfCountries = read.csv(CSV_file)


#Sort countries according to their population in ascending and descending order
countries_ascending = arrange(dfCountries,Total.Population.2017)
countries_descending = arrange(dfCountries,desc(Total.Population.2017))



#A vector of countries with population more than 10000000
Populated_countries = filter(dfCountries,Total.Population.2017>10000000)[,1]



#Create a dataframe "dfBigAndSmall" that has countries with population greater than 10M and less than 2M.
dfBigAndSmall = filter(dfCountries,Total.Population.2017>10000000 | Total.Population.2017<2000000)
 


#Create levels of income group from dataset "Countries region mapping" levels: Low, Lower mid, Upper mid, High
factor(dfRM$IncomeGroup)


Income_Level = pivot_wider(dfRM,names_from=IncomeGroup,values_from=Country.Code,values_fn = list)
rename(Income_Level,'Low' = 'Low income','Lower mid' = 'Lower middle income', 'Upper mid' = 'Upper middle income', 'High' = 'High income')


#Merge the 3 datasets attached into 1 dataframe : "dfCountryMaster"
DS1_2 = full_join(dfCountries, dfRM, by = join_by("Country.Code"))
dfCountryMaster = full_join(DS1_2, dfCI, by = join_by("Country.Code"))

#Summarize dfCountryMaster countries by region.
dfCountryMaster %>% group_by(Region) %>% summarise(Number.of.Country = n())
#36+58+42+21+3+8+48+44 

##8
#Summarize dfCountryMaster countries by region and income group.
Cntry_by_reg_inc = dfCountryMaster %>% group_by(Region,IncomeGroup) %>% summarise(Number.of.Country = n())

##9
#Summarize dfCountryMaster countries by region. Result to have the following columns in it.
#Number of countries.
#Total population in millions.
#Average of GDP per capita
#Countries with low income.
#Median GDP per capita
#minimum and maximum mortality rate under 5.

dfCountryMaster$GDP.per.capita.2017 = as.numeric(gsub(",", "", dfCountryMaster$GDP.per.capita.2017))


dfsummarise = dfCountryMaster %>% group_by(Region) %>% summarise(
  'Number of Countries' = n(),
  'Total population' = sum(Total.Population.2017,na.rm = TRUE)/1000000,
  'Average of GDP per capita'=mean(GDP.per.capita.2017,na.rm = TRUE),
  'Countries with low income.'= sum(IncomeGroup == 'Low income'),
  'Median GDP per capita'=median(GDP.per.capita.2017,na.rm = TRUE),
  'Min mortality rate under 5'=min(Under.5.Mortality.Rate.2017,na.rm = TRUE),
  'Max mortality rate under 5'=max(Under.5.Mortality.Rate.2017,na.rm = TRUE)) 

# 10. Write the above result in csv.
write.csv(dfsummarise,'./DS/Summarise_output.csv') 

##11.
#Histogram of gdp per capita

library(ggplot2)

ggplot(dfCountryMaster,aes(x=GDP.per.capita.2017))+geom_histogram(binwidth = 5000,na.rm = TRUE,aes(fill=..count..))+labs(title = 'Histogram of gdp per capita')

##12
#Plot of income group by region.
ggplot(dfCountryMaster,aes(x=IncomeGroup))+geom_bar(aes(fill=Region))+ylab('Number of countries')+labs(title = 'Plot of income group by region')


##13
#Plot of mortality rate under 5 by region
ggplot(dfCountryMaster,aes(x=Region,y=Under.5.Mortality.Rate.2017))+
  geom_boxplot()+labs(title = 'Plot of mortality rate under 5 by region')

##14
#Scatter plot of mortality rate under 5 against GDP per capita.
ggplot(dfCountryMaster,aes(x=GDP.per.capita.2017,y=Under.5.Mortality.Rate.2017))+
  geom_point(alpha=0.5,na.rm = TRUE)+
  labs(title = 'Scatter plot of mortality rate under 5 against GDP per capita')

##15
#Plot of mortality rate under 5 against GDP and region.
ggplot(dfCountryMaster,aes(x=GDP.per.capita.2017,y=Under.5.Mortality.Rate.2017))+
  geom_point(alpha=0.5,na.rm = TRUE,size=2,aes(colour = Region))+
  labs(title = 'Plot of mortality rate under 5 against GDP and region')



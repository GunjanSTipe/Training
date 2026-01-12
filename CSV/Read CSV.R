#getwd()

csv_location = "D:/ClinicalPOC/CSV"

setwd(csv_location)

csv_name = './heart_failure_clinical_records.csv'

csv_data = read.csv(csv_name)
 
str(csv_data)

csv_data[,c(2,4,6,10,11,13)] = sapply(csv_data[,c(2,4,6,10,11,13)],as.logical)

summary(csv_data)

# COVID-19 analysis
# Trying to predict death

# Kind of model--> Logistic
# Variables we can use--> Age, Gender, Country
# Also, DaysUntilHosp=How soon they went to hospital after symptoms

data=COVID19

Age=COVID19$age
Gender=COVID19$gender
Country=COVID19$country

# Convert characters to dates
DaysUntilHosp=as.numeric(as.Date(COVID19$hosp_visit_date, format = "%m/%d/%y")-as.Date(COVID19$symptom_onset, format = "%m/%d/%y"))




# CASE STUDY: DR. SEMMELWEIS AND THE DISCOVERY OF HANDWASHING

# In this case study we're going to analyze the data that made Semmelweis discover the importance of handwashing. 

# Load in the tidyverse package
library(tidyverse)
# Have a look at the data that made Semmelweis realize that something was wrong with the procedures at Vienna General Hospital.
yearly <- read_csv("yearly_deaths_by_clinic.csv")
# Print out yearly
yearly

# Adding a new column to yearly with proportion of deaths per no. births:
yearly <- yearly %>% mutate(proportion_deaths = deaths/births)
# Check out yearly again:
yearly
# Plot yearly proportion of deaths at the two clinics:
ggplot(yearly, aes(year, proportion_deaths, color = clinic)) + geom_line()
# Why is the proportion of deaths constantly so much higher in Clinic 1? The only difference between the clinics was that many medical students served at Clinic 1, while mostly midwife students served at Clinic 2. While the midwives only tended to the women giving birth, the medical students also spent time in the autopsy rooms examining corpses. 

# Semmelweis started to suspect that something on the corpses, spread from the hands of the medical students, caused childbed fever. So in a desperate attempt to stop the high mortality rates, he decreed: Wash your hands! This was an unorthodox and controversial request, nobody in Vienna knew about bacteria at this point in time. # Load the monthly data from Clinic 1 to see if the handwashing had any effect:
monthly <- read_csv("monthly_deaths.csv")
# Adding a new column with proportion of deaths per no. births:
monthly <- monthly %>% mutate(proportion_deaths = deaths/births)
# Print out the first rows in monthly:
head(monthly)
# Plot monthly proportion of deaths:
ggplot(monthly, aes(date, proportion_deaths)) + geom_line() + labs(x = "Date", y = "Proportion of death")

# From this date handwashing was made mandatory:
handwashing_start = as.Date('1847-06-01')
# Add a TRUE/FALSE column to monthly called handwashing_started:
monthly <- monthly %>% mutate(handwashing_started = date >= handwashing_start)
# Plot monthly proportion of deaths before and after handwashing:
ggplot(monthly, aes(date, proportion_deaths, color = handwashing_started)) + geom_line()+ 
labs(x = "Date", y = "Proportion of death")

# Calculating the mean proportion of deaths before and after handwashing:
monthly_summary <- monthly %>% 
    group_by(handwashing_started) %>% 
    summarise(mean(proportion_deaths))
# Printing out the summary.
monthly_summary

# Calculating a 95% Confidence interval using t.test: 
test_result <- t.test( proportion_deaths ~ handwashing_started, data = monthly)
test_result
# That the doctors didn't wash their hands increased the proportion of deaths by between 6.7 and 10 percentage points, according to a 95% confidence interval. All in all, it would seem that Semmelweis had solid evidence that handwashing was a simple but highly effective procedure that could save many lives.
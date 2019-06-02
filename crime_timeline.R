library(magrittr)
library(Hmisc)
library(tidyverse)
library(Hmisc)
library(corrr)
library(ggfortify)
library(MASS)
library(dplyr)
library(ggplot2)
library(purrr)

data_path <- "C:/Users/Public/Zarmina-Data science/data statistics/AT-2/Data/crime"
output_path <- "C:/Users/Public/Zarmina-Data science/data statistics/AT-2/Data/crime"


# Loading crime Data
data_df <- readr::read_csv(paste(data_path, "crime_1995_2018_by_month.csv", sep = "/"))
crime_df <- data_df
head(crime_df)

##################################################
crime_df<- crime_df %>% 
  gather(key = "date", value = "crime_count", "Jan 1995":"Dec 2018")

head(crime_df)
glimpse(crime_df)


# for LGA
ggplot(crime_df, aes(x = LGA, y = crime_count)) +
  geom_point() +
  labs(x = "LGA", y = "Crime count", 
       title = "Relationship between crime count and LGA") +
  geom_smooth(method = "lm", se = FALSE)

# for date
ggplot(crime_df, aes(x = date, y = crime_count)) +
  geom_point() +
  labs(x = "Date", y = "crime count", 
       title = "Relationship between crime count and Date") +
  geom_smooth(method = "lm", se = FALSE)

# for date
ggplot(crime_df, aes(x = date, y = crime_count)) +
  geom_boxplot() +
  labs(x = "Date", y = "crime count", 
       title = "Relationship between crime count and Date") +
  geom_smooth(method = "lm", se = FALSE)


##################################################
crime_df<- crime_df %>%
  separate(date, c("month", "year"))

head(crime_df)
glimpse(crime_df)

crime_df %<>%
  rename(lga = LGA)

head(crime_df)
glimpse(crime_df)


crime_df<- crime_df %>%
  mutate(date = paste(year, month, "01", sep = "-")) %>%
  mutate(date = as.Date(date, format = "%Y-%b-%d")) %>%
  arrange(date) %>%
  rename(crime_type = `Offence category`)

head(crime_df)
glimpse(crime_df)

# Aggregating the crime count to an LGA level
lga_crime_df <- crime_df %>%
  group_by(date,lga, month ) %>%
  summarise(total_crime_count = sum(crime_count)) %>%
  ungroup %>%
  mutate(year = as.integer(format(date, "%Y")))

head(lga_crime_df)
glimpse(lga_crime_df)
##################################################################
# Aggregating the crime count to yearly
  lga_crime_year_df <- lga_crime_df %>%
  group_by(lga, year) %>%
  summarise(total_crime_count = sum(total_crime_count)) %>%
  ungroup

head(  lga_crime_year_df )
glimpse(  lga_crime_year_df )

# for date
ggplot(  lga_crime_year_df , aes(x = year, y = total_crime_count)) +
  geom_point()+ stat_smooth(method = lm) +
  labs(x = "Year", y = "total crime count", 
       title = "Relationship between crime count and Year")


####################################################################
# Aggregating crime count for lga(Albury) and crime_type(Assault & Homicide)

Crime_lga<- crime_df %>% group_by(date, lga, crime_type, month, year) %>%
  summarise(monthly_crime= sum(crime_count))
Crime_lga
Crime_Albury<- Crime_lga %>% filter(lga== "Albury", crime_type== "Assault")
Crime_Albury_h<- Crime_lga %>% filter(lga== "Albury", crime_type== "Homicide")


# for year(albury- assault)
ggplot(Crime_Albury, aes(x = year, y = monthly_crime)) + geom_boxplot()+ stat_smooth(method = lm) +
  labs(x = "Year", y = "total crime count", 
       title = "Relationship between crime count and year for Albury")
# for month (albury- assault)
ggplot(Crime_Albury, aes(x = month, y = monthly_crime)) + geom_boxplot()+ stat_smooth(method = lm) +
  labs(x = "Year", y = "total crime count", 
       title = "Relationship between monthly crime and month for Albury")



# for year(albury- homicide)
ggplot(Crime_Albury_h, aes(x = year, y = monthly_crime)) + geom_boxplot()+ stat_smooth(method = lm) +
  labs(x = "Year", y = "total crime count", 
       title = "Relationship between crime count and year for Albury")
# for month (albury- homicide)
ggplot(Crime_Albury_h, aes(x = month, y = monthly_crime)) + geom_boxplot()+ stat_smooth(method = lm) +
  labs(x = "Year", y = "total crime count", 
       title = "Relationship between monthly crime and month for Albury")




######################################################################





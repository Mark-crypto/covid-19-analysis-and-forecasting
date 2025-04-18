#Access the dataset from the link: https://github.com/owid/covid-19-data?tab=readme-ov-file
#We will be analysing COVID-19 data.I advise you do not download as it is a big dataset.
#Instead read directly from https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv

#Download the packages, if you do not have them installed like below
install.packages('plotly')

#Loading Packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(scales)
library(tseries)

#Accessing our dataset
covid_data <- read.table('https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv', sep=',', head=T)

#Exploring the dataset
head(covid_data,5)
tail(covid_data,10)
sample(covid_data)
dim(covid_data)
names(covid_data)
glimpse(covid_data)


#Data Cleaning
covid_data$date <- as.Date(covid_data$date, format = "%Y-%m-%d")
complete_covid_data <- covid_data[!is.na(covid_data$total_cases),]
head(complete_covid_data,5)
tail(complete_covid_data)
filter(complete_covid_data, complete_covid_data$country == "Kenya")
covid_data <- covid_data %>% select(-data) # Removing a column I created
complete_covid_data <- complete_covid_data %>% select(-data) # Removing a column I created

#Data visualizations
# Deaths in East Africa
complete_covid_data %>% 
  filter(country %in% c('Kenya','Uganda', 'Tanzania', 'Rwanda', 'Burundi')) %>% 
  ggplot(aes(x= reorder(country, -total_deaths) , y= total_deaths)) +
  geom_bar(stat="identity",fill="steelblue")+
  labs(title="Total COVID Deaths for countries in East Africa", x="East Africa Countries", y="Count of deaths")+
  scale_y_continuous(labels=comma)+
  theme_minimal()

  
# Covid cases in Kenya in 2020 
complete_covid_data %>% 
  filter(country == "Kenya") %>% 
  filter(date >= "2020-01-01" & date<= "2020-12-31") %>% 
  ggplot(aes(x=date, y=`new_cases`)) +
    geom_line(linewidth=0.5, color='red') +
    labs(title="Covid cases in Kenya for 2020", x="", y="New COVID cases")

# Age Distribution of Hospital Patients During Covid
complete_covid_data %>% 
  ggplot(aes(x=median_age, y = hosp_patients )) +
  geom_point(color='mediumseagreen')+
  labs(title="Age Distribution of Hospital Patients During Covid", x='Age', y='Number of patients')+
  theme_minimal()

# Number of vaccinations Across Different Continents
complete_covid_data %>% 
  filter(continent %in% c("Africa", "Asia","Europe", "North America", "South America"), !is.na(total_vaccinations_per_hundred)) %>% 
  group_by(continent) %>%
  summarise(med_vaccinations = median(total_vaccinations_per_hundred, na.rm = TRUE)) %>% 
  ggplot(aes(x= reorder(continent, med_vaccinations), y=med_vaccinations))+
  geom_bar(stat="identity", fill='orchid')+
  labs(title="Number of vaccinations Across Different Continents", x="Continents", y="Total Vaccinations per hundred" )+
  coord_flip()+
  scale_y_continuous(labels = scales::comma) +
  theme_classic()

# Reproduction rate in the year 2020
complete_covid_data %>% 
  filter(date >= "2020-01-01" & date<= "2020-12-31") %>% 
  filter(!is.na(reproduction_rate)) %>% 
  ggplot(aes(x=date, y=reproduction_rate))+
  geom_point(color="steelblue")+
  labs(title="Reproduction Rate in 2020", x='', y='Reproduction Rate')+
  theme_minimal()


# GDP per capita per continent in 2020
complete_covid_data %>% 
  filter(continent %in% c("Africa", "Asia","Europe", "North America", "South America")) %>%
  group_by(continent) %>% 
  summarise(med_gdp = median(gdp_per_capita, na.rm = TRUE)) %>%
  ggplot(aes(x= reorder(continent, -med_gdp), y=med_gdp))+
  geom_bar(stat="identity", fill='mediumseagreen')+
  labs(title="Median GDP per capita per continent in 2020",x='Continents', y='GDP Per Capita')+
  scale_y_continuous(labels = comma)+
  theme_minimal()

# Vaccinations vs New cases
complete_covid_data %>% 
  ggplot(aes(x=new_cases, y=new_vaccinations))+
  geom_point( color='steelblue')+
  labs(title = "New Covid Cases vs New Vaccines Issued", x='New Cases of Covid', y='New Vaccinations Issued')+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)+
  theme_classic()


# Forecast of COVID-19 New Cases For 2025
kenya_cases <- complete_covid_data %>%
  filter(country == "Kenya") %>%
  select(date, new_cases) %>%
  drop_na() 
cases_ts <- ts(kenya_cases$new_cases, frequency = 7) 
model <- auto.arima(cases_ts)  
forecast_data <- forecast(model, h = 365)
autoplot(forecast_data) +
  labs(title = "Forecast of COVID-19 New Cases For 2025", y = "New Cases", x= "Number of days")+
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )

# Converting forecast to a data frame then exporting to CSV
forecast_df <- data.frame(
  date = seq(
    from = max(kenya_cases$date) + 1,
    by = "day",
    length.out = 365
  ),
  predicted_cases = as.numeric(forecast_data$mean),
  lower_80 = forecast_data$lower[, 1],
  upper_80 = forecast_data$upper[, 1],
  lower_95 = forecast_data$lower[, 2],
  upper_95 = forecast_data$upper[, 2]
)

# Exporting to CSV
write.csv(forecast_df, "kenya_covid_forecast_2025.csv", row.names = FALSE)

# Optional: If you want to export the new data set you can use write.csv(variable name, file name). I did not because the data set is large.



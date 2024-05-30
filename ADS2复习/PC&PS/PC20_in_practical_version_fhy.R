setwd("E:\\A大学\\大二下\\ADS2\\W20 3.18-3.22 Correlation and linear regression")
library(ggplot2)
library(dplyr)
library(zoo)

generate_rolling_avg <- function(subcovid, one_country, one_variable, days = 7){
  range_days_in_one_country <- range(subcovid$date[which(subcovid$location == one_country)])
  # range() is to get MIN and MAX in the datalist
  # Identifying the dates present in subcovid
  dates_included <- seq(range_days_in_one_country[1], range_days_in_one_country[2],
                        by = "days")
  # generate a sequence of continuous dates
  # Calculating 7-day rolling mean
  # delete the first 6 days, because the 7-day rolling mean is not exist.
  variable_means <- sapply(dates_included[-(1:6)], function(end_of_the_week){
    # end_of_the_week <- dates_included[7]
    x_days_cases <- sapply(-6:0, function(y){
      # y <- -6
      subcovid[which(subcovid$location == one_country & subcovid$date == (end_of_the_week + y)),
               one_variable]
    })
    mean(x_days_cases)
  })
  variable_means_df <- data.frame(Dates = dates_included[-(1:6)],
                                  new_variable_avg = variable_means)
}
covid <- read.csv("owid-covid-data.csv", header = TRUE)
head(covid)
# Selecting some columns
subcovid <- covid %>%
  select(iso_code, location, date, new_cases, new_deaths, new_cases_per_million,
         total_cases_per_million, new_vaccinations, people_fully_vaccinated,
         aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty,
         cardiovasc_death_rate, diabetes_prevalence, life_expectancy,
         human_development_index)

head(subcovid)
str(subcovid)
# To date format
subcovid$date <- as.Date(subcovid$date)
str(subcovid)
# Setting one country
one_country <- "France"
# Calculating the 7-days window average for new cases of COVID-19
cases_means_df <- generate_rolling_avg(subcovid, one_country, "new_cases", 7)

data2 = subcovid$new_cases[subcovid$location == one_country]
length(data2)
cases_means_df2 = rollmean(data2, 7, fill = NA, align = "left")
length(cases_means_df2)
subcovid[1,]
head(data2,10)
3/7
head(cases_means_df$new_variable_avg,10)
head(cases_means_df2,10)

# Merging cases_means_df to subcovid
subcovid$new_cases_avg <- NA
for(i in 1:nrow(cases_means_df)){
  # i <- 1
  subcovid$new_cases_avg[which(subcovid$location == one_country &
                                 subcovid$date == cases_means_df$Dates[i])] <-
    cases_means_df$new_variable_avg[i]
}
head(subcovid$new_cases_avg)
head(cases_means_df$new_variable_avg)

# Plot
g = ggplot() +
  geom_bar(stat = "identity",
           aes(x = subcovid$date[which(subcovid$location == one_country)],
               y = subcovid$new_deaths[which(subcovid$location == one_country)],
               colour = "New deaths"))
g
g = g +
  geom_point(aes(x = subcovid$date[which(subcovid$location == one_country)],
                 y = subcovid$new_cases[which(subcovid$location == one_country)],
                 colour = "New cases"), size = 0.7)
g
g = g + geom_line(aes(x = subcovid$date[which(subcovid$location == one_country)],
                      y = subcovid$new_cases_avg[which(subcovid$location == one_country)],
                      colour = "7-day rolling average")) +
  labs(x = "Date", y = "Cases") +
  ggtitle(paste0("COVID-19 Cases and Deaths in ", one_country)) +
  theme(legend.position = c(0.2, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(fill = NULL, color = NULL))
g
# Due to new deaths value are all under 1500, so in the final plot, we cannot
# clearly see the bar for new deaths, just seeing like a blue horizontal line.

# Finding all the days from 2021, as they probably contain vaccination data
dates_from_2021 <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "days")

one_country <- "Israel"
# Re-calculating the vector with 7-day rolling average of new COVID-19 cases
cases_means_df <- generate_rolling_avg(subcovid, one_country, "new_cases", 7)

# Attaching the file with the population
trying <- try(population <- read.csv("WBpopulation.csv", header = TRUE, sep = "\t"))
if(is(trying, "try-error")){
  download.file(url = paste0("https://raw.githubusercontent.com/hugocarlos/public_scripts/",
                             "master/teaching/WBpopulation.csv"),
                destfile = "WBpopulation.csv")
  population <- read.csv("WBpopulation.csv", header = TRUE, sep = "\t")
}
# "WBpopulation.csv" is just a dataset to show populations for different countries

head(population)
# Calculating the percentage of the population fully vaccinated
Israel_population <- population$X2020[which(population$Country.Name == one_country)]
Israel_population

subcovid$share_fully_vaccinated <- subcovid$people_fully_vaccinated * 100 / Israel_population
subcovid$share_fully_vaccinated

nrow(subcovid)
length(which(subcovid$location == "Israel"))
colnames(subcovid)
head(subcovid$new_caes_avg)

# Merging cases_means_df to subcovid
# because the variable "one_country" changes, so this process needs to be done again.
subcovid$new_cases_avg <- NA
for(i in 1:nrow(cases_means_df)){
  # i <- 1
  subcovid$new_cases_avg[which(subcovid$location == one_country &
                                 subcovid$date == cases_means_df$Dates[i])] <-
    cases_means_df$new_variable_avg[i]
}

subcovid %>%
  filter(location == one_country) %>%
  filter(date >= dates_from_2021[1] & date < as.Date("2021-04-01")) %>%
  ggplot() +
  geom_point(aes(x = date, y = share_fully_vaccinated * 200,
                 colour = "Share of people fully vaccinated")) +
  geom_point(aes(x = date, y = new_cases_avg, colour = "New COVID-19 cases (7-day avg)")) +
  scale_y_continuous(name = "% of total population vaccinated",
                     sec.axis = sec_axis(~./200, name = "Number of cases",
                                         labels = function(b){
                                           paste0(b, "%")
                                         })) +
  xlab("Date") +
  theme(axis.title.y = element_text(color = "cyan4"),
        axis.title.y.right = element_text(color = "tomato"),
        legend.position = "bottom") +
  ggtitle(paste0(one_country))


covid_onecountry <- subcovid[which(subcovid$location == "Israel" &
                                     subcovid$date >= as.Date("2021-01-15") &
                                     subcovid$date < as.Date("2021-03-31")), ]
# Calculating the Correlation Coefficient
cor(covid_onecountry$new_cases_avg,
    covid_onecountry$share_fully_vaccinated,
    use = "complete.obs")

lm_Israel <- lm(new_cases_avg ~ share_fully_vaccinated, covid_onecountry)

cor.test(x = covid_onecountry$new_cases_avg,
         y = covid_onecountry$share_fully_vaccinated)
## do t-test to test whether there is correlation
## because no corraltion r^2 = 0
## test where r^2 is really not 0!
## if the data is parametric, use pearson
## if the data is not parametric, use spearman


lm_Israel$coefficients
str(lm_Israel)
lm_Israel$residuals %>% hist()
lm_Israel$residuals %>% shapiro.test()
summary(lm_Israel)
mean(lm_Israel$residuals)
## NOTE: mean should be zero.
## If the distribution is normal, the mean and median value should be equal or very close.

par(mfrow = c(1, 3))
hist(residuals(lm_Israel), breaks = 15, col = "gray",
     main = "Histogram of the residuals", xlab = "Residuals", cex = 0.6)
plot(lm_Israel, which = c(1, 2), cex = 0.6)
par(mfrow = c(1, 1))

plot(x = covid_onecountry$share_fully_vaccinated, y = covid_onecountry$new_cases_avg,
     xlab = "New COVID-19 cases (7-day avg)",
     ylab = "% of population fully vaccinated")
abline(lm_Israel, col = "red")

### Tasks

## 1
# Yes, because r = -0.9692676

summary(lm_Israel)
nrow(covid_onecountry)

colnames(subcovid)
which(colnames(covid)=="new_tests")
head(subcovid$share_fully_vaccinated[!is.na(subcovid$share_fully_vaccinated)])

nrow(subcovid)
nrow(covid)
subcovid$positive_rate = covid$positive_rate
subcovid$new_tests = covid$new_tests

covid_onecountry2 <- subcovid[which(subcovid$location == "Israel" &
                                      subcovid$date >= as.Date("2021-01-15") &
                                      subcovid$date < as.Date("2021-03-31")), ]
model = lm(new_cases_avg ~ share_fully_vaccinated + positive_rate * new_tests, covid_onecountry2)
model_1 = lm(new_cases_avg ~ share_fully_vaccinated + positive_rate + new_tests, covid_onecountry2)
summary(lm_Israel)
summary(model)
anova(model, lm_Israel)
model_1 %>% summary()
anova(model, model_1)
anova(lm_Israel, model, model_1)

newdata <- expand.grid(  
  share_fully_vaccinated = seq(min(covid_onecountry2$share_fully_vaccinated), max(covid_onecountry2$share_fully_vaccinated), length.out = 100),  
  positive_rate = seq(min(covid_onecountry2$positive_rate), max(covid_onecountry2$positive_rate), length.out = 100),  
  new_tests = seq(min(covid_onecountry2$new_tests), max(covid_onecountry2$new_tests), length.out = 100)  
)  

# 使用模型预测新数据框中的值  
newdata$new_cases_avg_pred <- predict(model, newdata = newdata)

# 假设我们固定 new_tests 在某个值（比如中位数）来绘制二维图  
fixed_new_tests <- median(covid_onecountry2$new_tests)  
q1 = quantile(covid_onecountry2$new_tests, 0.25)
q3 = quantile(covid_onecountry2$new_tests, 0.75)
q1 = as.numeric(q1)
q3 = as.numeric(q3)
q1
q3

# 筛选固定 new_tests 值的数据点  
subset_data <- covid_onecountry2[covid_onecountry2$new_tests >= q1 &
                                   covid_onecountry2$new_tests <= q3, ]  
#nrow(subset_data)

# 绘制散点图  
p <- ggplot(subset_data, aes(x = share_fully_vaccinated, y = new_cases_avg, color = positive_rate)) +  
  geom_point() +  
  geom_smooth(method = "lm", aes(weight = ..density..), se = FALSE) + # 使用 lm 方法进行平滑，但这里可能只是绘制一条简单的线性拟合线  
  labs(title = "Model Fit with Fixed new_tests", x = "Share Fully Vaccinated", y = "New Cases Average")  

# 显示图形  
print(p)

## R-squared the number of variants explained by your model.

## F - statistic
## whether the model as a whole has a significant effect on the dependent variable
## have more than one variable
## anova: are the variant is due to explanatory variable more than non-explanatory variable.
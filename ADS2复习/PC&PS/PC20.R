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

### R is so powerful! This manually written function can be substitued by
### internal function zoo:rollmean()

covid <- read.csv("owid-covid-data.csv", header = TRUE)
# head(covid)
# Selecting some columns
subcovid <- covid %>%
  select(iso_code, location, date, new_cases, new_deaths, new_cases_per_million,
         total_cases_per_million, new_vaccinations, people_fully_vaccinated,
         aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty,
         cardiovasc_death_rate, diabetes_prevalence, life_expectancy,
         human_development_index)

#head(subcovid)
#str(subcovid)
# To date format
subcovid$date <- as.Date(subcovid$date)
str(subcovid)
# Setting one country
one_country <- "France"
# Calculating the 7-days window average for new cases of COVID-19
cases_means_df <- generate_rolling_avg(subcovid, one_country, "new_cases", 7)

data2 = subcovid$new_cases[subcovid$location == one_country]
# length(data2)
cases_means_df2 = rollmean(data2, 7, fill = NA, align = "left")

# which(is.na(cases_means_df$new_variable_avg))
# which(is.na(cases_means_df2))
# cases_means_df$new_variable_avg == cases_means_df2

### I have checked that cases_means_df$new_variable_avg and cases_means_df2 are
### equivalent!!!

# length(cases_means_df2)
# subcovid[1,]
# head(data2,10)
# 3/7
# head(cases_means_df$new_variable_avg,10)
# head(cases_means_df2,10)

# Merging cases_means_df to subcovid
subcovid$new_cases_avg <- NA
for(i in 1:nrow(cases_means_df)){
  # i <- 1
  subcovid$new_cases_avg[which(subcovid$location == one_country &
                                 subcovid$date == cases_means_df$Dates[i])] <-
    cases_means_df$new_variable_avg[i]
}
# head(subcovid$new_cases_avg)
# head(cases_means_df$new_variable_avg)

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
population <- read.csv("WBpopulation.csv", header = TRUE, sep = "\t")
# "WBpopulation.csv" is just a dataset to show populations for different countries

# head(population)
# Calculating the percentage of the population fully vaccinated
Israel_population <- population$X2020[which(population$Country.Name == one_country)]
# Israel_population

subcovid$share_fully_vaccinated <- subcovid$people_fully_vaccinated * 100 / Israel_population
# subcovid$share_fully_vaccinated

# nrow(subcovid)
# length(which(subcovid$location == "Israel"))
# colnames(subcovid)
# head(subcovid$new_caes_avg)

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
# DEFAULT: use = "everything" . Here, use = "complete.obs" means if both
# x and y of the row are complete, they will be used to calculate correlation
# coefficient.

lm_Israel <- lm(new_cases_avg ~ share_fully_vaccinated, covid_onecountry)

cor.test(x = covid_onecountry$new_cases_avg,
    y = covid_onecountry$share_fully_vaccinated)
## do t-test to test whether there is correlation
## because no corraltion r = 0
## test where r is really not 0!
## if the data is parametric, use pearson
## if the data is not parametric, use spearman
## To determine whether the data is parametric, the assumption for the
## correlation test is the same as that of the linear regression model.
## So, first generate a linear regression model and test the normality of the
## residuals. If the residuals are normally distributed, we can use parametric
## correlation test, which means method = "pearson".



# lm_Israel$coefficients
# str(lm_Israel)
lm_Israel$residuals %>% hist()
lm_Israel$residuals %>% shapiro.test()
summary(lm_Israel)

## in this summary,
## it has performed t-test for intercept and slope
## like mu = slope, for each data point, use the intercept can calculate a slope
## these slopes form a vector, now we have a vector and a mu, we can do t-test
## "Multiple R-squared" is just pearson correlation coefficient^2 !!!


## mean(lm_Israel$residuals)
## NOTE: mean should be zero. AND in this case, mean is truly very close to 0.
## If the distribution is normal, the mean and median value should be equal or
## very close.
## We can use shapiro.test to test normality, and in this case p>0.05, so
## residuals are normal!!

par(mfrow = c(1, 3))
hist(residuals(lm_Israel), breaks = 15, col = "gray",
     main = "Histogram of the residuals", xlab = "Residuals", cex = 0.6)
plot(lm_Israel, which = c(1, 2), cex = 0.6)
par(mfrow = c(1, 1))

## par(mfrow = NULL) did not work well!!

plot(x = covid_onecountry$share_fully_vaccinated, y = covid_onecountry$new_cases_avg,
     xlab = "New COVID-19 cases (7-day avg)",
     ylab = "% of population fully vaccinated")
abline(lm_Israel, col = "red")

### Tasks

colnames(covid_onecountry)
range(covid_onecountry$date)
## Task1
## Do you consider this correlation good enough to infer 
## that the vaccination in Israel had an effect on the 
## number of new infections during the first three months of 2021?
cor(x = covid_onecountry$new_cases_avg,
    y = covid_onecountry$share_fully_vaccinated)
# Yes, because r = -0.9692676 means the the two variables are strongly negatively correlated.


## Task2
## What other factors would you add to the linear regression to improve the fit?
## We may consider factors like positive_rate and new_tests.


# summary(lm_Israel)
# nrow(covid_onecountry)

# colnames(subcovid)
# which(colnames(covid)=="new_tests")
# head(subcovid$share_fully_vaccinated[!is.na(subcovid$share_fully_vaccinated)])

# nrow(subcovid)
# nrow(covid)
subcovid$positive_rate = covid$positive_rate
subcovid$new_tests = covid$new_tests

covid_onecountry2 <- subcovid[which(subcovid$location == "Israel" &
                                     subcovid$date >= as.Date("2021-01-15") &
                                     subcovid$date < as.Date("2021-03-31")), ]
model = lm(new_cases_avg ~ share_fully_vaccinated + positive_rate * new_tests, covid_onecountry2)

## dimytro says including interaction may not be a good idea. because
## positive_rate and new_tests are continuous?

model_1 = lm(new_cases_avg ~ share_fully_vaccinated + positive_rate + new_tests, covid_onecountry2)
summary(lm_Israel)
summary(model)
anova(model, lm_Israel)

## compare different models, we use anova to test whether two models are
## different. AND if they are significantly different, we will choose the one
## with smaller RSS (Residual Sum of Squares).
## RSS is the amount of variation that the model FAILS to account for.

model_1 %>% summary()
anova(model, model_1)
anova(lm_Israel, model, model_1)
## In this anova test, we found the two models that consider and not consider
## the interaction is the same.
## So the model should not include the interaction.


## "Multiple R-squared" means the proportion of variants explained by your model.

## F - statistic
## whether the model as a whole has a significant effect on the dependent variable
## have more than one variable
## anova: are the variant is due to explanatory variable more than non-explanatory variable.


## Task3
## Can you find another country (or countries) 
## where the correlation between the share of the population 
## that have received one or the full number of doses 
## closely correlates with the recent number of new COVID-19 infections?

# which(sapply(colnames(covid), FUN = grepl, pattern = "vaccinated"))
# nrow(covid)
# nrow(subcovid)
# 
# which(sapply(colnames(covid), FUN = grepl, pattern = "total_cases"))
# which(sapply(colnames(covid), FUN = grepl, pattern = "date"))

# data3 = cbind(subcovid,
#               people_vaccinated = covid$people_vaccinated,
#               total_cases = covid$total_cases)
# colnames(data3)
data3 = select(covid, c("location", "date", "new_cases", "people_vaccinated",
                        "people_fully_vaccinated", "population"))
data3 = filter(data3, date >= as.Date("2022-01-18") & date <= as.Date("2023-01-18"))
# data3 = filter(data3, between(date,as.Date("2022-01-18"),as.Date("2023-01-18")))
# data3 = filter(data3, !is.na(people_vaccinated) & !is.na(people_fully_vaccinated))
# head(data3)

# data3$new_cases = rollmean(data3$new_cases, 7, fill = NA, align = "left")
# !!!NOTE: Directly apply rollmean on this data is wrong, because there are
# many countries, and the first 7 days may use data from another country.

# data3 = na.omit(data3)

loclist = na.omit(unique(data3$location))

cor_vac = {}
cor_fully_vac = {}
loclist2 = {}

for(loc in loclist){
  locdata = filter(data3, location == loc)
  locdata$new_cases = rollmean(locdata$new_cases, 7, fill = NA)
  locdata = na.omit(locdata)
  if(nrow(locdata) <= 30){
    next
  }
  total = locdata$population[1]
  loclist2 = c(loclist2, loc)
  cor_vac = c(cor_vac,
              cor(locdata$new_cases,
                  locdata$people_vaccinated * 100 / total))
  cor_fully_vac = c(cor_fully_vac,
                    cor(locdata$new_cases,
                        locdata$people_fully_vaccinated * 100 / total))
}

# plot(x = locdata$new_cases, y = locdata$people_vaccinated * 100 / total)

df3 = data.frame(location = loclist2, cor_vac = cor_vac, cor_fully_vac = cor_fully_vac)
df3


## Task 4
## Consider a restricted period of time (around Winter or around Summer),
## not all countries might be equally affected by the change of the seasons.

## summer 6.18 - 9.18
data4 = select(covid, c("location", "date", "new_cases",
                        "people_fully_vaccinated", "population"))
data4 = filter(data4, date >= as.Date("2021-06-18") & date <= as.Date("2021-09-18"))
loclist = na.omit(unique(data4$location))
loclist2 = {}
slope = {}
slope_p = {}
r_squ = {}

for(loc in loclist){
  locdata = filter(data4, location == loc)
  locdata$new_cases = rollmean(locdata$new_cases, 7, fill = NA)
  locdata = na.omit(locdata)
  if(nrow(locdata) <= 30){
    next
  }
  locdata$share_fully_vaccinated = locdata$people_fully_vaccinated / locdata$population[1]
  loclist2 = c(loclist2, loc)
  model = summary(lm(new_cases ~ share_fully_vaccinated, locdata))
  slope = c(slope, model$coefficients[2, 1])
  slope_p = c(slope_p, model$coefficients[2, 4])
  r_squ = c(r_squ, model$r.squared)
}

df4 = data.frame(
  location = loclist2,
  slope = slope,
  slope_p_value = slope_p,
  r_squared = r_squ
)

df4
# summary(model)
# str(model)
# str(summary(model)) ### It is very useful!

## winter 2021-12-18 2022-02-18
data4 = select(covid, c("location", "date", "new_cases",
                        "people_fully_vaccinated", "population"))
data4 = filter(data4, date >= as.Date("2021-12-18") & date <= as.Date("2022-02-18"))
loclist = na.omit(unique(data4$location))
loclist2 = {}
slope = {}
slope_p = {}
r_squ = {}

for(loc in loclist){
  locdata = filter(data4, location == loc)
  locdata$new_cases = rollmean(locdata$new_cases, 7, fill = NA)
  locdata = na.omit(locdata)
  if(nrow(locdata) <= 30){
    next
  }
  locdata$share_fully_vaccinated = locdata$people_fully_vaccinated * 100/ locdata$population[1]
  loclist2 = c(loclist2, loc)
  model = summary(lm(new_cases ~ share_fully_vaccinated, locdata))
  slope = c(slope, model$coefficients[2, 1])
  slope_p = c(slope_p, model$coefficients[2, 4])
  r_squ = c(r_squ, model$r.squared)
}

df4 = data.frame(
  location = loclist2,
  slope = slope,
  slope_p_value = slope_p,
  r_squared = r_squ
)

df4

## Another pipeline format using %>%
df4 = covid %>%
  select(c("location", "date", "new_cases", "people_fully_vaccinated", "population")) %>%
  filter(between(as.Date(date), as.Date("2021-12-18"), as.Date("2022-02-18"))) %>%
  group_by(location) %>% group_split() %>%
  map(~mutate(., new_cases = rollmean(new_cases, 7, fill = NA),
                  share_fully_vaccinated = people_fully_vaccinated * 100 / population)) %>%
  map(~na.omit(.)) %>%
  keep(~nrow(.) > 30) ## NOTE: "keep" function does not need "map"!

sum_table = df4 %>%
  map(~lm(new_cases ~ share_fully_vaccinated, data = .x)) %>%
  map(~summary(.)) %>%
  map(~data.frame(slope = .$coefficients[2, 1],
                  slope_p = .$coefficients[2, 4],
                  r_squ = .$r.squared)) %>%
  bind_rows() %>%
  mutate(location = df4 %>% bind_rows() %>% .$location %>% unique(), .before = slope)
  # ".before = A" means put this new column before column A
sum_table

# NOTE:
# "~" :用于定义一个匿名函数
# "." :在管道中，"."代表前一个函数的输出

#################################### YEAH! ####################################

## Task 5
## Choose one country and develop a method that uses local linear regressions to assess
## when during the COVID-19 epidemic it is observed an increase, a decrease or no change
## in the number of new cases.
## The output could be a table showing that for some periods of time
## (for example from October to mid November) there was an increase or a decrease in cases.

## NOTE: 2020-01 is not a complete date, so it cannot be converted to "Date" type.
df5 = covid %>%
  filter(location == "Japan") %>%
  select(date, new_cases, people_fully_vaccinated, population) %>%
  mutate(date = as.Date(date),
         new_cases = rollmean(new_cases, 30, fill = NA),
         share_fully_vaccinated = people_fully_vaccinated * 100/ population)

head(df5)
str(df5)

# g = ggplot(df5, aes(group = 1)) + theme_light()
g = ggplot(df5)
g = g +
  geom_line(mapping = aes(
    x = date, y = share_fully_vaccinated * 1000,
    color = "share_fully_vaccinated"),
    linewidth = 1, alpha = 0.5)
g = g +
  geom_line(mapping = aes(
    x = date, y = new_cases,
    color = "new_cases_30_days_avg"),
    linewidth = 1, alpha = 0.5)
g = g +
  scale_y_continuous(name = "Number of cases",
                     n.breaks = 10,
                     sec.axis = sec_axis(~./1000,
                                         name = "% of total population vaccinated",
                                         labels = function(b){paste(b,"%")}))
g = g +
  scale_x_date(date_breaks = '3 months',
               labels = function(b){format(b,'%Y-%b')}) # %b 输出月份英文缩写
g = g + scale_color_manual(values = c("red", "blue"))
g = g +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust= 0.8),
        axis.title.y = element_text(color = "red"),
        axis.title.y.right = element_text(color = "blue"),
        legend.position = "top")

g

df5 = df5 %>%
  mutate(time_cut = cut(
    date,
    breaks = as.Date(
      c(
        '2020-01-01',
        '2021-06-01',
        '2021-11-01',
        '2022-02-01',
        '2022-06-01',
        '2022-08-01',
        '2022-11-01',
        '2023-01-01',
        '2023-02-01'
      )
    ),
    labels = c(
      '2020/01-2021/06',
      '2021/06-2021/11',
      '2021/11-2022/02',
      '2022/02-2022/06',
      '2022/06-2022/08',
      '2022/08-2022/11',
      '2022/11-2023/01',
      '2023/01-2023/02'
    )
  ))
head(df5)
unique(df5$time_cut)

g = ggplot(df5, mapping = aes(x = date, y = new_cases))
g = g +
  geom_line(
    mapping = aes(color = "new_cases_30_days_avg"),
    linewidth = 1,
    alpha = 0.8
  )
g = g + scale_color_manual(values = "orange")
g = g +
  scale_y_continuous(name = "new_cases_30_days_avg",
                     n.breaks = 10)
g = g +
  scale_x_date(
    date_breaks = '3 months',
    labels = function(b) {
      format(b, '%Y-%m')
    }
  )
g = g + facet_wrap( ~ time_cut)
g = g + geom_smooth(formula = y ~ x, method = "lm", color = "red", linewidth = 0.1)
g = g +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust= 0.8),
        legend.position = "top")
g

head(df5)

sum_table5 = df5 %>%
  group_by(time_cut) %>% group_split() %>%
  map(~lm(new_cases ~ date, data = .)) %>%
  map(~summary(.)) %>%
  map(~data.frame(slope = .x$coefficients[2,1],
                  slope.se = .x$coefficients[2,2],
                  slope.p = .x$coefficients[2,4],
                  rsqr = .x$r.squared)) %>%
  bind_rows() %>%
  mutate(period = df4 %>% bind_rows() %>% .$time_cut %>% unique(), .before = slope)
sum_table5

install.packages("ggpmisc")

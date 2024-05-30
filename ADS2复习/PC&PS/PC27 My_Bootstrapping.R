library(readr)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
setwd("E:\\A大学\\大二下\\GoodLuck\\ADS2\\W27 5.13-5.17 Bootstrapping")

### Does enhancer activity differ with histone modification
### or transcriptional activity?

data = read.table("Reporter_assay_4-1-15.txt", head = T)

head(data)

nrow(data)

# Task 1. Plot the data and examine it.
# Can you see the four groups and understand the difference between them?

data$Category = as.factor(data$Category)

g = ggplot(data, mapping = aes(x = Category, y = ave, group = Category)) +
  geom_boxplot(mapping = aes(color = Category))
g

# Task 2. Generate the first bootstrap sample
# Lets look first at the epigenetic status of these enhancers.
# Can you calculate the median difference between
# those which are marked as active and those which are marked as repressed?
head(data)
data$Epigenetic_status = as.factor(data$Epigenetic_status)
data$Transcription_status = as.factor(data$Transcription_status)
summary(data)
ave_active = median(data$ave[data$Epigenetic_status == "Active"])
ave_repressed = median(data$ave[data$Epigenetic_status == "Repressed"])
observed_dif = ave_active - ave_repressed
observed_dif

# In this situation, we prefer to use the one-sided hypothesis. Because it is
# assumed that the enhancer with active epigenetic status is higher than that
# with repressed epigenetic status.

# Can you generate one bootstrap sample of a median difference?
# Remember, you will need to write this as
# code which can be automatically run many times later.


n1 = sum(data$Epigenetic_status == "Active")
n1
n2 = sum(data$Epigenetic_status == "Repressed")
n2

# bootstrap = function(){
#   sample1 = sample(pool, size = n1, replace = T)
#   sample2 = sample(pool, size = n2, replace = T)
#   return (median(sample1) - median(sample2))
# }
bootstrap = function(){
  sample1 = sample(pool, size = n1, replace = T)
  sample2 = sample(pool, size = n2, replace = T)
  return (median(sample1) - median(sample2))
}
bootstrap()

# Task 3. Generate a large number of bootstraps
# Now try to run many replicates of the code you wrote above to
# generate a distribution of median differences. Can you plot this distribution?

pool = data$ave
# pool1 = data$ave[data$Epigenetic_status == "Active"]
# pool2 = data$ave[data$Epigenetic_status == "Repressed"]
sample_dif = {}
for(rep in 1:10000)
  sample_dif = c(sample_dif, bootstrap())
hist(sample_dif)
abline(v = observed_dif, col = "red")

# Task 4. Make a statistical inference Where does your observed difference fall
# on this distribution? Do you think there is a significant difference between
# enhancer activity across groups with different epigenetic marks?
sum(sample_dif > observed_dif) / length(sample_dif)
sum(sample_dif > observed_dif) / length(sample_dif) < 0.05
# we reject H0
# H0: active > repressed

# Task 5. Explore the number of replicates

# Task 6. Now, see whether you can repeat this procedure by looking at
# the ‘Transcription_status’ variable.

pool = data$ave
work = function(){
  sample_dif = {}
  for(rep in 1:10000)
    sample_dif = c(sample_dif, bootstrap())
  # hist(sample_dif)
  print(sum(sample_dif > observed_dif) / length(sample_dif) < 0.05)
}

n1 = sum(data$Transcription_status == "Active")
n2 = sum(data$Transcription_status == "None")
n1
n2
work()



### Which movie genres are most popular with Chinese university students?

# Task 1. Again, plot the data and examine it first.
# Can you see any differences across genres?
# Which do you think might be statistically significant?

data2 = read.table("movie_data.txt", head = T)
head(data2)
data2

# Task 2. Generate a first bootstrap sample.
# Lets look at comedy, for example. 73 students preferred this genre out of
# a total of 267 response. From these two numbers, can you generate a vector
# to sample from in order to generate a bootstrap sample?

n = sum(data2$students)
vec = {}
for(i in 1:nrow(data2))
  vec = c(vec, rep(data2$genre[i], data2$students[i]))

# Task 3. Generate a confidence interval by bootstrapping many times.
for(i in 1:nrow(data2)){
  list = {}
  for(rep in 1:1000){
    sample = sample(vec, size = n, replace = T)
    list = c(list, sum(sample == data2$genre[i]))
  }
  data2$ci_lower[i] = quantile(list, 0.025)
  data2$ci_upper[i] = quantile(list, 0.975)
}

data2

# Task 4. Repeat this bootstrapping for the entire dataset
# Already done.
# Q: Do you need to sample from different datasets for each genre?
# A: No.

# Task 5. Explore the differences across genres, 
# are there any differences that you can detect?

g2 = ggplot(data2, mapping = aes(x = genre, y = students)) +
  geom_point(aes(color = genre)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = genre), width = 0.2)
g2

print(data2$students - data2$ci_lower)
print(data2$ci_upper - data2$students)

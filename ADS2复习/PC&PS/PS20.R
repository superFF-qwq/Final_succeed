## problemset guidance
## These two data sets feature body weight, age (weeks), and some other parameters
## (ID number, tail length) of two types of mice, WT and KO.
## Build linear regression models on these data sets separately and try to explain changes
## in the body weight of mice given parameters you have in each file.
## Then, proceed to the problem set tasks.

setwd("E:\\A大学\\大二下\\GoodLuck\\ADS2\\W20 3.18-3.22 Correlation and linear regression")
library(ggpubr)

## Step 1: Import data, explore data and do some visualization.
WT = read.csv("WT.csv")
head(WT)
ggscatter(WT, x = "Weight", y = "Tail", add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = T)  +
  labs(x = "weight in (kg)", y = "Tail in (cm)")

library(corrplot)
corrplot(cor(WT[,-c(1,2)]), order = "AOE",
         method = "ellipse", type = "upper", tl.pos = "d")
corrplot(cor(WT[,-c(1,2)]), order = "AOE", add = T,
         method = "number", type = "lower", tl.pos = "n", diag = F)
corrplot(cor(WT[,-c(1,2)]), order = "AOE", method = "color", 
         addCoef.col = "black", type = "upper", col = rev(COL2("RdBu",200)))

summary(WT)
plot(x = WT$Tail, y = WT$Weight)
plot(x = WT$Age, y = WT$Weight)
plot(x = WT$Age, y = WT$Tail)

cor(x = WT$Tail, y = WT$Weight)
cor(x = WT$Age, y = WT$Weight)
cor(x = WT$Age, y = WT$Tail)

KO = read.csv("KO.csv")
head(KO)
summary(KO)

plot(x = KO$Tail, y = KO$Weight)
plot(x = KO$Age, y = KO$Weight)
plot(x = KO$Age, y = KO$Tail)

cor(x = KO$Tail, y = KO$Weight)
cor(x = KO$Age, y = KO$Weight)
cor(x = KO$Age, y = KO$Tail)

par(mfrow = c(1, 2))
plot(x = WT$Tail, y = WT$Weight)
plot(x = KO$Tail, y = KO$Weight)
par(mfrow = c(1, 1))

cor.test(x = WT$Tail, y = WT$Weight)
cor.test(x = KO$Tail, y = KO$Weight)

par(mfrow = c(1, 2))
plot(x = WT$Age, y = WT$Tail)
plot(x = KO$Age, y = KO$Tail)
par(mfrow = c(1, 1))

## Step 2:compare how the relationship between the age and body weight of WT and KO mice.

model1 = lm(Weight ~ Age, WT)
model1$residuals %>% shapiro.test()
par(mfrow = c(1, 3))
model1$residuals %>% hist()
plot(model1, which = c(1, 2))
par(mfrow = c(1, 1))

sum1 = summary(model1)
b1 = sum1$coefficients[2, 1]
se1 = sum1$coefficients[2, 2]
r1 = sqrt(sum1$r.squared)

model2 = lm(Weight ~ Age, KO)
model2$residuals %>% shapiro.test()
model2$residuals %>% hist()

sum2 = summary(model2)
b2 = sum2$coefficients[2, 1]
se2 = sum2$coefficients[2, 2]
r2 = sqrt(sum2$r.squared)

z_1 = (b1 - b2) / sqrt(abs(se1^2 - se2^2))
z_1

zr1 = 1/2*log((1+r1)/(1-r1))
zr2 = 1/2*log((1+r2)/(1-r2))
n1 = nrow(WT)
n2 = nrow(KO)
n1
n2
sezr = sqrt(1/(n1-3) + 1/(n2-3))
z_2 = (b1 - b2) / sezr
z_2

## Step 3: multiple regression analysis, several explanatory variables at once

multi_model <- lm(Weight ~ Age + Sex + Number + Tail, data = WT)
plot(multi_model, 4)
summary(multi_model)

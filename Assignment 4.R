# Assignment 4

rm(list=ls())

# install.packages("pacman")
pacman::p_load(dplyr, tidyr, caret, ggplot2, caTools, corrplot, PerformanceAnalytics, AER, MASS, stargazer, pscl, jtools, Hmisc, ggcorrplot, rpart, rpart.plot, readxl)

# Read file
df <- read_excel('C:/Users/Scott/Downloads/OnlineRetailPromotions.xlsx')

# Visualizations
df <- filter(df, conversion > 0)

hist(df$spend)

plot(spend ~ history, data = df)

chart.Correlation(df)

table(df$visit)
table(df$conversion)
table(df$mens)
table(df$womens)

# Data cleaning
df <- subset(df, select= c(-visit, -historysegment, -conversion))

str(df)

m4 <- glm(log(spend) ~., data = df, family = gaussian)
summary(m4)
plot(m4)

m1 <- glm(spend ~., data = df, family = poisson)
summary(m1)
plot(m1)

m2 <- glm(spend ~., data = df, family = quasi)
summary(m2)
plot(m2)

m3 <- glm(spend ~., data = df, family = quasipoisson)
summary(m3)
plot(m3)

stargazer(m1, m2, m3, type="text", single.row=TRUE)

vif(m1)

durbinWatsonTest(m1)

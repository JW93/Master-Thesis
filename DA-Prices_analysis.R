###############################
# Day Ahead Price Regressions #
###############################

rm(list = ls())

library("lubridate")
library("dummies")
library("jtools")
library("broom")
library("ggstance")
library("ggplot2")
library("tidyverse")
library("sjPlot")
library("sjmisc")
library("stargazer")
library("difR")
library("data.table")
library("dummies")
library("zoo")

# set working directory
setwd("C:/Users/Johannes/Documents/Johannes/WU/Masterarbeit")

# read data from csv file
data <- read.table("data_market_coupling.csv", header=TRUE, sep=",", dec=".")

# look at dataframe data 
names(data)
data$Area.y <- NULL
View(data)

# date-formats
class(data$Timestamp)
data$Timestamp <- as.POSIXct(data$Timestamp, format = "%Y-%m-%d %H:%M:%S")

# summary statistics
summary(data)
summary(data$DA_Price_EPEX)
stargazer(data, type = "html", out = "des_stat.doc")

# remove missing values (NA) from dataframe
data_clean <- na.omit(data) # delete missing values

# daten filtern
data_high_spread
data_2018 <- filter(data, data$Timestamp>="2018-10-01 00:00:00")
data_covid <- filter(data, data$Timestamp>="2020-02-01 00:00:00" & data$Timestamp<"2020-05-04")


# new variables
data$spread <- data$DA_Price_EPEX - data$DA_Price_EXAA
data$ResLoad_AT <- data$load_AT_forecast - data$fc_Wind_on_AT_DA - data$fc_Solar_AT_DA
data$ResLoad_DE <- data$load_DE_forecast - data$fc_Wind_on_DE_DA - data$fc_Wind_off_DE_DA - data$fc_Solar_DE_DA
data$MC <- ifelse(data$Timestamp < "2018-10-01 00:00:00", 0, 1)
data$Year <- year(data$Timestamp)
data$Month <- month(data$Timestamp)
dummies <- dummy(data$Month, sep="_")
data <- cbind(data, dummies)
data$MC_01 <- ifelse(data$Month==1 & data$MC == 1, 1, 0)
data$MC_06 <- ifelse(data$Month==6 & data$MC == 1, 1, 0)
data$MC_11_2018 <- ifelse(data$Month==11 & data$Year==2018 & data$MC == 1, 1, 0)
data$MC_02 <- ifelse(data$Month==2 & data$MC == 1, 1, 0)
data$MC_03 <- ifelse(data$Month==3 & data$MC == 1, 1, 0)
data$MC_04 <- ifelse(data$Month==4 & data$MC == 1, 1, 0)
data$MC_05 <- ifelse(data$Month==5 & data$MC == 1, 1, 0)
data$MC_07 <- ifelse(data$Month==7 & data$MC == 1, 1, 0)
data$MC_08 <- ifelse(data$Month==8 & data$MC == 1, 1, 0)
data$MC_09 <- ifelse(data$Month==9 & data$MC == 1, 1, 0)
data$MC_10 <- ifelse(data$Month==10 & data$MC == 1, 1, 0)
data$MC_11 <- ifelse(data$Month==11 & data$MC == 1, 1, 0)
data$MC_12 <- ifelse(data$Month==12 & data$MC == 1, 1, 0)
data$MC_2 <- ifelse(data$Timestamp < "2018-10-01 00:00:00" & data$ResLoad_AT==data$ResLoad_DE, 0, 1)
data$BZ <- ifelse(data$ResLoad_AT!=data$ResLoad_DE, 1, 0)

# dif in dif preparation
#control group
panel_h <- data[,1:2]
names(panel_h)[names(panel_h) == "DA_Price_EXAA"] <- "DA_Price"
panel_h$auction <- c("10:15-auction")
# treatment group
tg <- data[,c(1,3)]
names(tg)[names(tg) == "DA_Price_EPEX"] <- "DA_Price"
tg$auction <- c("12:00-auction")
# panel of treatment and control gorup
panel_h <- rbind(panel_h, tg)
rm(dummies, tg)

# linear model
ols <- lm(data$spread ~ 
            data$ResLoad_AT + data$ResLoad_DE + data$gen_RoR_AT_actual +
            data$gen_WaterReservoir_AT_actual + data$Month_1 + data$Month_2 +
            data$Month_3 + data$Month_4 + data$Month_5 + data$Month_6 +
            data$Month_7 + data$Month_8 + data$Month_9 + data$Month_10 +
            data$Month_11 + data$Month_12 + data$MC_01 + data$MC_02 +
            data$MC_03 + data$MC_04 +  data$MC_05 + data$MC_06 + 
            data$MC_07 + data$MC_08 + data$MC_09 + data$MC_10 +
            data$MC_12 + data$MC_11)

# dummies for diff in diff
panel_h$treated <- ifelse(panel_h$auction == "12:00-auction", 1, 0)
panel_h$post_treatment <- ifelse(panel_h$Timestamp >= "2018-10-01 00:00:00", 1, 0)
panel_h$interaction <- panel_h$treated*panel_h$post_treatment

## diff in diff estimation market coupling effect
DiD <- lm(panel_h$DA_Price ~ panel_h$treated + panel_h$post_treatment + panel_h$interaction)
summary(DiD)

# diff in diff covid
panel_covid_h <- filter(panel_h, panel_h$Timestamp>="2020-02-16 00:00:00" & panel_h$Timestamp<"2020-05-04")
panel_covid_h$treated <- ifelse(panel_covid_h$auction == "12:00-auction", 1, 0)
panel_covid_h$post_treatment <- ifelse(panel_covid_h$Timestamp >= "2020-03-16 00:00:00", 1, 0)
panel_covid_h$interaction <- panel_covid_h$treated*panel_covid_h$post_treatment

DiD_covid <- lm(panel_covid_h$DA_Price ~ panel_covid_h$treated + panel_covid_h$post_treatment + panel_covid_h$interaction)
summary(DiD_covid)


#regression table
tab_model(ols, file = "tab.doc", show.ci = FALSE, show.ci50 = FALSE, show.se = TRUE,
          show.p = TRUE, show.stat = TRUE, digits.re = 4, digits = 4, digits.p = 4)
stargazer(DiD_covid, title = "Covid 19 Effect", se=NULL, t=NULL,
          type = "html", out = "DiD_Covid.doc", dep.var.labels = ("DA Prices"))
         
 covariate.labels = c("ResLoad_AT","ResLoad_DE","gen_RoR_AT_actual",
                               "gen_WaterReservoir_AT_actual", "Month_1", "Month_2", "Month_3",
                               "Month_4", "Month_5", "Month_6", "Month_7", "Month_8", "Month_9",
                               "Month_10", "Month_11", "Month_12", "MC_01", "MC_02","MC_03",
                               "MC_04", "MC_05", "MC_06", "MC_07", "MC_08", "MC_09", "MC_10",
                               "MC_12", "MC_11"))

#coefficent plot
plot_coefs(ols, coefs = c("ResLoad_AT"="data$ResLoad_AT","ResLoad_DE"="data$ResLoad_DE",
                          "gen_RoR_AT_actual"="data$gen_RoR_AT_actual", "gen_WaterReservoir_AT_actual"="data$gen_WaterReservoir_AT_actual", 
                          "Month_1"="data$Month_1", "Month_2"="data$Month_2", "Month_3"="data$Month_3",
                          "Month_4"="data$Month_4", "Month_5"="data$Month_5", "Month_6"="data$Month_6", "Month_7"="data$Month_7",
                          "Month_8"="data$Month_8", "Month_9"="data$Month_9", "Month_10"="data$Month_10", "Month_11"="data$Month_11",
                          "Month_12"="data$Month_12", "MC_01"="data$MC_01", "MC_02"="data$MC_02","MC_03"="data$MC_03",
                          "MC_04"="data$MC_04", "MC_05"="data$MC_05", "MC_06"="data$MC_06", "MC_07"="data$MC_07",
                          "MC_08"="data$MC_08", "MC_09"="data$MC_09", "MC_10"="data$MC_10",
                          "MC_12"="data$MC_12", "MC_11"="data$MC_11"))

#Seasonality chart
plot <- ggplot(data = data) + geom_smooth(mapping = aes(x=Timestamp, y=DA_Price_EXAA, linetype="EXAA")) + 
  geom_smooth(mapping=aes(x=Timestamp, y=DA_Price_EPEX, linetype="EPEX Spot"))
plot_1 <- plot + ylab("DA Prices")
plot_2 <- plot_1 + xlab("Time")
plot(plot_2)


summary(ols) # summary of regression results
plot(ols$residuals) # residual plot
before <- ifelse(data$spread != 0 & data$Timestamp < "2018-10-01 00:00:00", 1, 0)

#spread before/after
panel_exaa <- data[1:2]
panel_epex <- data[c(1,3)]
panel_epex_b <- filter(panel_epex, data$Timestamp < "2018-10-01 00:00:00")
panel_exaa_b <- filter(panel_exaa, data$Timestamp < "2018-10-01 00:00:00")
panel_spread_b <- panel_epex_b - panel_exaa_b
summary(panel_spread_b)
stargazer(panel_spread_b, type="html", out="des_stat_spread_b.doc")
panel_epex_a <- filter(panel_epex, data$Timestamp >= "2018-10-01 00:00:00")
panel_exaa_a <- filter(panel_exaa, data$Timestamp >= "2018-10-01 00:00:00")
panel_spread_a <- panel_epex_a - panel_exaa_a
summary(panel_spread_a)
stargazer(panel_spread_a, type="html", out="des_stat_spread_a.doc")





summary(before)
plot(data$spread, data$Year)
show(min(data$spread))
show(max(data$spread))
View(data)
View(data$spread)
library(jtools)
summ(ols)
summary(ols)
plot_summs(ols)
library(broom)
plot_summs(ols)
library(ggstance)
plot_summs(ols)
plot_coefs(ols, plot.distributions = TRUE, rescale.distributions = TRUE)
plot_summs(ols, scale=TRUE)
library(table1xls)
plot(data$spread, xlab = data$Year, type = "l")
view(ols)
plot_summs(data$spread)
plot(data$Timestamp, data$spread, type = "h", ylab = "EUR/MWh", xlab = "Zeitraum")
data$Day <- aggregate(Timestamp ~ Date)
library(ggplot2)
library(tidyverse)

library(sjPlot)
tab_model(ols, file = "tab.doc", show.ci = FALSE, show.ci50 = FALSE, show.se = TRUE,
          show.p = TRUE, show.stat = TRUE, digits.re = 4, digits = 4, digits.p = 4)
ggplot(data = data$spread) + geom_bar(mapping = aes(x=data$Year))

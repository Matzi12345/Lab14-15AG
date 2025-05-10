#install.packages('ggplot2')
#install.packages('gridExtra')
#install.packages('gtsummary')

library(gtsummary)
library(ggplot2)
library(gridExtra)

setwd("~/Downloads/Agfinallab")
getwd()

WASDE <- read.csv('WASDE.csv')

head(WASDE)
str(WASDE)

g_price <- ggplot(data = WASDE, aes(x = year, y = corn_price)) +
  geom_line(color = 'red') +
  ggtitle('Corn Prices over Time (1973-2019)')+
  labs(y ='Corn Price ($)',x='Year')

g_demand <- ggplot(data = WASDE, aes(x = year, y = total_use)) +
  geom_line(color = 'blue') +
  ggtitle('Corn Demand over Time (1973-2019)')+
  labs(y ='Use (Mil bu.)',x='Year')

g_supply <- ggplot(data = WASDE, aes(x = year, y = total_supply)) +
  geom_line(color = 'green') +
  ggtitle('Corn Supply over Time (1973-2019)')+
  labs(y ='Supply (mil bu.)',x='Year')

combined <- grid.arrange(g_price, g_demand, g_supply, nrow=3)

ggsave('Corn_price.png',plot=g_price)
ggsave('Corn_demand.png',plot=g_demand)
ggsave('Corn_supply.png',plot=g_supply)
ggsave('ComibinedCorn.png',plot=combined)

WASDE$SUR <- WASDE$end_stocks / WASDE$total_use

SURplot <- ggplot(data = WASDE, aes(x = year, y = SUR)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, color = 'red') +
  ggtitle('Corn Prices vs. Stock-to-Use Ratio (1973-2019)') +
  labs(y = 'Corn Price($)', x = 'Stock-to-Use Ratio')

ggsave('SURPlot.png',plot=SURplot)

reg1 <- lm(corn_price ~ SUR, data = WASDE)
summary(reg1)
tbl_regression(reg1,intercept=TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))


#for averages
mean_sur <- mean(WASDE$SUR)
mean_price <- mean(WASDE$corn_price)

summary(resid(reg1))

hist(resid(reg1),
     main = 'Histogram of Linear Regression Errors',
     xlab = 'Linear Model Residuals')

ggplot(data = WASDE, aes(x=SUR,y=resid(reg1))) +
  geom_point(aes(x=SUR,y=resid(reg1)),shape=1) +
  ggtitle('Linear Regression Errors vs. Stock-to-Use Ratio')+
  labs(y='Errors', x='Stock-to-Use Ratio')


#inverse SUR Model

WASDE$SUR_Inv <- 1 / WASDE$SUR
reg2 <-lm(corn_price ~SUR_Inv, data=WASDE)
tbl_regression(reg2,intercept=TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))
summary(resid(reg2))

hist(resid(reg2), main='Histogram of Non-Linear Regression Errors', xlab = 'Non-Linear Model')

ggplot(data=WASDE, aes(x=SUR, y=resid(reg2))) +
  geom_point(shape=1) +
  ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(y="Errors", x="Stock-to-Use Ratio")


WASDE$period<-ifelse(WASDE$year >=2006, "2006-2019", '1973-2005')
WASDE$P2006<- as.numeric(WASDE$year>=2006)

TwoTimes <- ggplot(data=WASDE, aes(x=SUR, y=corn_price, color=period)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
  labs(y="Corn Price ($)", x="Stock-to-Use Ratio")

ggsave('Corn_to_Stock_price.png',plot=TwoTimes)

reg3<-lm(corn_price~SUR + P2006 +SUR:P2006,data=WASDE)
summary(reg3)
tbl_regression(reg3,intercept=TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))



error <- ts(resid(reg3), start=1973, end=2019, frequency=1)
lag_error <- lag(error, -1)  
error <- cbind(error, lag_error)  
reg4 <- lm(error ~ lag_error, data=error)
summary(reg4)
tbl_regression(reg4,intercept=TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))


m

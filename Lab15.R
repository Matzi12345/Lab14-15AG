packages <- c("tseries", "forecast", "TTR", "ggplot2", "dplyr", "tidyr", "lubridate")
installed <- packages %in% installed.packages()
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(tseries)
library(forecast)
library(TTR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

setwd("~/Downloads/Agfinallab")
soybeans <- read.csv('soybean-prices-historical-chart-data.csv')
print(head(soybeans,3))

CPI <- read.csv("Copy of historical-cpi-u-202503.csv")
print(head(CPI, 3))





CPI_long <- CPI %>%
  pivot_longer(
    cols = -Year,          
    names_to = "Month",      
    values_to = "CPI"       
  )
print(head(CPI_long))




CPI_long <- CPI_long %>%
  mutate(Month = match(Month, month.abb))

CPI_long <- CPI_long %>%
  mutate(date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))

CPI_long <- CPI_long %>%
  mutate(Month = match(Month, month.abb))

CPI_long <- CPI_long %>%
  arrange(as.Date(date, format = "%m/%d/%Y"))

CPI_long <- CPI_long %>%
  select(date, CPI)

CPI_long <- CPI_long %>%
  arrange(as.Date(date, format = "%m/%d/%Y"))


head(CPI_long)

soybeans <- soybeans %>%
  mutate(date = as.Date(date)) 

soybeans <- soybeans %>%
  mutate(date = floor_date(date, "month")) %>%  
  group_by(date) %>%
  summarize(price = mean(value, na.rm = TRUE)) %>% 
  ungroup()

soybeans <- soybeans %>%
  filter(date >= as.Date("1969-01-01") & date <= as.Date("2025-03-31"))


print(head(soybeans))
print(tail(soybeans))

str(CPI_long)

CPI_long <- CPI_long %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
CPI_long <- CPI_long %>%
  filter(date >= as.Date("1969-01-01") & date <= as.Date("2025-03-31"))
print(head(CPI_long))
print(tail(CPI_long))

soybeans_merged <- merge(soybeans, CPI_long, by = "date", all = FALSE)

print(head(soybeans_merged))

soybeans_merged <- soybeans_merged %>%
  mutate(price_real = (price / (CPI / 100)))

print(head(soybeans_merged))

ggplot(soybeans_merged, aes(x = date)) +
  geom_line(aes(y = price, color = "Nominal Price"), linewidth = 1) + 
  geom_line(aes(y = price_real, color = "Real Price"), linewidth = 1) + 
  scale_color_manual(values = c("Nominal Price" = "blue", "Real Price" = "red")) + 
  labs(title = "Nominal vs Real Soybean Prices",x = "Date",y = "Price (USD)",color = "Price Type") +
  theme_minimal() + 
  theme( legend.position = "bottom", plot.title = element_text(hjust = 0.5))


price.ts <- ts(soybeans_merged$price, start=c(1969, 1), end=c(2020, 7), frequency=12)
print(price.ts)

price_components <- decompose(price.ts, type="additive")
plot(price_components)
price_components$figure

plot.ts(price.ts, 
        main = "Soybeans Prices(seasonally Adjusted) over time", 
        xlab = "Year", ylab = "Nominal Price ($)")

price_sma3 <- SMA(price.ts, n = 3)  
price_sma6 <- SMA(price.ts, n = 6)  
price_sma12 <- SMA(price.ts, n = 12)

par(mfrow = c(3, 1))

plot.ts(price_sma3, main = "Soybean Price - 3 Month SMA", xlab = "Year", ylab = "SMA Price ($)")


plot.ts(price_sma6, main = "Soybean Price - 6 Month SMA", xlab = "Year", ylab = "SMA Price ($)")


plot.ts(price_sma12, main = "Soybean Price - 12 Month SMA", xlab = "Year", ylab = "SMA Price ($)")

par(mfrow = c(1, 1)) 
print(price_components$seasonal)


price_diff.ts <- diff(price.ts, differences = 1)
plot.ts(price_diff.ts, main = "Month-overMonth Change in Soybeans Prices over time", xlab = "Year", ylab = "Price Change ($)")
log_price.ts <- log(price.ts)
log_price_diff.ts <- diff(log_price.ts, differences = 1)
plot.ts(log_price_diff.ts, main = "Month-over-Month % Change in Soybean Prices Over Time", xlab = "Year", ylab = "% Price Change")

ar_model <- ar(log_price_diff.ts, order.max = 5)
ar
print(ar_model)

checkresiduals(ar_model, lag.max = 60)
ar_coefs <- ar_model$ar
print(ar_coefs)


forecast_result <- forecast(ar_model, h = 6)
forescast_result
autoplot(forecast_result, include=36, xlab="", ylab="Soybeans price - monthly % returns")
print(forecast_result)


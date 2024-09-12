# Statistical Forecasting_Project 1_Drashti Hindocha

# Install and load packages
install.packages("tidyverse", repos = "https://cran.rstudio.com/")
install.packages("caret", repos = "https://cran.rstudio.com/")
install.packages("readxl", repos = "https://cran.rstudio.com/")
install.packages("ggplot2", repos = "https://cran.rstudio.com/")
install.packages("dplyr", repos = "https://cran.rstudio.com/")
install.packages("lubridate", repos = "https://cran.rstudio.com/")
install.packages("gridExtra", repos = "https://cran.rstudio.com/")
install.packages("tsibble", repos = "https://cran.rstudio.com/")
install.packages("forecast", repos = "https://cran.rstudio.com/")


# Load libraries
library(fpp3)
library(tidyverse)
library(forecast)
library(dplyr)
library(tsibble)
library(ggplot2)
library(readr)
library(seasonal)
library(knitr)
library(ggfortify)
library(gridExtra)
library(fpp2)
library(tibbletime)
library(cowplot)
library(zoo)
library(fable)
library(fabletools)

#Load the Avocado dataset
Avodata <- read_csv("C:/Users/admin/Documents/Conestoga college/SEM 2/Statistical Forcasting/Project 1/avocado.csv")

#Summary
summary(Avodata)

# Data Cleaning
sum(is.na(Avodata))
data <- na.omit(data)

#Since every value in the matrix is zero in this instance, your data object has no missing values (NA). This shows that none of the variables in this dataset have any empty or missing values.

# Selecting region and type of avocado to forecast
set.seed(133)

# Time series plot
ggplot(Avodata, aes(x = Date, y = AveragePrice)) +
  geom_line() +
  labs(x = "Year", y = "Avocado Price", title = "Avocado Price forecast") + theme_minimal()

# Additional plot 
ggplot() +
  geom_line(data = Avodata, aes(x = Date, y = AveragePrice), color = "blue", size = 0.6) +
  geom_vline(xintercept = c(as.Date("2017-09-10"), as.Date("2018-03-25")), linetype = "dashed", size = 1) +
  annotate("rect", xmin = as.Date("2017-09-10"), xmax = as.Date("2018-03-25"), ymin = -Inf,
           ymax = Inf, alpha = 0.1, fill = "green") +
  xlab("Year") + ylab("Avocado Price($)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "red"), legend.position = "none")


avocado_ts <- ts(Avodata$AveragePrice)

#ACF Plot
acf(avocado_ts, main = "Autocorrelation of Avocado Prices", lag.max = 52)
pacf(Avodata$AveragePrice, lag.max = 52, main = "")

# Create a time series object with weekly avocado prices
week_price <- ts(na.omit(Avodata$AveragePrice), frequency = 52)

# Decompose the time series using STL
decomp <- stl(week_price, s.window = "periodic")

# Plot the decomposition components
plot(decomp, cex = 0.6)

#Convert the "Date" Column to Date Format
Avodata$Date <- as.Date(Avodata$Date, "%Y-%m-%d")
class(Avodata$Date)
Avodata <- Avodata[order(as.Date(Avodata$Date, format="%Y-%m-%d")),]

# Create an Area Plot for Two Types of Avocado
price_trend <- Avodata %>%
  select(Date, AveragePrice, type, region) %>%
  ggplot(aes(x = Date, y = AveragePrice)) +
  geom_area(aes(color = type, fill = type), alpha = 0.3, position = position_dodge(0.8)) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green")) +
  scale_fill_manual(values = c("red", "blue"))

# Print the plot
print(price_trend)

# Create a Facet Wrap for each product
ggplot(data = Avodata, aes(x = Date, y = AveragePrice, col = type)) +
  geom_line() +
  facet_wrap(~ type) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Filter by type: Organic
organic <- Avodata %>%
  select(Date, AveragePrice, type, `Total Volume`) %>%
  filter(type == "organic")

# Filter by type: Conventional
conventional <- Avodata %>%
  select(Date, AveragePrice, type, `Total Volume`) %>%
  filter(type == "conventional")

# For Organic Avocado prices:
# Convert the "Date" column to a Date type
organic$Date <- as.Date(organic$Date)

# Create a time series object
organic_ts <- ts(organic$AveragePrice, start = c(year(organic$Date[1]), month(organic$Date[1])), frequency = 12)

# Generate the ACF plot
acf(organic_ts, main = "Autocorrelation of Organic Avocado Prices")


#For Conventional Avocado Prices:
# Convert the "Date" column to a Date type
conventional$Date <- as.Date(conventional$Date)

# Create a time series object
conventional_ts <- ts(conventional$AveragePrice, start = c(year(conventional$Date[1]), month(conventional$Date[1])), frequency = 12)

# Generate the ACF plot
acf(conventional_ts, main = "Autocorrelation of Conventional Avocado Prices")


# Filter the data for conventional avocados
conventional <- subset(Avodata, type == "conventional")

# Filter the data for organic avocados
organic <- subset(Avodata, type == "organic")

# Create time series plots
conventional_plot <- ggplot(conventional, aes(x = Date, y = AveragePrice)) +
  geom_line() +
  labs(x = "Date", y = "Average Price", title = "Conventional Avocado Prices")

organic_plot <- ggplot(organic, aes(x = Date, y = AveragePrice)) +
  geom_line() +
  labs(x = "Date", y = "Average Price", title = "Organic Avocado Prices")

# Arrange the plots in a grid
grid_arrange <- gridExtra::grid.arrange(conventional_plot, organic_plot, nrow = 2)

# Display the grid of plots
print(grid_arrange)

# Detecting seasonality patterns for Conventional Avocados
conv_patterns <- Avodata %>%
  select(year, AveragePrice, type) %>%
  filter(type == "conventional") %>%
  group_by(year) %>%
  summarize(avg = mean(AveragePrice)) %>%
  ggplot(aes(x = year, y = avg)) +
  geom_point(color = "#F35D5D", aes(size = avg)) +
  geom_line(group = 1, color = "#7FB3D5") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#F9E79F")) +
  labs(title = "Conventional Avocados", x = "Year", y = "Average Price")

# Detecting seasonality patterns for Organic Avocados
org_patterns <- Avodata %>%
  select(year, AveragePrice, type) %>%
  filter(type == "organic") %>%
  group_by(year) %>%
  summarize(avg = mean(AveragePrice)) %>%
  ggplot(aes(x = year, y = avg)) +
  geom_point(color = "#F35D5D", aes(size = avg)) +
  geom_line(group = 1, color = "#58D68D") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#F9E79F")) +
  labs(title = "Organic Avocados", x = "Year", y = "Average Price")

# Arrange the plots in a grid
grid_arrange <- grid.arrange(conv_patterns, org_patterns, nrow = 2)

# Display the grid of plots
print(grid_arrange)

# Create a copy of the original dataset
seasonal_df <- Avodata
seasonal_df$month_year <- format(as.Date(Avodata$Date), "%Y-%m")
seasonal_df$month <- format(as.Date(Avodata$Date), "%m")
seasonal_df$year <- format(as.Date(Avodata$Date), "%Y")
seasonal_df$monthabb <- sapply(seasonal_df$month, function(x) month.abb[as.numeric(x)])
seasonal_df$monthabb <- factor(seasonal_df$monthabb, levels = month.abb)

# Plot for Conventional Avocados
conv_patterns <- seasonal_df %>% 
  select(monthabb, AveragePrice, type) %>% 
  filter(type == "conventional") %>%
  group_by(monthabb) %>% 
  summarize(avg = mean(AveragePrice)) %>%
  ggplot(aes(x = monthabb, y = avg)) + 
  geom_point(color = "#F35D5D", aes(size = avg)) + 
  geom_line(group = 1, color = "#7FB3D5") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#F9E79F")) + 
  labs(title = "Conventional Avocados", x = "Month", y = "Average Price")

# Plot for Organic Avocados
org_patterns <- seasonal_df %>% 
  select(monthabb, AveragePrice, type) %>% 
  filter(type == "organic") %>%
  group_by(monthabb) %>% 
  summarize(avg = mean(AveragePrice)) %>%
  ggplot(aes(x = monthabb, y = avg)) + 
  geom_point(color = "#F35D5D", aes(size = avg)) + 
  geom_line(group = 1, color = "#58D68D") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#F9E79F")) + 
  labs(title = "Organic Avocados", x = "Month", y = "Average Price")

# Display the grid of plots
grid_arrange <- grid.arrange(conv_patterns, org_patterns, nrow = 2)
print(grid_arrange)

# Set plot dimensions
options(repr.plot.width=8, repr.plot.height=6) 

# Create a new column for the season
seasonal_df$season <- ifelse(seasonal_df$month %in% c("03", "04","05"), "Spring",
                             ifelse(seasonal_df$month %in% c("06","07" ,"08"), "Summer",
                                    ifelse(seasonal_df$month %in% c("09","10","11"), "Fall", "Winter")))

# Plot for conventional avocados by season
seasonality.plot.conventional <- seasonal_df %>% select(season, year, AveragePrice, type) %>% 
  filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(season, year) %>%
  summarize(avg=mean(AveragePrice)) %>% 
  ggplot(aes(x=season, y=avg, color=season)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=season, 
                   xend=season, 
                   y=0, 
                   yend=avg)) + 
  coord_flip() + 
  facet_wrap(~as.factor(year)) + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7")) + 
  scale_color_manual(values=c("#a06a31", "#9bd16b", "#d1706b", "#3bbf9e")) + 
  labs(title="Conventional Avocados by Season", x="Season", y="Average Price") + 
  geom_text(aes(x=season, y=0.01, label= paste0("$ ", round(avg,2))),
            hjust=-0.5, vjust=-0.5, size=4, 
            colour="black", fontface="italic",
            angle=372)

# Plot for organic avocados by season
seasonality.plot.organic <- seasonal_df %>% select(season, year, AveragePrice, type) %>% 
  filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(season, year) %>%
  summarize(avg=mean(AveragePrice)) %>% 
  ggplot(aes(x=season, y=avg, color=season)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=season, 
                   xend=season, 
                   y=0, 
                   yend=avg)) + 
  coord_flip() + 
  facet_wrap(~as.factor(year)) + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7")) + 
  scale_color_manual(values=c("#a06a31", "#9bd16b", "#d1706b", "#3bbf9e")) + 
  labs(title="Organic Avocados by Season", x="Season", y="Average Price") + 
  geom_text(aes(x=season, y=0.01, label= paste0("$ ", round(avg,2))),
            hjust=-0.5, vjust=-0.5, size=4, 
            colour="black", fontface="italic",
            angle=372)

# Display the plots
print(seasonality.plot.conventional)
print(seasonality.plot.organic)

# Selecting data
conv <- seasonal_df %>% filter(type == "conventional")
org <- seasonal_df %>% filter(type == "organic")

# Declare data as time series
conventional <- as_tbl_time(conv, index = Date)
conventional <- as_period(conventional, '1 month')
conventional$type <- NULL

organic <- as_tbl_time(org, index = Date)
organic <- as_period(organic, '1 month')
organic$type <- NULL

# Create time series objects
conv_ts <- ts(conventional$AveragePrice, start = c(2015, 1), frequency = 12)
org_ts <- ts(organic$AveragePrice, start = c(2015, 1), frequency = 12)

# Calculate the differences between consecutive months for conventional avocado prices
differences_conv <- diff(conv_ts)

# Plotting the differences and seasonality plot
main_diff <- autoplot(differences_conv) + theme_minimal()
seasonality_diff <- ggseasonplot(differences_conv) + theme_minimal()

# Arrange the plots in a grid
grid_arrange <- grid.arrange(main_diff, seasonality_diff, nrow=2)


# Forecasting

# Method 1 Naive method


# Naive Forecast for Conventional Avocados
conv_naive_forecast <- naive(conv_ts, h = 12)

# Plot the Naive forecast for Conventional Avocados
options(repr.plot.width=12, repr.plot.height=6)
plot(conv_naive_forecast, main = "Naive Forecast for Conventional Avocados", xlab = "Year", ylab = "Average Price")

# Display the Naive forecast values
print(conv_naive_forecast)

# Naive Forecast for Organic Avocados
org_naive_forecast <- naive(org_ts, h = 12)

# Plot the Naive forecast for Organic Avocados
plot(org_naive_forecast, main = "Naive Forecast for Organic Avocados", xlab = "Year", ylab = "Average Price")

# Display the Naive forecast values
print(org_naive_forecast)

# Calculate residuals for the naive method on conventional avocados
rescv_nv <- residuals(naive(conv_ts, h = 72))
p1 <- autoplot(rescv_nv, color = "red") + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method \n Conventional Avocados") + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.20, color = "black"),
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title = element_text(colour = "black"))

# Calculate residuals for the naive method on organic avocados
resorg_nv <- residuals(naive(org_ts, h = 72))
p2 <- autoplot(resorg_nv, color = "blue") + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method \n Organic Avocados") + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.20, color = "black"),
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title = element_text(colour = "black"))


grid_arrange <- grid.arrange(p1, p2, nrow=2)
sqrt(0.05354)


# Method 2 Seasonal Naive method


options(repr.plot.width=10, repr.plot.height=7)

# Convert time series data to tsibble objects
conv_ts <- as_tsibble(conventional, key = NULL, index = Date)
org_ts <- as_tsibble(organic, key = NULL, index = Date)

# Convert the data to tsibble format
conv_tsibble <- as_tsibble(conventional, key = NULL, index = Date)
org_tsibble <- as_tsibble(organic, key = NULL, index= Date )

# Fill implicit gaps in time with explicit missing values
conv_tsibble <- fill_gaps(conv_tsibble)
org_tsibble <- fill_gaps(org_tsibble)

# Specify the seasonal lag value
seasonal_lag <- 12  # Assuming monthly data

# Forecast using seasonal naive method for coventional avocados
conv_forecast_sn <- conv_tsibble %>%
  model(
    mean = MEAN(AveragePrice),
    naive = NAIVE(AveragePrice),
    snaive = SNAIVE(AveragePrice ~ RW())
  ) %>%
  forecast(h = 72) %>%
  autoplot() +
  ggplot2::labs(title = "Conventional Avocado \n Seasonal Naive Method", x = "Date", y = "Price") +
  scale_color_manual(values = c("#FA5858", "#00BFFF", "#FF8000")) +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#F4F6F7"))

# Forecast using seasonal naive method for organic avocados
org_forecast_sn <- org_tsibble %>%
  model(
    mean = MEAN(AveragePrice),
    naive = NAIVE(AveragePrice),
    snaive = SNAIVE(AveragePrice ~ RW())
  ) %>%
  forecast(h = 72) %>%
  autoplot() +
  ggplot2::labs(title = "Organic Avocado \n Seasonal Naive Method", x = "Date", y = "Price") +
  scale_color_manual(values = c("#FA5858", "#00BFFF", "#FF8000")) +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#F4F6F7"))

# Set the plot dimensions
options(repr.plot.width = 10, repr.plot.height = 7)
grid_arrange <- grid.arrange(conv_forecast_sn, org_forecast_sn, nrow=2)

# Convert the data to a time series object
conv_ts <- ts(conventional$AveragePrice, start = c(2015, 1), frequency = 12)
org_ts <- ts(organic$AveragePrice, start = c(2015, 1), frequency = 12)
conv_ts_index <- as.Date(time(conv_ts))
org_ts_index <- as.Date(time(org_ts))


#Residual using Seasonal naive method
# Seasonal Naive Forecast for Conventional Avocados
conv_seasonal_naive_forecast <- snaive(conv_ts, h = 12)

# Extract the predicted values
conv_seasonal_naive_pred <- fitted(conv_seasonal_naive_forecast)

# Calculate residuals for Conventional Avocados
conv_residuals <- residuals(conv_seasonal_naive_forecast)

# Display the residuals for Conventional Avocados
print(conv_residuals)

# Seasonal Naive Forecast for Organic Avocados
org_seasonal_naive_forecast <- snaive(org_ts, h = 12)

# Extract the predicted values
org_seasonal_naive_pred <- fitted(org_seasonal_naive_forecast)

# Calculate residuals for Organic Avocados
org_residuals <- residuals(org_seasonal_naive_forecast)

# Display the residuals for Organic Avocados
print(org_residuals)


#Prediction Interval for Seasonal Naive method 


# Calculate the standard deviation of residuals
residual_sd <- sd(conv_residuals)
residual_sd <- sd(org_residuals)

# Set the confidence level (e.g., 95%)
confidence_level <- 0.95

# Calculate the z-score for the given confidence level
z_score <- qnorm((1 + confidence_level) / 2)

# Calculate the prediction interval
conv_prediction_interval <- cbind(lower = conv_seasonal_naive_pred - z_score * residual_sd,
                             upper = conv_seasonal_naive_pred + z_score * residual_sd)
org_prediction_interval <- cbind(lower = org_seasonal_naive_pred - z_score * residual_sd,
                             upper = org_seasonal_naive_pred + z_score * residual_sd)

# Combine the prediction interval with the forecasted values
conv_with_prediction_interval <- cbind(conv_seasonal_naive_pred, conv_prediction_interval)
org_with_prediction_interval <- cbind(org_seasonal_naive_pred, org_prediction_interval)

# Print the results
print(conv_with_prediction_interval)
print(org_with_prediction_interval)

rm(list=ls())
getwd()
setwd("")
##################
#   libraries
##################

install.packages()
install.packages("summarytools")
library(jsonlite)
library(quantmod)
library(ggplot2)
library(dplyr)
library(rdrobust)
library(stargazer)
library(tidyr)
library(modelsummary)
library(readr)
library(summarytools)
library(kableExtra)
library(psych)
library(corrplot)
library(gridExtra)
library(lubridate)
library(boot)

##################
#   datasets
##################

###BTC##########################################################################

ticker <- "BTC-USD"
start_date <- "2020-01-01"
end_date <- "2024-04-30"


# Retrieve Bitcoin historical data
btc_data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
btc_data <- data.frame(Date = index(btc_data),
                       BTC_Price = as.numeric(Ad(btc_data)),
                       BTC_Volume = as.numeric(Vo(btc_data)))

###S&P500 ######################################################################

ticker_symbol <- "^GSPC"

# Define start and end dates
start_date <- "2020-01-01"
end_date <- "2024-04-30"


getSymbols(ticker_symbol, from = start_date, to = end_date)

sp500_data <- Cl(GSPC)

sp500_df <- data.frame(Date = index(sp500_data), SP500 = coredata(sp500_data))

colnames(sp500_df)[2] <- "SP500"

missing_dates <- setdiff(seq.Date(as.Date(start_date), as.Date(end_date), by = "day"), sp500_df$Date)

smoothed_values <- loess(SP500 ~ as.numeric(Date), data = sp500_df, span = 0.5)

predicted_values <- predict(smoothed_values, newdata = data.frame(Date = missing_dates))

missing_data <- data.frame(Date = missing_dates, SP500 = predicted_values)

combined_data <- merge(sp500_df, missing_data, by = "Date", all = TRUE)

combined_data$SP500 <- ifelse(is.na(combined_data$SP500.x), combined_data$SP500.y, combined_data$SP500.x)

combined_data <- combined_data[, !(names(combined_data) %in% c("SP500.x", "SP500.y"))]

plot(combined_data$Date, combined_data$SP500, type = "l", xlab = "Date", ylab = "SP500", main = "Local Regression around Missing Values")

index_to_replace <- which(combined_data$Date == as.Date("2020-01-01"))

combined_data$SP500[index_to_replace] <- 3257.850

SP500Data <- combined_data

rm(GSPC, sp500_data, sp500_df, missing_data, smoothed_values, predicted_values, missing_dates, combined_data)

# Add the value for April 30, 2024
SP500Data <- rbind(SP500Data, data.frame(Date = as.Date("2024-04-30"), SP500 = 5116.17))

SP500Data <- na.omit(SP500Data)

###Inflation####################################################################

# https://www.imf.org/external/datamapper/PCPIPCH@WEO/OEMDC/ADVEC/WEOWORLD

inflation_rates <- c(3.2, 4.7, 8.7, 6.8, 5.9)

dates <- seq(as.Date("2020-01-01"), as.Date("2024-04-30"), by = "day")

inflation_data <- rep(inflation_rates, times = c(366, 365, 365, 365, 121)) # Adjust for leap year

inflation<- data.frame(
  Date = dates,
  InflationRate = inflation_data
)


###Interest Rate US ############################################################

#https://fred.stlouisfed.org/series/FEDFUNDS

file_path <- "FEDFUNDS.csv"

FEDFUNDS <- read_csv(file_path)

FEDFUNDS<- FEDFUNDS[FEDFUNDS$DATE >= as.Date("2020-01-01"), ]

days_per_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, # Anno bisestile (2020)
                    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, # Anno normale (2021)
                    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, # Anno normale (2022)
                    31, 31, 28, 30, 31, 30, 31, 31, 30, 31, 30, 31) # Anno normale (2023)

repeated_data <- unlist(mapply(rep.int, FEDFUNDS$FEDFUNDS, times = days_per_month))

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2024-04-30")
dates <- seq(start_date, end_date, by = "day")

InterestUS<- data.frame(Date = dates, FEDFUNDS = repeated_data)

rm(FEDFUNDS)

colnames(InterestUS) <- c("Date", "IntRateUS")

###Volatility ##################################################################

#https://it.investing.com/indices/crypto-volatility-index-historical-data

volatilitydata <- read.csv("CVI.csv")

volatilitydata <- volatilitydata[rev(row.names(volatilitydata)), ]

volatilitydata <- volatilitydata[, c("Data", "Ultimo")]

volatilitydata$Data <- as.Date(volatilitydata$Data, format = "%d.%m.%Y")

missing_data <- data.frame(
  Data = as.Date(c("2022-07-20", "2022-03-04", "2022-03-05", "2023-06-07")),
  Ultimo = c(115.73, 77.95, 77.95, 43.35)
)

volatilitydata$Data <- as.Date(volatilitydata$Data, format = "%d.%m.%Y")

missing_data$Data <- as.Date(missing_data$Data)

merged_data <- rbind(volatilitydata, missing_data)

volatilitydata$Data <- as.Date(volatilitydata$Data, format = "%d.%m.%Y")

missing_data$Data <- as.Date(missing_data$Data)

CVIdata <- rbind(volatilitydata, missing_data)

rm(volatilitydata, missing_data, merged_data)

colnames(CVIdata) <- c("Date", "CVI")

###MKTCap ######################################################################

# Create the Date and BTC_MKTCAP columns
Date <- c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
          "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
          "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01",
          "2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01",
          "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01",
          "2022-07-01", "2022-08-01", "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01",
          "2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01", "2023-05-01", "2023-06-01",
          "2023-07-01", "2023-08-01", "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01",
          "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01")
BTC_MKTCAP <- c(170.11, 156.90, 117.81, 158.92, 174.00, 168.32, 208.90, 215.82, 199.56, 255.37, 364.23, 539.05,
                616.45, 841.43, 1100.02, 1079.67, 698.99, 656.85, 781.43, 886.87, 824.62, 1156.49, 1076.69, 875.94,
                729.07, 819.41, 865.20, 717.60, 605.80, 377.53, 445.94, 383.71, 372.41, 393.41, 330.00, 318.52, 446.08,
                446.85, 550.59, 566.66, 527.77, 591.76, 568.37, 504.96, 525.89, 677.05, 737.56, 827.81, 835.23, 1202.01,
                1403.11, 1371.14)

# Create mktcapData data frame
mktcapData <- data.frame(Date = as.Date(Date), BTC_MKTCAP = BTC_MKTCAP)

# Define days per month for each year
days_per_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, # Leap year (2020)
                    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, # Normal year (2021)
                    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, # Normal year (2022)
                    31, 31, 28, 30, 31, 30, 31, 31, 30, 31, 30, 31) # Normal year (2023)

# Repeat the data for each day
repeated_data <- unlist(mapply(rep.int, mktcapData$BTC_MKTCAP, times = days_per_month))

# Define start and end dates
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2024-04-30")

# Generate sequence of dates
dates <- seq(start_date, end_date, by = "day")

# Create the mktcapData data frame
mktcapData<- data.frame(Date = dates, BTC_MKTCAP = repeated_data)

rm(Date, BTC_MKTCAP, days_per_month, repeated_data, start_date, end_date, dates)

###shock dummies ###############################################################

#TESLA BUY BTC +
#8 feb 2021

#ELON WORRED FOR SUSTAINABILITY -
#16 MAY 2021

#TESLA EXIT BTC 75% in Q2 -
#20 JULY 22

dates <- seq(as.Date("2020-01-01"), as.Date("2024-04-30"), by = "day")

# Crea la variabile dummy ELON21
ELON_2_21 <- ifelse(dates >= as.Date("2021-02-08"), 1, 0)
dummy_dataset <- data.frame(Date = dates, ELON_2_21 = ELON_2_21)
dummy_dataset$ELON_5_21 <- ifelse(dates >= as.Date("2021-05-16"), 1, 0)
dummy_dataset$ELON_7_22 <- ifelse(dates >= as.Date("2022-07-20"), 1, 0)
# Imposta ELON_2_21 a 1 fino alla data di ELON_5_21
dummy_dataset$ELON_2_21 <- ifelse(dates <= as.Date("2021-05-16"), 1, 0)

# Imposta ELON_5_21 a 1 fino alla data successiva a ELON_2_21
dummy_dataset$ELON_5_21 <- ifelse(dates >= as.Date("2021-05-16") & dates <= as.Date("2022-07-19"), 1, 0)

# Imposta ELON_7_22 a 1 dalla data specificata in poi
dummy_dataset$ELON_7_22 <- ifelse(dates >= as.Date("2022-07-20"), 1, 0)

##################
#   data merging
##################


# Merge all datasets by Date
merged_data <- merge(SP500Data, inflation, by = "Date", all = TRUE)
merged_data <- merge(merged_data, InterestUS, by = "Date", all = TRUE)
merged_data <- merge(merged_data, CVIdata, by = "Date", all = TRUE)
merged_data <- merge(merged_data, mktcapData, by = "Date", all = TRUE)
merged_data <- merge(merged_data, btc_data, by = "Date", all = TRUE)
duplicated_dates <- duplicated(merged_data$Date)

# Check if any date is duplicated
if (any(duplicated_dates)) {
  print("There are duplicated dates.")
} else {
  print("Every date is unique.")
}

duplicated_indices <- which(duplicated(merged_data$Date))

# Subset the dataset to retrieve the duplicated dates
duplicated_dates <- merged_data$Date[duplicated_indices]

# Print the duplicated dates
print(duplicated_dates)

duplicated_indices <- which(duplicated(merged_data$Date))

# Remove one occurrence of the duplicated date "2022-07-20"
merged_data <- merged_data[-duplicated_indices[1], ]

# Print the modified dataset
print(merged_data)

rm(dates_with_na, duplicated_dates, duplicated_indices, file_path, index_to_replace, inflation_data, inflation_rates, na_indices, ticker, ticker_symbol, mktcapData, inflation, InterestUS, SP500Data, btc_data, CVIdata)
data<-merged_data
rm(merged_data)

data <- data %>% 
  mutate(CVI = ifelse(Date == as.Date("2022-06-20"), "110", CVI))
data$CVI <- as.numeric(gsub(",", ".", data$CVI))  # Replace commas with periods and convert to numeric

data <- data %>% 
  rename(Inflation = InflationRate)

##################
#   statistics
##################

###table########################################################################
str(data)
datastat <- data[-1]  
summary_stats <- describe(datastat)

required_stats <- summary_stats[, c( "min", "mean", "median", "max", "sd")]

required_stats_rounded <- lapply(required_stats, function(x) round(x, 2))

# Convert rounded values to character format
required_stats_char <- lapply(required_stats_rounded, function(x) as.character(x))

# Create a data frame with character values and set row names
summary_stats_char <- as.data.frame(required_stats_char)
rownames(summary_stats_char) <- rownames(summary_stats)

# Display the formatted table in the viewer
kable(summary_stats_char, "html", align = "c", col.names = c("Variable", "Minimum", "Mean", "Median", "Maximum", "SD")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)

rm(date_with_non_numeric, summary_stats, required_stats, median_val, na_values, non_numeric, required_stats_char, required_stats_rounded, summary_stats_char)

data <- merge(data, dummy_dataset[, c("Date", "ELON_2_21", "ELON_5_21", "ELON_7_22")], by = "Date", all.x = TRUE)

data <- data %>%
  filter(BTC_Volume < 350967941478)
###correlation##################################################################

str(datastat)

correlation_matrix <- cor(datastat)

corrplot(correlation_matrix, method = "color", tl.col = "black", addCoef.col = "black",
         number.cex = 1.0, tl.cex = 1.0, type = "lower")

#SP500 and MKTCAP are very highly correlated with the X, making multicollinearity problems
#correlations with y are equilibrated and not problematic
#correlation between x and y are maybe a little too low.

################################################################################




##################
#      RDD 
##################

###Threshold choice ############################################################

thresholds <- seq(19000, 70000, by = 1000)

results_df <- data.frame(Threshold = numeric(),
                         Intercept = numeric(),
                         Coef_D = numeric(),
                         Coef_Close = numeric(),
                         Coef_D_Close = numeric(),
                         R_squared = numeric())

for (threshold in thresholds) {
  
  data$D <- ifelse(data$BTC_Price >= threshold, 1, 0)
  
  model <- lm(BTC_Volume ~ D  + I(BTC_Price - threshold), data = data)
  
  intercept <- coef(model)[1]
  coef_D <- coef(model)[2]
  coef_Close <- coef(model)[3]
  coef_D_Close <- coef(model)[4]
  r_squared <- summary(model)$r.squared
  
  results_df <- rbind(results_df, data.frame(Threshold = threshold,
                                             Intercept = intercept,
                                             Coef_D = coef_D,
                                             Coef_Close = coef_Close,
                                             Coef_D_Close = coef_D_Close,
                                             R_squared = r_squared))
}

print(results_df)


ggplot(data, aes(x = Date, y = BTC_Price)) +
  geom_line(color = "black") +  # Line plot for Bitcoin price
  geom_hline(yintercept = 29050, linetype = "dashed", color = "red", size = 1.5) +  # Red dashed line at $31,000 with thicker line
  labs(x = "Date", y = "Bitcoin Price (USD)", title = "Bitcoin Price Over Time with Threshold") +  # Axis labels and title
  theme_minimal()
# Minimal theme

#################
### assumptions

### distribution analysis#######################################################
d1 <- ggplot(data, aes(x = BTC_Price, y = BTC_Volume)) +
  geom_point() +
  geom_vline(xintercept = 29050, linetype = "dotted", color = "red", size = 1.0) +
  labs(title = "BTC_Volume vs BTC_Price",
       x = "BTC_Price",
       y = "BTC_Volume") +
  theme_minimal()
print(d1)

# Set threshold value
threshold <- 29050

# Create separate datasets for BTC_Price before and after threshold
data_before <- subset(data, BTC_Price <= threshold)
data_after <- subset(data, BTC_Price > threshold)

# Plot overall density of BTC_Price
ggplot(data, aes(x = BTC_Price, fill = "Overall Density")) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of BTC_Price",
       x = "BTC Price", y = "Density") +
  scale_fill_manual(values = c("Overall Density" = "blue", "Before Threshold" = "green", "After Threshold" = "red")) +
  theme_minimal() +
  theme(legend.position = "top") +
  
  # Add density for BTC_Price before threshold
  geom_density(data = data_before, aes(x = BTC_Price, fill = "Before Threshold"), alpha = 0.5) +
  
  # Add density for BTC_Price after threshold
  geom_density(data = data_after, aes(x = BTC_Price, fill = "After Threshold"), alpha = 0.5) +
  
  # Legend
  guides(fill = guide_legend(title = "Density"))+
  geom_vline(xintercept = 29050, linetype = "dashed", color = "black", size=1.0)

#discontinuity between Y and X
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$CVI + data$Inflation))

summary(result)
rdplot(result)
plot(data$BTC_Volume, data$BTC_Price, xlab = "BTC Volume", ylab = "BTC Price", pch = 20)

abline(result$coefficients[1], result$coefficients[2], col = "red")
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050)
summary(result)

#results in a table for the values around the selected ##################
# Create a data frame with the summary information
# Create a data frame with the updated summary information
summary_data <- data.frame(
  Method = c("Conventional", "Robust", "Conventional", "Robust", "Conventional", "Robust"),
  Threshold = c(29000, 29000, 29050, 29050, 29100, 29100),
  Coefficient = c(--7849357629.252, NA, -11277128244.570, NA, -11972849840.057, NA),
  Std_Error = c(4894062298.606, NA,5269267958.997, NA, 5040185070.693, NA),
  z_value = c(-1.604, -1.868, -2.140, -2.317, -2.375, -2.550),
  P_value = c(0.109, 0.062, 0.032, 0.021, 0.018, 0.011),
  Confidence_Interval = c("[-17441543472.615, 1742828214.111]", "[-20336345042.840, 490632977.620]",
                          "[-21604703669.095 , -949552820.045]", "[-24229681518.861 , -2021773568.826]",
                          "[-21851431054.031 , -2094268626.083]", "[-24364529466.816 , -3187441137.057]")
)


# Print the table using kable
kable(summary_data, "html", align = "c")%>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

library(rdd)
cutpoint<-29100
DCdensity(data$BTC_Price, cutpoint, bin = NULL, bw = NULL, verbose = FALSE,
          plot = TRUE, ext.out = FALSE, htest = FALSE)


# McCrary test results
threshold <- c(29000, 29050, 29100)
p_value <- c(0.9240973, 0.2364568, 0.1411992)

# Create a data frame
mccrary_results <- data.frame(Threshold = threshold, P_value = p_value)
kable(mccrary_results, "html", align = "c")%>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
#Accepting the Null Hypothesis (H0): If the p-value associated with the coefficient 
#estimate for the treatment effect at the threshold is greater than your chosen 
#significance level (e.g., 0.05), you would fail to reject the null hypothesis. 
#This suggests that there is no clear evidence of a significant discontinuity in 
#the outcome variable at the threshold. In other words, you do not find convincing 
#evidence that the outcome variable behaves differently just above and just below 
#the threshold.
#Rejecting the Null Hypothesis (H0): If the p-value associated with the coefficient
#estimate is less than your chosen significance level (e.g., 0.05), you would 
#reject the null hypothesis. This indicates that there is evidence of a significant 
#discontinuity in the outcome variable at the threshold. In other words, you find
#convincing evidence that the outcome variable exhibits a sharp change just above 
#and just below the threshold, supporting the validity of the regression
#discontinuity design.

data_before <- data %>% filter(BTC_Price <= 29000)
data_after <- data %>% filter(BTC_Price > 29000)

j1 <- ggplot(data, aes(x = BTC_Price, y = BTC_Volume)) +
  geom_point() +
  geom_vline(xintercept = 29000, linetype = "dotted", color = "red", size = 1.0) +
  geom_smooth(data = data_before, method = "loess", se = FALSE, color = "cyan", size = 1.5) +
  geom_smooth(data = data_after, method = "loess", se = FALSE, color = "green", size = 1.5) +
  labs(title = "BTC_Volume vs BTC_Price",
       x = "BTC_Price",
       y = "BTC_Volume") +
  theme_minimal()
print(j1)

data_before <- data %>% filter(BTC_Price <= 29050)
data_after <- data %>% filter(BTC_Price > 29050)

j2 <- ggplot(data, aes(x = BTC_Price, y = BTC_Volume)) +
  geom_point() +
  geom_vline(xintercept = 29050, linetype = "dotted", color = "red", size = 1.0) +
  geom_smooth(data = data_before, method = "loess", se = FALSE, color = "cyan", size = 1.5) +
  geom_smooth(data = data_after, method = "loess", se = FALSE, color = "green", size = 1.5) +
  labs(title = "BTC_Volume vs BTC_Price",
       x = "BTC_Price",
       y = "BTC_Volume") +
  theme_minimal()
print(j2)

data_before <- data %>% filter(BTC_Price <= 30000)
data_after <- data %>% filter(BTC_Price > 30000)

j3 <- ggplot(data, aes(x = BTC_Price, y = BTC_Volume)) +
  geom_point() +
  geom_vline(xintercept = cutoff, linetype = "dotted", color = "red", size = 1.0) +
  geom_smooth(data = data_before, method = "loess", se = FALSE, color = "cyan", size = 1.5) +
  geom_smooth(data = data_after, method = "loess", se = FALSE, color = "green", size = 1.5) +
  labs(title = "BTC_Volume vs BTC_Price",
       x = "BTC_Price",
       y = "BTC_Volume") +
  theme_minimal()
print(j3)

grid.arrange(j1, j2, j3,
             nrow = 1, ncol = 3)

###CIA##########################################################################
#balance test
cutpoint<-29100
DCdensity(data$BTC_Price, cutpoint, bin = NULL, bw = NULL, verbose = FALSE,
          plot = TRUE, ext.out = FALSE, htest = FALSE)

# Creating a dummy variable for the cutoff
data$cutoff <- ifelse(data$BTC_Price > 29050, 1, 0)

# Balance test for CVI
t_test_CVI <- t.test(CVI ~ cutoff, data = data)
print(t_test_CVI)

# Balance test for Inflation
t_test_Inflation <- t.test(Inflation ~ cutoff, data = data)
print(t_test_Inflation)

# Balance test for IntRateUS
t_test_IntRateUS <- t.test(IntRateUS ~ cutoff, data = data)
print(t_test_IntRateUS)

# Balance test for SP500
t_test_SP500<- t.test(SP500 ~ cutoff, data = data)
print(t_test_SP500)

# Balance test for BTC_MKTCAP
t_test_BTC_MKTCAP <- t.test(BTC_MKTCAP ~ cutoff, data = data)
print(t_test_BTC_MKTCAP)

# Full sample results
full_sample_results <- data.frame(
  Variable = c("CVI", "Inflation", "IntRateUS", "SP500", "BTC_MKTCAP"),
  Bandwidth = rep("Full Sample", 5),
  p_value = c("< 2.2e-16", "3.546e-05", "0.0006726", "< 2.2e-16", "< 2.2e-16"),
  Mean_Group_0 = c(72.67364, 5.647935, 2.253066, 3673.738, 351.6440),
  Mean_Group_1 = c(82.34576, 6.062276, 1.870524, 4433.522, 862.1046)
)

# Reduced bandwidth results
reduced_bandwidth_results <- data.frame(
  Variable = c("CVI", "Inflation", "IntRateUS", "SP500", "BTC_MKTCAP"),
  Bandwidth = rep(500, 5),
  p_value = c("0.05719", "0.656", "0.2032", "0.004148", "0.463"),
  Mean_Group_0 = c(70.21445, 6.94375, 3.151250, 4093.040, 561.0481),
  Mean_Group_1 = c(54.43859, 7.15000, 4.043438, 4291.836, 549.5753)
)

# Combine the results
results <- rbind(full_sample_results, reduced_bandwidth_results)

# Create the table
kable(results, "html", align = c("c", "c", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

#all of them are bad, sure enough if i get a smaller span of values around the average value
#that they have when btc price is 29050 the results will improve.

threshold_margin <- 500
btc_threshold <-29050

btc_data_around_threshold <- data[abs(data$BTC_Price - btc_threshold) <= threshold_margin, ]

# Balance test for CVI
btc_data_around_threshold$cutoff <- ifelse(btc_data_around_threshold$BTC_Price > 29050, 1, 0)
t_test_CVI <- t.test(CVI ~ cutoff, data = btc_data_around_threshold)
print(t_test_CVI)

# Balance test for Inflation
t_test_Inflation <- t.test(Inflation ~ cutoff, data = btc_data_around_threshold)
print(t_test_Inflation)

# Balance test for IntRateUS
t_test_IntRateUS <- t.test(IntRateUS ~ cutoff, data = btc_data_around_threshold)
print(t_test_IntRateUS)

# Balance test for SP500
t_test_SP500<- t.test(SP500 ~ cutoff, data = btc_data_around_threshold)
print(t_test_SP500)

# Balance test for BTC_MKTCAP
t_test_BTC_MKTCAP <- t.test(BTC_MKTCAP ~ cutoff, data = btc_data_around_threshold)
print(t_test_BTC_MKTCAP)

#and now only SP500 is bad and rejects h0 of being equal on both sides

#Bandwidth Sensitivity

library(broom)
data$D <- ifelse(data$BTC_Price >= 29050, 1, 0)
loess_nocontrol <- loess(BTC_Volume ~ I(BTC_Price - 29050) + D, data = data, span = 0.52)
loess_good <- loess(BTC_Volume ~ I(BTC_Price - 29050) + D + CVI + Inflation, data = data, span = 0.54)
loess_sq <- loess(BTC_Volume ~ I((BTC_Price)^2 - 29050) + D + CVI + Inflation, data = data, span = 0.62)
loess_cub <- loess(BTC_Volume ~ I((BTC_Price)^3 - 29050) + D + CVI + Inflation, data = data, span = 0.73)
# Function to calculate ATE within a specified bandwidth
calc_ate_bandwidth <- function(model, data, bandwidth) {
  # Filter data within the specified bandwidth
  subset_data <- data %>%
    filter(BTC_Price > (29050 - bandwidth) & BTC_Price < (29050 + bandwidth))
  
  
  # Predict the outcome variable using the model
  predictions <- predict(model, newdata = subset_data)
  
  
  # Separate predictions for treated and control groups
  treated_predictions <- predictions[subset_data$BTC_Price > cutoff]
  control_predictions <- predictions[subset_data$BTC_Price <= cutoff]
  
  
  # Calculate the treatment effect (ATE)
  if (length(treated_predictions) > 0 & length(control_predictions) > 0) {
    treatment_effect <- mean(treated_predictions) - mean(control_predictions)
  } else {
    treatment_effect <- NA  # If no treated or control data points found within bandwidth
  }
  
  return(treatment_effect)
}

loocv_bandwidth_ate <- function(model, data, bandwidths) {
  loocv_errors <- numeric(length(bandwidths))
  
  for (i in seq_along(bandwidths)) {
    bw <- bandwidths[i]
    errors <- numeric()
    
    for (j in 1:nrow(data)) {
      if (data$BTC_Price[j] > (29050 - bw) & data$BTC_Price[j] < (29050 + bw)) {
        training_data <- data[-j, ]
        test_data <- data[j, , drop = FALSE]
        
        predictions <- predict(model, newdata = test_data)
        
        treatment_effect <- calc_ate_bandwidth(model, training_data, bw)
        errors <- c(errors, (test_data$BTC_Volume - predictions)^2)
      }
    }
    
    loocv_errors[i] <- mean(errors, na.rm = TRUE)
  }
  
  optimal_bandwidth <- bandwidths[which.min(loocv_errors)]
  return(optimal_bandwidth)
}

# Define candidate bandwidths
bandwidths <- seq(10, 2000, by = 10)



# Calculate ATE for each model with different bandwidths
ate_bandwidths_nocontrol <- sapply(bandwidths, function(bw) calc_ate_bandwidth(loess_nocontrol, data, bw))
ate_bandwidths_good <- sapply(bandwidths, function(bw) calc_ate_bandwidth(loess_good, data, bw))
ate_bandwidths_sq <- sapply(bandwidths, function(bw) calc_ate_bandwidth(loess_sq, data, bw))
ate_bandwidths_cub <- sapply(bandwidths, function(bw) calc_ate_bandwidth(loess_cub, data, bw))

optimal_bandwidth_nocontrol <- loocv_bandwidth_ate(loess_nocontrol, data, bandwidths)
optimal_bandwidth_good <- loocv_bandwidth_ate(loess_good, data, bandwidths)
optimal_bandwidth_sq <- loocv_bandwidth_ate(loess_sq, data,  bandwidths)
optimal_bandwidth_cub <- loocv_bandwidth_ate(loess_cub, data,  bandwidths)
ate_nocontrol <- calc_ate_bandwidth(loess_nocontrol, data, optimal_bandwidth_nocontrol)
ate_good <- calc_ate_bandwidth(loess_good, data,  optimal_bandwidth_good)
ate_sq <- calc_ate_bandwidth(loess_sq, data,  optimal_bandwidth_sq)
ate_cub <- calc_ate_bandwidth(loess_cub, data, optimal_bandwidth_cub)
# Print the ATE results
cat("ATE for loess no controls with optimal bandwidth:", ate_nocontrol, "\n")
cat("ATE for loess linear w controls with optimal bandwidth:", ate_good, "\n")
cat("ATE for loess squared w controls with optimal bandwidth:", ate_sq, "\n")
cat("ATE for loess cubic w controls with optimal bandwidth:", ate_cub, "\n")

# Create the table with kable
df_ate <- data.frame(
  Models = c("loess no controls", "loess linear w controls", "loess squared w controls", "loess cubic w controls"),
  Optimal_Bandwidth = c(optimal_bandwidth_nocontrol, optimal_bandwidth_good, optimal_bandwidth_sq, optimal_bandwidth_cub),
  ATE = c(ate_nocontrol, ate_good, ate_sq, ate_cub)
)

# Create the table with kable
kable(df_ate, "html", align = c("c", "c", "c")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


###linearity assumption#########################################################


btc_threshold <-29050


result <- rdrobust(data$SP500, data$BTC_Price, c=29050)
summary(result)
result <- rdrobust(data$Inflation, data$BTC_Price, c=29050)
summary(result)
result <- rdrobust(data$IntRateUS, data$BTC_Price, c=29050)
summary(result)
result <- rdrobust(data$CVI, data$BTC_Price, c=29050)
summary(result)
result <- rdrobust(data$BTC_MKTCAP, data$BTC_Price, c=29050)
summary(result)

Variables <- c('SP500', 'Inflation', 'IntRateUS', 'CVI', 'BTC_MKTCAP')
p_value <- c(0.001, 0.187, 0.324, 0.032 , 0.000)
p_value_r <- c(0.000, 0.359, 0.184, 0.037 , 0.000)
# Create a data frame
linearity <- data.frame(Variables = Variables, P_value = p_value, Robust_P_value=p_value_r)


kable(linearity, "html", align = "c")%>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
################################################################################
# Define the cutoff
cutoff <- 29050

# Create scatterplot for SP500 vs BTC_Price
data_before <- data %>% filter(BTC_Price <= cutoff)
data_after <- data %>% filter(BTC_Price > cutoff)

p1 <- ggplot(data, aes(x = BTC_Price, y = SP500)) +
  geom_point() +
  geom_vline(xintercept = cutoff, linetype = "dotted", color = "red", size = 1.0) +
  geom_smooth(data = data_before, method = "loess", se = FALSE, color = "cyan", size = 1.5) +
  geom_smooth(data = data_after, method = "loess", se = FALSE, color = "green", size = 1.5) +
  labs(title = "SP500 vs BTC_Price",
       x = "BTC_Price",
       y = "SP500") +
  theme_minimal()
print(p1)

# Create scatterplot for Inflation vs BTC_Price
p2 <- ggplot(data, aes(x = BTC_Price, y = Inflation)) +
  geom_point() +
  geom_vline(xintercept = cutoff, linetype = "dotted", color = "red", size = 1.0) +
  geom_smooth(data = data_before, method = "loess", se = FALSE, color = "cyan", size = 1.5) +
  geom_smooth(data = data_after, method = "loess", se = FALSE, color = "green", size = 1.5) +
  labs(title = "Inflation vs BTC_Price",
       x = "BTC_Price",
       y = "Inflation") +
  theme_minimal()
print(p2)

# Create scatterplot for IntRateUS vs BTC_Price
p3 <- ggplot(data, aes(x = BTC_Price, y = IntRateUS)) +
  geom_point() +
  geom_vline(xintercept = cutoff, linetype = "dotted", color = "red", size = 1.0) +
  geom_smooth(data = data_before, method = "loess", se = FALSE, color = "cyan", size = 1.5) +
  geom_smooth(data = data_after, method = "loess", se = FALSE, color = "green", size = 1.5) +
  labs(title = "IntRateUS vs BTC_Price",
       x = "BTC_Price",
       y = "IntRateUS") +
  theme_minimal()
print(p3)

# Create scatterplot for CVI vs BTC_Price
p4 <- ggplot(data, aes(x = BTC_Price, y = CVI)) +
  geom_point() +
  geom_vline(xintercept = cutoff, linetype = "dotted", color = "red", size = 1.0) +
  geom_smooth(data = data_before, method = "loess", se = FALSE, color = "cyan", size = 1.5) +
  geom_smooth(data = data_after, method = "loess", se = FALSE, color = "green", size = 1.5) +
  labs(title = "CVI vs BTC_Price",
       x = "BTC_Price",
       y = "CVI") +
  theme_minimal()
print(p4)

# Create scatterplot for BTC_MKTCAP vs BTC_Price
p5 <- ggplot(data, aes(x = BTC_Price, y = BTC_MKTCAP)) +
  geom_point() +
  geom_vline(xintercept = cutoff, linetype = "dotted", color = "red", size = 1.0) +
  geom_smooth(data = data_before, method = "loess", se = FALSE, color = "cyan", size = 1.5) +
  geom_smooth(data = data_after, method = "loess", se = FALSE, color = "green", size = 1.5) +
  labs(title = "BTC_MKTCAP vs BTC_Price",
       x = "BTC_Price",
       y = "BTC_MKTCAP") +
  theme_minimal()
print(p5)

grid.arrange(p1, p2, p3, p4, p5,
             nrow = 2, ncol = 3)



#################
###No control linear model######################################################NON USABLE

threshold <- 29050

data <- data %>%
  mutate(before_threshold = ifelse(BTC_Price < threshold, TRUE, FALSE))


model_before <- lm(BTC_Volume ~ BTC_Price, data = filter(data, before_threshold == TRUE))
model_after <- lm(BTC_Volume ~ BTC_Price, data = filter(data, before_threshold == FALSE))

ggplot(data, aes(x = BTC_Price, y = BTC_Volume)) +
  geom_point() +
  geom_vline(xintercept = threshold, linetype = "dashed", color = "red", size = 1.5) +
  geom_smooth(data = filter(data, before_threshold == TRUE), method = "lm", formula = y ~ x, se = FALSE, color = "blue", linetype = "solid") +
  geom_smooth(data = filter(data, before_threshold == FALSE), method = "lm", formula = y ~ x, se = FALSE, color = "green", linetype = "solid") +
  labs(x = "Price", y = "Volume", title = "Bitcoin Volume vs. Price") +
  theme_minimal()

ggplot(data, aes(x = BTC_Price, y = BTC_Volume)) +
  geom_point() +
  geom_vline(xintercept = threshold, linetype = "dashed", color = "red", size = 1.5) +
  labs(x = "Price", y = "Volume", title = "Bitcoin Volume vs. Price") +
  theme_minimal()

###No control quadrat model#####################################################NON USABLE

ggplot(data, aes(x = BTC_Price, y = BTC_Volume)) +
  geom_point() +
  geom_vline(xintercept = threshold, linetype = "dashed", color = "red", size = 1.5) +
  geom_smooth(data = filter(data, before_threshold == TRUE), method = "lm", formula = y ~ I(x^2), se = FALSE, color = "blue", linetype = "solid") +
  geom_smooth(data = filter(data, before_threshold == FALSE), method = "lm", formula = y ~ I(x^2), se = FALSE, color = "green", linetype = "solid") +
  labs(x = "Price", y = "Volume", title = "Bitcoin Volume vs. Price") +
  theme_minimal()

###No control cubic model ######################################################NON USABLE

ggplot(data, aes(x = BTC_Price, y = BTC_Volume)) +
  geom_point() +
  geom_vline(xintercept = threshold, linetype = "dashed", color = "red", size = 1.5) +
  geom_smooth(data = filter(data, before_threshold == TRUE), method = "lm", formula = y ~ I(x^3), se = FALSE, color = "blue", linetype = "solid") +
  geom_smooth(data = filter(data, before_threshold == FALSE), method = "lm", formula = y ~ I(x^3), se = FALSE, color = "green", linetype = "solid") +
  labs(x = "Price", y = "Volume", title = "Bitcoin Volume vs. Price") +
  theme_minimal()

###model with all controls #####################################################


data$D <- ifelse(data$BTC_Price >= 29050, 1, 0)

model_good <- lm(BTC_Volume ~ I(BTC_Price - 29050) + D +Inflation + CVI, data = data)


model_nocontrol <- lm(BTC_Volume ~  I(BTC_Price - 29050) + D  , data = data)

model_sq <- lm(BTC_Volume ~  I((BTC_Price)^2 - 29050) + D + Inflation + CVI, data = data)

model_cub <- lm(BTC_Volume ~ I((BTC_Price)^3 - 29050)+ D + Inflation + CVI  , data = data)


modelsummary(list(model_nocontrol,model_good, model_sq, model_cub), stars=T)

threshold <- 29050



data$D <- ifelse(data$BTC_Price >= 29050, 1, 0)

model_good <- lm(BTC_Volume ~ I(poly(BTC_Price,1) - 29050) + D +Inflation + CVI, data = data)


model_nocontrol <- lm(BTC_Volume ~  I(poly(BTC_Price,1) - 29050) + D  , data = data)

model_sq <- lm(BTC_Volume ~  I(poly(BTC_Price,2) - 29050) + D + Inflation + CVI, data = data)

model_cub <- lm(BTC_Volume ~ I(poly(BTC_Price,3) - 29050)+ D + Inflation + CVI  , data = data)


modelsummary(list(model_nocontrol,model_good, model_sq, model_cub), stars=T)

threshold <- 29050

data$D <- ifelse(data$BTC_Price >= 29050, 1, 0)

model_good <- lm(BTC_Volume ~ I(poly(BTC_Price,1) - 29050) + D +Inflation + CVI , data = data)
model_nocontrol <- lm(BTC_Volume ~  I(poly(BTC_Price,1) - 29050) + D  , data = data)

model_elon <- lm(BTC_Volume ~  I(poly(BTC_Price,1) - 29050) + D +  ELON_7_22  , data = data)

model_sq <- lm(BTC_Volume ~  I(poly(BTC_Price,2) - 29050) + D + Inflation + CVI , data = data)

model_cub <- lm(BTC_Volume ~ I(poly(BTC_Price,3) - 29050)+ D + Inflation + CVI, data = data)


modelsummary(list(model_nocontrol,model_elon, model_good, model_sq, model_cub), stars=T)

threshold <- 29050
##################

###F TEST BINS##################################################################
#i would not add this since the estimated loess regression is misleadingly positive
# Define a function to create bins and compute averages
bin_data <- function(data, binwidth) {
  data %>%
    mutate(bin = cut(BTC_Price, breaks = seq(min(BTC_Price), max(BTC_Price), by = binwidth), include.lowest = TRUE, labels = FALSE)) %>%
    group_by(bin) %>%
    summarize(Bin_Midpoint = mean(BTC_Price), Avg_Volume = mean(BTC_Volume)) %>%
    na.omit()
}

# Define bin widths
binwidths <- c(500, 1000, 2000)

# Create binned data for each bin width
binned_data <- lapply(binwidths, function(bw) bin_data(data, bw))

# Plot the binned data
plot_list <- lapply(seq_along(binned_data), function(i) {
  ggplot(binned_data[[i]], aes(x = Bin_Midpoint, y = Avg_Volume)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 29050, linetype = "dashed") +
    labs(title = paste("Binned Data with Bin Width =", binwidths[i]),
         x = "BTC Price",
         y = "Average BTC Volume") +
    theme_minimal()
})

# Print plots
plot_list

# Function to conduct cross-validation F-test
conduct_f_test_bins <- function(data, binwidth) {
  data_bin <- data %>%
    mutate(bin = cut(BTC_Price, breaks = seq(min(BTC_Price), max(BTC_Price), by = binwidth), include.lowest = TRUE, labels = FALSE))
  
  # Regression with K bins
  model_k_bins <- lm(BTC_Volume ~ as.factor(bin), data = data_bin)
  
  # Regression with 2K bins
  data_bin <- data %>%
    mutate(bin = cut(BTC_Price, breaks = seq(min(BTC_Price), max(BTC_Price), by = binwidth / 2), include.lowest = TRUE, labels = FALSE))
  model_2k_bins <- lm(BTC_Volume ~ as.factor(bin), data = data_bin)
  
  f_test_result <- anova(model_k_bins, model_2k_bins)
  return(f_test_result)
}

# Conduct F-tests for different bin widths
f_test_results_bins <- lapply(binwidths, function(bw) conduct_f_test_bins(data, bw))

# Print F-test results
f_test_results_bins

# Create a data frame for F-test results
f_test_df_bins <- data.frame(
  Bin_Width = binwidths,
  F_Value = sapply(f_test_results_bins, function(res) res$F[2]),
  P_Value = sapply(f_test_results_bins, function(res) res$`Pr(>F)`[2])
)

# Print F-test results table
library(knitr)
library(kableExtra)

kable(f_test_df_bins, "html", align = c("c", "c", "c")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

###bins in the parametric models ###############################################

# Define the binning function
bin_data <- function(data, bin_width) {
  data %>%
    mutate(bin = cut(BTC_Price, breaks = seq(min(BTC_Price), max(BTC_Price), by = bin_width), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarize(BTC_Price = mean(BTC_Price, na.rm = TRUE),
              BTC_Volume = mean(BTC_Volume, na.rm = TRUE),
              D = mean(D, na.rm = TRUE),
              CVI = mean(CVI, na.rm = TRUE),
              Inflation = mean(Inflation, na.rm = TRUE),
              ELON_7_22 = mean(ELON_7_22, na.rm = TRUE),
              .groups = 'drop')
}

# Estimate models and calculate statistics
estimate_model <- function(data, formula) {
  model <- lm(formula, data = data)
  mse <- mean(model$residuals^2)
  summary_model <- summary(model)
  t_test_p_value <- summary_model$coefficients["D", "Pr(>|t|)"]
  coefficient_D <- summary_model$coefficients["D", "Estimate"]
  return(list(coefficient_D = coefficient_D, mse = mse, t_test_p_value = t_test_p_value, summary_model = summary_model))
}

# Bin widths to consider
bin_widths <- c(100, 500, 1000, 2000)

# Formulas for the models
formulas <- list(
  "lm linear Musk dummy" = BTC_Volume ~ I(BTC_Price - 29050) + D + ELON_7_22,
  "lm linear w controls" = BTC_Volume ~ I(BTC_Price - 29050) + D + CVI + Inflation,
  "lm quadratic w controls" = BTC_Volume ~ I(poly(BTC_Price,2) - 29050) + D + CVI + Inflation,
  "lm cubic w controls" = BTC_Volume ~ I(poly(BTC_Price,3) - 29050) + D + CVI + Inflation
)

# Placeholder for results
results <- data.frame(
  Model = character(),
  BinWidth = integer(),
  ATE = numeric(),
  MSE = numeric(),
  PValue = numeric(),
  stringsAsFactors = FALSE
)

# Calculate estimates for each model and bin width
for (bin_width in bin_widths) {
  binned_data <- bin_data(data, bin_width)
  for (model_name in names(formulas)) {
    formula <- formulas[[model_name]]
    estimation <- estimate_model(binned_data, formula)
    results <- rbind(results, data.frame(
      Model = model_name,
      BinWidth = bin_width,
      ATE = estimation$coefficient_D,
      MSE = estimation$mse,
      PValue = estimation$t_test_p_value
    ))
  }
}

# Print results table
results %>%
  mutate(
    ATE = round(ATE, 2),
    MSE = round(MSE, 2),
    PValue = format.pval(PValue, digits = 4)
  ) %>%
  kable("html", align = c("c", "c", "c", "c", "c"), col.names = c("Model", "Bin Width", "ATE (D)", "MSE", "P-Value")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


###############Non parametric from scratch######################################
#variables
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050)
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$ELON_7_22))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$CVI))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22))
summary(result)


#poly
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(1))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(2))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(3))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(4))
summary(result) #best but sensitive to bandwidth

#bandwidth

result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(2), rho=(0.5))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(2),  h=(500), rho=(0.5))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(2),  h=(1000), rho=(0.5))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(2),  h=(2000), rho=(0.5))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(2),  h=(5000), rho=(0.5))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(2),  h=(10000), rho=(0.5))
summary(result)
result <- rdrobust(data$BTC_Volume , data$BTC_Price, c=29050, covs=(data$Inflation + data$ELON_7_22), p=(2))
summary(result)



table <- data.frame(
  Model = c("No controls", "Musk only", "CVI only", "Inflation only", "Inflation + Musk"),
  Estimate = c("-11.27 Bln", "-9.62 Bln", "-12.43 Bln", "-11.98 Bln", "-10.60 Bln"),
  SE = c("5.26 Bln", "2.98 Bln", "1.80 Bln", "5.32 Bln", "5.44 Bln"),
  `Robust Pr>|z|` = c(0.021, 0.001, 0.541, 0.018, 0.037),
  Bandwidth = c(2560, 2838, 8452, 2478, 2421),
  `Rho (h/b)` = c(0.420, 0.435, 0.602, 0.435, 0.433),
  check.names = FALSE # Ensures column names are taken literally
)

# Create the table using kableExtra
kable(table, format = "html",align = c("c", "c", "c", "c", "c"), table.attr = "style='width:100%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "center")


table2 <- data.frame(
  Model = c("p(1)", "p(2)", "p(3)", "p(4)"),
  Estimate = c("-10.60 Bln", "-12.71 Bln", "-14.55 Bln", "-15.50 Bln"),
  SE = c("5.44 Bln", "5.74 Bln", "5.36 Bln", "5.35 Bln"),
  `Robust Pr>|z|` = c(0.037, 0.023, 0.010, 0.004),
  Bandwidth = c(2421, 4948, 8799, 13728),
  `Rho (h/b)` = c(0.433, 0.606, 0.777, 0.783),
  check.names = FALSE # Ensures column names are taken literally
)

# Create the table using kableExtra
kable(table2, format = "html",align = c("c", "c", "c", "c", "c"), table.attr = "style='width:100%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "center")


table3 <- data.frame(
  Fixed = c("Rho=0.5","Rho=0.5 and h=1000", "Rho=0.5 and h=2000", "Rho=0.5 and h=5000", "Rho=0.5 and h=10000"),
  Estimate = c("-12.71 Bln", "-12.92 Bln", "-15.00 Bln", "-12.82 Bln", "-4.32 Bln"),
  SE = c("5.74 Bln", "14.00 Bln", "9.82 Bln", "5.68 Bln", "3.33 Bln"),
  `Robust Pr>|z|` = c(0.022, 0.367, 0.137, 0.020, 0.145),
  Bandwidth = c(4248, 1000, 2000, 5000, 10000),
  check.names = FALSE # Ensures column names are taken literally
)

# Create the table using kableExtra
kable(table3, format = "html",align = c("c", "c", "c", "c", "c"), table.attr = "style='width:100%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "center")

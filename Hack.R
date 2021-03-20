# install.packages("dygraphs")

library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(dygraphs)
library(xts)

data_samsung <- read.csv("/files/all_columns.csv")

# Delete some columns we don't need
data_samsung2 <- transform(data_samsung, Day = substr(ActivityTime, 1, 10), Time = substr(ActivityTime, 12, 26))
data_samsung2 <- select(data_samsung2, -ActivityTime, -Time, -LabelsJson, -ScreenshotFindingsJson, -X.No.column.name.)
data_samsung2$Day <- ymd(data_samsung2$Day)

# choose user
user <- "512F6A289EFC189D12B7F1A763A0DE7B"

# get data of chosen user
user_data <- filter(data_samsung2, UserId == user)
user_data <- add_column(user_data, 1)
user_data$activities_day <- ave(seq_along(user_data$Day), user_data$Day, FUN = length)

# filter unique dates in order to plot the data
unique_date <- duplicated(user_data$Day)
plot_data <- user_data[!unique_date,]

# add missing date values
plot_data <- complete(plot_data, Day = seq.Date(min(Day), max(Day), by="day")) 
plot_data$activities_day[is.na(plot_data$activities_day)] <- 0


# # plot activity level -> how many entries were there on one day
# ggplot(plot_data, aes(x = Day, y = activities_day)) +
#   geom_bar(stat="identity", fill = "indianred3") +
#   theme_classic() 
# 


# creating a function to calculate the moving average
movingaverage <- function(activecount, N) {
  # activecount: 
  # N: parameter that controls the length of the moving average window
  MA <- rep(NA, length(activecount)) #create an empty container
  
  #within the loop we want to access the activity level from i to i-N+1
  for (i in N:length(activecount)){
    MA[i] <- sum(activecount[(i-N+1):i])/N
  }
  return(MA)
}

# Calculating the moving average for N days:
N = 12
movingaverage(plot_data$activities_day, N)


#checking if the function equals 5.229239e-02 for priceUsa
plot_data$momentum <- tsmomentum(movingaverage(plot_data$activities_day, N), 14)

plot_data$alert <- ifelse(plot_data$momentum<= -0.8,1,0)


# Use time series to track activity level over time
don <- xts(x = plot_data$activities_day, order.by = plot_data$Day)

p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A", connectSeparatedPoints = TRUE) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 14) %>%
  dyEvent(plot_data[which(plot_data$alert==1),]$Day, color = "grey",)
p



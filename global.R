#import libs
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(scales)
library(glue)
library(GGally)
library(lubridate)

stock <- read_csv("data_input/stock.csv")

# tail(stock)
# 
stock$Date <- as.Date(stock$Date)

label_percent <- label_dollar(suffix = '%' ,prefix = '')

label_dollars <- label_dollar()

# stock[]
stock_monthly <- stock %>%
  mutate(pct_change = (stock$SP500/lag(stock$SP500) - 1) * 100, month = format(stock$Date, "%m")) %>%
  group_by(month) %>%
  summarise(avg.change = round(mean(pct_change, na.rm = TRUE), digits=3)) %>%
  mutate(tooltip = glue("percent_change: {label_percent(avg.change)}")) %>%
  ungroup()

stock_monthly %>% head()

ggcorr(stock)

#monthly chart data
monthly_chart <- ggplot(stock_monthly, aes(x = avg.change, y = month,  text = tooltip)) +
  geom_col(aes(fill = avg.change), show.legend = F) +
  labs(title = "S&P500 return monthly",
       subtitle = "For the period 01/01/1871 - 04/01/2018",
       x = "Price change (in %)",
       y = "Month",
       caption = "Source: Robert Shiller"
  ) +
  scale_x_continuous(labels = label_percent) +
  guides(fill= FALSE) +
  geom_label(aes(label = avg.change), hjust = 1.05) +
  scale_fill_continuous(low = "red", high = "black") +
  theme_minimal()

ggplotly(monthly_chart, tooltip = c("text")) %>%
  config(displayModeBar = F, scrollzoom = T) %>%
  layout(dragmode = "pan")



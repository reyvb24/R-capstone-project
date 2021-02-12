server <- function(input,output){
  output$stockChart <- renderPlotly({
    data <- stock %>% 
      filter(as.POSIXct(Date)>=format(input$chooseYear[1]) & as.POSIXct(Date)<=format(input$chooseYear[2]))
    label_dollar <- label_dollar(prefix = '$')
    p <- ggplot(data, aes(x = Date,y = SP500))+
      geom_line()+
      scale_y_continuous(sec.axis = sec_axis(trans = ~.*5,
                                             name = "S&P500 Price (01/01/1871 - 04/01/2018)"),
                         labels = label_dollar
      ) +
      labs(title = "S&P500 stock market index historical price",
           subtitle = "(in USD)",
           x = "Date",
           y = "Price",
           caption = "Based on the data provided by Prof. Robert Shiller"
           )
    
    ggplotly(p)
  })
  
  output$return <- renderValueBox({
    percentage <- ((stock %>% filter(Date <= input$chooseYear[2]) %>% tail(1) %>% select(SP500)/stock %>% filter(Date >= input$chooseYear[1]) %>% head(1) %>% select(SP500) - 1) * 100) %>% round(2) %>% select(SP500)
    valueBox(
      label_percent(percentage[1,1]),
      subtitle = "You would have returned this much if you have invested your money during the time period (in percent)")
    
  })
  
  output$return_money <- renderValueBox({
    percentage <- ((stock %>% filter(Date <= input$chooseYear[2]) %>% tail(1) %>% select(SP500)/stock %>% filter(Date >= input$chooseYear[1]) %>% head(1) %>% select(SP500) - 1) * 100) %>% round(2) %>% select(SP500)
    valueBox(
      label_dollars(percentage[1,1]*10000),
      subtitle = "You would have returned this much if you have invested $10,000 during the time period",
      color = "orange"
    )
    
  })
  
  output$return_compound <- renderValueBox({
    percentage <- (((stock %>% filter(Date <= input$chooseYear[2]) %>% tail(1) %>% select(SP500)/stock %>% filter(Date >= input$chooseYear[1]) %>% head(1) %>% select(SP500))^(1/(as.numeric((lubridate::decimal_date(input$chooseYear[2])-lubridate::decimal_date(input$chooseYear[1])))))-1) * 100) %>% round(2) %>% select(SP500)
    valueBox(
      label_percent(percentage[1,1]),
      subtitle = "The annual compounded growth rate of S&P500 index during the time period",
      color = "red"
    )
  })
  
  output$adjustedStock <- renderPlotly({
    adjusted <- stock %>% 
      filter(as.POSIXct(Date)>=format(input$chooseYear[1]) & as.POSIXct(Date)<=format(input$chooseYear[2]))
    names(adjusted) <- str_replace_all(names(adjusted), c(" " = "_" , "," = "" ))
    label_dollar <- label_dollar(prefix = '$')
    p <- ggplot(adjusted, aes(x = Date,y = Real_Price))+
      geom_line()+
      scale_y_continuous(sec.axis = sec_axis(trans = ~.*5,
                                             name = "S&P500 Real price (01/01/1871 - 04/01/2018)"),
                         labels = label_dollar
      ) +
      labs(title = "S&P500 stock market index historical price (adjusted)",
           subtitle = "(adjusted to inflation in USD)",
           x = "Date",
           y = "Price",
           caption = "Based on the data provided by Prof. Robert Shiller"
      )
    
    ggplotly(p)
  })

  
  
  output$plot_monthly <- renderPlotly({
    label_percent <- label_dollar(suffix = '%' ,prefix = '')
    
    monthly_change <- stock %>% 
      mutate(pct_change = (SP500/lag(SP500) - 1) * 100, month = format(stock$Date, "%m"))
  
    stock_monthly <- monthly_change %>% 
      filter(as.POSIXct(Date) >= as.POSIXct(input$chooseYearMonthly)) %>%
      group_by(month) %>%
      summarise(avg.change = round(mean(pct_change, na.rm = TRUE), digits=3)) %>%
      mutate(tooltip = glue("percent_change: {label_percent(avg.change)}"), upDown = as.factor(ifelse(avg.change>=0, 1, 0))) %>%
      ungroup()
    
    monthly_chart <- ggplot(stock_monthly, aes(x = month, y = avg.change,  text = tooltip, fill = upDown)) +
                              geom_col(position = "identity", show.legend = F) +
                              labs(title = "S&P500 return monthly",
                                   subtitle = "For the period {input$chooseYearMonthly} - 04/01/2018",
                                   x = "Month",
                                   y = "Price change (in %)",
                                   caption = "Source: Robert Shiller"
                                   ) +
                              scale_fill_manual(values = c("#FF0000", "#228B22"), guide = FALSE) +
                              guides(fill= FALSE) +
                              geom_label(aes(label = avg.change), hjust = 1.05) +
                              theme_minimal()
    
    ggplotly(monthly_chart, tooltip = c("text"))
  })
  
  output$real_plot_monthly <- renderPlotly({
    label_percent <- label_dollar(suffix = '%' ,prefix = '')
    
    names(stock) <-str_replace_all(names(stock), c(" " = "_" , "," = "" ))
    
    monthly_change <- stock %>% 
      mutate(pct_change = (Real_Price/lag(Real_Price) - 1) * 100, month = format(stock$Date, "%m"))
    
    stock_monthly <- monthly_change %>% 
      filter(as.POSIXct(Date) >= as.POSIXct(input$chooseYearMonthly)) %>%
      group_by(month) %>%
      summarise(avg.change = round(mean(pct_change, na.rm = TRUE), digits=3)) %>%
      mutate(tooltip = glue("percent_change: {label_percent(avg.change)}"), upDown = as.factor(ifelse(avg.change>=0, 1, 0))) %>%
      ungroup()
    
    monthly_chart <- ggplot(stock_monthly, aes(x = month, y = avg.change,  text = tooltip, fill = upDown)) +
      geom_col(position = "identity", show.legend = F) +
      labs(title = "S&P500 return monthly when adjusted to inflation",
           subtitle = "For the period {input$chooseYearMonthly} - 04/01/2018",
           x = "Month",
           y = "Price change (in %)",
           caption = "Source: Robert Shiller"
      ) +
      scale_fill_manual(values = c("#FF0000", "#228B22"), guide = FALSE) +
      guides(fill= FALSE) +
      geom_label(aes(label = avg.change), hjust = 1.05) +
      theme_minimal()
    
    ggplotly(monthly_chart, tooltip = c("text"))
  })
  
  output$yield <- renderPlotly({
    names(stock) <- str_replace_all(names(stock), c(" " = "_" , "," = "" ))
    dividend <- stock %>% 
      filter(as.POSIXct(Date) >= as.Date(input$chooseYearDividend))
    yield <- stock %>% 
      mutate(yield_stock = Dividend/SP500*100, yield_real = Real_Dividend/Real_Price*100) %>% 
      filter(as.POSIXct(Date) >= as.POSIXct(input$chooseYearDividend))
    yield_plot <- ggplot(yield, aes(x = Date)) +
                           geom_line(aes(y = yield_stock), color = "darkred") +
                           labs(title = "Dividend yield of S&P500",
                                x = "date",
                                y = "dividend yield",
                                caption = "Source: Robert Shiller"
                           ) +
                           scale_y_continuous(labels = label_percent)
    ggplotly(yield_plot)
  })
  output$dividends <- renderPlotly({
    names(stock) <- str_replace_all(names(stock), c(" " = "_" , "," = "" ))
    dividend <- stock %>% 
      filter(as.POSIXct(Date) >= as.Date(input$chooseYearDividend))
    dividends <- stock %>% 
      filter(as.POSIXct(Date) >= as.POSIXct(input$chooseYearDividend)) %>% 
      select(Date, Dividend, Real_Dividend) %>%
      gather(key = "variable", value = "value", -Date)
    dividends_plot <- ggplot(dividends, aes(x = Date, y = value)) +
      geom_line(aes(color = variable)) +
      scale_color_manual(values = c("darkred", "steelblue")) +
      labs(title = "Dividend and adjusted dividend of S&P500",
           subtitle = "Dividend is indicated in red, while the one adjusted to inflation in blue",
           x = "date",
           y = "dividend (in $)",
           caption = "Source: Robert Shiller"
      ) +
      scale_y_continuous(labels = label_dollar())
    ggplotly(dividends_plot)
  })
  
  output$custom <- renderPlotly({
    # if (length(input$customSelect) > 0) {
    #   custom <- stock %>% 
    #     select(c(input$customSelect, Date)) %>% 
    #     gather(key = "variable", value = "value", -Date)
    #   custom_plot <- ggplot(custom, aes(x = Date, y = value)) +
    #     scale_color_manual(values = c("darkred", "steelblue"))
    #   
    #   ggplotly(custom_plot)
    # }
    stock_mutate <- stock %>% mutate(input = input$customSelect)
    custom_plot <- ggplot(stock_mutate, aes(x = Date, y = input)) +
      geom_line()
    
    ggplotly(custom_plot)
  })
  
  output$txt <- renderText({
    paste("You chose", input$customSelect)
  })
  
  output$table <- renderDataTable({
    stock <- stock %>% mutate(Date = format(as.Date(Date)))
    
    stock
  })
  
}
# ui.R
header <- dashboardHeader(
  title = "S&P500 analysis"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "S&P500 line graph",tabName = "pageOne"),
    menuItem(text = "Monthly returns",tabName = "pageTwo"),
    menuItem(text = "Dividends", tabName = "pageThree"),
    # menuItem(text = "Custom plot", tabName = "pageFive"),
    menuItem(text = "Raw data", tabName = "pageFour")
  )
)
body <- dashboardBody(
    tabItems(
      tabItem(
        tabName = "pageOne", 
        fluidPage(
          setBackgroundColor(
            color = "#fefeff",
            shinydashboard = T
          ),
          fluidRow(
            column(width = 12,
                   titlePanel(title = tags$b("S&P 500 Index")),
                   align = "center"
                   )
          ),
          tags$br(),
          tags$br(),
          
          column(tags$h4("The S&P500 index is a stock market index that measures the stock performance of the top 500 leading companies listed on the United States stock exchange.
             It is deemed as one of the world's leading stock indicator due to its historical returns, supplemented with the fact that it is continuously being sustained by the largest public enterprises in the world.
             The current top 10 most enormous companies account for 27.5% of its total market capitalization, with most of them being the leading firms in their own industries respectively on a global scale, such as Apple, Alphabet, Tesla, Berkshire Hathaway, and more.
             Index funds that track the movement of the S&P500 have long been recommended by legendary investors such as Warren Buffett, as it mitigates the risk in a client's portfolio, while being able to generate sustainable, yet satisfactory returns.", style = "color:#585e70"),
                 align = "justify",
                 width = 12,
                 ),
          fluidRow(
          ),
          div(style =  "padding: 2% 0; margin: 0 auto; width: 80%;",
              dateRangeInput(
                inputId = "chooseYear",
                label = "Choose the starting and ending date:",
                min = stock$Date %>% 
                        subset(format.Date(stock$Date, "%m") == "01") %>% 
                        head(1),
                max = stock$Date %>% 
                        subset(format.Date(stock$Date, "%m") == "04") %>% 
                        tail(1),
                start = "1871-01-01",
                end = "2018-04-01",
                # value = as.Date(stock$Date %>% head(1)),
                # timeFormat = "%Y",
                # animate = T,
                width = '100%'
                )
              ),
          fluidRow(
            column(width = 6,
                   align = "justify",
                   plotlyOutput(outputId = "stockChart"),
                   ),
            column(width = 6,
                 align = "justify",
                 plotlyOutput(outputId = "adjustedStock")
            )
          ),
          tags$br(),
          fluidRow(
            
            valueBoxOutput("return"),
            valueBoxOutput("return_money"),
            valueBoxOutput("return_compound")
          )
        )
      ),
      tabItem(
        tabName = "pageTwo",
        fluidPage(
          fluidRow(
            column(width = 12,
                   titlePanel(title = tags$b("Monthly returns")),
                   align = "center"
            )
            
          ),
          tags$br(),
          tags$br(),
          column(tags$h4("The following are bar charts which evaluate the S&P500 index return monthly between the user selected input and the final date of record. 
                         The objective of this measure is to give investors insights on what to expect on a given month on S&P500 based on its historical performance."),
                 align = "justify",
                 width = 12),
          fluidRow(),
          div(style =  "padding: 2% 0; margin: 0 auto; width: 80%;",
            sliderInput(
              inputId = "chooseYearMonthly",
              label = "Choose the starting year:",
              min = stock$Date %>% 
                subset(format.Date(stock$Date, "%m") == "01") %>% 
                head(1),
              max = stock$Date %>% 
                subset(format.Date(stock$Date, "%m") == "01") %>% 
                tail(1),
              value = as.Date(stock$Date %>% head(1)),
              timeFormat = "%Y",
              animate = T,
              width = '100%'
            )
          ),
          fluidRow(
            column(plotlyOutput(outputId = "plot_monthly"),
                   width = 6),
            column(plotlyOutput(outputId = "real_plot_monthly"),
                   width = 6)
          )
        )
      ),
      tabItem(
        tabName = "pageThree",
        fluidPage(
          fluidRow(
            column(width = 12,
                   titlePanel(title = tags$b("S&P500 Dividend")),
                   align = "center"
            )
            
          ),
          tags$br(),
          br(),
          column(
            tags$h4("Dividend is defined as the distribution of some portion of a company's earnings towards its shareholders as determined by the company's board of directors. 
                    In this context,the S&P 500 dividend mirrors the average dividend the companies included in the index share out to their shareholder annually."),
            width = 12,
            algin = "justify"
          ),
          div(style =  "padding: 2% 0; margin: 0 auto; width: 80%;",
              sliderInput(
                inputId = "chooseYearDividend",
                label = "Choose the starting year:",
                min = stock$Date %>% 
                  subset(format.Date(stock$Date, "%m") == "01") %>% 
                  head(1),
                max = stock$Date %>% 
                  subset(format.Date(stock$Date, "%m") == "01") %>% 
                  tail(1),
                value = as.Date(stock$Date %>% head(1)),
                timeFormat = "%Y",
                animate = T,
                width = '100%'
              )
          ),
          fluidRow(),
          fluidRow(
            column(
              plotlyOutput(outputId = "yield"),
              width = 6
            ),
            column(
              plotlyOutput(outputId = "dividends"),
              width = 6
            )
          )
        )
      ),
      tabItem(
        tabName = "pageFive",
        fluidPage(
          fluidRow(
            column(width = 12,
                   titlePanel(title = tags$b("Custom plot")),
                   align = "center"
            )
            
          ),
          fluidRow(
            column(width = 8,
                   plotlyOutput(outputId = "custom")
            ),
            column(width = 4,
                   fluidRow(
                     selectInput(
                       inputId = "customSelect",
                       label = "Select the data which you would like to plot:",
                       choices = unique(stock) %>% select(-Date),
                       # choiceValues = list(
                       #   "sp500", "dividend", "earning", "cpi", "lir", "rp", "rd", "re", "pe"
                       # )
                      )
                   ),
                   fluidRow(
                     textOutput(outputId = "txt")
                   )
            )
          )
        )
      ),
      tabItem(
        tabName = "pageFour",
        fluidPage(
          dataTableOutput(outputId = "table"),
        )
      ),
      tabItem(
        tabName = "pageSix",
        fluidPage(
          
        )
      )
  )
)

ui<- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)


# server <- function(input, output) {
#   output$plot1 <- renderPlot({
#     data <- price[seq_len(input$slider)]
#     hist(data)
#   })
# }

# shinyApp(ui, server)


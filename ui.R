library(shiny)
library(shinydashboard)
library(shinycssloaders)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "Revenue Forecasting", disable = TRUE),
    dashboardSidebar(
      tags$style(HTML(".main-sidebar li a { font-size: 20px; }")),
      menuItem("Revenue Forecasting", tabName = "Revenue Forecasting", icon = icon("dashboard")),
          sliderInput("month_forecast", "Months To Forecast", 2, 24, 12),
          actionButton("exportButton", "Generate Report", width="88%")
      ),
    dashboardBody(
      tabsetPanel(id="tab",
                  tabPanel("FB Prophet Automatic Forecast", withSpinner(plotOutput("prophet_plot")), br(), DT::dataTableOutput("sales_table_prophet")),
                  tabPanel("ARIMA Forecast", withSpinner(plotOutput("arima_plot")), br(), DT::dataTableOutput("arima_table")),
                  tabPanel("Holt-Winters Forecast (Additive)", withSpinner(plotOutput("hw_additive_plot")), br(), DT::dataTableOutput("hwa_table")),
                  tabPanel("Holt-Winters Forecast (Multiplicative)", withSpinner(plotOutput("hw_multiplcative_plot")), br(), DT::dataTableOutput("hwm_table")),
                  tabPanel("Holt-Winters Forecast (Damped)", withSpinner(plotOutput("hw_damped_plot")), br(), DT::dataTableOutput("hwd_table"))
      )
    )
)




library(shiny)
library(zoo)
library(DT)
library(ggplot2)
library(DBI)
library(forecast)
library(datasets)
library(lubridate)
library(reshape2)
library(dplyr)
library(TSA)
library(tseries)
library(MASS)
library(scales)
library(prophet)
options(stringsAsFactors=FALSE)

sqlQuery <- function (query) {
  # Connection to db
  conn <- dbConnect(RMySQL::MySQL(), 
                    dbname="super_data", 
                    host="34.87.121.216", 
                    user="root", 
                    password="superdata123")
  # Close db connection after function call exits
  on.exit(dbDisconnect(conn))
  
  rs <- dbGetQuery(conn, query)
  return(rs)
}

shinyServer(function(input, output, session) {
    # connect to database
    orders <- sqlQuery("select PDTID, NETPRICE, PCHASEDATE from orders")
    products <- sqlQuery("select PDTID, CSTPRICE from product")

    # merge price with the order data
    df <- orders[!format(as_date(orders$PCHASEDATE), "%Y") == "-001",]
    df <- merge(df, products,by="PDTID",all.x = TRUE)
    df$Revenue <- df$NETPRICE - df$CSTPRICE
    
    # convert to monthly
    monthly_df <- df %>% group_by(Time=floor_date(as_date(df$PCHASEDATE), "month")) %>%
      summarize(Revenue=sum(Revenue, na.rm = TRUE))

    # convert to time series
    tsData <- ts(monthly_df$Revenue, start=c(year(monthly_df$Time[1]), month(monthly_df$Time[1])), frequency=12)
    
    
    #=============================FB Prophet=============================
    output$prophet_plot <- renderPlot({
      
      # convert to Date series
      monthly_df$Date <- as_date(monthly_df$Time)
      df2_prophet <-
        data.frame(Date = seq(min(monthly_df$Date), max(monthly_df$Date), by = "month"))
      df1_prophet <- data.frame(monthly_df[, c("Date", "Revenue")])
      
      df_final_prophet <- dplyr::right_join(df1_prophet, df2_prophet)
      if (sum(is.na(df_final_prophet$Revenue)) > 0) {
        df_final_prophet$Revenue[is.na(df_final_prophet$Revenue)] <- 0
      }
      
      df_prophet <- df_final_prophet[, c("Date", "Revenue")]
      colnames(df_prophet) <-   c("ds", "y")
      df_prophet$ds <-
        as.character(df_prophet$ds)
      df_prophet <- data.frame(df_prophet)
      
      # fit prophet model
      m <- prophet(df_prophet, seasonality.mode = 'multiplicative')
      
      # make predictions
      forecast_data_prophet <-
        make_future_dataframe(m,
                              freq = "month",
                              periods  = as.numeric(input$month_forecast))
      fcst <- predict(m, forecast_data_prophet)
      
      # plot the model
      forecast_df_prophet <- data.frame(Date = fcst$ds, Revenue = fcst$yhat)
      forecast_df_prophet <- tail(forecast_df_prophet, n = input$month_forecast)
      render_df_prophet <- rbind(df_final_prophet, forecast_df_prophet)
      render_df_prophet$Date <- format(render_df_prophet$Date, format = "%b-%Y")
      
      rownames(render_df_prophet) <- NULL
      numeric_columns <- sapply(render_df_prophet, mode) == 'numeric'
      render_df_prophet[numeric_columns] <-  round(render_df_prophet[numeric_columns], 2)
      prophet_set <<- render_df_prophet
      
      forecast_df_prophet <- data.frame(Date = fcst$ds, Revenue = fcst$yhat)
      forecast_df_prophet <- tail(forecast_df_prophet, n = input$month_forecast)
      forecast_df_prophet$Date <- as_date(forecast_df_prophet$Date)
      
      ggplot() +
        geom_line(data = df_final_prophet, aes(x = Date, y = Revenue)) +
        geom_line(data = forecast_df_prophet, aes(x = Date, y = Revenue, color = "Prophet Forecast")) +
        xlab("Date") +
        ylab("Revenue") +
        ggtitle("Product Sales") +
        guides(colour = guide_legend(title = "Monthly Revenue Forecast")) +
        theme_classic() + theme(legend.position = "bottom")
    })
    
    output$sales_table_prophet <- DT::renderDT({
      prophet_set
    }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    
    
    # plot arima
    output$arima_plot <- renderPlot(
        {
            # fit arima model
            fitted_final_model <- Arima(tsData,order=c(1,0,0),seasonal=list(order=c(0,1,2),period=12),lambda=0)
            
            # plot the model
            forecast_data <- forecast(fitted_final_model, h = input$month_forecast)
            
            # Render the table
            originial_df <- data.frame(Date=as.Date(as.yearmon(time(tsData))),Revenue=as.matrix(tsData))
            df <- data.frame(forecast_data)
            df$Date <- rownames(df)
            df <- df[,c("Date","Point.Forecast")]
            df$Date <- paste(df$Date,"01")
            df$Date <- as.Date(df$Date,"%b %Y %d")
            colnames(df)[2] <- "Revenue"
            render_df <- rbind(originial_df, df)
            render_df$Date <- format(render_df$Date,"%b-%Y")
            
            # Edit rownames
            rownames(render_df) <- NULL
            render_df$Revenue <- format(round(render_df$Revenue, 2), nsmall = 2)
            
            arima_set <<- render_df
            
            # plot the model
            autoplot(tsData) +
              autolayer(forecast_data, series="ARIMA Forecast") +
              xlab("Time") +
              ylab("Revenue") +
              ggtitle("Revenue Forecast") +
              guides(colour=guide_legend(title="Monthly Revenue Forecast"))+theme_classic()+theme(
                legend.position = "bottom"
              )+scale_y_continuous(labels=dollar_format(prefix="$")) 
            
             }
    )
    
    output$arima_table <- renderDataTable({
      arima_set
    }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    
    # plot hw additive
    output$hw_additive_plot <- renderPlot({
        # fit model
        fit1_model <- HoltWinters(tsData,seasonal="additive")
        # do forecasting
        fit1 <- forecast(fit1_model, h = input$month_forecast)
        forecast_data <- fit1
        
        # plot the forecast data
        originial_df <- data.frame(Date=as.Date(as.yearmon(time(tsData))),Revenue=as.matrix(tsData))
        df <- data.frame(forecast_data)
        df$Date <- rownames(df)
        df <- df[,c("Date","Point.Forecast")]
        df$Date <- paste(df$Date,"01")
        df$Date <- as.Date(df$Date,"%b %Y %d")
        colnames(df)[2] <- "Revenue"
        render_df <- rbind(originial_df, df)
        render_df$Date <- format(render_df$Date,"%b-%Y")
        
        # Edit rownames
        rownames(render_df) <- NULL
        render_df$Revenue <- format(round(render_df$Revenue, 2), nsmall = 2)
        
        hwa_set <<- render_df
        
        # plot the model
        autoplot(tsData) +
            autolayer(fit1, series="HW Additive Forecast") +
            xlab("Time") +
            ylab("Revenue") +
            ggtitle("Revenue Forecast") +
            guides(colour=guide_legend(title="Monthly Revenue Forecast"))+theme_classic()+theme(
                legend.position = "bottom"
            )+scale_y_continuous(labels=dollar_format(prefix="$")) 

    })
    
    output$hwa_table <- renderDataTable({
      hwa_set
    }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    
    # plot hw multiplicateive
    output$hw_multiplcative_plot <- renderPlot({
        # fit the model
        fit2_model <- HoltWinters(tsData,seasonal="multiplicative")
        # do forecast
        fit2 <- forecast(fit2_model, h = input$month_forecast)
        forecast_data <- fit2
        
        # plot the forecast table
        originial_df <- data.frame(Date=as.Date(as.yearmon(time(tsData))),Revenue=as.matrix(tsData))
        df <- data.frame(forecast_data)
        df$Date <- rownames(df)
        df <- df[,c("Date","Point.Forecast")]
        df$Date <- paste(df$Date,"01")
        df$Date <- as.Date(df$Date,"%b %Y %d")
        colnames(df)[2] <- "Revenue"
        render_df <- rbind(originial_df, df)
        render_df$Date <- format(render_df$Date,"%b-%Y")
        
        # Edit rownames
        rownames(render_df) <- NULL
        render_df$Revenue <- format(round(render_df$Revenue, 2), nsmall = 2)
        
        hwm_set <<- render_df
        
        # plot the forecast
        autoplot(tsData) +
            autolayer(fit2, series="HW Multiplicative Forecast") +
            xlab("Time") +
            ylab("Revenue") +
            ggtitle("Revenue Forecast") +
            guides(colour=guide_legend(title="Monthly Revenue Forecast"))+theme_classic()+theme(
                legend.position = "bottom"
            )+scale_y_continuous(labels=dollar_format(prefix="$")) 
        
     
    })
    
    output$hwm_table <- renderDataTable({
      hwm_set
    }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    
    # hw damped forecast
    output$hw_damped_plot <- renderPlot({
        # fit model
        fc_model <- hw(tsData, damped = TRUE, seasonal="multiplicative")
        # do forecasting
        fc <- forecast(fc_model,h= input$month_forecast)
        forecast_data <- fc
        
        # forecast table
        originial_df <- data.frame(Date=as.Date(as.yearmon(time(tsData))),Revenue=as.matrix(tsData))
        df <- data.frame(forecast_data)
        df$Date <- rownames(df)
        df <- df[,c("Date","Point.Forecast")]
        df$Date <- paste(df$Date,"01")
        df$Date <- as.Date(df$Date,"%b %Y %d")
        colnames(df)[2] <- "Revenue"
        render_df <- rbind(originial_df, df)
        render_df$Date <- format(render_df$Date,"%b-%Y")
        
        # Edit rownames
        rownames(render_df) <- NULL
        numeric_columns <- sapply(render_df, mode) == 'numeric'
        render_df[numeric_columns] <-  round(render_df[numeric_columns], 2)
        
        hwd_set <<- render_df
        colMax <- max(render_df$Revenue, na.rm = TRUE)
        colMin <- min(render_df$Revenue, na.rm = TRUE)
        
        # plot forecasting
        autoplot(tsData) +
            autolayer(fc, series="HW Damped Forecast")+
            guides(colour=guide_legend(title="Monthly Revenue Forecast"))+theme_classic()+theme(
                legend.position = "bottom"
            ) +xlab("Time") +
          ggtitle("Revenue Forecast") +
          ylab("Revenue") +scale_y_continuous(labels=dollar_format(prefix="$")) + ylim(colMin, colMax)
        
    })
    
    output$hwd_table <- renderDataTable({
      hwd_set
    }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
 
    # To upload table to the database
    observeEvent(input$exportButton, {
      # Check which tab is currently active
      method <- input$tab
      months_predicted <- input$month_forecast
      
      if (input$tab == "ARIMA Forecast") {
        currentTable <- arima_set
        
      } else if (input$tab == "Holt-Winters Forecast (Additive)") {
        currentTable <- hwa_set

      } else if (input$tab == "Holt-Winters Forecast (Multiplicative)"){
        currentTable <- hwm_set
        
      } else if (input$tab == "FB Prophet Automatic Forecast"){
        currentTable <- prophet_set
        
      } else {
        currentTable <- hwd_set
      }
      
      #print(currentTable)
      print(method)
      
      # Format data table timesteps and revenue columns for exporting to db
      timestep <- ""
      revenue <- ""
      
      for (row in 1:nrow(currentTable)) {
          timestep <- paste(timestep, currentTable[row, "Date"], sep="|")
          revenue <- paste(revenue, currentTable[row, "Revenue"], sep="|")
      }
      
      # Prepare sql statement for insert db
      sql <- "INSERT INTO revenue_forecast(METHOD, MONTHS_PREDICTED, TIMESTEP, REVENUE) VALUES("
      sql <- paste(sql, method, sep="'")
      sql <- paste(sql, months_predicted, sep="', '")
      sql <- paste(sql, timestep, sep="', '")
      sql <- paste(sql, revenue, sep="', '")
      sql <- paste(sql, "');", sep="")
      print(sql)
      
      sqlQuery(sql)
    
      showModal(modalDialog(
        title = input$tab,
        paste0("Successfully exported all information to the database!"),
        easyClose = TRUE,
        footer = NULL
      ))
      
    })

})

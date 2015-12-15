# title: "Shiny server"
# author: "Jason Taylor"

# todos:
# - create .RData files for earnings dates add to options.data package
# - output section cleanup and organization
# - change envir .Global to current shiny envir
# - move commissions to function for resusability
# - work on earnings dates trade entry requirement
# - create ability to have stock be ALL
# - grab ALL Stock list from the available choices, it is hard coded now
# - add stock name to results table strangle done, need to add others
# - add beep at the end install.packages("beepr")
# - use semi-join rather than merge?
# - add percentage winners to graph as an axis
# - Format the HTML at top of chart 
# - add stock name to the results and results table because of all run
# - change the icons for the dashboard

# shinyServer function used to run application
shinyServer(function(input, output, session) {
  # Uncomment next line for debugging to console
  # options(shiny.trace = TRUE)
  # debug
  # Rprof("boot.out")
  # Rprof(NULL)
  # Reactive section for building executed trade list
  # debug
  beep()
  
  observe({
    input$stock
    isolate({
      print("31")
      if (exists("results", envir = .GlobalEnv) && is.data.frame(get("results"))) {
        rm(results, envir = .GlobalEnv)
      }
    })
  })
  # end debug
  
  trades <- reactive({
    input$goPlot  # This enables the script to run once prior to clicking run
    isolate( {# Isolate the expensive code to only run when Go button clicked
      withProgress(message = "Progress Bar", detail = "Setting up study", value = .05, {
        t <- 0 # Set inital trade number to zero
        progress.int <- .001 # Set progress bar increment amount
        
        # Values defined by the customer in shiny ui
        assign("study", input$study, envir = .GlobalEnv)
        assign("stock", input$stock, envir = .GlobalEnv)
        assign("low.iv", input$open.ivrank[1], envir = .GlobalEnv)
        assign("high.iv", input$open.ivrank[2], envir = .GlobalEnv)
        assign("o.dte", input$open.dte, envir = .GlobalEnv)
        assign("s.dte", input$second.dte, envir = .GlobalEnv)
        assign("c.delta", input$call.delta, envir = .GlobalEnv)
        assign("p.delta", input$put.delta, envir = .GlobalEnv)
        assign("prof.targ", input$proftarg / 100, envir = .GlobalEnv)
        assign("loss.lim", input$loss.lim + 1, envir = .GlobalEnv)
        assign("l.loss.lim", input$l.loss.lim / 100, envir = .GlobalEnv)
        assign("openOption", input$openOption, envir = .GlobalEnv)
        assign("g", input$gamma.days, envir = .GlobalEnv)
        assign("earn.close", input$earn.close, envir = .GlobalEnv)
        assign("min.roc", input$min.roc, envir = .GlobalEnv)
        assign("p.delta.lim", p.delta + .1, envir = .GlobalEnv)
        assign("c.delta.lim", c.delta - .1, envir = .GlobalEnv)
        # debug
        assign("stock.list", as.data.frame(c("AMZN", "EEM", "EWZ", "FXI", "GDX",
                                             "GS", "IBM", "SLV", "XLE"),
                                           stringsAsFactors = FALSE),
               envir = .GlobalEnv)
        # end debug
        # Load option chain data for stock chosen by customer
        if (!stock == "ALL") {
          data(list = paste0(stock, ".options"))
        } 
        
        # Opening frequency
        if (openOption == "First of Month") {
          # Find the First trading day of the month dates
          data(list = "open.first.day.month")
          assign("first.day", open.first.day.month, envir = .GlobalEnv)
        } else if (openOption == "First of Week") {
          # Find the First trading day of the week dates
          data(list = "open.first.day.week")
          assign("first.day", open.first.day.week, envir = .GlobalEnv)
        } else if (openOption == "Daily") {
          # Find each unique possible trading date in underlying chosen to perform study daily
          data(list = "open.daily")
          assign("first.day", open.daily, envir = .GlobalEnv)
        } else if (openOption == "Earnings") {
          # Find each unique possible trading date in underlying chosen to perform study for earnings
          data(list = "earnings.dates")
        } else if (openOption == "Previous Close") {
          # Use custom dates normally chosen as the close of prior trades to open new ones
          # Fill in the custom dates .csv for this
          # source("Shared/customopen.R")
        }
        
        # Close prior to earnings?
        if (earn.close == "Yes")  {
          # Find the earnings dates for this underlying for possible close dates
          source("Shared/earningsdates.R")
        }
        
      }) # End setting up studies progress bar
      if (study == "Call Calendar") {
        call.calendar(progress.int, t)
      } # End Calendar Spread Strategy
      if (study == "Poor Mans Cov Call") {
        pmcc(progress.int, t)
      } # End Poor Man's Covered Call Strategy
      if (study == "Short Put") {
        short.put(progress.int, t)
      } # End Short Put Strategy
      if (study == "Long Stock") {
        LongStock(progress.int, t)
      } # End Long Stock Strategy
      if (study == "Strangle") {
        if (stock == "ALL") {
          for (i in 1:nrow(stock.list)) {
            data(list = paste0(stock.list[i, ], ".options"))
            strangle(progress.int, t)
          }
        }
        else {
          strangle(progress.int, t)
        }
      } # End Strangle Study
      if (study == "Straddle") {
        straddle(progress.int, t)
      } # End Straddle Study
      # Output totals to results panel
      # Use the current environment when calling the HTML output function for scoping
      environment(outputHTML) <- environment()
      outputHTML()
      results # This line is required as the last line in the Isolate section!
    }) # End Isolate
  }) # End Trades reactive script
  
  # Function for generating tooltip (hover over) text
  trade_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$trade.num)) return(NULL)
    
    # Pick out the trade with this trade.num
    trades <- isolate(trades())
    trade <- trades[trades$trade.num == x$trade.num, ]
    
    paste0("Open: ", trade$open.date, "<br>",
           "Close: ", trade$close.date, "<br>",
           "Call strike: ", trade$call.strike, "<br>",
           "Put strike: ", trade$put.strike, "<br>",
           "DTE: ", trade$dte, "<br>",
           "IVRank: ", trade$open.ivrank, "<br>",
           "rsi: ", round(trade$open.rsi, digits = 0), "<br>",
           "Exit: ", trade$exit.reason, "<br>",
           "Profit: $", format(trade$profit, big.mark = ",", scientific = FALSE)
    )
  } # End function for generating tooltip text
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    trades %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200, shape = ~factor(exit.reason),
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~has_profit, key := ~trade.num) %>%
      layer_model_predictions(model = "lm") %>%
      add_tooltip(trade_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", orient = "left", title = "Profitable",
                 values = c("Yes", "No")) %>%
      add_legend("shape", orient = "right", title = "Exit Reason") %>%
      scale_nominal("stroke", domain = c("Yes", "No"),
                    range = c("green", "red")) %>%
      set_options(width = 1000, height = 600)
  })
  
  vis %>% bind_shiny("plot1")
  
  output$n_trades <- renderUI({
    str.num.trades <- paste0("Number of trade chains: ", nrow(trades()))
    HTML(str.num.trades)
  })
  # Check for results data.frame from each study
  observe({
    input$goPlot # This line is what redraws the table after run is clicked
    # Output of a table to show the trade details
    output$table <- renderDataTable({results.table},options = list(
      pageLength = 15,
      lengthMenu = c(5, 15, 30)
    ))
    # Download table
    output$downloadData <- downloadHandler(
      filename = function() { paste(stock, 'pdelta', as.character(p.delta),
                                    'cdelta', as.character(c.delta), 'results.csv') },
      content = function(file) {
        write.csv(results.table, file)
      }
    )
  })
  # Check for st.results data.frame from each study
  observe({
    input$goPlot # This line is what redraws the table after run is clicked
    # Output of a table to show the trade details
    output$table2 <- renderDataTable({st.results.table},options = list(
      pageLength = 15,
      lengthMenu = c(5, 15, 30)
    ))
    # Download table
    output$downloadData2 <- downloadHandler(
      filename = function() { paste('st.results.table.csv') },
      content = function(file) {
        write.csv(st.results.table, file)
      }
    )
  })
  
  # Reset default values when inputs change to give a good starting point
  observe({
    if (input$stock == "SLV" || input$stock == "SPY")  {
      updateSelectInput(session, "earn.close", selected = "No")
    }
    if (input$study == "Call Calendar") {
      updateSelectInput(session, "open.dte", selected = 60)
      updateSelectInput(session, "second.dte", selected = 30)
      updateSelectInput(session, "call.delta", selected = .16)
      updateSelectInput(session, "open.ivrank", selected = c(0, 25))
      updateSelectInput(session, "proftarg", selected = 50)
    } else if (input$study == "Poor Mans Cov Call")  {
      updateSelectInput(session, "open.dte", selected = 120)
      updateSelectInput(session, "second.dte", selected = 30)
      updateSelectInput(session, "call.delta", selected = .16)
      updateSelectInput(session, "open.ivrank", selected = c(0, 25))
      updateSelectInput(session, "proftarg", selected = 50)
      updateSelectInput(session, "loss.lim", selected = 2)
      updateSelectInput(session, "l.loss.lim", selected = 50)
    } else if (input$study == "Short Put")  {
      updateSelectInput(session, "open.dte", selected = 45)
      updateSelectInput(session, "put.delta", selected = -.16)
      updateSelectInput(session, "open.ivrank", selected = c(50, 100))
      updateSelectInput(session, "proftarg", selected = 50)
      updateSelectInput(session, "loss.lim", selected = 2)
      updateSelectInput(session, "l.loss.lim", selected = 0)
      updateSelectInput(session, "gamma.days", selected = 0)
      updateSelectInput(session, "earn.close", selected = "No")
    } else if (input$study == "Straddle")  {
      updateSelectInput(session, "open.dte", selected = 45)
      updateSelectInput(session, "open.ivrank", selected = c(50, 100))
      updateSelectInput(session, "proftarg", selected = 25)
      updateSelectInput(session, "loss.lim", selected = 2)
      updateSelectInput(session, "gamma.days", selected = 0)
      updateSelectInput(session, "earn.close", selected = "No")
    } else if (input$study == "Strangle")  {
      updateSelectInput(session, "open.dte", selected = 45)
      #updateSelectInput(session, "call.delta", selected = .16)
      #updateSelectInput(session, "put.delta", selected = -.16)
      #updateSelectInput(session, "open.ivrank", selected = c(50, 100))
      updateSelectInput(session, "proftarg", selected = 50)
      updateSelectInput(session, "loss.lim", selected = 2)
      updateSelectInput(session, "l.loss.lim", selected = 0)
      updateSelectInput(session, "gamma.days", selected = 0)
      updateSelectInput(session, "earn.close", selected = "No")
    }
    if (input$openOption == "Earnings") {
      updateSelectInput(session, "open.dte", selected = 1)
      updateSelectInput(session, "earn.close", selected = "No")
    }
  })
})  # End shiny server content

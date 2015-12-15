# title: "Shiny UI"
# author: "Jason Taylor"

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href = 'javascript:void',
         id = inputId,
         class = 'action-button',
         ...)
}

library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Visual Backtesting"),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Study", tabName = "Study", icon = icon("cog"),
               # selectInput("stock", "Stock", c("AMZN", "EEM", "EWZ", "FXI",
               #                                 "GDX", "GS", "IBM", "SLV",
               #                                 "SPY", "XLE", "ALL")),
               selectInput("stock", "Stock", symbol.list),
               selectInput("study", "Study", c("Strangle")),
               # selectInput("study", "Study", c("Strangle",
               #                                 "Short Put",
               #                                 "Long Stock",
               #                                 "Poor Mans Cov Call",
               #                                 "Straddle",
               #                                 "Call Calendar")),
               selectInput("openOption", "Open on", c("First of Month",
                                                      "First of Week",
                                                      "Daily",
                                                      "Earnings",
                                                      "Previous Close"))
      ),
      menuItem("Entry Criteria", tabName = "Entry Criteria", icon = icon("cogs"),
               sliderInput("open.dte", "DTE", 0, 365, 45, step = 1),
               conditionalPanel(
                 condition = ("input.study == 'Call Calendar' ||
                                input.study == 'Poor Mans Cov Call' ||
                                input.study == 'Strangle'"),
                 sliderInput("call.delta", "Call delta", 0, 1, .16, step = .01)),
               conditionalPanel(
                 condition = ("input.study == 'Short Put' ||
                                input.study == 'Strangle' ||
                                input.study == 'Long Stock'"),
                 sliderInput("put.delta", "Put delta", -1, 0, -.16, step = .01)),
               conditionalPanel(
                 condition = ("input.study == 'Call Calendar' ||
                                input.study == 'Poor Mans Cov Call'"),
                 sliderInput("second.dte", "Min short DTE", 0, 90, 30, step = 5)),
               sliderInput("open.ivrank", "IV Rank", 0, 100, c(0, 100), step = 1),
               sliderInput("min.roc", "Min ROC", 0, 50, 11, step = 1)
      ),
      menuItem("Exit Criteria", tabName = "Exit Criteria", icon = icon("dashboard"),
               sliderInput("proftarg", "Profit target %", 0, 100, 50, step = 5),
               conditionalPanel(
                 condition = ("input.study == 'Poor Mans Cov Call' ||
                                input.study == 'Short Put' ||
                                input.study == 'Strangle' ||
                                input.study == 'Straddle' ||
                                input.study == 'Long Stock'"),
                 sliderInput("loss.lim", "Max loss x times credit received",
                             0, 10, 2, step = .25)),
               conditionalPanel(
                 condition = ("input.study == 'Poor Mans Cov Call'"),
                 sliderInput("l.loss.lim", "Long max loss % debit paid",
                             10, 100, 50, step = 5)),
               conditionalPanel(
                 condition = ("input.study == 'Short Put' ||
                                input.study == 'Strangle'||
                                input.study == 'Straddle'"),
                 sliderInput("gamma.days", "Days prior to expiration to exit",
                             0, 10, 0, step = 1)),
               conditionalPanel(
                 condition = ("(input.study == 'Short Put' ||
                                input.study == 'Strangle' ||
                                input.study == 'Straddle') &&
                                (input.stock == 'AMZN' ||
                                input.stock == 'GS' ||
                                input.stock == 'IBM' ||
                                input.stock == 'AAPL' ||
                                input.stock == 'GOOG')  &&
                                (input.openOption == 'First of Week' ||
                                input.openOption == 'First of Month' ||
                                input.openOption == 'Previous Close' ||
                                input.openOption == 'Daily')"),
                 selectInput("earn.close", "Close day prior to earnings?",
                             c("No", "Yes")))
      ),
      menuItem("Run", tabName = "run", icon = icon("dashboard"),
               actionButton('goPlot', 'Run Study')
      )
    )
  ),
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(2,
                       h4("")
                ),
                column(10,
                       h4(
                       htmlOutput("this.run"),
                       htmlOutput("details")
                       )
                )
              ),
              fluidRow(
                column(2,
                       h4("")
                ),
                column(3,
                       htmlOutput("n_trades"),
                       htmlOutput("total_profit"),
                       htmlOutput("avg_prof_trade"),
                       htmlOutput("avg_prof_day")
                ),
                column(3,
                       htmlOutput("avg_days"),
                       htmlOutput("percent_winners"),
                       htmlOutput("max_loss"),
                       htmlOutput("max_win"),
                       htmlOutput("avg.exit.roc"),
                       htmlOutput("avg.entry.margin")
                ),
                column(3,
                       htmlOutput("exit.reason"),
                       htmlOutput("exit.profit.target"),
                       htmlOutput("exit.loss.limit"),
                       htmlOutput("exit.expiration"),
                       htmlOutput("exit.gamma.risk"),
                       htmlOutput("exit.earnings")
                       # htmlOutput("total_profit2"),
                       # htmlOutput("avg_prof_trade2"),
                       # htmlOutput("avg_prof_day2"),
                       # htmlOutput("percent_winners2"),
                       # htmlOutput("max_loss2"),
                       # htmlOutput("max_win2"),
                       # htmlOutput("avg.exit.roc2"),
                       # htmlOutput("avg.entry.margin2")
                )
              ),
              fluidRow(
                column(1,
                       h4("")
                ),
                column(10,
                       ggvisOutput("plot1")
                ),
                column(1,
                       h4("")
                )
              ),
              fluidRow(
                column(1,
                       h4("")
                ),
                column(5,
                       selectInput("xvar", "X-axis variable", axis_vars,
                                   selected = "open.ivrank")
                ),
                column(5,
                       selectInput("yvar", "Y-axis variable", axis_vars,
                                   selected = "profit")
                ),
                column(1,
                       h4("")
                )
              ),
              fluidRow(
                column(1,
                       h4("")
                ),
                column(10,
                       downloadButton('downloadData', 'Download'),
                       dataTableOutput('table')
                ),
                column(1,
                       h4("")
                )
              )
      )
    )
  )
)

# title: "global.R"
# author: "Jason Taylor"

# todos:
# - Should axis variable be in global?

# Global setup
library(shiny) # main library to load shiny application
library(shinythemes) # themes for shiny
library(ggvis) # visualization used in shiny app
library(options.data) # custom local package with options data
library(options.studies) # custom local package with options strategy functions

# Variables that can be put on the axis
axis_vars <- c(
  "Days Held" = "days.held",
  "IV Rank" = "open.ivrank",
  "Profit" = "profit",
  "RSI" = "open.rsi",
  "Year" = "year",
  "Open ROC" = "open.roc"
)

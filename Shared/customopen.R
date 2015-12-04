# Read in the csv that has all the custom dates
# Make data frame so that it can be used as entry dates
first.day.file <- paste0("Data/customDates.csv")
first.day <- fread(first.day.file)
first.day <- as.data.frame(first.day)
colnames(first.day) <- c("row.num", "open.date", "exit.reason")
first.day[, "open.date"] <- as.Date(first.day[, "open.date"])

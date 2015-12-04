# Read in the first day of the month for opening trade dates

# Read in the csv that has all the first trading days of the month
# Make data frame so that it can be used as entry dates
first.day.file <- "Data/first.day.week.csv"
first.day <- fread(first.day.file)
first.day <- as.data.frame(first.day)
colnames(first.day) <- c("row.num", "open.date")
first.day[, "open.date"] <- as.Date(first.day[, "open.date"])
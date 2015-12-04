# Read in the csv that has all the earnings dates of the symbol chosen
# Make data frame so that it can be used as exit dates
earnings.day.file <- paste0("Data/", stock, "/earningsDates.csv")
earn.dates <- fread(earnings.day.file)
earn.dates <- as.data.frame(earn.dates)
colnames(earn.dates) <- c("row.num", "earn.date")
earn.dates[, "earn.date"] <- as.Date(earn.dates[, "earn.date"])

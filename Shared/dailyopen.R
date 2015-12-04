# Read in the stock chosen to find possible open dates
# Make data frame so that it can be used as entry dates
first.day.file <- paste0("Data/", stock, "/calls.csv")
first.day <- fread(first.day.file)
first.day <- as.data.frame(first.day)
first.day <- select(first.day, V1, date)
colnames(first.day) <- c("row.num", "open.date")
first.day <- mutate(first.day, open.date = as.Date(open.date))

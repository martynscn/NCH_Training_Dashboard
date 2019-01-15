url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ5Zgn_CarwUu09Y_0r9Zc9TB1ZAo5Qh1UjhdnhS_h6RfNq7QolP-bA-56dX31mWG-vtK4OEENcrnQ-/pub?gid=0&single=true&output=csv"
csv_data <- read.csv(file = url, stringsAsFactors = FALSE, na.strings = "N/A", check.names = FALSE)
original_names <- names(csv_data)
cleaned_names <- make.names(original_names)
names(csv_data) <- cleaned_names

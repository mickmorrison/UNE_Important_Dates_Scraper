# Script to create a calendar file from UNE calendar
# Mick Morrison and ChatGPT
# 1 June 2024

# 1 - Load libraries ------------------------------------------------------

# Required libraries
required_packages <- c("rvest", "magrittr", "dplyr", "calendar")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set the working directory to ~/Downloads (adjust as needed)
setwd("~/Downloads")

# Set the input URL to scrape
url <- "https://www.une.edu.au/about-une/principal-dates"

# 2 - Scrape and format as table -----------------------------------------

# Function to scrape the UNE calendar and create a data frame
scrape_une_calendar <- function(url) {
  # Read the webpage content
  webpage <- read_html(url)
  
  # Extract the relevant table from the webpage
  table_nodes <- webpage %>% html_nodes("table")
  
  # Initialize an empty data frame
  all_events_df <- data.frame(Day = character(), Date = character(), Event = character(), stringsAsFactors = FALSE)
  
  # Loop through each table and parse the data
  for (table_node in table_nodes) {
    table_data <- html_table(table_node, fill = TRUE)
    if (ncol(table_data) >= 3) {
      colnames(table_data) <- c("Day", "Date", "Event")  # Ensure correct column names
      all_events_df <- rbind(all_events_df, table_data)
    }
  }
  
  return(all_events_df)
}

# Scrape the UNE calendar and create a data frame
all_events_df <- scrape_une_calendar(url)

# Function to generate iCalendar content from a data frame
generate_ics_content <- function(df) {
  ics_start <- "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN"
  ics_end <- "END:VCALENDAR"
  
  event_template <- function(day, date, event) {
    dtstart <- as.Date(date, "%d %B %Y")
    dtend <- dtstart  # For a day-long event, start and end are the same
    event_content <- sprintf("BEGIN:VEVENT\nDTSTART;VALUE=DATE:%s\nDTEND;VALUE=DATE:%s\nSUMMARY:%s\nTRANSP:TRANSPARENT\nEND:VEVENT",
                             format(dtstart, "%Y%m%d"), format(dtend + 1, "%Y%m%d"), event)  # +1 for inclusive end date in iCal
    return(event_content)
  }
  
  events_content <- ""
  for (i in 1:nrow(df)) {
    event_str <- event_template(df$Day[i], df$Date[i], df$Event[i])
    events_content <- paste(events_content, event_str, sep = "\n")
  }
  
  ics_full_content <- paste(ics_start, events_content, ics_end, sep = "\n")
  return(ics_full_content)
}

# Generate the iCalendar content
ics_content <- generate_ics_content(all_events_df)

# Specify the file path (adjust the path as needed)
ics_file_path <- "~/Downloads/academic_calendar.ics"

# Write the content to the file
writeLines(ics_content, con = ics_file_path)

# Inform the user
cat("iCalendar file has been generated at:", ics_file_path)

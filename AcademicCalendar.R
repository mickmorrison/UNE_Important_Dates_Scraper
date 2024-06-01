# Script to create a calendar file from UNE calendar
# Mick Morrisona and ChatGPT
# Script to create a calendar file from UNE calendar
# Mick Morrison and ChatGPT


# 1 - Load libraries ------------------------------------------------------



# Required libraries
library(rvest)
library(magrittr)
library(dplyr) # Added for data manipulation

# Check and install missing packages
required_packages <- c("rvest", "magrittr", "dplyr", "calendar")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set the working directory to ~/Downloads (adjust as needed)
setwd("~/Downloads")


# 2 - Scrape and formast as table -----------------------------------------

# Function to generate iCalendar content from a data frame

# Function to generate iCalendar content from a data frame
generate_ics_content <- function(df) {
  ics_start <- "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN"
  ics_end <- "END:VCALENDAR"
  
  event_template <- function(day, date, event) {
    dtstart <- as.Date(date, "%d %B %Y")
    dtend <- dtstart # For a day-long event, start and end are the same
    event_content <- sprintf("BEGIN:VEVENT\nDTSTART;VALUE=DATE:%s\nDTEND;VALUE=DATE:%s\nSUMMARY:%s\nTRANSP:TRANSPARENT\nEND:VEVENT",
                             format(dtstart, "%Y%m%d"), format(dtend + 1, "%Y%m%d"), event) # +1 for inclusive end date in iCal
    return(event_content)
  }
  
  events_content <- ""
  for(i in 1:nrow(df)) {
    event_str <- event_template(df$Day[i], df$Date[i], df$Event[i])
    events_content <- paste(events_content, event_str, sep="\n")
  }
  
  ics_full_content <- paste(ics_start, events_content, ics_end, sep="\n")
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
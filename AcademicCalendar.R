library(rvest)
library(dplyr)
library(stringr)

local_html    <- "~/Downloads/PrincipalDates2026.html"
ics_file_path <- "~/Downloads/academic_calendar_2026.ics"

# 1. Read the locally saved HTML
webpage <- read_html(local_html)

# 2. Extract the tables
table_nodes <- webpage %>% html_nodes("table")

all_events_df <- lapply(table_nodes, function(node) {
  tbl <- html_table(node, fill = TRUE)
  if (ncol(tbl) >= 3) {
    tbl <- tbl[, 1:3]
    colnames(tbl) <- c("Day", "Date", "Event")
    return(tbl)
  }
  return(NULL)
}) %>% bind_rows()

# 3. Clean the data and strip out the hidden HTML prefixes
clean_events <- all_events_df %>%
  mutate(
    # Strip the literal words "Day", "Date", and "Event" from the start of the strings
    Day   = str_remove(Day, "^Day"),
    Date  = str_remove(Date, "^Date"),
    Event = str_remove(Event, "^Event")
  ) %>%
  filter(!is.na(Date), Date != "", tolower(Date) != "date") %>%
  mutate(
    # Now it will correctly parse "1 January 2026"
    dtstart = as.Date(Date, format = "%d %B %Y"),
    Event = str_squish(Event)
  ) %>%
  filter(!is.na(dtstart))

# 4. Build the iCal strings with strict \r\n line endings
ics_start <- "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//UNE//Academic Calendar//EN"
ics_end <- "END:VCALENDAR"
current_stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")

events_content <- sapply(1:nrow(clean_events), function(i) {
  dt  <- clean_events$dtstart[i]
  evt <- clean_events$Event[i]
  
  uid <- paste0(format(dt, "%Y%m%d"), "-", i, "@une.edu.au")
  
  sprintf(
    "BEGIN:VEVENT\r\nUID:%s\r\nDTSTAMP:%s\r\nDTSTART;VALUE=DATE:%s\r\nDTEND;VALUE=DATE:%s\r\nSUMMARY:%s\r\nTRANSP:TRANSPARENT\r\nEND:VEVENT",
    uid,
    current_stamp,
    format(dt, "%Y%m%d"), 
    format(dt + 1, "%Y%m%d"), 
    evt
  )
})

# 5. Flatten and write out enforcing CRLF line endings
ics_full_content <- paste(c(ics_start, events_content, ics_end), collapse = "\r\n")

f <- file(ics_file_path, "wb")
writeChar(ics_full_content, f, eos = NULL)
close(f)

cat("Successfully parsed", nrow(clean_events), "events.\n")
cat("iCalendar file generated at:", ics_file_path, "\n")
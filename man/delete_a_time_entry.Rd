% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timesheets.R
\name{delete_a_time_entry}
\alias{delete_a_time_entry}
\title{Delete a time entry}
\usage{
delete_a_time_entry(TIME_ENTRY_ID)
}
\description{
Delete a time entry. Deleting a time entry is only possible if it’s not closed and the associated project and task haven’t been archived.  However, Admins can delete closed entries. Returns a 200 OK response code if the call succeeded.
From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#delete-a-time-entry}{Delete a time entry}
}

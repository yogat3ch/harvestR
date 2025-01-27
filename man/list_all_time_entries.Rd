% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timesheets.R
\name{list_all_time_entries}
\alias{list_all_time_entries}
\title{List all time entries}
\usage{
list_all_time_entries(
  user_id = NULL,
  client_id = NULL,
  project_id = NULL,
  task_id = NULL,
  external_reference_id = NULL,
  is_billed = NULL,
  is_running = NULL,
  updated_since = NULL,
  from = NULL,
  to = NULL,
  page = NULL,
  per_page = NULL
)
}
\arguments{
\item{user_id}{\code{(integer)} \strong{optional} Only return time entries belonging to the user with the given ID.}

\item{client_id}{\code{(integer)} \strong{optional} Only return time entries belonging to the client with the given ID.}

\item{project_id}{\code{(integer)} \strong{optional} Only return time entries belonging to the project with the given ID.}

\item{task_id}{\code{(integer)} \strong{optional} Only return time entries belonging to the task with the given ID.}

\item{external_reference_id}{\code{(string)} \strong{optional} Only return time entries with the given external_reference ID.}

\item{is_billed}{\code{(boolean)} \strong{optional} Pass true to only return time entries that have been invoiced and false to return time entries that have not been invoiced.}

\item{is_running}{\code{(boolean)} \strong{optional} Pass true to only return running time entries and false to return non-running time entries.}

\item{updated_since}{\code{(datetime)} \strong{optional} Only return time entries that have been updated since the given date and time. Use the ISO 8601 Format.}

\item{from}{\code{(date)} \strong{optional} Only return time entries with a spent_date on or after the given date.}

\item{to}{\code{(date)} \strong{optional} Only return time entries with a spent_date on or before the given date.}

\item{page}{\code{(integer)} \strong{optional} The page number to use in pagination. For instance, if you make a list request and receive 100 records, your subsequent call can include page=2 to retrieve the next page of the list. (Default: 1)}

\item{per_page}{\code{(integer)} \strong{optional} The number of records to return per page. Can range between 1 and 100.  (Default: 100)}
}
\description{
Returns a list of your time entries. The time entries are returned sorted by spent_date date. At this time, the sort option can’t be customized.
The response contains an object with a time_entries property that contains an array of up to per_page time entries. Each entry in the array is a separate time entry object. If no more time entries are available, the resulting array will be empty. Several additional pagination properties are included in the response to simplify paginating your time entries.
From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#list-all-time-entries}{List all time entries}
}

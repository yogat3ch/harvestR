% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timesheets.R
\name{update_a_time_entry}
\alias{update_a_time_entry}
\title{Update a time entry}
\usage{
update_a_time_entry(
  TIME_ENTRY_ID,
  project_id = NULL,
  task_id = NULL,
  spent_date = NULL,
  started_time = NULL,
  ended_time = NULL,
  hours = NULL,
  notes = NULL,
  external_reference = NULL
)
}
\arguments{
\item{TIME_ENTRY_ID}{\code{(integer)} \strong{required} The ID of the time entry with which the call will be associated.}

\item{project_id}{\code{(integer)} \strong{optional} The ID of the project to associate with the time entry.}

\item{task_id}{\code{(integer)} \strong{optional} The ID of the task to associate with the time entry.}

\item{spent_date}{\code{(date)} \strong{optional} The ISO 8601 formatted date the time entry was spent.}

\item{started_time}{\code{(time)} \strong{optional} The time the entry started. Defaults to the current time. Example: “8:00am”.}

\item{ended_time}{\code{(time)} \strong{optional} The time the entry ended.}

\item{hours}{\code{(decimal)} \strong{optional} The current amount of time tracked.}

\item{notes}{\code{(string)} \strong{optional} Any notes to be associated with the time entry.}

\item{external_reference}{\code{(object)} \strong{optional} An object containing the id, group_id, account_id, and permalink of the external reference.}
}
\description{
Updates the specific time entry by setting the values of the parameters passed. Any parameters not provided will be left unchanged. Returns a time entry object and a 200 OK response code if the call succeeded.
From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#update-a-time-entry}{Update a time entry}
}

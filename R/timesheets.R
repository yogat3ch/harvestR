

#' @title The time entry object
#' @name the_time_entry_object
#' @description
#' Returns a list of your time entries. The time entries are returned sorted by spent_date date. At this time, the sort option can’t be customized.
#' The response contains an object with a time_entries property that contains an array of up to per_page time entries. Each entry in the array is a separate time entry object. If no more time entries are available, the resulting array will be empty. Several additional pagination properties are included in the response to simplify paginating your time entries.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#the-time-entry-object}{The time entry object}
#'\itemize{
#'		\item{id}{\code{(integer)} Unique ID for the time entry.}
#'		\item{spent_date}{\code{(date)} Date of the time entry.}
#'		\item{user}{\code{(object)} An object containing the id and name of the associated user.}
#'		\item{user_assignment}{\code{(object)} A user assignment object of the associated user.}
#'		\item{client}{\code{(object)} An object containing the id and name of the associated client.}
#'		\item{project}{\code{(object)} An object containing the id and name of the associated project.}
#'		\item{task}{\code{(object)} An object containing the id and name of the associated task.}
#'		\item{task_assignment}{\code{(object)} A task assignment object of the associated task.}
#'		\item{external_reference}{\code{(object)} An object containing the id, group_id, account_id, permalink, service, and service_icon_url of the associated external reference.}
#'		\item{invoice}{\code{(object)} Once the time entry has been invoiced, this field will include the associated invoice’s id and number.}
#'		\item{hours}{\code{(decimal)} Number of (decimal time) hours tracked in this time entry.}
#'		\item{hours_without_timer}{\code{(decimal)} Number of (decimal time) hours already tracked in this time entry, before the timer was last started.}
#'		\item{rounded_hours}{\code{(decimal)} Number of (decimal time) hours tracked in this time entry used in summary reports and invoices. This value is rounded according to the Time Rounding setting in your Preferences.}
#'		\item{notes}{\code{(string)} Notes attached to the time entry.}
#'		\item{is_locked}{\code{(boolean)} Whether or not the time entry has been locked.}
#'		\item{locked_reason}{\code{(string)} Why the time entry has been locked.}
#'		\item{is_closed}{\code{(boolean)} Whether or not the time entry has been approved via Timesheet Approval.}
#'		\item{is_billed}{\code{(boolean)} Whether or not the time entry has been marked as invoiced.}
#'		\item{timer_started_at}{\code{(datetime)} Date and time the timer was started (if tracking by duration). Use the ISO 8601 Format.}
#'		\item{started_time}{\code{(time)} Time the time entry was started (if tracking by start/end times).}
#'		\item{ended_time}{\code{(time)} Time the time entry was ended (if tracking by start/end times).}
#'		\item{is_running}{\code{(boolean)} Whether or not the time entry is currently running.}
#'		\item{billable}{\code{(boolean)} Whether or not the time entry is billable.}
#'		\item{budgeted}{\code{(boolean)} Whether or not the time entry counts towards the project budget.}
#'		\item{billable_rate}{\code{(decimal)} The billable rate for the time entry.}
#'		\item{cost_rate}{\code{(decimal)} The cost rate for the time entry.}
#'		\item{created_at}{\code{(datetime)} Date and time the time entry was created. Use the ISO 8601 Format.}
#'		\item{updated_at}{\code{(datetime)} Date and time the time entry was last updated. Use the ISO 8601 Format.}
#'}
the_time_entry_object <- 
structure(list(Attribute = c("id", "spent_date", "user", "user_assignment", 
"client", "project", "task", "task_assignment", "external_reference", 
"invoice", "hours", "hours_without_timer", "rounded_hours", "notes", 
"is_locked", "locked_reason", "is_closed", "is_billed", "timer_started_at", 
"started_time", "ended_time", "is_running", "billable", "budgeted", 
"billable_rate", "cost_rate", "created_at", "updated_at"), Type = c("integer", 
"date", "object", "object", "object", "object", "object", "object", 
"object", "object", "decimal", "decimal", "decimal", "string", 
"boolean", "string", "boolean", "boolean", "datetime", "time", 
"time", "boolean", "boolean", "boolean", "decimal", "decimal", 
"datetime", "datetime"), Description = c("Unique ID for the time entry.", 
"Date of the time entry.", "An object containing the id and name of the associated user.", 
"A user assignment object of the associated user.", "An object containing the id and name of the associated client.", 
"An object containing the id and name of the associated project.", 
"An object containing the id and name of the associated task.", 
"A task assignment object of the associated task.", "An object containing the id, group_id, account_id, permalink, service, and service_icon_url of the associated external reference.", 
"Once the time entry has been invoiced, this field will include the associated invoice’s id and number.", 
"Number of (decimal time) hours tracked in this time entry.", 
"Number of (decimal time) hours already tracked in this time entry, before the timer was last started.", 
"Number of (decimal time) hours tracked in this time entry used in summary reports and invoices. This value is rounded according to the Time Rounding setting in your Preferences.", 
"Notes attached to the time entry.", "Whether or not the time entry has been locked.", 
"Why the time entry has been locked.", "Whether or not the time entry has been approved via Timesheet Approval.", 
"Whether or not the time entry has been marked as invoiced.", 
"Date and time the timer was started (if tracking by duration). Use the ISO 8601 Format.", 
"Time the time entry was started (if tracking by start/end times).", 
"Time the time entry was ended (if tracking by start/end times).", 
"Whether or not the time entry is currently running.", "Whether or not the time entry is billable.", 
"Whether or not the time entry counts towards the project budget.", 
"The billable rate for the time entry.", "The cost rate for the time entry.", 
"Date and time the time entry was created. Use the ISO 8601 Format.", 
"Date and time the time entry was last updated. Use the ISO 8601 Format."
), Required = c("optional", "optional", "optional", "optional", 
"optional", "optional", "optional", "optional", "optional", "optional", 
"optional", "optional", "optional", "optional", "optional", "optional", 
"optional", "optional", "optional", "optional", "optional", "optional", 
"optional", "optional", "optional", "optional", "optional", "optional"
)), row.names = c(NA, -28L), class = c("tbl_df", "tbl", "data.frame"
))


#' @title List all time entries
#' @name list_all_time_entries
#' @description
#' Returns a list of your time entries. The time entries are returned sorted by spent_date date. At this time, the sort option can’t be customized.
#' The response contains an object with a time_entries property that contains an array of up to per_page time entries. Each entry in the array is a separate time entry object. If no more time entries are available, the resulting array will be empty. Several additional pagination properties are included in the response to simplify paginating your time entries.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#list-all-time-entries}{List all time entries}
#' @param user_id \code{(integer)} **optional** Only return time entries belonging to the user with the given ID.
#' @param client_id \code{(integer)} **optional** Only return time entries belonging to the client with the given ID.
#' @param project_id \code{(integer)} **optional** Only return time entries belonging to the project with the given ID.
#' @param task_id \code{(integer)} **optional** Only return time entries belonging to the task with the given ID.
#' @param external_reference_id \code{(string)} **optional** Only return time entries with the given external_reference ID.
#' @param is_billed \code{(boolean)} **optional** Pass true to only return time entries that have been invoiced and false to return time entries that have not been invoiced.
#' @param is_running \code{(boolean)} **optional** Pass true to only return running time entries and false to return non-running time entries.
#' @param updated_since \code{(datetime)} **optional** Only return time entries that have been updated since the given date and time. Use the ISO 8601 Format.
#' @param from \code{(date)} **optional** Only return time entries with a spent_date on or after the given date.
#' @param to \code{(date)} **optional** Only return time entries with a spent_date on or before the given date.
#' @param page \code{(integer)} **optional** The page number to use in pagination. For instance, if you make a list request and receive 100 records, your subsequent call can include page=2 to retrieve the next page of the list. (Default: 1)
#' @param per_page \code{(integer)} **optional** The number of records to return per page. Can range between 1 and 100.  (Default: 100)
#' @export
list_all_time_entries = function (user_id = NULL, client_id = NULL, project_id = NULL, 
    task_id = NULL, external_reference_id = NULL, is_billed = NULL, 
    is_running = NULL, updated_since = NULL, from = NULL, to = NULL, 
    page = NULL, per_page = NULL) 
{
    api_fn <- "GET"
    .pp <- api_fn %in% c("PATCH", "POST")
    .url <- list(api_url, path = c("v2", "time_entries"))
    .args <- list(verb = api_fn)
    .query <- list(user_id = user_id, client_id = client_id, 
        project_id = project_id, task_id = task_id, external_reference_id = external_reference_id, 
        is_billed = is_billed, is_running = is_running, updated_since = updated_since, 
        from = from, to = to, page = page, per_page = per_page)
    if (.pp) 
        .args$body <- .query
    else .url$query <- .query
    .url <- do.call(httr::modify_url, .url)
    .url <- glue::glue(.url)
    .args$url <- .url
    .args$config <- httr::add_headers(.headers = c(`Harvest-Account-ID` = get_harvest_account(), 
        Authorization = paste0("Bearer ", get_harvest_pat()), 
        `User-Agent` = get_user_agent()))
    resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
    out <- paginate(resp, .args)
    return(out)
}


#' @title Retrieve a time entry
#' @name retrieve_a_time_entry
#' @description
#' Retrieves the time entry with the given ID. Returns a time entry object and a 200 OK response code if a valid identifier was provided.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#retrieve-a-time-entry}{Retrieve a time entry}
#' @param TIME_ENTRY_ID \code{(integer)} **required** The ID of the time entry with which the call will be associated.
#' @param project_id \code{(integer)} **required** The ID of the project to associate with the time entry.
#' @param task_id \code{(integer)} **required** The ID of the task to associate with the time entry.
#' @param spent_date \code{(date)} **required** The ISO 8601 formatted date the time entry was spent.
#' @param user_id \code{(integer)} **optional** The ID of the user to associate with the time entry. Defaults to the currently authenticated user’s ID.
#' @param hours \code{(decimal)} **optional** The current amount of time tracked. If provided, the time entry will be created with the specified hours and is_running will be set to false. If not provided, hours will be set to 0.0 and is_running will be set to true.
#' @param notes \code{(string)} **optional** Any notes to be associated with the time entry.
#' @param external_reference \code{(object)} **optional** An object containing the id, group_id, account_id, and permalink of the external reference.
#' @export
retrieve_a_time_entry = function (TIME_ENTRY_ID, project_id, task_id, spent_date, user_id = NULL, 
    hours = NULL, notes = NULL, external_reference = NULL) 
{
    api_fn <- "GET"
    .pp <- api_fn %in% c("PATCH", "POST")
    .url <- list(api_url, path = c("v2", "time_entries", "{TIME_ENTRY_ID}"
    ))
    .args <- list(verb = api_fn)
    .query <- list(project_id = project_id, task_id = task_id, 
        spent_date = spent_date, user_id = user_id, hours = hours, 
        notes = notes, external_reference = external_reference)
    if (.pp) 
        .args$body <- .query
    else .url$query <- .query
    .url <- do.call(httr::modify_url, .url)
    .url <- glue::glue(.url)
    .args$url <- .url
    .args$config <- httr::add_headers(.headers = c(`Harvest-Account-ID` = get_harvest_account(), 
        Authorization = paste0("Bearer ", get_harvest_pat()), 
        `User-Agent` = get_user_agent()))
    resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
    out <- paginate(resp, .args)
    return(out)
}


#' @title Create a time entry via duration
#' @name create_a_time_entry_via_duration
#' @description
#' Creates a new time entry object. Returns a time entry object and a 201 Created response code if the call succeeded.
#' You should only use this method to create time entries when your account is configured to track time via duration. You can verify this by visiting the Settings page in your Harvest account or by checking if wants_timestamp_timers is false in the Company API.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#create-a-time-entry-via-duration}{Create a time entry via duration}
#' @param user_id \code{(integer)} **optional** The ID of the user to associate with the time entry. Defaults to the currently authenticated user’s ID.
#' @param project_id \code{(integer)} **required** The ID of the project to associate with the time entry.
#' @param task_id \code{(integer)} **required** The ID of the task to associate with the time entry.
#' @param spent_date \code{(date)} **required** The ISO 8601 formatted date the time entry was spent.
#' @param hours \code{(decimal)} **optional** The current amount of time tracked. If provided, the time entry will be created with the specified hours and is_running will be set to false. If not provided, hours will be set to 0.0 and is_running will be set to true.
#' @param notes \code{(string)} **optional** Any notes to be associated with the time entry.
#' @param external_reference \code{(object)} **optional** An object containing the id, group_id, account_id, and permalink of the external reference.
#' @export
create_a_time_entry_via_duration = function (user_id = NULL, project_id, task_id, spent_date, hours = NULL, 
    notes = NULL, external_reference = NULL) 
{
    api_fn <- "POST"
    .pp <- api_fn %in% c("PATCH", "POST")
    .url <- list(api_url, path = c("v2", "time_entries"))
    .args <- list(verb = api_fn)
    .query <- list(user_id = user_id, project_id = project_id, 
        task_id = task_id, spent_date = spent_date, hours = hours, 
        notes = notes, external_reference = external_reference)
    if (.pp) 
        .args$body <- .query
    else .url$query <- .query
    .url <- do.call(httr::modify_url, .url)
    .url <- glue::glue(.url)
    .args$url <- .url
    .args$config <- httr::add_headers(.headers = c(`Harvest-Account-ID` = get_harvest_account(), 
        Authorization = paste0("Bearer ", get_harvest_pat()), 
        `User-Agent` = get_user_agent()))
    resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
    out <- paginate(resp, .args)
    return(out)
}


#' @title Create a time entry via start and end time
#' @name create_a_time_entry_via_start_and_end_time
#' @description
#' Creates a new time entry object. Returns a time entry object and a 201 Created response code if the call succeeded.
#' You should only use this method to create time entries when your account is configured to track time via start and end time. You can verify this by visiting the Settings page in your Harvest account or by checking if wants_timestamp_timers is true in the Company API.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#create-a-time-entry-via-start-and-end-time}{Create a time entry via start and end time}
#' @param user_id \code{(integer)} **optional** The ID of the user to associate with the time entry. Defaults to the currently authenticated user’s ID.
#' @param project_id \code{(integer)} **required** The ID of the project to associate with the time entry.
#' @param task_id \code{(integer)} **required** The ID of the task to associate with the time entry.
#' @param spent_date \code{(date)} **required** The ISO 8601 formatted date the time entry was spent.
#' @param started_time \code{(time)} **optional** The time the entry started. Defaults to the current time. Example: “8:00am”.
#' @param ended_time \code{(time)} **optional** The time the entry ended. If provided, is_running will be set to false. If not provided, is_running will be set to true.
#' @param notes \code{(string)} **optional** Any notes to be associated with the time entry.
#' @param external_reference \code{(object)} **optional** An object containing the id, group_id, account_id, and permalink of the external reference.
#' @export
create_a_time_entry_via_start_and_end_time = function (user_id = NULL, project_id, task_id, spent_date, started_time = NULL, 
    ended_time = NULL, notes = NULL, external_reference = NULL) 
{
    api_fn <- "POST"
    .pp <- api_fn %in% c("PATCH", "POST")
    .url <- list(api_url, path = c("v2", "time_entries"))
    .args <- list(verb = api_fn)
    .query <- list(user_id = user_id, project_id = project_id, 
        task_id = task_id, spent_date = spent_date, started_time = started_time, 
        ended_time = ended_time, notes = notes, external_reference = external_reference)
    if (.pp) 
        .args$body <- .query
    else .url$query <- .query
    .url <- do.call(httr::modify_url, .url)
    .url <- glue::glue(.url)
    .args$url <- .url
    .args$config <- httr::add_headers(.headers = c(`Harvest-Account-ID` = get_harvest_account(), 
        Authorization = paste0("Bearer ", get_harvest_pat()), 
        `User-Agent` = get_user_agent()))
    resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
    out <- paginate(resp, .args)
    return(out)
}


#' @title Update a time entry
#' @name update_a_time_entry
#' @description
#' Updates the specific time entry by setting the values of the parameters passed. Any parameters not provided will be left unchanged. Returns a time entry object and a 200 OK response code if the call succeeded.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#update-a-time-entry}{Update a time entry}
#' @param TIME_ENTRY_ID \code{(integer)} **required** The ID of the time entry with which the call will be associated.
#' @param project_id \code{(integer)} **optional** The ID of the project to associate with the time entry.
#' @param task_id \code{(integer)} **optional** The ID of the task to associate with the time entry.
#' @param spent_date \code{(date)} **optional** The ISO 8601 formatted date the time entry was spent.
#' @param started_time \code{(time)} **optional** The time the entry started. Defaults to the current time. Example: “8:00am”.
#' @param ended_time \code{(time)} **optional** The time the entry ended.
#' @param hours \code{(decimal)} **optional** The current amount of time tracked.
#' @param notes \code{(string)} **optional** Any notes to be associated with the time entry.
#' @param external_reference \code{(object)} **optional** An object containing the id, group_id, account_id, and permalink of the external reference.
#' @export
update_a_time_entry = function (TIME_ENTRY_ID, project_id = NULL, task_id = NULL, spent_date = NULL, 
    started_time = NULL, ended_time = NULL, hours = NULL, notes = NULL, 
    external_reference = NULL) 
{
    api_fn <- "PATCH"
    .pp <- api_fn %in% c("PATCH", "POST")
    .url <- list(api_url, path = c("v2", "time_entries", "{TIME_ENTRY_ID}"
    ))
    .args <- list(verb = api_fn)
    .query <- list(project_id = project_id, task_id = task_id, 
        spent_date = spent_date, started_time = started_time, 
        ended_time = ended_time, hours = hours, notes = notes, 
        external_reference = external_reference)
    if (.pp) 
        .args$body <- .query
    else .url$query <- .query
    .url <- do.call(httr::modify_url, .url)
    .url <- glue::glue(.url)
    .args$url <- .url
    .args$config <- httr::add_headers(.headers = c(`Harvest-Account-ID` = get_harvest_account(), 
        Authorization = paste0("Bearer ", get_harvest_pat()), 
        `User-Agent` = get_user_agent()))
    resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
    out <- paginate(resp, .args)
    return(out)
}


#' @title Delete a time entry’s external reference
#' @name delete_a_time_entry_s_external_reference
#' @description
#' Delete a time entry’s external reference. Returns a 200 OK response code if the call succeeded.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#delete-a-time-entrys-external-reference}{Delete a time entry’s external reference}
#' @export
delete_a_time_entry_s_external_reference = function (TIME_ENTRY_ID) 
{
    api_fn <- "DELETE"
    .pp <- api_fn %in% c("PATCH", "POST")
    .url <- list(api_url, path = c("v2", "time_entries", "{TIME_ENTRY_ID}", 
    "external_reference"))
    .args <- list(verb = api_fn)
    .query <- list()
    if (.pp) 
        .args$body <- .query
    else .url$query <- .query
    .url <- do.call(httr::modify_url, .url)
    .url <- glue::glue(.url)
    .args$url <- .url
    .args$config <- httr::add_headers(.headers = c(`Harvest-Account-ID` = get_harvest_account(), 
        Authorization = paste0("Bearer ", get_harvest_pat()), 
        `User-Agent` = get_user_agent()))
    resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
    out <- paginate(resp, .args)
    return(out)
}


#' @title Delete a time entry
#' @name delete_a_time_entry
#' @description
#' Delete a time entry. Deleting a time entry is only possible if it’s not closed and the associated project and task haven’t been archived.  However, Admins can delete closed entries. Returns a 200 OK response code if the call succeeded.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#delete-a-time-entry}{Delete a time entry}
#' @export
delete_a_time_entry = function (TIME_ENTRY_ID) 
{
    api_fn <- "DELETE"
    .pp <- api_fn %in% c("PATCH", "POST")
    .url <- list(api_url, path = c("v2", "time_entries", "{TIME_ENTRY_ID}"
    ))
    .args <- list(verb = api_fn)
    .query <- list()
    if (.pp) 
        .args$body <- .query
    else .url$query <- .query
    .url <- do.call(httr::modify_url, .url)
    .url <- glue::glue(.url)
    .args$url <- .url
    .args$config <- httr::add_headers(.headers = c(`Harvest-Account-ID` = get_harvest_account(), 
        Authorization = paste0("Bearer ", get_harvest_pat()), 
        `User-Agent` = get_user_agent()))
    resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
    out <- paginate(resp, .args)
    return(out)
}


#' @title Restart a stopped time entry
#' @name restart_a_stopped_time_entry
#' @description
#' Restarting a time entry is only possible if it isn’t currently running. Returns a 200 OK response code if the call succeeded.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#restart-a-stopped-time-entry}{Restart a stopped time entry}
#' @export
restart_a_stopped_time_entry = function (TIME_ENTRY_ID) 
{
    api_fn <- "PATCH"
    .pp <- api_fn %in% c("PATCH", "POST")
    .url <- list(api_url, path = c("v2", "time_entries", "{TIME_ENTRY_ID}", 
    "restart"))
    .args <- list(verb = api_fn)
    .query <- list()
    if (.pp) 
        .args$body <- .query
    else .url$query <- .query
    .url <- do.call(httr::modify_url, .url)
    .url <- glue::glue(.url)
    .args$url <- .url
    .args$config <- httr::add_headers(.headers = c(`Harvest-Account-ID` = get_harvest_account(), 
        Authorization = paste0("Bearer ", get_harvest_pat()), 
        `User-Agent` = get_user_agent()))
    resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
    out <- paginate(resp, .args)
    return(out)
}


#' @title Stop a running time entry
#' @name stop_a_running_time_entry
#' @description
#' Stopping a time entry is only possible if it’s currently running. Returns a 200 OK response code if the call succeeded.
#' From \href{https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/#stop-a-running-time-entry}{Stop a running time entry}
#' @export
stop_a_running_time_entry = function (TIME_ENTRY_ID) 
{
    api_fn <- "PATCH"
    .pp <- api_fn %in% c("PATCH", "POST")
    .url <- list(api_url, path = c("v2", "time_entries", "{TIME_ENTRY_ID}", 
    "stop"))
    .args <- list(verb = api_fn)
    .query <- list()
    if (.pp) 
        .args$body <- .query
    else .url$query <- .query
    .url <- do.call(httr::modify_url, .url)
    .url <- glue::glue(.url)
    .args$url <- .url
    .args$config <- httr::add_headers(.headers = c(`Harvest-Account-ID` = get_harvest_account(), 
        Authorization = paste0("Bearer ", get_harvest_pat()), 
        `User-Agent` = get_user_agent()))
    resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
    out <- paginate(resp, .args)
    return(out)
}

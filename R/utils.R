.listFunctions <- function(filename) {
  temp.env <- new.env()
  sys.source(filename, envir = temp.env)
  functions <- lsf.str(envir=temp.env)
  return(functions)
}

#' @export
rlang::`%||%`

#' @title Fetch extra pages (if present)
#' @description Some endpoints return pages. This function fetches and collates them.
#' @param resp \code{(response)} The \link[httr]{response} object
#' @param .args \code{(list)} The `.args` provided to \link[httr]{RETRY}

paginate <- function(resp, .args) {
  out <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  if ((out$total_pages %||% 0) > 1) {
    out <- list(out)
    for (p in 2:out$total_pages) {
      .args$url <- httr::modify_url(.args$url, query = list(page = p))
      resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
      out <- append(out, list(httr::content(resp, as = "parsed", encoding = "UTF-8")))
    }
  }
  out <- make_time_entries_tbl(out)
  out
}

to_date <- function(x) {
  lubridate::with_tz(lubridate::parse_date_time(x, orders = c("Ymd", "YmdHMS")), tzone = Sys.timezone())
}

make_time_entries_tbl <- function(x) {
  if ("time_entries" %in% names(x) || all(names(x) %in% the_time_entry_object$Attribute)) {
    if ("time_entries" %in% names(x)) {
      te <- x$time_entries
      .md <- x[!names(x) %in% "time_entries"]
    } else
      te <- list(x)

    out <- purrr::map_dfr(te, ~tibble::tibble_row(!!!purrr::map(.x, ~purrr::when(is.list(.x), . ~ list(.x), .x)))) |>
      dplyr::mutate(dplyr::across(dplyr::matches("(?:at$)|(?:date$)"), to_date))
    if (exists(".md", inherits = FALSE))
      attr(out, "metadata") <- .md
  } else
    out
  return(out)
}

#' Round time entries to the hour decimal
#'
#' @param entries \code{(tbl)} of the entries to round
#' @param round_hours \code{(numeric)} the decimal hours to round to (IE 15 min = .25)
#'
#' @return \code{(tbl)} of changed entries with a `prev_hours` column with the previous amount of hours and a `difference` column with the total hours added
#' @export

round_time_entries <- function (entries = list_all_time_entries(from = lubridate::floor_date(Sys.Date() - 2, "week")), round_hours = .25) {
  out <- purrr::pmap_dfr(entries, ~{
    .x <- list(...)
    if (.x$hours %% round_hours != 0) {
      .h <- .x$hours + round_hours - .x$hours %% round_hours
      .entry <- update_a_time_entry(.x$id, hours = .h)
    } else
      .entry <- NULL
  })
  if (nrow(out)) {
     out <- dplyr::mutate(out, prev_hours = entries$hours[entries$id %in% id],
                          difference = hours - prev_hours)
     msg <- paste0(paste0(out$id,": ", out$prev_hours, " to ", out$hours), collapse = "\n")
     cli::cli_alert_success("Successfully rounded:\n {cli::col_grey(msg)}\nwhich added {.val {sum(out$difference)}} hrs.")
  }
  out
}

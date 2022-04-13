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
  .tp <- out$total_pages
  if ((out$total_pages %||% 0) > 1) {
    out <- list(out)
    for (p in 2:.tp) {
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
    out <- purrr::map_dfr(x$time_entries, ~ tibble::tibble_row(!!!purrr::map(.x, ~purrr::when(is.list(.x), . ~ list(.x), .x)))) |>
      dplyr::mutate(dplyr::across(dplyr::matches("(?:at$)|(?:date$)"), to_date))

      # sanity check
      stopifnot(x$total_entries == nrow(out))
  } else
    out <- x
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
     msg <- paste0(paste0(out$spent_date,": ", out$prev_hours, " to ", out$hours), collapse = "\n")
     cli::cli_alert_success("Successfully rounded:\n {cli::col_grey(msg)}\nwhich added {.val {sum(out$difference)}} hrs.")
  }
  out
}


time_entry_tag_sums <- function(entries) {
  tags <- stringr::str_extract_all(entries$notes, "\\#[^\\s]+") |>
    unique() |>
    purrr::keep(UU::is_legit)
  purrr::map(rlang::set_names(tags), ~{
    out <- dplyr::filter(entries, stringr::str_detect(notes, .x) %|% FALSE) |>
      dplyr::mutate(id = purrr::map_dbl(user, "id"), name = purrr::map_chr(user, "name")) |>
      dplyr::group_by(id, name) |>
      dplyr::summarise(hours = sum(rounded_hours))
  })
}

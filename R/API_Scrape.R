api_url <- "https://api.harvestapp.com"
scrape_api <- function(.url = "https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/") {
  htm <- xml2::read_html(.url)
  eps <- htm |>
    rvest::html_elements("h2")
  ids <- eps |>
    rvest::html_attr("id")
  names(ids) <- rvest::html_text(eps)
  eps <- purrr::imap(ids, ~{
    out <- list()
    els <- c("table", "div", "div[1]/preceding-sibling::p[preceding-sibling::h2[@id='{.x}']]")
    els <- glue::glue("//h2[@id='{.x}']/following-sibling::{els}")
    els[3] <- glue::glue(els[3])
    els <- purrr::map2(els, list(rvest::html_element, rvest::html_element, rvest::html_elements), ~rlang::exec(.y, htm, xpath = .x))

    if (!inherits(els[[1]], "xml_missing"))
      out$tbl<- rvest::html_table(els[[1]])
    out$ep <- rvest::html_text(els[[2]])
    out$description <- stringr::str_subset(rvest::html_text(els[[3]]), "^Example", negate = TRUE)
    out$from <- glue::glue("From \\href{{{paste0(.url, '#', .x)}}}{{{.y}}}")
    out
  })
}

xpath_sibling_between <- function(start_tag, end_tag, between_tag_type) {
  glue::glue("//{start_tag}/following-sibling::{end_tag}/preceding-sibling::{between_tag_type}[preceding-sibling::{start_tag}]")
}

tbl_roxygen <- function(x) {
  .attribs <- names(x)[1] == "Attribute"
  out <- purrr::pmap_chr(x, ~{
    r <- list(...)
    if (.attribs) {
      glue::glue_data(r, "\t\\item{{{Attribute}}}{{\\code{{({Type})}} {Description}}}")
    } else
      glue::glue_data(r, " @param {Parameter} \\code{{({Type})}} {ifelse('Required' %in% names(r), paste0('**', Required, '**'), '')} {Description}")
  })

  if (.attribs)
    out <- c("\\itemize{",
                paste0("\t", out),
                "}")

  paste0("#'", out)
}


rox_docs <- function(.x, .y) {
  has_tbl <- !is.null(.x$tbl)
  .x$fn_nm <- snakecase::to_snake_case(.y)
  docs <- c(
    glue::glue("#' @title {.y}"),
    glue::glue("#' @name {.x$fn_nm}"),
    glue::glue("#' @description"),
    glue::glue("#' {.x$description}"),
    glue::glue("#' {.x$from}")
    )

  if (has_tbl && !"Required" %in% names(.x$tbl))
    .x$tbl$Required <- "optional"
  if (stringr::str_detect(.x$ep, "TIME_ENTRY_ID")) {
    tid_r <- list(Parameter = "TIME_ENTRY_ID", Type = "integer", Required = "required", Description = "The ID of the time entry with which the call will be associated.")
    if (has_tbl)
      .x$tbl <- tibble::add_row(.x$tbl, !!!tid_r, .before = 1)
    else
      .x$tbl <- tibble::tibble(!!!tid_r)
    .x$tbl <- dplyr::arrange(.x$tbl, dplyr::desc(Required))
  }

  if (has_tbl)
    docs <- append(docs, tbl_roxygen(.x$tbl))

    .x$docs <- append(docs, if (stringr::str_detect(.y, "object$", negate = TRUE)) "#' @export" else NULL)
  return(.x)
}


ep_fun <- function(.x, .y) {
  if (!is.null(.x$ep)) {
    fn_chr <- stringr::str_extract(.x$ep, UU::regex_or(c("GET", "PATCH", "DELETE", "POST")))

    .path <- stringr::str_extract_all(.x$ep, "[\\w\\d\\_\\-\\{\\}]+")[[1]]
    .path <- .path[!.path %in% fn_chr]

    uses_time_entry <- stringr::str_detect(.x$ep, "TIME_ENTRY_ID")
    has_params <- suppressWarnings(!is.null(.x$tbl$Parameter))

    if (uses_time_entry || has_params) {
      if (has_params) {
        .args <- rlang::pairlist2(!!!purrr::pmap(.x$tbl, ~{
          .x <- list(...)
          if(.x$Required == "required" )
            rlang::missing_arg()
          else
            NULL
          }) |> rlang::set_names(.x$tbl$Parameter))
        # TIME_ENTRY_ID must be removed as it's a path param
        .query <- purrr::imap(rlang::set_names(purrr::when(uses_time_entry, . ~ .x$tbl$Parameter[-1], ~.x$tbl$Parameter)), ~rlang::expr(!!rlang::sym(.y)))
      } else {
        .query <- NULL
        .args <- NULL
      }


      # function body ----
      # Fri Jan 28 11:07:14 2022
      .body <- rlang::expr({
        api_fn <- !!fn_chr
        .pp <- api_fn %in% c("PATCH", "POST")
        .url <- list(api_url, path = !!.path)
        .args <- list(verb = api_fn)
        .query <- !!.query
        if (.pp)
          .args$body <- .query
        else
          .url$query <- .query

        .url <- do.call(httr::modify_url, .url)
        .url <- glue::glue(.url)
        .args$url <- .url
        .args$config <- httr::add_headers(.headers = c("Harvest-Account-ID" = get_harvest_account(),
                                                         Authorization = paste0("Bearer ", get_harvest_pat()),
                                                       "User-Agent" = get_user_agent()))

        resp <- httr::stop_for_status(do.call(httr::RETRY, .args))
        out <- paginate(resp, .args)
        return(out)
      })
      out_fn <- rlang::new_function(.args, .body)
    }
  }
  if (!exists("out_fn", inherits = FALSE))
    out_fn <- NULL
  .x$fun <- out_fn
  return(.x)
}

make_method <- function(.x, .y) {
  docs <- .x$docs
  if (!is.null(.x$fun)) {
    fun_chr <- capture.output(dput(.x$fun))
    fun_chr[1] <- paste0(.x$fn_nm," = ",fun_chr[1])
  } else {
    fun_chr = c(paste0(snakecase::to_snake_case(.y), " <- "), capture.output(dput(.x$tbl)))
  }
  docs <- append(docs, fun_chr)
  .x$docs <- docs
  .x
}



render_api <- function(.url = "https://help.getharvest.com/api-v2/timesheets-api/timesheets/time-entries/", output_file, append = TRUE) {
  eps <- scrape_api(.url)

  # the_obj <- eps[[1]]$tbl
  # api <- snakecase::to_snake_case(stringr::str_extract(.url, "[\\w\\-]+\\/?$"))
  # make_obj <- rlang::new_function(args = rlang::pairlist2(x = ), body = rlang::expr({
  #   if (names(x) %in% !!the_obj$Attribute)
  #     out <- dplyr::bind_rows(x)
  #   else if (!is.null(x[[!!api]])) {
  #     out <- dplyr::bind_rows(x[[1]])
  #     attr("metadata", out) <- x[-1]
  #   } else
  #     out <- x
  #   out
  # }))
  # make_fn_chr <- paste0("make_",api)
  # make_obj_chr <- capture.output(dput(make_obj))
  # make_obj_chr <- c(paste0(make_fn_chr, " <- ", make_obj_chr[1]), make_obj_chr[-1])

  eps <- purrr::imap(eps, rox_docs)
  eps <- purrr::map(eps, ep_fun)
  eps <- purrr::imap(eps, make_method)

  .write <- do.call(c, purrr::map(eps, ~c("","",.x$docs)))
  write(.write, file = output_file, append = append)
}
extract2 <- .Primitive("[[")


# purrr::imap(eps[-1], ~{
#   .args <- do.call(rlang::pairlist2, as.list(rlang::set_names(.x$tbl$Parameter)))
#   .call <- rlang::sym(paste0("httr::",stringr::str_extract(.x$ep, "^\\w+")))
#   rlang::new_function(.args, body = rlang::expr({
#     rlang::inject(.call)
#   }))
# })
#
#
# .args <- do.call(c, purrr::map(purrr::compact(purrr::map(eps[-c(1,2)], "tbl")), "Parameter")) |>
#   unique()
# .method <- tolower(stringr::str_extract(names(eps[-c(1,2)]), "^\\w+"))
# .switches <- purrr::imap(eps[-c(1,2)], ~{
#   rlang::expr(`=`(!!rlang::sym(tolower(stringr::str_extract(.y, "^\\w+"))),!!rlang::sym(paste0("httr::",stringr::str_extract(.x$ep, "^\\w+")))))
# }) |>
#   unique()
# rlang::expr(switch(
#   !!!.switches
# ))

#' @title Setup harvestR
#' @description The Harvest API requires the account ID,API key and a User Agent header for each request. See the [API Overview](https://help.getharvest.com/api-v2/introduction/overview/general/#api-requests) for details.
#' @seealso create_harvest_creds, set_harvest_creds
#' @param user_agent \code{(chr)} An identifier and email address to set as your User-Agent. See link in description for details.
#' @param overwrite \code{(log)} Whether to overwrite the existing value for HARVESTR_USER_AGENT in the user-level _.Renviron_ file.
#'
#' @return Nothing. Used for side-affects.
#' @export

setup_harvestR <- function(user_agent, overwrite = TRUE) {
  if (!all(nzchar(c(get_harvest_account(), get_harvest_pat()))) || overwrite) {
    create_harvest_creds()
    set_harvest_creds()
  }
  if (!nzchar(Sys.getenv("HARVESTR_USER_AGENT")) || overwrite) {
    Sys.setenv(HARVESTR_USER_AGENT = user_agent)
    .renv <- Sys.getenv("R_ENVIRON_USER")
    if (!nzchar(.renv))
      .renv <- "~/.Renviron"
    UU::mkpath(.renv)
    if (!any(stringr::str_detect(readLines(.renv), "HARVESTR_USER_AGENT")) || overwrite) {
      write(paste0("HARVESTR_USER_AGENT = \"", user_agent,"\""), .renv, append = TRUE)
      cli::cli_alert_success("{.val HARVESTR_USER_AGENT} set to {.val {user_agent}} in {.path {.renv}}")
    }
  }
}

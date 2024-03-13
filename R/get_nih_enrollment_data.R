#' Retrieve NIH-relevant Demographic Data From Databrary Volume(s)
#' 
#' @param vol_ids A single positive integer or an array of integers. 
#' The volume ids for a set of Databrary volumes. Default is 4.
#' @param start_date A character string in YYYY-MM-DD format indicating the 
#' earliest date for a range of sessions. Default is "2013-01-01".
#' @param end_date A character string in YYYY-MM-DD format indicating the 
#' latest date for a range of sessions. Default is "2014-01-01".
#' @param vb A logical value. Show verbose feedback.
#' @param rq An `httr2` request object. Default is NULL. To extract non-public 
#' (restricted) data, generate a request object (e.g., `drq <- databraryr::make_default_request()`).
#' Then login to Databrary via `databraryr::login_db()`. If this returns `TRUE`, then you may supply
#' the default request object as a parameter.
#' 
#' @returns A data frame with the demographic data for the selected volumes and sessions.
get_nih_enrollment_data <- function(vol_ids = 4,
                                    start_date = "2009-01-01",
                                    end_date = "2012-12-31",
                                    vb = FALSE,
                                    rq = NULL) {
  assertthat::is.number(vol_ids)
  assertthat::is.string(start_date)
  assertthat::is.string(end_date)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  if (is.null(rq)) {
    message("NULL request object supplied.")
    message("Only public (non-restricted) data will be shown.")
  }
  
  # Get data from Databrary
  if (vb) message("There are n=", length(vol_ids), " volumes to query.")
  demog_data <-
    purrr::map(vol_ids, suppressWarnings(get_volume_demog), vb = vb, rq = rq) |>
    purrr::list_rbind()
  
  # Filter
  if (vb)
    message("There are n=",
            dim(demog_data)[1],
            " sessions from n=",
            length(vol_ids),
            " volumes.")
  if (vb)
    message("Selecting session dates between ", start_date, " and ", end_date, ".")
  demog_filtered <- dplyr::filter(
    demog_data,
    lubridate::as_date(sess_d) > lubridate::as_date(start_date),
    lubridate::as_date(sess_d) < lubridate::as_date(end_date)
  )
  if (vb)
    message("Selected n=", dim(demog_filtered)[1], " sessions to summarize.")
  
  demog_filtered
}

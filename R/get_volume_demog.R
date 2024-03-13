#' Get Demographic Data From A Single Databrary Volume
#'
#' @param vol_ids A single positive integer.
#' The volume ids for a set of Databrary volumes. Default is NULL.
#' @param vb A logical value. Show verbose feedback. Default is FALSE.
#' @param suppressWarns A logical value. Suppress parsing warnings. Default is TRUE.
#' @param rq An `httr2` request object. Default is NULL. To extract non-public
#' (restricted) data, generate a request object (e.g., `drq <- databraryr::make_default_request()`).
#' Then login to Databrary via `databraryr::login_db()`. If this returns `TRUE`, then you may supply
#' the default request object as a parameter.
#'
#' @returns A summary table.
get_volume_demog <- function(vol_id = NULL,
                             vb = FALSE,
                             suppressWarns = TRUE,
                             rq = NULL) {
  assertthat::is.number(vol_id)
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(is.logical(suppressWarns))
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  if (is.null(rq)) {
    message("NULL request object supplied.")
    message("Only public (non-restricted) data will be shown.")
  }
  
  if (vb)
    message("Retrieving session CSV from volume ", vol_id, ".")
  
  if (suppressWarns) {
    v_ss <- suppressWarnings(databraryr::download_session_csv(
      vol_id,
      as_df = TRUE,
      vb = vb,
      rq = rq
    ))
  } else {
    v_ss <- databraryr::download_session_csv(vol_id,
                                             as_df = TRUE,
                                             vb = vb,
                                             rq = rq)
  }
  
  if (!is.null(v_ss)) {
    if (vb)
      message("CSV appears valid; processing")
    v_ss <-
      dplyr::filter(v_ss, !stringr::str_detect(session_date, "materials"))
    if ("participant_birthdate" %in% names(v_ss)) {
      if (any(is.na(v_ss$participant_birthdate))) {
        if (vb) {
          message("Invalid birthdates returned.")
          message("Are you logged-in to Databrary?")
          message("Removing NAs from participant_birthdate.")
        }
        v_ss <- v_ss |>
          dplyr::filter(!is.na(participant_birthdate))
      }
      v_ss <- dplyr::rename(
        v_ss,
        b_date = participant_birthdate,
        race = participant_race,
        ethnicity = participant_ethnicity,
        gender = participant_gender,
        sess_id = session_id,
        sess_rel = session_release,
        sess_d = session_date
      )
      
      # Force dates to be character
      v_ss <- dplyr::mutate(v_ss,
                            b_date = as.character(b_date),
                            sess_d = as.character(sess_d))
      
      # Add vol_id
      v_ss <- dplyr::mutate(v_ss, vol_id = vol_id)
      v_ss |>
        dplyr::select(all_of(
          c(
            "vol_id",
            "sess_id",
            "sess_rel",
            "sess_d",
            "b_date",
            "race",
            "ethnicity",
            "gender"
          )
        ))
    }
  } else {
    v_ss
  }
}

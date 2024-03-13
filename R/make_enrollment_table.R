#' Make NIH-format Enrollment Table From Demographic Data From Databrary Volume(s)
#' 
#' @param vol_ids A single positive integer or an array of integers. 
#' The volume ids for a set of Databrary volumes. Default is 4.
#' @param start_date A character string in YYYY-MM-DD format indicating the 
#' earliest date for a range of sessions. Default is "2009-01-01".
#' @param end_date A character string in YYYY-MM-DD format indicating the 
#' latest date for a range of sessions. Default is "2019-12-31".
#' @param vb A logical value. Show verbose feedback. Default is FALSE.
#' @param rq An `httr2` request object. Default is NULL. To extract non-public 
#' (restricted) data, generate a request object (e.g., `drq <- databraryr::make_default_request()`).
#' Then login to Databrary via `databraryr::login_db()`. If this returns `TRUE`, then you may supply
#' the default request object as a parameter.
#' 
#' @returns A summary table.
make_enrollment_table <- function(vol_ids = 4,
                                  start_date = "2009-01-01",
                                  end_date = "2019-12-31",
                                  vb = TRUE,
                                  rq = NULL) {
  
  
  assertthat::is.number(vol_ids)
  assertthat::assert_that(length(vol_ids) == sum(vol_ids > 0))
  assertthat::is.string(start_date)
  assertthat::is.string(end_date)
  assertthat::assert_that(lubridate::as_date(start_date) 
                          < lubridate::as_date(end_date))
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  if (is.null(rq)) {
    message("NULL request object supplied.")
    message("Only public (non-restricted) data will be shown.")
  }
  
  data_summ <- get_nih_enrollment_data(vol_ids,
                                       start_date,
                                       end_date,
                                       vb,
                                       rq) |>
    clean_nih_demog_data() |>
    dplyr::select(race, gender, ethnicity) |>
    dplyr::group_by(race, gender, ethnicity) |>
    dplyr::summarise(n = dplyr::n())
  
  ftable(
    xtabs(n ~ race + ethnicity + gender, data = data_summ),
    row.vars = 1,
    col.vars = c(2, 3)
  )
}

#-------------------------------------------------------------------------------
clean_nih_demog_data <- function(df) {
  df |>
    dplyr::mutate(
      race = replace(race, race == "Not reported", "Unknown"),
      race = replace(race, race == "Unknown or not reported", "Unknown"),
      race = replace(race, race == "Black or African American", "Black"),
      race = replace(race, race == "White and Afro LAtina", "More than one"),
      race = replace(race, race == "Black or African American White", "More than one"),
      race = replace(race, race == "White, Asian, Hawaiian", "More than one"),
      race = replace(race, race == "While", "White"),
      race = replace(race, race == "White and Asian", "More than one"),
      race = replace(race, is.na(race), "Unknown"),
      ethnicity = replace(ethnicity, ethnicity == "Hispanic or Latino", "Hisp/Lat"),
      ethnicity = replace(ethnicity, ethnicity == " Hispanic or Latino", "Hisp/Lat"),
      ethnicity = replace(ethnicity, ethnicity == "Not Hispanic or Latino", "Not Hisp/Lat"),
      ethnicity = replace(ethnicity, ethnicity == "did not answer", "Unknown"),
      ethnicity = replace(ethnicity, ethnicity == "Chose not to answer", "Unknown"),
      ethnicity = replace(ethnicity, ethnicity == "Choose not to answer", "Unknown"),
      ethnicity = replace(ethnicity, ethnicity == "Unknown or not reported", "Unknown"),
      ethnicity = replace(ethnicity, is.na(ethnicity), "Unknown"),
      gender = replace(gender, is.na(gender), "Unknown or not reported"),
      gender = replace(gender, gender == "Non-binary", "Unknown or not reported"),
      gender = replace(gender, gender == "Non binary", "Unknown or not reported")
    )     
}
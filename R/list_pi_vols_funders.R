#' Generate Dataframe with PI Volumes and Funders
#'
#' @param party_id An integer. The party ID for an Authorized Investigator.
#' Default is 6.
#' @param vb A logical value. Show verbose output. Default is TRUE.
#' @rq rq An `httr2` request object. Default is NULL.
#'
list_pi_vols_funders <- function(party_id, vb = TRUE, rq = NULL) {
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id > 0)
  
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  # Handle NULL rq
  if (is.null(rq)) {
    if (vb) {
      message("NULL request object. Will generate default.")
      message("\nNot logged in. Only public information will be returned.")
    }
    rq <- databraryr::make_default_request()
  }
  
  # TODO: Replace with databraryr::list_party_volumes() when fix is pushed.
  pi_vols <- list_party_vols(party_id, vb, rq)
  
  vol_funders <-
    purrr::map(pi_vols$vol_id,
               get_vol_funders,
               vb = vb,
               rq = rq, .progress = TRUE) %>%
    purrr::list_rbind()
  
  dplyr::left_join(pi_vols, vol_funders, by = "vol_id")
}

#-------------------------------------------------------------------
get_vol_funders <- function(vol_id, vb = TRUE, rq = NULL) {
  these_vol_funders <- databraryr::list_volume_funding(vol_id, vb, rq)
  
  dplyr::mutate(these_vol_funders, vol_id = vol_id)
}
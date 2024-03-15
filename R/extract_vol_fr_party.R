extract_vol_fr_party <- function(p_info) {
  this_vol <- p_info$volume
  
  vol_id <- this_vol$id
  vol_name <- this_vol$name
  vol_body <- this_vol$body
  vol_alias <- this_vol$alias
  vol_created <- this_vol$creation
  vol_permission <- this_vol$permission
  
  tibble::tibble(vol_id,
                 vol_name,
                 vol_body,
                 vol_alias,
                 vol_created,
                 vol_permission)
}

list_party_vols <- function(party_id, vb, rq) {
  # Check parameters
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
  
  if (vb)
    message(paste0("Retrieving data for party ", party_id, "."))
  party_info <- databraryr::get_party_by_id(party_id, vb, rq)
  
  if (!is.null(party_info)) {
    if (vb)
      message(paste0("Info retrieved. Filtering."))
    purrr::map(party_info$access, extract_vol_fr_party) %>%
      purrr::list_rbind() %>%
      dplyr::mutate(
        party_id = party_id,
        prename = party_info$prename,
        sortname = party_info$sortname,
        affiliation = party_info$affiliation
      ) %>%
      dplyr::arrange(vol_id)
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    party_info
  }
}

# helpers

Code to help with interactions with the Databrary API.

## Install the `databraryr` package

```
install.packages("databraryr")
```

This will also request you to install the package dependencies.

## Generate an NIH enrollment table

1. Source the helper functions:

```
purrr::map(list.files("R", "\\.R$", full.names = TRUE), source)
```

3. Log-in to Databrary:

```
# Make a default httr2 request
drq <- databraryr::make_default_request()

# Log-in using your Databrary credentials and store them in your
# secure system credentials file.
databraryr::login_db(email = YOUR_EMAIL, store = TRUE, rq = drq)
```

3. Choose the Databrary volume IDs you want to summarize.

4. Choose the date range (start_date and end_date) for the sessions you want to include.

5. Generate the enrollment table:

```
make_enrollment_table <- function(vol_ids = c(1, 2, 3),
                                  start_date = "2009-01-01",
                                  end_date = "2019-12-31",
                                  vb = FALSE,
                                  rq = drq)
```


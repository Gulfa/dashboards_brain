#' amort
#' @import R6
#' @export amort
amort <- R6::R6Class(
  "amort",
  portable = FALSE,
  cloneable = FALSE,
  list(
    run_all = function() {
      # check to see if it can run
      if (!fd::exists_rundate("normomo")) {
        return()
      }
      if (!fd::exists_rundate("sykdomspuls")) {
        return()
      }
      if (!fd::exists_rundate("weather")) {
        return()
      }

      rundate <- fd::get_rundate()

      # determine if it should run
      run <- TRUE
      if (fd::exists_rundate("brain_amort")) {
        if (rundate[package == "brain_amort"]$date_extraction >= rundate[package == "normomo"]$date_extraction) run <- FALSE
        if (rundate[package == "brain_amort"]$date_extraction >= rundate[package == "sykdomspuls"]$date_extraction) run <- FALSE
      }
      if (rundate[package == "normomo"]$date_results != rundate[package == "sykdomspuls"]$date_results) run <- FALSE

      if (!run & fd::config$is_production) {
        return()
      }

      fd::msg("Brain amort - creating/uploading RRs", slack = T)
      amort_upload_rrs()

      years <- seq(2012, fhi::isoyear_n(), 1)
      for (i in years) {
        fd::msg(glue::glue("Brain amort - estimating numbers {i}"))
        amort_results(year_max = i)
      }

      date_extraction <- max(
        rundate[package == "normomo"]$date_extraction,
        rundate[package == "sykdomspuls"]$date_extraction
      )

      date_results <- max(
        rundate[package == "normomo"]$date_results,
        rundate[package == "sykdomspuls"]$date_results
      )

      # update rundate
      fd::update_rundate(
        package = "brain_amort",
        date_extraction = date_extraction,
        date_results = date_results,
        date_run = lubridate::today()
      )
    }
  )
)

amort_get_fits <- function(
                           year_max = fhi::isoyear_n(),
                           year_min = year_max - 2) {
  weather <- fd::get_weather(impute_missing = TRUE)

  locs <- unique(c("norge", fhidata::norway_locations_current$county_code))
  fits <- vector("list", length = length(locs))
  for (i in seq_along(locs)) {
    loc <- locs[i]
    mem <- fd::tbl("spuls_mem_results") %>%
      dplyr::filter(tag == "influensa") %>%
      dplyr::filter(location_code == !!loc) %>%
      dplyr::collect() %>%
      fd::latin1_to_utf8()

    ils <- data.table(date = seq.Date(min(mem$date), max(mem$date), 1))
    ils[mem, on = "date", ils := rate]
    ils[, ils := zoo::na.locf(ils, fromLast = T)]

    d <- fd::tbl("normomo_daily_results") %>%
      dplyr::filter(location_code == !!loc) %>%
      dplyr::filter(age == "Total") %>%
      dplyr::collect() %>%
      fd::latin1_to_utf8()

    dates <- intersect(weather$date, d$date)
    dates <- intersect(dates, ils$date)
    dates <- dates[fhi::isoyear_n(as.Date(dates, origin = "1970-01-01")) %in% year_min:year_max]

    w <- weather[date %in% dates & location_code == loc]
    d <- d[date %in% dates]
    ils <- ils[date %in% dates]
    dates <- sort(dates)
    dates <- as.Date(dates, origin = "1970-01-01")

    outcome <- d$nbc
    temp <- w$tx
    ils <- ils$ils * 10

    fits[[i]] <- attrib::fit_attrib(
      dates = dates,
      outcome = outcome,
      exposure_values = list(
        "tx" = temp,
        "ilsper1000" = ils
      ),
      exposure_types = list(
        "tx" = "cubic",
        "ilsper1000" = "linear"
      ),
      exposure_knots = list(
        "tx" = c(-5, 20)
      ),
      exposure_boundary_knots = list(
        "tx" = c(-25, 35)
      )
    )
  }

  x <- attrib::create_blup(
    fits[-1]
  )

  return(list(
    location_codes = locs,
    dates = dates,
    norge = fits[[1]],
    counties = x
  ))
}

amort_upload_rrs <- function(
                             year_max = fhi::isoyear_n(),
                             year_min = year_max - 2) {
  x <- amort_get_fits(
    year_max = year_max,
    year_min = year_min
  )

  brain_amort_rr_field_types <- c(
    "location_code" = "TEXT",
    "age" = "TEXT",
    "year_train_min" = "INTEGER",
    "year_train_max" = "INTEGER",
    "exposure" = "TEXT",
    "exposure_value" = "INTEGER",
    "rr_est" = "DOUBLE",
    "rr_l95" = "DOUBLE",
    "rr_u95" = "DOUBLE"
  )

  brain_amort_rr_keys <- c(
    "location_code",
    "age",
    "year_train_min",
    "year_train_max",
    "exposure",
    "exposure_value"
  )

  rr_x <- fd::schema$new(
    db_config = fd::config$db_config,
    db_table = glue::glue("brain_amort_rr"),
    db_field_types = brain_amort_rr_field_types,
    db_load_folder = "/xtmp/",
    keys = brain_amort_rr_keys,
    check_fields_match = TRUE
  )
  rr_x$db_connect()

  year_train_min <- fhi::isoyear_n(min(x$dates))
  year_train_max <- fhi::isoyear_n(max(x$dates))
  age <- "Totalt"

  for (i in 1:(length(x$counties) + 1)) {
    if (i == 1) {
      attrib_small <- x$norge$attrib_fixed
    } else {
      attrib_small <- x$counties[[i - 1]]$attrib_blup
    }
    for (ex in names(attrib_small$pred)) {
      exposure <- ex
      exposure_value <- as.numeric(names(attrib_small$pred[[ex]]$allRRfit))
      rr_est <- as.numeric(attrib_small$pred[[ex]]$allRRfit)
      rr_l95 <- as.numeric(attrib_small$pred[[ex]]$allRRlow)
      rr_u95 <- as.numeric(attrib_small$pred[[ex]]$allRRhigh)

      upload <- data.table(
        location_code = x$location_codes[i],
        age = age,
        year_train_min = year_train_min,
        year_train_max = year_train_max,
        exposure = exposure,
        exposure_value = exposure_value,
        rr_est = rr_est,
        rr_l95 = rr_l95,
        rr_u95 = rr_u95
      )

      rr_x$db_upsert_load_data_infile(upload)
    }
  }
}


amort_results <- function(
                          year_max = fhi::isoyear_n(),
                          year_min = year_max - 2) {
  brain_amort_results_field_types <- c(
    "granularity_time" = "TEXT",
    "granularity_geo" = "TEXT",
    "location_code" = "TEXT",
    "age" = "TEXT",
    "season" = "TEXT",
    "yrwk" = "TEXT",
    "date" = "DATE",
    "exposure" = "TEXT",
    "exposure_value" = "TEXT",
    "attr_est" = "DOUBLE",
    "attr_low" = "DOUBLE",
    "attr_high" = "DOUBLE"
  )

  brain_amort_results_keys <- c(
    "granularity_time",
    "granularity_geo",
    "location_code",
    "age",
    "season",
    "yrwk",
    "date",
    "exposure",
    "exposure_value"
  )

  results_x <- fd::schema$new(
    db_config = fd::config$db_config,
    db_table = glue::glue("brain_amort_results"),
    db_field_types = brain_amort_results_field_types,
    db_load_folder = "/xtmp/",
    keys = brain_amort_results_keys,
    check_fields_match = TRUE
  )
  results_x$db_connect()

  x <- amort_get_fits(
    year_max = year_max,
    year_min = year_min
  )

  # hot summers
  index_summers <- fhi::isoweek_n(x$dates) %in% 21:39
  number_summers <- which(index_summers)
  seasons <- fhi::isoyear_c(x$dates)[index_summers]
  summers <- split(number_summers, seasons)

  a <- attrib::get_attrib(x$counties, use_blup = T, tag = "tx", range = c(25, 100), sub = summers)
  a$granularity_time <- "seasonal"
  a$granularity_geo <- "national"
  a$location_code <- "norge"
  a$age <- "Totalt"
  a$season <- names(summers)
  a$yrwk <- glue::glue("{a$season}-21")
  a[fhidata::days, on = "yrwk", date := sun]
  a$exposure <- "tx"
  a$exposure_value <- "hot"

  results_x$db_upsert_load_data_infile(a)

  # winter dates
  index_winters <- !fhi::isoweek_n(x$dates) %in% 21:39
  number_winters <- which(index_winters)
  seasons <- fhi::season(fhi::isoyearweek(x$dates), start_week = 40)[index_winters]
  winters <- split(number_winters, seasons)[-1]

  # winters - cold
  a <- attrib::get_attrib(x$counties, use_blup = T, tag = "tx", range = c(-100, -5), sub = winters)
  a$granularity_time <- "seasonal"
  a$granularity_geo <- "national"
  a$location_code <- "norge"
  a$age <- "Totalt"
  a$season <- names(winters)
  a$yrwk <- glue::glue("{stringr::str_sub(a$season,1,4)}-40")
  a[fhidata::days, on = "yrwk", date := sun]
  a$exposure <- "tx"
  a$exposure_value <- "cold"

  results_x$db_upsert_load_data_infile(a)

  # winters - ils
  a <- attrib::get_attrib(x$counties, use_blup = T, tag = "ilsper1000", range = c(1, 1000), sub = winters)
  a$granularity_time <- "seasonal"
  a$granularity_geo <- "national"
  a$location_code <- "norge"
  a$age <- "Totalt"
  a$season <- names(winters)
  a$yrwk <- glue::glue("{stringr::str_sub(a$season,1,4)}-40")
  a[fhidata::days, on = "yrwk", date := sun]
  a$exposure <- "ilsper1000"
  a$exposure_value <- "any"

  results_x$db_upsert_load_data_infile(a)
}

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
      rundate <- fd::get_rundate()
      run <- TRUE
      if ("brain_amort" %in% rundate$package) {
        if (rundate[package == "brain_amort"]$date_extraction >= rundate[package == "normomo"]$date_extraction) run <- FALSE
        if (rundate[package == "brain_amort"]$date_extraction >= rundate[package == "sykdomspuls"]$date_extraction) run <- FALSE
      }
      if (!run & fd::config$is_production) {
        return()
      }

      amort_upload_rrs()

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

amort_get_fits <- function(){
  weather <- fd::get_weather(impute_missing = TRUE)

  locs <- unique(c("norge",fhidata::norway_locations_current$county_code))
  fits <- vector("list", length = length(locs))
  for(i in seq_along(locs)){
    loc <- locs[i]
    mem <- fd::tbl("spuls_mem_results") %>%
      dplyr::filter(tag == "influensa") %>%
      dplyr::filter(location_code == !!loc) %>%
      dplyr::collect() %>%
      fd::latin1_to_utf8()

    ils <- data.table(date=seq.Date(min(mem$date),max(mem$date),1))
    ils[mem,on="date",ils:=rate]
    ils[,ils:=zoo::na.locf(ils, fromLast=T)]

    d <- fd::tbl("normomo_daily_results") %>%
      dplyr::filter(location_code == !!loc) %>%
      dplyr::filter(age == "Total") %>%
      dplyr::collect() %>%
      fd::latin1_to_utf8()

    dates <- intersect(weather$date, d$date)
    dates <- intersect(dates, ils$date)

    w <- weather[date %in% dates & location_code==loc]
    d <- d[date %in% dates]
    ils <- ils[date %in% dates]
    dates <- sort(dates)
    dates <- as.Date(dates, origin = "1970-01-01")

    outcome <- d$nbc
    temp <- w$tx
    ils <- ils$ils

    fits[[i]] <- attrib::fit_attrib(
      dates = dates,
      outcome = outcome,
      exposure_values = list(
        "tx" = temp,
        "ils" = ils
      ),
      exposure_types = list(
        "tx" = "cubic",
        "ils" = "linear"
      ),
      exposure_knots = list(
        "tx" = c(-10,20)
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
    norge=fits[[1]],
    counties=x
  ))
}



amort_upload_rrs <- function(){
  x <- amort_get_fits()

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
    db_config = CONFIG$db_config,
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

  for(i in 1:(length(x$counties)+1)){
    if(i==1){
      attrib_small <- x$norge$attrib_fixed
    } else {
      attrib_small <- x$counties[[i-1]]$attrib_blup
    }
    for(ex in names(attrib_small$pred)){
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

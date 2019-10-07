.onLoad <- function(libname, pkgname) {
  fd::initialize(
    package = "ui",
    load_package = FALSE
  )

  tryCatch({
    actions[["weather_download"]] <- fd::action$new(
      key = "ui_weather_download",
      value = lubridate::today(),
      dev_always_performs = TRUE,
      production_days = c(1:7),
      first_date_of_production = "2019-09-21"
    )
  },
  error = function(e) {
    fd::msg("No database available")
  }
  )
}

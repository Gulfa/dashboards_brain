#' amort
#' @import R6
#' @export weather_download
weather_download <- R6::R6Class(
  "weather_download",
  portable = FALSE,
  cloneable = FALSE,
  list(
    run_all = function() {
      # weather_download
      if (actions[["weather_download"]]$can_perform_action()) {
        fd::update_weather()
        actions[["weather_download"]]$action_performed()
      }
    }
  )
)

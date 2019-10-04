#' CONFIG
#' @export CONFIG
CONFIG <- new.env()
CONFIG$VALID_FYLKE <- c(1:12, 14:15, 50, 18:20)
CONFIG$WEEKS_UNRELIABLE <- 1
CONFIG$HISTORICAL_ANALYSES <- seq(0, 4 * 52 * 7, by = 7)
# CONFIG$HISTORICAL_DELAY_VERSIONS <- c("original","2017-12","richard")
CONFIG$HISTORICAL_DELAY_VERSIONS <- c("original", "richard")

CONFIG$db_config <- list(
  driver = Sys.getenv("DB_DRIVER", "MySQL"),
  server = Sys.getenv("DB_SERVER", "db"),
  port = as.integer(Sys.getenv("DB_PORT", 3306)),
  user = Sys.getenv("DB_USER", "root"),
  password = Sys.getenv("DB_PASSWORD", "example"),
  db = Sys.getenv("DB_DB", "sykdomspuls")
)

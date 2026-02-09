#' Read a value from golem config (safe for top-level use)
#'
#' Uses fully-qualified config::get() so it works during load_all()
#' when the package namespace may not be fully available yet.
#'
#' @param value Value to retrieve from the config file.
#' @return The config value
#' @noRd
get_db_config <- function(value) {
  config_file <- system.file("golem-config.yml", package = "kypoliticaldata")
  if (config_file == "") {
    # Fallback for load_all() / dev: try inst/ directly
    config_file <- file.path("inst", "golem-config.yml")
  }
  config::get(
    value = value,
    config = Sys.getenv(
      "GOLEM_CONFIG_ACTIVE",
      Sys.getenv("R_CONFIG_ACTIVE", "default")
    ),
    file = config_file,
    use_parent = TRUE
  )
}

#' Create a database connection pool
#'
#' Reads connection parameters from golem-config.yml.
#' In production, connects via local socket (no credentials).
#' In dev, connects to localhost (SSH tunnel) with keyring credentials.
#'
#' @return A pool::Pool object
#' @noRd
create_db_pool <- function() {
  db_name <- get_db_config("db_name")
  db_host <- get_db_config("db_host")
  db_port <- get_db_config("db_port")
  use_keyring <- get_db_config("db_use_keyring")

  db_driver <- get_db_config("db_driver")

  if (isTRUE(use_keyring)) {
    db_user <- keyring::key_list("kypolitics_db")$username[1]
    db_pass <- keyring::key_get("kypolitics_db", username = db_user)

    pool::dbPool(
      odbc::odbc(),
      .connection_string = paste0(
        "Driver={", db_driver, "};",
        "Server=", db_host, ";",
        "Port=", db_port, ";",
        "Database=", db_name, ";",
        "Uid=", db_user, ";",
        "Pwd=", db_pass, ";"
      )
    )
  } else {
    pool::dbPool(
      odbc::odbc(),
      .connection_string = paste0("Driver={", db_driver, "};"),
      Database = db_name
    )
  }
}

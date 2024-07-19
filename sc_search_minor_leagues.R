sc_search_minor_leagues <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
                             playerid = NULL,
                             player_type = "batter", ...) {
  
  message("Minor League Statcast tracking is available since the 2021 season for certain levels and ballparks. 
          Data is available for (per Baseball Savant):
          All Triple-A games starting with the 2023 season
          Pacific Coast League games and Charlotte home games for the 2022 season
          Florida State League (Single-A) games starting with the 2021 season")
  # Check for other user errors.
  if (start_date <= "2021-03-01") { # March 1, 2015 was the first date of Spring Training.
    message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled at the minor league level since 2021")
  }
  if (start_date == Sys.Date()) {
    message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
  }
  if (start_date > as.Date(end_date)) {
    stop("The start date is later than the end date.")
    return(NULL)
  }
  
  playerid_var <- ifelse(player_type == "pitcher",
                         "pitchers_lookup%5B%5D", "batters_lookup%5B%5D")
  
  vars <- tibble::tribble(
    ~var, ~value,
    "all", "true",
    "hfPT", "",
    "hfAB", "",
    "hfBBT", "",
    "hfPR", "",
    "hfZ", "",
    "stadium", "",
    "hfBBL", "",
    "hfNewZones", "",
    "hfGT", "R%7CPO%7CS%7C&hfC",
    "hfSea", paste0(lubridate::year(start_date), "%7C"),
    "hfSit", "",
    "hfOuts", "",
    "opponent", "",
    "pitcher_throws", "",
    "batter_stands", "",
    "hfSA", "",
    "player_type", player_type,
    "hfInfield", "",
    "team", "",
    "position", "",
    "hfOutfield", "",
    "hfRO", "",
    "home_road", "",
    playerid_var, ifelse(is.null(playerid), "", as.character(playerid)),
    "game_date_gt", as.character(start_date),
    "game_date_lt", as.character(end_date),
    "hfFlag", "",
    "hfPull", "",
    "metric_1", "",
    "hfInn", "",
    "min_pitches", "0",
    "min_results", "0",
    "group_by", "name",
    "sort_col", "pitches",
    "player_event_sort", "h_launch_speed",
    "sort_order", "desc",
    "min_abs", "0",
    "type", "details",
    "minors","true") %>%
    dplyr::mutate(pairs = paste0(.data$var, "=", .data$value))
  
  if (is.null(playerid)) {
    # message("No playerid specified. Collecting data for all batters/pitchers.")
    vars <- vars %>% 
      dplyr::filter(!grepl("lookup", .data$var))
  }
  
  url_vars <- paste0(vars$pairs, collapse = "&")
  url <- paste0("https://baseballsavant.mlb.com/statcast-search-minors/csv?", url_vars)
  # message(url)
  
  # Do a try/catch to show errors that the user may encounter while downloading.
  tryCatch(
    { 
      suppressMessages(
        suppressWarnings(
          payload <- baseballr:::csv_from_url(url, encoding ="UTF-8")
        )
      )
    },
    error = function(cond) {
      message(cond)
      stop("No payload acquired")
    },
    # this will never run??
    warning = function(cond) {
      message(cond)
    }
  )

  return(payload)
}
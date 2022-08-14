#' Title Convert Fangraphs MLB Team Abbreviations to Full names
#'
#' @param df a dataframe containing team names pulling from Fangraphs
#'  (like using fg_pitcher_game_logs or fg_pitch_leaders or fg_bat_leaders)
#'
#' @return a column that replaces the abbreviated team name with the full name
#' @export
#'
#' @examples
convert_mlb_abbr_to_full_name <- function(df) {
  
  df <- df %>% mutate(Team = case_when(Team == "ARI" ~ "Arizona Diamondbacks",
                                       Team == "ATL" ~ "Atlanta Braves",
                                       Team == "BAL" ~ "Baltimore Orioles",
                                       Team == "BOS" ~ "Boston Red Sox",
                                       Team == "CHC" ~ "Chicago Cubs",
                                       Team == "CHW" ~ "Chicago White Sox",
                                       Team == "CIN" ~ "Cincinnati Reds",
                                       Team == "CLE" ~ "Cleveland Indians",
                                       Team == "COL" ~ "Colorado Rockies",
                                       Team == "DET" ~ "Detroit Tigers",
                                       Team == "HOU" ~ "Houston Astros",
                                       Team == "KCR" ~ "Kansas City Royals",
                                       Team == "LAA" ~ "Los Angeles Angels",
                                       Team == "LAD" ~ "Los Angeles Dodgers",
                                       Team == "MIA" ~ "Miami Marlins",
                                       Team == "MIL" ~ "Milwaukee Brewers",
                                       Team == "MIN" ~ "Minnesota Twins",
                                       Team == "NYM" ~ "New York Mets",
                                       Team == "NYY" ~ "New York Yankees",
                                       Team == "OAK" ~ "Oakland Athletics",
                                       Team == "PHI" ~ "Philadelphia Phillies",
                                       Team == "PIT" ~ "Pittsburgh Pirates",
                                       Team == "SDP" ~ "San Diego Padres",
                                       Team == "SEA" ~ "Seattle Mariners",
                                       Team == "SFG" ~ "San Francisco Giants",
                                       Team == "STL" ~ "St. Louis Cardinals",
                                       Team == "TBR" ~ "Tampa Bay Rays",
                                       Team == "TEX" ~ "Texas Rangers",
                                       Team == "TOR" ~ "Toronto Blue Jays",
                                       Team == "WSN" ~ "Washington Nationals",
                                       TRUE ~ "Multiple Teams"))
  
  return(df)
  
}
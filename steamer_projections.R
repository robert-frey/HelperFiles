library(jsonlite)
library(dplyr)

steamer_projections <- function() {
fg_api <- "https://www.fangraphs.com/api/projections?type=steamer&stats=bat&pos=all&team=0&players=0&lg=all"

data <- jsonlite::fromJSON(fg_api) %>%
  dplyr::mutate(season = 2024) %>%
  # dplyr::select(season,
  #               player_id = playerids,
  #               name = PlayerName,
  #               team_abbr = Team,
  #               team_name = ShortName,
  #               everything(),
  #               -Name,-League,-ADP,-Pos) %>%
  dplyr::select(season,
                player_id = playerids,
                name = PlayerName,
                team_abbr = Team,
                team_name = ShortName,
                G,
                PA,
                HR,
                RBI,
                OPS,
                BB_pct = `BB%`,
                K_pct = `K%`,
                `wRC+`,
                WAR) %>%
  dplyr::mutate(OPS = round(OPS,3),
                BB_pct = round(BB_pct,3),
                K_pct = round(K_pct,3),
                `wRC+` = round(`wRC+`),
                WAR = round(WAR,1))

fg_23_api <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2023&season1=2023&startdate=2023-03-01&enddate=2023-11-01&month=0&hand=&team=0&pageitems=100000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"

data3 <- jsonlite::fromJSON(fg_23_api)[['data']] %>%
  dplyr::mutate(playerid = as.character(playerid),
                OPS = round(OPS,3),
                `BB%` = round(`BB%`,3),
                `K%` = round(`K%`,3),
                `wRC+` = round(`wRC+`),
                WAR = round(WAR,1)) %>%
  dplyr::select(player_id = playerid,
         G_23 = G,
         PA_23 = TPA,
         HR_23 = HR,
         RBI_23 = RBI,
         OPS_23 = OPS,
         BB_pct_23 = `BB%`,
         K_pct_23 = `K%`,
         `wRC+_23` = `wRC+`,
         WAR_23 = WAR)

projections <- dplyr::left_join(data,data3,by="player_id")
  

projections
}

steamer_projections()
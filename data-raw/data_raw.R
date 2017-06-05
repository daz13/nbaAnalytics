# data_raw.R


## Load all players
all_players <- nbaStatsGrabber(urlBuilder(statCat = "commonallplayers", TeamID = "1610612751"))

## Teams
teams <- unique(all_players$TEAM_CODE)
team_ids <- unique(all_players$TEAM_ID)
team_ids <- team_ids[team_ids != "0"]

teams_list <- lapply(as.character(team_ids), function(id) nbaStatsGrabber(urlBuilder(statCat = "teaminfocommon", TeamID = id)))
teams_list <-  do.call("rbind", teams_list)

### Players
players_per_team <- lapply(as.character(teams_list$TEAM_CODE),
                           function(team) all_players %>% filter(TEAM_CODE == team))
names(players_per_team) <- as.character(teams_list$TEAM_CODE)

devtools::use_data(teams_list, overwrite = T)
devtools::use_data(players_per_team, overwrite = T)
devtools::use_data(all_players, overwrite = T)


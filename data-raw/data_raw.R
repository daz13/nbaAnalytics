# data_raw.R


## Load all players
all_players <- nbaStatsGrabber(urlBuilder(statCat = "commonallplayers", parameters = list(LeagueID         = "00",
                                                                                      isOnlyCurrentSeason  = "1",
                                                                                      Season               = "2016-17",
                                                                                      DateFrom             = "",
                                                                                      DateTo               = "",
                                                                                      SeasonType           = "Regular+Season",
                                                                                      TeamID               = "1610612751",
                                                                                      PlayerID             = "",
                                                                                      PerMode              = "PerGame")
                                          )
                               )

## Teams
teams <- unique(all_players$TEAM_CODE)
team_ids <- unique(all_players$TEAM_ID)
team_ids <- team_ids[team_ids != "0"]

teams_list <- lapply(as.character(team_ids), function(id) nbaStatsGrabber(urlBuilder(statCat = "teaminfocommon", list(TeamID = id))))
teams_list <-  do.call("rbind", teams_list)

allteamsDF <- lapply(team_ids, function(x) create_TeamSeasonDF(x, seasons = seasons))
names(allteamsDF) <- teams[-10]
devtools::use_data(allteamsDF)

### Players
players_per_team <- lapply(as.character(teams_list$TEAM_CODE),
                           function(team) all_players %>% filter(TEAM_CODE == team))
names(players_per_team) <- as.character(teams_list$TEAM_CODE)

devtools::use_data(teams_list, overwrite = T)
devtools::use_data(players_per_team, overwrite = T)
devtools::use_data(all_players, overwrite = T)


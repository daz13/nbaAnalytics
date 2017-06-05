# players.R

library(ggplot2)
## Plot Career stats

#'
#'
#' @examples
#' get_PlayerCareerStats_by_name("Dirk Nowitzki")
get_PlayerCareerStats_by_name <- function(n) {
  id <- nbaAnalytics::all_players %>% filter(DISPLAY_FIRST_LAST == n) %>% select(PERSON_ID)
  nbaStatsGrabber((urlBuilder(statCat = "playercareerstats", PlayerID = as.character(id[1,1]))))
}



#'
#'
#' @examples
#' get_PlayerGameLog_by_name("Dirk Nowitzki")
get_PlayerGameLog_by_name <- function(n) {
  id <- nbaAnalytics::all_players %>% filter(DISPLAY_FIRST_LAST == n) %>% select(PERSON_ID)
  nbaStatsGrabber((urlBuilder(statCat = "playergamelog", PlayerID = as.character(id[1,1]))))
}


#'
#'
#' @examples
#' testDat <- get_PlayerCareerStats_by_name("Dirk Nowitzki")
#' plot_PlayerStats(testDat, "PTS")
#' plot_PlayerStats(get_PlayerCareerStats_by_name("LeBron James"), "FG_PCT")
#' plot_PlayerStats(get_PlayerCareerStats_by_name("LeBron James"), "PTS")
plot_PlayerStats <- function(data, statistic) {
  data %>%
    mutate_(STAT = statistic) %>% mutate(STAT = as.numeric(as.character(STAT))) %>%
    ggplot(aes(x = SEASON_ID, y = STAT, fill = TEAM_ABBREVIATION)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_bw()
}



# Some probably useful links:
#     - Player Info commonplayerinfor
#     - Player Career Stats http://stats.nba.com/stats/playercareerstats?LeagueID=00&PerMode=PerGame&PlayerID=201167
#     - Player Season Stats http://stats.nba.com/stats/playergamelog?DateFrom=&DateTo=&LeagueID=00&PlayerID=201566&Season=2016-17&SeasonType=Regular+Season
#     - Player Game Log http://stats.nba.com/stats/playergamelog?DateFrom=&DateTo=&LeagueID=00&PlayerID=201167&Season=2016-17&SeasonType=Regular+Season

#'
#' Resource playergamelog
#' MeasureType
#' PerMode
#' LeagueID
#' SeasonType
#' SeasonYear
#' PORound
#' TeamID
#' PlayerID
#' Outcome
#' Location
#' Month
#' SeasonSegment
#' DateFrom
#' DateTo
#' OppTeamID
#' VsConference
#' VSDivision
#' GameSegment
#' Period
#' ShotClockRange
#' LastNGames
#'
#' Resource: commonplayerinfo
#' PlayerID
#' LeagueID
#'
#' Resource: playercareerstats
#' PlayerID
#' LeagueID
#' PerMode: Totals, PerGame, Per36 (required)
#'
#' Resource: playerdashptshots
#' PerMode
#' LeagueID
#' SeasonType
#' SeasonYear
#' PORound
#' TeamID
#' PlayerID
#' Outcome
#' Location
#' Month
#' SeasonSegment
#' DateFrom
#' DateTo
#' OppTeamID
#' VsConference
#' VSDivision
#' GameSegment
#' Period
#' ShotClockRange
#' LastNGames


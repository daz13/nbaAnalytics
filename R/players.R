# players.R

library(ggplot2)
## Plot Career stats

#'
#'
#' @examples
#' get_PlayerCareerStats_by_name("Dirk Nowitzki")
get_PlayerCareerStats_by_name <- function(n) {
  id <- nbaAnalytics::all_players %>%
    filter(DISPLAY_FIRST_LAST == n) %>%
    select(PERSON_ID)
  nbaStatsGrabber((urlBuilder(statCat = "playercareerstats",
                              PlayerID = as.character(id[1,1])
                              )
                   )
                  )
}

#'
#'
#' @examples
#' get_PlayerGameLog_by_name("Dirk Nowitzki")
get_PlayerGameLog_by_name <- function(n) {
  id <- nbaAnalytics::all_players %>%
    filter(DISPLAY_FIRST_LAST == n) %>%
    select(PERSON_ID)
  nbaStatsGrabber((urlBuilder(statCat = "playergamelog",
                              PlayerID = as.character(id[1,1]))))
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

#'
#' @examples
#' plot_playerPoints(testDat)
plot_playerPoints <- function(data) {
  data %>% select(SEASON_ID, TEAM_ABBREVIATION,
                  PTS, FG_PCT) %>%
    #reshape2::melt(id.vars = c("SEASON_ID", "TEAM_ABBREVIATION", "FG_PCT")) %>%
    mutate(PTS  = as.numeric(as.character(PTS)),
           FG_PCT = as.numeric(as.character(FG_PCT))) %>%
    ggplot(aes(x = SEASON_ID, y = PTS, fill = FG_PCT, col = TEAM_ABBREVIATION)) +
    geom_bar(stat = "identity", position = "dodge")
}



#' @examples
#' plot_playerScoring(testDat)
plot_playerScoring <- function(data) {
  data %>% select(SEASON_ID, TEAM_ABBREVIATION,
                  FGA, FGM, FG_PCT, FG3A, FG3M, FG3_PCT) %>%
    reshape2::melt(id.vars = c("SEASON_ID", "TEAM_ABBREVIATION")) %>%
    mutate(value = as.numeric(as.character(value))) %>%
    reshape2::dcast(SEASON_ID + TEAM_ABBREVIATION ~ variable) %>%
    mutate(FG2A = FGA - FG3A,
           FG2_PCT = (FGM - FG3M)/FG2A) %>%
    select(SEASON_ID, TEAM_ABBREVIATION,
           FGA, FG_PCT, FG2A, FG2_PCT, FG3A, FG3_PCT) %>%
    reshape2::melt(id.vars = c("SEASON_ID", "TEAM_ABBREVIATION")) %>%
    mutate(value = as.numeric(as.character(value))) %>%
    ggplot(aes(x = SEASON_ID, y = value, col = variable, group = variable )) +
    geom_line() +
    facet_wrap(~ variable, scales = "free_y", ncol = 2)
}
#
# all_Players_data <- lapply(all_players$DISPLAY_FIRST_LAST, function(x) {
#   print(x)
#   Sys.sleep(1)
#   get_PlayerCareerStats_by_name(x)})



# Some probably useful links:
#     - Player Info commonplayerinfor
#     - Player Career Stats http://stats.nba.com/stats/playercareerstats?LeagueID=00&PerMode=PerGame&PlayerID=201167
#     - Player Season Stats http://stats.nba.com/stats/playergamelog?DateFrom=&DateTo=&LeagueID=00&PlayerID=201566&Season=2016-17&SeasonType=Regular+Season
#     - Player Game Log http://stats.nba.com/stats/playergamelog?DateFrom=&DateTo=&LeagueID=00&PlayerID=201167&Season=2016-17&SeasonType=Regular+Season

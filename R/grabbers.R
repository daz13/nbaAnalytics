library(jsonlite)
library(curl)
library(dplyr)
library(roxygen2)
library(ggplot2)


# Strucutre
#  1. Get Team list
#     --> Plot team stats
#  2. Get Player list for given team
#     --> Plot basic player stats
#     --> Plot advanced player stats
#     --> Plot player season game log

# How to find the strings that return the JSON URLS:
# Go to stats.nba.com > open 'developer tools' > find the JSON object that returns the data of interest >
# > deconstruct query and put it into a function

#' @title Arument builder
#' @description Take to arguments (key and value) and return a charater string
buildArg <- function(key, value){
  return(paste(key, "=", value,"&", sep=""))

}
buildArg2 <- function(vec){
  paramsList <- lapply(names(vec), function(x) paste(x, "=", vec[[x]][1],"&", sep=""))
  paste(unlist(paramsList), collapse = "")
}

#' JSON workaround function
#'
#' @param URL to download JSON from
#'
#' @examples
#' fromJson_workaround(urlBuilder())
fromJson_workaround <- function(url, ...) {
  curl::curl_download(url, "temp")
  fromJSON("temp", ...)
}


#' @title Link
#'
#' @description Function constructs a urls that will yield a json object from stats.nba.com.
#'
#' @param statCat Identifier needed to define which data is requested
#' @param LeagueID Default "00", needed for most requests
#' @param isOnlyCurrentSeason Needed for the commonallpltayers table, "1" = only current players, "0" = all players
#' @param Season Season identifier; Default = "2016-17"
#' @param DateFrom Date filter
#' @param DateTo Date filter
#' @param SeasonType "RegularSeason" (default) or "Playoffs"
#' @param TeamID Chr string needed for all team related statistics
#' @param PlayerID Chr string needed for all player related statistics
#'
#' @details The return string is constructed by pasting the base url ("http://stats.nba.com/stats/"), the stat-category, and all additional parameters.
#' Additional url arguments are build using the buildArg function. All available arguments are pasted to the url assuming that those who are empty will
#' not cause an error as long as they are not needed for the request
#'
#' @examples
#' urlBuilder(statCat = "commonallplayers", TeamID = "1610612751")
#'
#' ToDO: Simplfy function that arguments can be passed
urlBuilder <- function(statCat    = "commonallplayers",
                       parameters = list(LeagueID             = "00",
                                         isOnlyCurrentSeason  = "1",
                                         Season           = "2016-17",
                                         DateFrom             = "",
                                         DateTo               = "",
                                         SeasonType           = "Regular+Season",
                                         TeamID               = "",
                                         PlayerID             = "",
                                         PerMode              = "PerGame")){

    return(paste("http://stats.nba.com/stats/",statCat,"?", buildArg2(parameters), sep=""))
}


#' @title  getPlayers
#' @description Grab the complete players data table from stats.nba.com and returns as it
#' a data frame
#' @param url url from stats.nba.com which returns a standard formatted JSON object
#' (cf. details for more info about the expected JSON structure)
#' @param dim if the JSON object returns more than one data tables this argument
#' can be used to specify which data shall be returned
#' @details The typical JSON return objext from stats.nba.com is a list of three objects:
#'  1. resource: Charater identifier (could be coupled to a base url string)
#'  2. parameters: List; The parameters submitted in the JSON query to generate the data (in combinationo with the base URL stirng should reproduce update the data table)
#'  3. resultsSet: The actual results, data lies in rowSet (df or vector there might be more than one rowset), column headers in headers, the id of the row set can be found under names
#'  The function extracts the rowSet and renames the columns of the rowSet to the corresponding headers (all other information in the JSON object are currently ommitted,
#'  although this might change in the future). By default always the first row set is extracted, behaviour can be changed by the index argument.
#'
#' @examples
#' all_players <- nbaStatsGrabber(urlBuilder(statCat = "commonallplayers", TeamID = "1610612751"))
careerStatsGrabber<- function(url,
                           index = 1){
  raw_list <- fromJson_workaround(url)

    # Convert raw input to player list
  all_players <- as.data.frame(raw_list$resultSet$rowSet[[index]])
  names(all_players)  <- raw_list$resultSets$headers[[index]]

  return(all_players)
}

getTeamStats <- function(url, teamID, value = "common"){
  #teamID <- "1610612751"
  url <- paste("http://stats.nba.com/stats/teaminfocommon?LeagueID=00&SeasonType=Regular+Season&TeamID=",teamID,"&season=2016-17", sep = "")
  raw <- fromJson_workaround(url)

  if (value == "common"){
    team_stats <- as.data.frame(raw$resultSets$rowSet[[1]])
    names(team_stats) <- raw$resultSets$headers[[1]]; team_stats
    return(team_stats)
  }
  if (value == "ranks"){
    team_stats <- as.data.frame(raw$resultSets$rowSet[[2]])
    names(team_stats) <- raw$resultSets$headers[[2]]; team_stats
    return(team_stats)
  }
}

#' Grab Team Season data
#'
#' Sends JSON request to nba.stats.com and returns processed list of data frame
#'
#' @param parameters: a list of arguments send as URL parameters, value given here will overwrite
#' the default parameters (hard-coded in the function)
#'
#' @return A list of data frames
grab_TeamSeasonData <- function(parameters = list(TeamID = basicTeamInfo %>% select(TEAM_ID),
                                                 SeasonType = "Playoffs",
                                                 Season = "2016-17")){

  params <- list(TeamID = "",
                 MeasureType = "Base",
                 SeasonType = "Regular+Season",
                 PerMode = "PerGame",
                 PlusMinus = "N",
                 PaceAdjust = "N",
                 Rank = "N",
                 Season = "2016-17",
                 Outcome = "",
                 Location = "",
                 Month	= "0",
                 SeasonSegment = "",
                 DateFrom	= "",
                 DateTo	= "",
                 OpponentTeamID	= "0",
                 VsConference	= "",
                 VsDivision	= "",
                 GameSegment	= "",
                 Period	= "0",
                 ShotClockRange	= "",
                 LastNGames	= "0"
  )

  params[] <- lapply(names(params), function(x) if(x %in% names(parameters)) parameters[[x]] else params[[x]] )

  url <- urlBuilder(statCat = "teamdashboardbygeneralsplits", parameters = params)
  raw <-  fromJson_workaround(url)

  dfs <- convertNestedJson(raw)
  dfs <- lapply(dfs, function(x) x %>% mutate(Season = params$Season))
  dfs
}
grab_TeamSeasonData()


create_TeamSeasonDF <- function(teamID = basicTeamInfo %>% select(TEAM_ID),
                                seasons = c("2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12", "2010-11", "2009-10", "2008-09", "2007-08")) {
  seasonStatList <- lapply(seasons, function(x) grab_TeamSeasonData(parameters = list(TeamID = teamID,
                                                                                     Season = x)) %>%
                             bind_rows() %>%
                             mutate(SEASON_YEAR = x) %>%
                             mutate_at(.cols = -(1:3), .funs = as.numeric))
  names(seasonStatList) <- seasons

  bind_rows(lapply(names(seasonStatList),
                   function(x) bind_rows(seasonStatList[[x]])))
}
create_TeamSeasonDF()


convertNestedJson <- function(jsonObj) {
  df <- lapply(1:length(jsonObj$resultSets$name), function(x) {
    tmp <- data.frame(jsonObj$resultSets$rowSet[[x]])
    names(tmp) <- jsonObj$resultSets$header[[x]]
    tmp
  })
  names(df) <- jsonObj$resultSets$name
  df
}

# Some probably useful links:
#     - Player Career Stats http://stats.nba.com/stats/playercareerstats?LeagueID=00&PerMode=PerGame&PlayerID=201167
#     - Player Game Log http://stats.nba.com/stats/playergamelog?DateFrom=&DateTo=&LeagueID=00&PlayerID=201167&Season=2016-17&SeasonType=Regular+Season
#     - Team Game Logs http://stats.nba.com/stats/teamgamelog?DateFrom=&DateTo=&LeagueID=00&Season=2016-17&SeasonType=Regular+Season&TeamID=1610612741




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


library(jsonlite)
library(curl)
library(dplyr)
library(roxygen2)


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
urlBuilder <- function(statCat              = "commonallplayers",
                       LeagueID             = "00",
                       isOnlyCurrentSeason  = "1",
                       Season               = "2016-17",
                       DateFrom             = "",
                       DateTo               = "",
                       SeasonType           = "Regular+Season",
                       TeamID               = "",
                       PlayerID             = "",
                       PerMode              = "PerGame"){

    return(paste("http://stats.nba.com/stats/",statCat,"?",
                 buildArg("LeagueID",LeagueID),
                 buildArg("isOnlyCurrentSeason",isOnlyCurrentSeason),
                 buildArg("Season",Season),
                 buildArg("TeamID", TeamID),
                 buildArg("PlayerID",PlayerID),
                 buildArg("SeasonType",SeasonType),
                 buildArg("PerMode",PerMode),
                 sep=""))
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
nbaStatsGrabber<- function(url,
                           index = 1){
  raw_list <- fromJSON(url)

  # Convert raw input to player list
  all_players <- as.data.frame(raw_list$resultSet$rowSet[[index]])
  names(all_players)  <- raw_list$resultSets$headers[[index]]
  return(all_players)
}
fromJSON(urlBuilder(statCat = "commonallplayers", TeamID = "1610612751"))
all_players <- nbaStatsGrabber(urlBuilder(statCat = "commonallplayers", TeamID = "1610612751"))


# Some probably useful links:
#     - Player Career Stats http://stats.nba.com/stats/playercareerstats?LeagueID=00&PerMode=PerGame&PlayerID=201167
#     - Player Game Log http://stats.nba.com/stats/playergamelog?DateFrom=&DateTo=&LeagueID=00&PlayerID=201167&Season=2016-17&SeasonType=Regular+Season
#     - Team Game Logs http://stats.nba.com/stats/teamgamelog?DateFrom=&DateTo=&LeagueID=00&Season=2016-17&SeasonType=Regular+Season&TeamID=1610612741
teaminfo_example <- nbaStatsGrabber(urlBuilder(statCat = "teaminfocommon", TeamID = "1610612751"))
playerCareer_example <- nbaStatsGrabber((urlBuilder(statCat = "playercareerstats", PlayerID = "201167"))) ; playerCareer_example

# Extract team names
teams <- unique(all_players$TEAM_CODE)

#Extract active player IDs
activeplayerIds <- all_players %>% filter(TEAM_CODE != "") %>% select(c(PERSON_ID,DISPLAY_FIRST_LAST, TEAM_ID, TEAM_CODE))

getTeamStats <- function(teamID, value = "common"){
  # teamID <- "1610612751"
  url <- paste("http://stats.nba.com/stats/teaminfocommon?LeagueID=00&SeasonType=Regular+Season&TeamID=",teamID,"&season=2016-17", sep = "")
  raw <- fromJSON(url)
#  str(raw)
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
getTeamStats("1610612751", "common")


#'
#' # Test Rvest craping from BB refc.om
#'
#'
library(dplyr)
library(rvest)

read_table <-  function(url, html_container){
  read_html(url) %>%
    html_nodes(html_container) %>% html_nodes(css = "table") %>% html_table()
}

#' # Teams overall
read_html("https://www.basketball-reference.com/teams/") %>% html_nodes("#div_teams_active") %>% html_nodes(css = "table") %>% html_table()


#' ## Per team
#+
read_table("https://www.basketball-reference.com/teams/ATL/", html_container = "#div_ATL") -> atl_hist

#' # Players
#'
#+

mapply(letters[-24], FUN = function(x) {print(x)
  read_table(paste0("https://www.basketball-reference.com/players/",x,"/"), "#div_players") }) -> all_players
all_players %>%  bind_rows() -> all_players


gen_id <- function(name) {
  stringr::str_split(string = name, pattern = " ") %>% unlist()-> name_split

  (name_split[length(name_split)] %>% strsplit("") %>% unlist())[1:5] %>% paste(collapse = "") %>% tolower() -> five
  (name_split[1] %>% strsplit("") %>% unlist())[1:2] %>% paste(collapse = "") %>% tolower() -> two

  paste0(five,two,"01")
}

all_players %>% mutate(html_id = as.character(purrr::map(Player, gen_id)) )-> all_players

all_players %>% group_by(html_id) %>% filter(n() > 3)

rollapply <- function(x, n, f, ...) {
  out <- rep(NA, length(x))

  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- f(x[(i - offset):(i + offset - 1)], ...)
  }
  out
}


#' #Games
#'
#'
#+

read_html("https://www.basketball-reference.com/boxscores/?month=1&day=12&year=2017") %>% html_nodes(".teams") %>% html_table()

#' Game log link can be created using the table object created from this statement based on day-mpnth-year-HOMETEAM-3-Letter code: e.g.
#' https://www.basketball-reference.com/boxscores/201701120DEN.html

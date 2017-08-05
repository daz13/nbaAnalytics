#'
#' # Test Rvest craping from BB refc.om
#'
#'

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
read_table("https://www.basketball-reference.com/players/a/", "#div_players") -> players_a

gen_id <- function(name) {
  name = "Dirk Nowitzki"
  stringr::str_split(string = name, pattern = " ") %>% unlist()-> name_split

  (name_split[length(name_split)] %>% strsplit("") %>% unlist())[1:5] %>% paste(collapse = "") %>% tolower() -> five
  (name_split[1] %>% strsplit("") %>% unlist())[1:2] %>% paste(collapse = "") %>% tolower() -> two

  paste0(five,two)
}

players_a %>% mutate(id_521 = )

#' #Games
#'
#'
#+

read_html("https://www.basketball-reference.com/boxscores/?month=1&day=12&year=2017") %>% html_nodes(".teams") %>% html_table()

#' Game log link can be created using the table object created from this statement based on day-mpnth-year-HOMETEAM-3-Letter code: e.g.
#' https://www.basketball-reference.com/boxscores/201701120DEN.html

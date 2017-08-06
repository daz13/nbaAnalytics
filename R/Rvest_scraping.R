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
read_html("https://www.basketball-reference.com/teams/") %>%
  html_nodes("#div_teams_active") %>%
  html_nodes(css = "table") %>% html_table()


#' ## Per team
#+
read_table("https://www.basketball-reference.com/teams/ATL/", html_container = "#div_ATL") -> atl_hist

#' # Players
#'
#' Step 1: Read all player dataa dn summarize this to one table
mapply(letters[-24], FUN = function(x) {print(x)
  read_table(paste0("https://www.basketball-reference.com/players/",x,"/"), "#div_players") }) -> all_players
all_players %>%  bind_rows() -> all_players

#' Step 2: Generate player html_id needed to scrap player data tables on
#' basketball-reference.com. Specific function is needed for this:

gen_id <- function(name) {
  stringr::str_split(string = name, pattern = " ") %>% unlist()-> name_split
  first_name =  name_split[[1]]
  last_name = name_split[[length(name_split)]]

  if (stringi::stri_length(last_name) > 5){
    (last_name %>%
       strsplit("") %>%
       unlist())[1:5] %>%
      paste(collapse = "") %>%
      tolower() -> five
  } else {
    last_name %>% tolower -> five
  }

  (first_name %>%
      gsub("\\.", "", x = .) %>%
      strsplit("") %>%
      unlist())[1:2] %>%
    paste(collapse = "") %>%
    tolower() -> two
  paste0(five,two,"01")
}

all_players %>%
  mutate(html_id = as.character(purrr::map(Player, gen_id)) ) -> all_players
View(all_players)
#' Some ID appear more than once. For these casea a correciton function is
#' applied (which still needs validation)
id_count_correction <- function(string) {
  strsplit(string, "")  %>% unlist() -> split
  split[[length(split)]] <- as.numeric(split[[length(split)]]) + 1
  paste0(split, collapse = "")
}

id_correction <- function(vector){
  out <- rep(NA, length(vector))
  for ( i in 2:length(vector)){
    id <- vector[[i]]
    while(id %in% out[1:(i-1)]) {
      id <- id_count_correction(id)
      #print(id)
    }
    out[[i]] <- id
  }
 vector[-1] <- out[-1]
 vector
}

all_players %>%
  mutate(html_id = id_correction(html_id)) %>% as_tibble() -> all_players
all_players


#' Step 3: Scrap player career data using the html_id


scrap_player <- function(letter, id) {
  print(id)
  try(paste0("https://www.basketball-reference.com/players/", letter, "/", id, ".html") %>%
    read_table("#div_per_game") %>% unlist(recursive = F))
}

all_players %>%
  mutate(first_letter = purrr::map(html_id,
                                   function(x) unlist(strsplit(x,""))[1])) -> all_players
all_players %>%
  filter(first_letter == "a") %>%
  mutate(career_stats = purrr::map2(.x = first_letter,
                                    .y = html_id,
                                    .f = scrap_player)) -> players_a

players_career <- players_a

for(letter in letters[-1]){
  all_players %>%
    mutate(first_letter = purrr::map(html_id,
                                     function(x) unlist(strsplit(x,""))[1])) %>%
    filter(first_letter == letter) %>%
    mutate(career_stats = purrr::map2(.x = first_letter,
                                      .y = html_id,
                                      .f = scrap_player)) -> tmp
  bind_rows(players_career, tmp) -> players_career
}

devtools::use_data(players_career)
devtools::use_data(all_players, overwrite = T)


#' #Games
#'
#'
#+

read_html("https://www.basketball-reference.com/boxscores/?month=1&day=12&year=2017") %>%
  html_nodes(".teams") %>% html_table()

#' Game log link can be created using the table object created from this statement
#' based on day-mpnth-year-HOMETEAM-3-Letter code: e.g.
#' https://www.basketball-reference.com/boxscores/201701120DEN.html

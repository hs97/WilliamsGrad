#' preprocess the roster data frame
#'

preprocess <- function(){
  roster <- read.csv(file = 'data/roster.csv') %>%
    select(name, year, team) %>%
    rowwise %>%
    mutate(year = 2000 + as.integer(gsub(' ', ' ', as.character(sub("\\'", '', year)))),
           name = as.character(name)) %>%
    filter(year <= 2015) %>%
    unique()
  save(roster, file = 'data/roster.RData')
}

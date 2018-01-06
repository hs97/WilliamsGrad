#' Categorize names into origins
#'
#' @param x a data frame returned by \code{read_honor}
#' which contains information about one's name, year,
#' and honor
#'
#' @return a data frame indicating every student's
#' potential cultural origin
#'
#' @import stringr
#' @import wru
#' @export

analyze_surnames <- function(x) {
  x %>%
    ## separate the last name from name
    ## by eliminating anything after a
    ## comma and take the substring before
    ## the last space appeared in a name

    mutate(name = as.character(name)) %>%
    rowwise %>%
    mutate(name = strsplit(name, ",")[[1]][1]) %>%
    mutate(surname = tolower(gsub(".* ", "", name))) %>%
    predict_race(surname.only = T) %>%
    mutate(asian = round(pred.asi),
           hispanic = round(pred.his)) %>%
    select(-starts_with("pred"))
}

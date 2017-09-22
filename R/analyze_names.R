#' Categorize names into origins
#'
#' @param x a data frame returned by \code{read_honor}
#' which contains information about one's name, year,
#' and honor
#'
#' @return a data frame indicating every student's
#' potential cultural origin
#'
#' @export

analyze_names <- function(x) {

  ## load last name data

 # data(last_names, package = "kane2017")

  x %>%

    ## separate the last name from name
    ## by eliminating anything after a
    ## comma and take the substring before
    ## the last space appeared in a name

    mutate(name = as.character(name)) %>%
    rowwise %>%
    mutate(name = strsplit(name, ",")[[1]][1]) %>%
    mutate(last_name = tolower(gsub(".* ", "", name))) %>%

    ## joining the last_names dataset, which contains
    ## last_name and origin to define everyone's origin

    left_join(last_names, by = c("last_name")) %>%
    mutate(origin = ifelse(is.na(origin), "non-Asian", origin)) %>%
    ungroup

}


#' This function summarizes the ratio of honor among each ethnic group
#'
#' @param x A dataset containing name, honor, origin
#'
#' @return honor ratio for each origin group
#' @export

summarize_ratio <- function(x) {

  x %>% group_by(year, honor, value, origin) %>%
    summarize(count = length(name)) %>%
    group_by(honor) %>%
    ## If there is no student of honor from this particular origin group,
    ## Complete the dataset with 0
    complete(year, honor, value, origin, fill = list(count = 0)) %>%
    group_by(year, origin, honor) %>%
    mutate(ratio = round(count / sum(count), 4)) %>%
    mutate(ratio = ifelse(is.na(ratio), 0, ratio)) %>%
    ungroup

}

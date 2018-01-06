#' This method reads honors such as Summa Cum Laude, Magna Cum Laude, and Phi Beta
#' Kappa.
#'
#' @param x a dataframe including rows \code{name} and others
#' @return a dataframe including rows \code{name} \code{phi_beta_kappa} \code{sigma_xi}
#' \code{summa} \code{magna} \code{cum} and others
#' @import stringr
#' @export

read_honor <- function(x) {
  x %>%
    # Indicate academic club honors. Use none instead of
    # NAs to avoid potential conflict from future `complete`
    mutate(phi_beta_kappa = ifelse(str_detect(name, "\\*"),
                                   1, 0),
           sigma_xi = ifelse(str_detect(name, "\\+") || str_detect(name, "\\†"),
                             1, 0)) %>%
    ungroup %>%
    mutate(index = 1:nrow(.)) %>%
    mutate(index = findInterval(index, which(str_detect(name, "Bachelor"))) %% 4) %>%
    rowwise() %>%
    mutate(summa = ifelse(index == 1, 1, 0),
           magna = ifelse(index == 2, 1, 0),
           cum = ifelse(index == 3, 1, 0)) %>%
    select(-index) %>%
    filter(!str_detect(name, "Bachelor"))
}


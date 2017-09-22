#' This function process the student info data frame and mutate their academic honors
#'
#' We determine whether a student is part of phi beta kappa or sigma xi by detecting
#' if the name contains "*" or "+"
#'
#' We also attain a list of indices from rows saying "Bachelor of Arts, Summa Cum Laude"
#' and find out their places in the data frame, and mutate the data frame based on where
#' an entry belongs in the list of indices. Then apply %% 4.
#'
#' @param x the dataframe containing all student info
#'
#' @return a dataframe now containing columns \code{name}, \code{year}, \code{honor},
#' and \code{value}
#'
#' @import dplyr
#' @import tidyr
#' @export

read_honor <- function(x) {

  x %>%
    # Indicate academic club honors. Use none instead of
    # NAs to avoid potential conflict from future `complete`
    mutate(phi_beta_kappa = ifelse(str_detect(name, "\\*"),
                                   "phi_beta_kappa", "None"),
           sigma_xi = ifelse(str_detect(name, "\\+"),
                             "sigma_xi", "None")) %>%
    mutate(index = 1:nrow(.)) %>%
    mutate(index = findInterval(index, which(str_detect(name, "Bachelor"))) %% 4) %>%
    rowwise() %>%
    mutate(latin = if (index == 1) {
      "summa"
    } else if (index == 2) {
      "magna"
    } else if (index == 3) {
      "cum"
    } else {
      "None"
    }) %>%
    select(-index) %>%
    filter(!str_detect(name, "Bachelor")) %>%
    gather(key = honor, value = value, phi_beta_kappa, latin, sigma_xi)

}

 plot_summary <- function(x, type) {

  stopifnot(type %in% c("count", "ratio"))
  honor_names <- c(
    "cum" = "Cum Laude",
    "magna" = "Magna",
    "summa" = "Summa",
    "phi_beta_kappa" = "PBK",
    "sigma_xi" = "Sigma Xi"
  )

  x %>%
    {
    if (type == "count")
      filter(value != "None")  %>%
      filter(origin != "non-Asian") %>%
      ggplot(aes(x = year, group = origin, fill = origin)) +
      theme_bw(base_size = 16) +
      facet_grid(value~origin, scales = "free", labeller = labeller(value = honor_names)) +
      geom_bar() +
      theme(legend.position = "bottom",
            plot.margin = margin(5, 25, 0, 25))
    else if (type == "ratio")
      summarize_ratio %>%
      filter(value != "None")  %>%
      mutate(set = "Ratio Indicator") %>%
      ggplot(aes(x = year, y = ratio, group = origin, fill = origin)) +
      theme_bw(base_size = 16) +
      facet_grid(value~origin, scales = "free", labeller = labeller(value = honor_names)) +
      geom_bar(stat = "identity") +
      theme(legend.position = "bottom",
            plot.margin = margin(5, 25, 0, 25))
  }
}

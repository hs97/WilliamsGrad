#' It allows us to create a graph that illustrates the average percentage of certain honor for all countries of origin
#'
#' @param honor Summa, Magna or Cum
#' @return The average of many countries and total
#' @examples
#' originAverage("Summa")
#' add(10, 1)

originAverage<- function(honor) {
  #add average from different origins
  aver <- c(mean(with(read(honor, chineseLastName),percent)))
  origin <-c ("China")
  aver <- c(aver,mean(with(read(honor, cantoneseLastName),percent)))
  origin <-c (origin, "HK")
  aver <- c(aver,mean(with(read(honor, totalVeri),percent)))
  origin <-c (origin, "Total")
  output <-data.frame(country = origin, average = aver)
}

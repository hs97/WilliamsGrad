#' @title ratioIndicator()
#' @description
#' ratioIndicator allows users to examine the ratio between percentage
#' of Latin honors among students of Asian descent and students of non-
#' Asian descent. A ratio that is larger than 1 inidicates that the hon
#' -nor is more frequent among the Asian students and a ratio that is
#' lower than 1 indicates the opposite. A ratio of 1 indicates the
#' equal percentage of honors.
#'
#' @param data a list of student names and graduation year.
#' @return a data frame that contains ratio of magna, summa,
#' cum, sigmaXi, and phiBetaKappa between Asian and non-Asian
#' students
#' @examples
#' ratioIndicator(honorStats(readHonor(students, totalVeri)))
#' @export

ratioIndicator<-function(asianSummary) {
  #attain total summary from totalStats
  totalSummary<- totalStats(students)
  #percentage is indicative of this Latin honor and above- provides a more accurate measure
  asianSummary$magna<- asianSummary$magna+asianSummary$summa
  asianSummary$cum<- asianSummary$cum+asianSummary$magna
  summaAsian <- asianSummary$summa / asianSummary$total
  magnaAsian <- asianSummary$magna / asianSummary$total
  cumAsian <- asianSummary$cum / asianSummary$total
  sigmaXiAsian <- asianSummary$sigmaXi / asianSummary$total
  phiBetaKappaAsian <- asianSummary$phiBetaKappa / asianSummary$total
  nonAsian <- totalSummary$total - asianSummary$total
  totalSummary$magna<- totalSummary$magna+totalSummary$summa
  totalSummary$cum<- totalSummary$cum+totalSummary$magna
  summa <- (totalSummary$summa - asianSummary$summa) / nonAsian
  magna <- (totalSummary$magna - asianSummary$magna) / nonAsian
  cum <- (totalSummary$cum - asianSummary$cum) / nonAsian
  sigmaXi <- (totalSummary$sigmaXi - asianSummary$sigmaXi) / nonAsian
  phiBetaKappa <- (totalSummary$phiBetaKappa - asianSummary$phiBetaKappa) / nonAsian
  #obtain all odds ratio
  summa <- summaAsian/ summa
  magna <- magnaAsian/ magna
  cum <- cumAsian/ cum
  sigmaXi <- sigmaXiAsian/ sigmaXi
  phiBetaKappa<- phiBetaKappaAsian/ phiBetaKappa

  output<- data.frame(year = totalSummary$year, summa = summa, magna= magna, cum = cum, sigmaXi = sigmaXi, phiBetaKappa= phiBetaKappa)
  output
}

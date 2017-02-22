#' @title statsSum()
#' @description
#' Going through the list of summary given by honorStats and totalStats and find
#' the total sum of all honors of all students. With the total sum for both Asians and non-Asians, we can then calculate
#' the ratio of the percentage of getting honors
#' @param data a list of student names and graduation year.
#' @return a data frame including a column of honors and a column of ratios
#' @examples
#' statsSum(honorStats(readHonor(students, totalVeri)))
#' @export

statsSum<-function(asianSummary) {
  #attain all totals from 2002- 2016 for asians and non-asians
  asianTotal<- sum(asianSummary$total)
  asianSumma<- sum(asianSummary$summa)
  asianMagna<- sum(asianSummary$magna)+asianSumma
  asianCum<- sum(asianSummary$cum)+asianMagna
  asianSigma<- sum(asianSummary$sigmaXi)
  asianPhi<- sum(asianSummary$phiBetaKappa)
  totalSummary<- totalStats(students)
  total<- sum(totalSummary$total)-asianTotal
  totalSumma<- sum(totalSummary$summa)-asianSumma
  totalMagna<- sum(totalSummary$magna)+totalSumma-asianMagna+asianSumma
  totalCum<- sum(totalSummary$cum)+totalMagna-asianCum+asianMagna+asianSumma
  totalSigma<- sum(totalSummary$sigmaXi)
  totalPhi<- sum(totalSummary$phiBetaKappa)
  #attain the ratios
  summaRatio= (asianSumma/asianTotal)/(totalSumma/total)
  magnaRatio= (asianMagna/asianTotal)/(totalMagna/total)
  cumRatio= (asianCum/asianTotal)/(totalCum/total)
  sigmaRatio= (asianSigma/asianTotal)/(totalSigma/total)
  phiRatio= (asianPhi/asianTotal)/(totalPhi/total)
  honor <-c ("summa","magna","cum","sigma","phi")
  ratio <-c (summaRatio,magnaRatio,cumRatio,sigmaRatio,phiRatio)
  output<-data.frame(honor =honor, ratio = ratio)
  output
}

#' @title honorStats()
#' @description
#' create a simple count list of academic honors of all the asian students from 2002-2016,
#' including the number of each honor
#'
#' @param honorList data frame returned by readHonor() that contains a data frame of asian last names, their honors, their sigmaXi and phiBetaKappa status(TRUE/ FALSE)
#' @return data frame of \code{year} \code{total} \code{summa}\code{magna}\code{cum}\code{sigmaXi} and \code{phiBetaKappa}
#' @example
#' honorStats(readHonor(students,chineseLastName))
#' @export

honorStats <- function(honorList) {
  #create null data frame to avoid nullpointer exception
  output<-data.frame(year = NULL, total = NULL, summa = NULL , magna = NULL, cum = NULL, sigmaXi = NULL, phiBetaKappa = NULL)
  for (j in 2002:2016){
    #get the indices of a given year in the honorList
    index = which (honorList$year == j)
    total = index[length(index)] - index[1] + 1
    summa = 0
    magna =0
    cum= 0
    sigmaXi<-0
    phiBetaKappa<-0
    for (i in 1: length(index)) {
      #check the honors
      if (honorList$honor[index[i]] == "Summa") {
        summa <- summa + 1
      }
      else if (honorList$honor[index[i]] == "Magna") {
        magna <- magna + 1
      }
      else if (honorList$honor[index[i]] == "Cum") {
        cum <- cum + 1
      }
      if (honorList$sigmaXi[index[i]] == TRUE) {
        sigmaXi <- sigmaXi + 1
      }
      if (honorList$phiBetaKappa[index[i]] == TRUE) {
        phiBetaKappa <- phiBetaKappa + 1
      }
    }
    #merge each year's data frame
    output<-rbind (output, data.frame(year = j, total = total, summa = summa , magna = magna, cum = cum, sigmaXi = sigmaXi, phiBetaKappa = phiBetaKappa))
  }
  output
}

#' @title totalStats()
#' @description
#' going through the list of all students and figure out the number of total students, summa, magna, cum, sigmaXi, and phiBetaKappa each year.
#'
#' @param data a list of student names and graduation year.
#' @return a data frame including year, total, summa, magna, cum, sigmaXi, and phiBetaKappa
#' @examples
#' totalStats(students)
#' @export

totalStats<-function (data) {
  index <- grep("Bachelor",data$name,value=F)
  index <- c(index,length(data$name))
  summa<- vector()
  magna<- vector()
  cum <- vector()
  total<- vector()
  year<- (2002:2016)
  countTotal<- 0
  countHonor<- 0
  for (i in 1: length(index)){

      #check the honors based on counter
      if (i%%4 == 2) {
        summa <-c (summa, (index[i]- countHonor-2))
        countHonor = index[i]
      }
      else if (i%%4 == 3) {
        magna <-c (magna, (index[i]- countHonor-1))
        countHonor= index[i]
      }
      else if (i%%4 == 0) {
        cum<- c(cum,(index[i]-countHonor - 1))
        countHonor= index[i]
      }
      else if (i%%4 == 1&& i>1){
        total <- c (total, (index[i]-countTotal-4))
        countTotal= index[i]-1
        countHonor= index[i]-1
      }
  }
  sigmaXiTotal<- vector()
  phiBetaKappaTotal<- vector()
  for (j in 2002:2016){
    #get the indices of a given year in the honorList
    index = which (data$year == j)
    sigmaXi<-0
    phiBetaKappa<-0
    for (i in 1: length(index)) {
      #check the honors
      if (substr(data$name[i],1,1)=="*"){
        phiBetaKappa<- phiBetaKappa+1
        if (substr(data$name[i],2,2)=="+"||substr(data$name[i],2,2)=="†"){
          sigmaXi<- sigmaXi+1
        }
        else {
          sigmaXi<- sigmaXi+1
        }

      }
      if (substr(data$name[i],1,1)=="+"||substr(data$name[i],1,1)=="†"){
        sigmaXi<- sigmaXi+1

      }
    }
    sigmaXiTotal<-c (sigmaXiTotal, sigmaXi)
    phiBetaKappaTotal<-c (phiBetaKappaTotal,phiBetaKappa)
  }
  output<-data.frame(year = year, total = total, summa = summa , magna = magna, cum = cum, sigmaXi = sigmaXiTotal, phiBetaKappa= phiBetaKappaTotal)
  output
}

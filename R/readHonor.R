#' Pick out last names based on reference list, and introduce academic honors
#'
#' @param data list of graduation names and honors
#' @param verifier A list of last names
#' @return data frame of \code{ethnic last names} and \code{honors}
#' @examples
#' readHonor(Data2011, chineseLastNames)
readHonor <- function(data, verifier){
  #initiate storage vector for name and honor
  name <- vector ()
  honor <- vector ()
  count <- 1
  #index the list based on where bachelor of summa, magna, and cum laude are
  index <- grep("Bachelor",data,value=F)
  index <- c(index,length(data))
  #loop through the index point
  for(i in 1: 4){
      for(j in (index[i]+1): (index[i+1]-1)){
        #separate string based on ",", for individuals who got department honors
        temp <-unlist(str_split(data[j],","))
        name <- c (name,temp[1])
        #check the honors based on counter
        if (i == 1) {
          honor <- c (honor,"Summa")
         }
         else if (i == 2) {
         honor <- c (honor,"Magna")
          }
         else if (i == 3) {
         honor <- c (honor,"Cum")
        }
        else if (i == 4){
          honor <- c (honor,"")
        }
      }
  }
  #find the substring before the last space
  lastName <- gsub(".* ", "", name)
  na.omit(name)
  summa <- 0
  magna <-0
  cum<- 0
  total <-0
  sigmaXi<-0
  sigmaXiTotal<-0
  phiBetaKappa<-0
  phiBetaKappaTotal<-0
  #filter and filteredHonor are used to store the ones who are verified to be of Asian origin
  filtered <-vector()
  filteredHonor <- vector()
  for (i in 1:length(lastName)){
    temp <-  paste("^",lastName[i],"$",sep = "")
    #make sure whole string is fully equal to the reference list
    if (substr(name[i],1,1)=="*"){
      phiBetaKappaTotal<- phiBetaKappaTotal+1
      if (substr(name[i],2,2)=="+"){
        sigmaXiTotal<- sigmaXiTotal+1
      }
    }
    else if (substr(name[i],1,1)=="+"){
      sigmaXiTotal<- sigmaXiTotal+1
    }
    #check if this last name matches with any in the reference list
    verify <- grepl(temp,verifier,ignore.case = TRUE)
    for (j in 1: length(verify)) {
      if (verify[j]==TRUE&&lastName[i]!=""){
        filtered<-c(filtered,lastName[i])
        filteredHonor<-c(filteredHonor,honor[i])
        #keep track of total # of students of asian descend
        total<- total+1
        if (honor[i] == "Summa") {
          summa <- summa + 1
        }

        else if (honor[i] == "Magna") {
          magna <- magna + 1
        }
        else if (honor[i] == "Cum") {
          cum <- cum + 1
        }
        if (substr(name[i],1,1)=="*"){
          phiBetaKappa<- phiBetaKappa+1
          if (substr(name[i],2,2)=="+"){
            sigmaXi<- sigmaXi+1
          }
        }
        else if (substr(name[i],1,1)=="+"){
          sigmaXi<- sigmaXi+1
        }
      }
    }
  }

  percentSigma <- sigmaXi*100/sigmaXiTotal
  percentPhi <- phiBetaKappa*100 / phiBetaKappaTotal
  percentSumma <- summa*100/total
  percentMagna <- (magna+summa)*100/total
  percentCum <- (summa+magna+cum)*100/total
  output <- data.frame(name=filtered, honor = filteredHonor,SummaPer = percentSumma, MagnaAbovePer = percentMagna, CumAbovePer = percentCum, percentSigma = percentSigma, percentPhi = percentPhi)
  output
  }

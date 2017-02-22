#' @title readHonor()
#' @description
#' Pick out Asian last names based on reference list, and check for their academic honors
#' Return a data frame that has all the Asian last names and honors.
#' @param data data frame of columns name and year
#' @param verifier A list of last names
#' @return data frame of \code{ethnic last names}, \code{honors} and sigmaXi and phiBetaKappa status
#' @import stringr
#' @examples
#' readHonor(students, chineseLastName)
#' @export

readHonor <- function(data, verifier){
  #initiate storage vector for name and honor
  name <- vector ()
  honor <- vector ()
  year <- vector()
  count <-  1
  #index the list based on where bachelor of summa, magna, and cum laude are
  index <- grep("Bachelor",data$name,value=F)
  index <- c(index,length(data$name))
  #loop through the index point
  for(i in 1: (length(index)-1)){
      for(j in (index[i]+1): (index[i+1]-1)){
        #separate string based on ",", for individuals who got department honors
        temp <-unlist(str_split(data$name[j],","))
        name <- c (name,temp[1])
        year <- c (year,data$year[j])
        #check the honors based on counter
        if (i%%4 == 1) {
          honor <- c (honor,"Summa")
         }
         else if (i%%4 == 2) {
         honor <- c (honor,"Magna")
          }
         else if (i%%4 == 3) {
         honor <- c (honor,"Cum")
        }
        else if (i%%4 == 0){
          honor <- c (honor,"")
        }
      }
  }
  #find the substring before the last space
  lastName <- gsub(".* ", "", name)

  #filter and filteredHonor are used to store the ones who are verified to be of Asian origin
  filtered <-vector()
  filteredHonor <- vector()
  filteredYear <- vector()
  phiBetaKappa <- vector()
  sigmaXi <- vector()
  for (i in 1:length(lastName)){
    temp <-  paste("^",lastName[i],"$",sep = "")
    #make sure whole string is fully equal to the reference list

    #check if this last name matches with any in the reference list
    verify <- grepl(temp,verifier,ignore.case = TRUE)
    for (j in 1: length(verify)) {
      if (verify[j]==TRUE&&lastName[i]!=""){
        filtered<-c(filtered,lastName[i])
        filteredHonor<-c(filteredHonor,honor[i])
        filteredYear<-c (filteredYear, year[i])

        if (substr(name[i],1,1)=="*"){
          phiBetaKappa<- c(phiBetaKappa, TRUE)
          if (substr(name[i],2,2)=="+"||substr(name[i],2,2)=="†"){
            sigmaXi<- c(sigmaXi , TRUE)
          }
          else {
            sigmaXi<- c(sigmaXi , TRUE)
          }

        }
        else {
          phiBetaKappa<- c(phiBetaKappa, FALSE)
          if (substr(name[i],1,1)=="+"||substr(name[i],1,1)=="†"){
            sigmaXi<- c(sigmaXi, TRUE)

          }
          else{
            sigmaXi<- c(sigmaXi , FALSE)
          }
        }
      }
    }
  }


  output <- data.frame(name=filtered, honor = filteredHonor,year = filteredYear, sigmaXi = sigmaXi, phiBetaKappa = phiBetaKappa)
  output
  }

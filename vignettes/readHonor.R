#' Add together two numbers
#'
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' add(1, 1)
#' add(10, 1)
readHonor <- function(data, verifier){

  #file <- "2011.txt"
  #pinyin<- "2012.txt"

  #con <- file(description=pinyin, open="r")
  #chinese <- readLines(con,warn = FALSE)
  #con <- file(description=file, open="r")

  #input <- readLines(con,warn = FALSE)


 # close(con)
 # con2 <- file(description=pinyin, open="r")
 # close(con2)

  #print(chinese)
  name <- vector ()
  honor <- vector ()
  #honors <- c (
   # "Summa Cum Laude",
   # "Magna Cum Laude",
   # "Cum Laude",
   # "Bachelor of Arts"
  #)
  count <- 1
  index <- grep("Bachelor",data,value=F)
  index <- c(index,length(data))

  ##

  #print (index)
    for(i in 1: 4){
      for(j in (index[i]+1): (index[i+1]-1)){
       #if (grepl(input[j],"-",fixed = TRUE)== TRUE){
        #  print ("nice")
       # }
        temp <-unlist(str_split(data[j],","))
        #print (temp[1])
        name <- c (name,temp[1])
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
  lastName <- gsub(".* ", "", name)
  na.omit(name)
  #print(lastName)
  summa <- 0
  magna <-0
  cum<- 0
  total <-0
  sigmaXi<-0
  sigmaXiTotal<-0
  phiBetaKappa<-0
  phiBetaKappaTotal<-0
  filtered <-vector()
  filteredHonor <- vector()
  for (i in 1:length(lastName)){
    temp <-  paste("^",lastName[i],"$",sep = "")
    #print(temp)
    #print(substr(name[i],1,1))
    if (substr(name[i],1,1)=="*"){
      phiBetaKappaTotal<- phiBetaKappaTotal+1
      if (substr(name[i],2,2)=="+"){
        sigmaXiTotal<- sigmaXiTotal+1
      }
    }
    else if (substr(name[i],1,1)=="+"){
      sigmaXiTotal<- sigmaXiTotal+1
    }
    verify <- grepl(temp,verifier,ignore.case = TRUE)
    for (j in 1: length(verify)) {
      if (verify[j]==TRUE&&lastName[i]!=""){
        filtered<-c(filtered,lastName[i])
        filteredHonor<-c(filteredHonor,honor[i])
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
  #print (summa)
  #print (magna)
  #print (cum)
  #print (total)
  #print (sigmaXi)
  #print (sigmaXiTotal)
  percentSumma <- summa*100/total
  percentMagna <- (magna+summa)*100/total
  percentCum <- (summa+magna+cum)*100/total
  output <- data.frame(name=filtered, honor = filteredHonor,SummaPer = percentSumma, MagnaAbovePer = percentMagna, CumAbovePer = percentCum)
  output
  }

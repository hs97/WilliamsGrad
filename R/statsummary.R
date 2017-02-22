#' @title statSummary()
#' @description
#' statsummary provides different ways of evaluating the given data set.
#' Summary by count allows for a visualization of number of asian students
#' each year broken down by honors. Summary by origin looks at the number
#' of Asian students categorized by origins, breakdown gives a detailed
#' breakdown of number of asian students each year with the number of honors,
#' summary by percentage illustrates the percentage of honors attained by
#' Asian students each year, histogram gives a regression plot of Asian student
#' count throughout the years.
#'
#' It also offers useful tools to analyze the ratio. OR cum allows for the examination
#' of cum ratios from 2002 to 2016 while OR sigma and OR phi looks at sigmaXi and
#' phiBetaKappa respectively.
#'
#' @param type types of analytical representations, including: summay by count, summary by origin, breakdown, histogram, summary by percentage,
#' OR cum, OR sigma, OR phi, OR all, and Overall honor ratio.
#' @return ggplot of graphical analysis according to the given param.
#' @import ggplot2
#' @examples
#' statsummary("breakdown")
#' @export

statsummary<- function(type) {
  data = readHonor(students, totalVeri)
  if (type == "summary by count") {
    ggplot(data, aes (year, fill = honor))+geom_bar()+labs(y = "Count", x = "Year", title = "Number of Asian Students 2002 to 2016")
  }
  else if (type == "summary by percentage") {
    ggplot(data, aes (year, fill = honor))+geom_bar(aes(fill = honor), position = "fill")+geom_hline(yintercept = 0.02)+geom_hline(yintercept = 0.15)+geom_hline(yintercept = 0.35)+labs(y = "Percent", x = "Year", title = "Percentage of Asian students with Latin Honor")}
  else if (type == "summary by origin") {
    #sort out the data by origin
    data = readHonor(students,chineseLastName)
    data$origin<- "Chinese"
    temp <- readHonor(students,cantoneseLastName)
    temp$origin<- "Cantonese"
    data <- rbind (data, temp)
    temp2 <- readHonor(students,japaneseLastName)
    temp2$origin<-"Japanese"
    data <- rbind (data, temp2)
    ggplot(data, aes(origin, fill =  honor))+ geom_bar()+labs(y = "Count", x = "Origin", title = "Origin of students of Asian descent 2002-2016")
  }

  else if (type == "histogram"){
    data <- honorStats(data)
    ggplot(data ,aes(year, total))+geom_point()+geom_smooth()+labs(y = "Count", x="Year",title = "Smooth Line Graph of Number of students of Asian Descent")
  }
  else if (type == "breakdown") {
    output<-honorStats(data)
    output
  }
  else if (type == "OR cum"){
    data<-ratioIndicator(honorStats(data))
    ggplot(data , aes(year, cum))+geom_point(stat="identity")+geom_smooth()+geom_hline(yintercept=1)+labs(title = "OR of Cum 2002-2016")
  }
  else if (type == "OR sigma"){
    data<-ratioIndicator(honorStats(data))
    ggplot(data , aes(year, sigmaXi))+geom_point(stat="identity")+geom_smooth()+geom_hline(yintercept=1)+labs(title = "OR of Sigma 2002-2016")
  }
  else if (type == "OR phi"){
    data<-ratioIndicator(honorStats(data))
    ggplot(data , aes(year, phiBetaKappa))+geom_point(stat="identity")+geom_smooth()+geom_hline(yintercept=1)+labs(title = "OR of Phi 2002-2016")
  }
  else if (type == "OR all"){
    data<-ratioIndicator(honorStats(data))
    plot (c(2002,2016),c(0,8),type="n",
          xlab="Year",ylab="Odds Ratio",main = "OR of All Measures 2002-2016")
    lines(data$year,data$sigmaXi,col="red",lwd=2.5)
    lines(data$year,data$cum, col="green", lwd=2.5)
    lines(data$year, data$summa, col="yellow", lwd=2.5)
    lines(data$year, data$magna, col = "purple", lwd=2.5)
    lines(data$year,data$phiBetaKappa,col="blue",lwd=2.5)
    legend(2005,8,lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red","green","yellow","purple"),legend= c("phi","sigmaXi","cum","summa","magna"))
  }
  else if(type =="Overall Honor Ratio"){
    data<-statsSum(honorStats(data))
    ggplot(data,aes(honor,ratio, fill = honor))+geom_bar(stat="identity")+geom_hline(yintercept = 1)+labs(title="Odds Ratio of Sum 2002-2016")
  }
}

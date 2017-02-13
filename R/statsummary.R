#' provides different ways of evaluating the given data set
#'
#' @param type types of analytical representations, including: summay by count, summary by origin, breakdown, histogram, summary by percentage
#' @return ggplot of graphs
#' @import ggplot2
#' @examples
#' statsummary("breakdown")
#' @export

statsummary<- function(type) {
  data <- readHonor(DataSummary,chineseLastName)
  data$origin<- "Chinese"
  temp <- readHonor(DataSummary,cantoneseLastName)
  temp$origin<- "Cantonese"
  data <- rbind (data, temp)
  temp2 <- readHonor(DataSummary,japaneseLastName)
  temp2$origin<-"Japanese"
  data <- rbind (data, temp2)
  if (type == "summary by count") {
  ggplot(data, aes (year, fill = honor))+geom_bar()+labs(y = "Count", x = "Year", title = "Number of Students of Asian Descent from 2002 to 2016")

  }
  else if (type == "summary by percentage") {
  ggplot(data, aes (year, fill = honor))+geom_bar(aes(fill = honor), position = "fill")+geom_hline(yintercept = 0.02)+geom_hline(yintercept = 0.15)+geom_hline(yintercept = 0.35)+labs(y = "Percent", x = "Year", title = "Numbers of students of Asian descent from 2002 to 2016(compared to average Latin Honor)")}
  else if (type == "summary by origin") {
    ggplot(data, aes(origin, fill =  honor))+ geom_bar()+labs(y = "Count", x = "Origin", title = "Number of students of Asian descent from 2002 to 2016 grouped by origin")

  }

  else if (type == "histogram"){
    data <- honorStats(data)
    ggplot(data ,aes(year, total))+geom_point()+geom_smooth()+labs(y = "Count", x="Year",title = "Smooth Line Graph of Number of students of Asian Descent")
  }
  else if (type == "breakdown") {
    output<-honorStats(data)
    output
  }
}

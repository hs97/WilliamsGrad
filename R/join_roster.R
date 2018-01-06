#' This function match a student athlete's roster name with his graduation catalog name

join_roster <- function(students, roster){
  extract(roster, name, c("FirstName", "LastName"), "([^ ]+) (.*)") %>%
    mutate(LastName = gsub(" ", "", LastName)) %>%
    rowwise %>%
    mutate(match = ifelse(length(intersect(grep(FirstName, students$name, ignore.case = TRUE),
                                           grep(LastName, students$name, ignore.case = TRUE))) == 0,
                          0,
                          intersect(grep(FirstName, students$name, ignore.case = TRUE),
                                    grep(LastName, students$name, ignore.case = TRUE)))) %>%
    filter(match != 0) %>%
    mutate(name = students$name[match]) %>%
    select(-FirstName, -LastName, -match) %>%
    right_join(students)
}

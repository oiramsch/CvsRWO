rm(kTableAllSubjects)
allSubjects <- select(objectList,1) %>% count(subject) %>% select(1)
setSize <- c(2,4,6)

for (item in allSubjects[[1]]) {
  for (element in setSize) {
    if (exists("kTableAllSubjects")) {
      kTableAllSubjects <- add_row(kTableAllSubjects,subject = item, setsize = element,k = kCapatity(filter(sumObjectBySubject, subject == item) %>% select(2:4), element))
    } else {
      kTableAllSubjects <- tibble(subject = item, setsize = element,k = kCapatity(filter(sumObjectBySubject, subject == item) %>% select(2:4), element))
    }
  }
}


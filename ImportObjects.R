rm(objectList) # Aufräumen von alten Daten
dateiNamen <- dir(path="./Objects",pattern="object_....txt")
for (datei in dateiNamen) {
  eingabeDatei <- paste("./Objects/",datei,sep="")
  object_temp <- read_delim(eingabeDatei,"\t", escape_double = FALSE, trim_ws = TRUE)
  if (exists("objectList")) {
    objectList <- rbind(objectList,object_temp)
  } else {
    objectList <- object_temp
  }
}
colnames(objectList)[1] <- "subject"
rm (object_temp)
rm (datei)
rm (dateiNamen)
rm (eingabeDatei)
objectList <- objectList %>% mutate(answer_change = (((responses-200)%%10))%/%2)
objectList <- objectList %>% mutate(answer_correct = if_else(responses-200 < 10,1,0))
objectList <- objectList %>% mutate(signal_detection = 2*change + answer_correct)
objectList <- objectList %>% mutate(age = if_else(subject-100 > 100,"old","youngfavstat~~"))
objectList <- objectList %>% mutate(condition_text = if_else(condition == 1,"novel",if_else(condition == 2,"exemplar",if_else(condition == 3,"state","feature"))))
objectList <- objectList %>% mutate(signal_detection_text = if_else(signal_detection == 0,"false alarm",if_else(signal_detection == 1,"correct rejection",if_else(signal_detection == 2,"miss","hit"))))
sumObjectAll <- count(objectList, setsize, signal_detection_text)
sumObjectByAgeSetSize <- count(objectList, age, setsize, signal_detection_text)
sumObjectBySubject <- count(objectList, subject, setsize, signal_detection_text)






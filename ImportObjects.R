rm(objectList) # Aufräumen von alten Daten
# Create the mode function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
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
objectList <- objectList %>% mutate(age = if_else(subject-100 > 100,"old","young"))
objectList <- objectList %>% mutate(condition_text = if_else(condition == 1,"novel",if_else(condition == 2,"exemplar",if_else(condition == 3,"state","feature"))))
objectList <- objectList %>% mutate(signal_detection_text = if_else(signal_detection == 0,"false alarm",if_else(signal_detection == 1,"correct rejection",if_else(signal_detection == 2,"miss","hit"))))
#sumObjectAll <- count(objectList, setsize, signal_detection_text)
#sumObjectByAgeSetSize <- count(objectList, age, setsize, signal_detection_text)
#sumObjectBySubject <- count(objectList, subject, setsize, signal_detection_text)
objectGroupBySubAgeSet <- group_by(objectList, subject, age, setsize)
summarise(objectGroupBySubAgeSet, totalCases = n(), 
          mean_answer_correct = mean(answer_correct), 
          mean_response_time = mean(time_1),
          median_response_time = median(time_1),
          sum_answer_correct = sum(answer_correct), 
          sum_of_changes = sum(change == 1),
          sum_of_noChanges = sum(change == 0),
          falseAlarm = sum(signal_detection == 0), 
          correct_rejections = sum(signal_detection == 1),
          miss = sum(signal_detection == 2),
          hit = sum(signal_detection == 3), 
          totalOfChange = correct_rejections+falseAlarm,
          totalOfHit = hit+miss,
          falseAlarmRate = falseAlarm/totalOfChange, 
          hitRate = hit/totalOfHit) %>% 
  mutate(k = setsize*(hitRate-falseAlarmRate)) %>% 
  mutate(kPashler = setsize*(hitRate-falseAlarmRate)/(1-falseAlarmRate))-> objectResult
objectGroupBySubAgeSetCon <- group_by(objectList, subject, age, setsize,condition_text)
summarise(objectGroupBySubAgeSetCon, totalCases = n(), 
          mean_answer_correct = mean(answer_correct),
          mean_response_time = mean(time_1),
          median_response_time = median(time_1),
          sum_answer_correct = sum(answer_correct), 
          sum_of_changes = sum(change == 1),
          sum_of_noChanges = sum(change == 0),
          falseAlarm = sum(signal_detection == 0), 
          correct_rejections = sum(signal_detection == 1),
          miss = sum(signal_detection == 2),
          hit = sum(signal_detection == 3), 
          totalOfChange = correct_rejections+falseAlarm,
          totalOfHit = hit+miss,
          falseAlarmRate = falseAlarm/totalOfChange, 
          hitRate = hit/totalOfHit) %>% 
  mutate(k = setsize*(hitRate-falseAlarmRate)) %>% 
  mutate(kPashler = setsize*(hitRate-falseAlarmRate)/(1-falseAlarmRate)) -> objectResultCondition
objectGroupBySubject <- group_by(objectList, subject, age)
summarise(objectGroupBySubject, 
          SetSize2_correct = mean(answer_correct[setsize == 2]),
          SetSize4_correct = mean(answer_correct[setsize == 4]),
          SetSize6_correct = mean(answer_correct[setsize == 6]),
          SetSize2_sd = sd(answer_correct[setsize == 2]),
          SetSize4_sd = sd(answer_correct[setsize == 4]),
          SetSize6_sd = sd(answer_correct[setsize == 6]),
          SetSize2_var = var(answer_correct[setsize == 2]),
          SetSize4_var = var(answer_correct[setsize == 4]),
          SetSize6_var = var(answer_correct[setsize == 6]),
          SetSize2_aad = aad(answer_correct[setsize == 2]),
          SetSize4_aad = aad(answer_correct[setsize == 4]),
          SetSize6_aad = aad(answer_correct[setsize == 6]),
          SetSize2_time = median(time_1[setsize == 2]),
          SetSize4_time = median(time_1[setsize == 4]),
          SetSize6_time = median(time_1[setsize == 6]),
          ) -> objectResultSetSize

objectGroupAge <- group_by(objectResult, age)
summarise(objectGroupAge,
          SetSize2 = mean(mean_answer_correct[setsize == 2]),
          SetSize4 = mean(mean_answer_correct[setsize == 4]),
          SetSize6 = mean(mean_answer_correct[setsize == 6]),
          hit_2 = mean(hit[setsize == 2]),
          hit_4 = mean(hit[setsize == 4]),
          hit_6 = mean(hit[setsize == 6]),
          miss_2 = mean(miss[setsize == 2]),
          miss_4 = mean(miss[setsize == 4]),
          miss_6 = mean(miss[setsize == 6]),
          falseAlarmRate_Set_2 = mean(falseAlarmRate[setsize == 2]),
          falseAlarmRate_Set_4 = mean(falseAlarmRate[setsize == 4]),
          falseAlarmRate_Set_6 = mean(falseAlarmRate[setsize == 6]),
          HitRate_Set_2 = mean(hitRate[setsize == 2]),
          HitRate_Set_4 = mean(hitRate[setsize == 4]),
          HitRate_Set_6 = mean(hitRate[setsize == 6]),
          K_2_mean = mean(k[setsize == 2]),
          K_4_mean = mean(k[setsize == 4]),
          K_6_mean = mean(k[setsize == 6]),
          K_2_median = median(k[setsize == 2]),
          K_4_median = median(k[setsize == 4]),
          K_6_median = median(k[setsize == 6]),
          K_2_Max = max(k[setsize == 2]),
          K_4_Max = max(k[setsize == 4]),
          K_6_Max = max(k[setsize == 6])) -> objectResultGroupAge

objectGroupAgeCon <- group_by(objectResultCondition, age)
summarise(objectGroupAgeCon,
          SetSize2 = mean(mean_answer_correct[setsize == 2]),
          SetSize4 = mean(mean_answer_correct[setsize == 4]),
          SetSize6 = mean(mean_answer_correct[setsize == 6]),
          hit_2 = mean(hit[setsize == 2]),
          hit_4 = mean(hit[setsize == 4]),
          hit_6 = mean(hit[setsize == 6]),
          miss_2 = mean(miss[setsize == 2]),
          miss_4 = mean(miss[setsize == 4]),
          miss_6 = mean(miss[setsize == 6]),
          falseAlarmRate_Set_2 = mean(falseAlarmRate[setsize == 2]),
          falseAlarmRate_Set_4 = mean(falseAlarmRate[setsize == 4]),
          falseAlarmRate_Set_6 = mean(falseAlarmRate[setsize == 6]),
          HitRate_Set_2 = mean(hitRate[setsize == 2]),
          HitRate_Set_4 = mean(hitRate[setsize == 4]),
          HitRate_Set_6 = mean(hitRate[setsize == 6]),
          K_2_mean = mean(k[setsize == 2]),
          K_4_mean = mean(k[setsize == 4]),
          K_6_mean = mean(k[setsize == 6]),
          K_2_median = median(k[setsize == 2]),
          K_4_median = median(k[setsize == 4]),
          K_6_median = median(k[setsize == 6]),
          K_2_Max = max(k[setsize == 2]),
          K_4_Max = max(k[setsize == 4]),
          K_6_Max = max(k[setsize == 6])) -> objectResultGroupAgeCondition


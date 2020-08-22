dateiNamen <- dir(path="./Colors",pattern="colors_....txt")
for (datei in dateiNamen) {
  eingabeDatei <- paste("./Colors/",datei,sep="")
  color_temp <- read_delim(eingabeDatei,"\t", escape_double = FALSE, trim_ws = TRUE)
  if (exists("colorList")) {
    colorList <- rbind(colorList,color_temp)
  } else {
    colorList <- color_temp
  }
}
colnames(colorList)[1] <- "subject"
rm (color_temp)
rm (datei)
rm (dateiNamen)
rm (eingabeDatei)
colorList <- colorList %>% mutate(answer_change = (((responses-200)%%10))%/%2)
colorList <- colorList %>% mutate(answer_correct = if_else(responses-200 < 10,1,0))
colorList <- colorList %>% mutate(signal_detection = 2*change + answer_correct)
colorList <- colorList %>% mutate(age = if_else(subject-100 > 100,"old","young"))
colorList %>% 
  mutate(color_text = car::recode(.$color, "1 = 'rot';2 = 'grün';3 = 'blau';4 = 'gelb';5 = 'pink';6 = 'schwarz';7 = 'orange';8 = 'türkis'", 
                                  as.factor = TRUE)) -> colorList
colorList %>% 
  mutate(signal_detection_text = car::recode(.$signal_detection, "0 = 'false alarm';1 = 'correct rejection'; 2 = 'miss'; 3 = 'hit'")) -> colorList

colorGroup <- group_by(colorList, subject,age ,condition)
summarize(colorGroup,totalCases = n(), correct_answer = sum(answer_correct == 1) , falseAlarm = sum(signal_detection == 0),correct_rejection = sum(signal_detection == 1), miss = sum(signal_detection == 2), hit = sum(signal_detection == 3)) %>% mutate (noChangeTotal = falseAlarm+correct_rejection) %>% mutate(changeTotal = miss+hit) %>% mutate(falseAlarmRate = falseAlarm / noChangeTotal) %>% mutate(hitRate = hit / changeTotal) %>% mutate (k = condition*(hitRate - falseAlarmRate)) -> colorListResult
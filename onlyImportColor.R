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
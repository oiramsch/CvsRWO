kCapatity <- function(x, setSize) {
  temp4F <- filter(x, setsize == setSize)
  falseAlarm <- ifelse(is_empty(filter(temp4F, signal_detection_text == "false alarm") %>% select(n) %>%  pull()),0,filter(temp4F, signal_detection_text == "false alarm") %>% select(n) %>%  pull())
  correctRejection <- filter(temp4F, signal_detection_text == "correct rejection") %>% select(n) %>%  pull()
  hit <- filter(temp4F, signal_detection_text == "hit") %>% select(n) %>%  pull()
  miss <- filter(temp4F, signal_detection_text == "miss") %>% select(n) %>%  pull()
  falseAlarmRate <- falseAlarm / (falseAlarm+correctRejection)
  hitRate <- hit/(hit+miss)
  k <- setSize*(hitRate-falseAlarmRate)
  return (k)
}

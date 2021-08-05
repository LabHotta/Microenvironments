library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(cowplot)
theme_set(theme_minimal_grid())

C1_timecourses <- read.csv("C1_timecourse.txt", sep="\t")
C1_timecourses <- select(C1_timecourses, 1:2, 6:17)
rownames(C1_timecourses) <- C1_timecourses[,1]
C1_timecourses <- C1_timecourses[,-1]
C1_timecourses <- data.frame(C1_timecourses)

C1_timecourses_reorganized <- C1_timecourses %>% pivot_longer(!Module, names_to = "Time", values_to = "Expression")

C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X.2"="-2"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X0"="0"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X2"="2"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X4"="4"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X6"="6"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X8"="8"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X10"="10"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X12"="12"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X14"="14"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X16"="16"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X18"="18"))
C1_timecourses_reorganized$Time <- revalue(C1_timecourses_reorganized$Time, c("X20"="20"))
C1_timecourses_reorganized$Time <- as.numeric(as.character(C1_timecourses_reorganized$Time))


ggplot(C1_timecourses_reorganized, aes(x=Time, y=Expression)) +
  geom_point() +
  #geom_jitter(width = 0.3,colour="black")+
  stat_summary(geom = "line", fun= "median", size = 1, color="red")+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(name="Normalized Expression")+
  theme(text = element_text(size=18))+
  facet_wrap(~ Module, ncol = 4)



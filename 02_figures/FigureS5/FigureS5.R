library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)

#exportar 940x940

C1_line = "solid"
C2_line = "solid"

C1 <- "#007849"
C2 <- "#82b135"


#FigureS5A
sugarcane4mo9mo_Threonine_timecourses <- read.csv("4mo9mo_Threonine.txt", sep="\t")
ggplot(data=sugarcane4mo9mo_Threonine_timecourses, aes(x=CT, y=Threonine, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.96/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*26.18/79)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.96/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*26.18/79)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

sugarcane4mo9mo_Threonine <- ggplot(data=sugarcane4mo9mo_Threonine_timecourses, aes(x=CT, y=Threonine, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Threonine", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none"
  )
sugarcane4mo9mo_Threonine
FinalMaxMin


#FigureS5B
sugarcane4mo9mo_Serine_timecourses <- read.csv("4mo9mo_Serine.txt", sep="\t")
ggplot(data=sugarcane4mo9mo_Serine_timecourses, aes(x=CT, y=Serine, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.96/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*26.18/79)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.96/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*26.18/79)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

sugarcane4mo9mo_Serine <- ggplot(data=sugarcane4mo9mo_Serine_timecourses, aes(x=CT, y=Serine, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Serine", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
sugarcane4mo9mo_Serine
FinalMaxMin


#FigureS5C
sugarcane4mo9mo_Tagatose_timecourses <- read.csv("4mo9mo_Tagatose.txt", sep="\t")
ggplot(data=sugarcane4mo9mo_Tagatose_timecourses, aes(x=CT, y=Tagatose, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.96/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*26.18/79)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.96/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*26.18/79)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

sugarcane4mo9mo_Tagatose <- ggplot(data=sugarcane4mo9mo_Tagatose_timecourses, aes(x=CT, y=Tagatose, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Tagatose", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none"
  )
sugarcane4mo9mo_Tagatose
FinalMaxMin


#FigureS5D
sugarcane4mo9mo_Oxoproline_timecourses <- read.csv("4mo9mo_Oxoproline.txt", sep="\t")
ggplot(data=sugarcane4mo9mo_Oxoproline_timecourses, aes(x=CT, y=Oxoproline, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.96/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*26.18/79)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.96/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*26.18/79)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

sugarcane4mo9mo_Oxoproline <- ggplot(data=sugarcane4mo9mo_Oxoproline_timecourses, aes(x=CT, y=Oxoproline, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Oxoproline", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
sugarcane4mo9mo_Oxoproline
FinalMaxMin


plot_grid(sugarcane4mo9mo_Threonine, sugarcane4mo9mo_Serine, sugarcane4mo9mo_Tagatose, sugarcane4mo9mo_Oxoproline,  labels = c("A","B","C","D"), ncol = 2, align = "none", rel_widths = c(1.2, 1),rel_heights = c(1,1.1),label_size = 20)


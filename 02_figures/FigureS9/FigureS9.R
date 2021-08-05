library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)

#exportar 1400 x 1400

C1_line = "solid"
C2_line = "solid"

C1 <- "#007849"
C2 <- "#82b135"

campos_Valine_timecourses <- read.csv("campos_Valine.txt", sep="\t")
ggplot(data=campos_Valine_timecourses, aes(x=CT, y=Valine, group=Campo)) +
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

campos_Valine <- ggplot(data=campos_Valine_timecourses, aes(x=CT, y=Valine, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Valine", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized metabolite levels", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
campos_Valine
FinalMaxMin

campos_Leucine_timecourses <- read.csv("campos_Leucine.txt", sep="\t")
ggplot(data=campos_Leucine_timecourses, aes(x=CT, y=Leucine, group=Campo)) +
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

campos_Leucine <- ggplot(data=campos_Leucine_timecourses, aes(x=CT, y=Leucine, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Leucine", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized metabolite levels", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
campos_Leucine
FinalMaxMin


campos_Isoleucine_timecourses <- read.csv("campos_Isoleucine.txt", sep="\t")
ggplot(data=campos_Isoleucine_timecourses, aes(x=CT, y=Isoleucine, group=Campo)) +
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

campos_Isoleucine <- ggplot(data=campos_Isoleucine_timecourses, aes(x=CT, y=Isoleucine, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Isoleucine", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized metabolite levels", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
campos_Isoleucine
FinalMaxMin


#changed interval
campos_Citrate_timecourses <- read.csv("campos_Citrate.txt", sep="\t")
ggplot(data=campos_Citrate_timecourses, aes(x=CT, y=Citrate, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[8:80, ]
fit4 <- fit2[82:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*23.66/71)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*25.85/77)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*23.66/71)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*25.85/77)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

campos_Citrate <- ggplot(data=campos_Citrate_timecourses, aes(x=CT, y=Citrate, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Citrate", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized metabolite levels", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
campos_Citrate
FinalMaxMin



campos_Glycerate_timecourses <- read.csv("campos_Glycerate.txt", sep="\t")
ggplot(data=campos_Glycerate_timecourses, aes(x=CT, y=Glycerate, group=Campo)) +
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

campos_Glycerate <- ggplot(data=campos_Glycerate_timecourses, aes(x=CT, y=Glycerate, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Glycerate", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized metabolite levels", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
campos_Glycerate
FinalMaxMin

#method to identify the peak and though of the smoothed fit in C1
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
FinalMaxMin


campos_Malate_timecourses <- read.csv("campos_Malate.txt", sep="\t")
ggplot(data=campos_Malate_timecourses, aes(x=CT, y=Malate, group=Campo)) +
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

campos_Malate <- ggplot(data=campos_Malate_timecourses, aes(x=CT, y=Malate, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Malate", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized metabolite levels", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
campos_Malate
FinalMaxMin


#changed interval
campos_Aspartate_timecourses <- read.csv("campos_Aspartate.txt", sep="\t")
ggplot(data=campos_Aspartate_timecourses, aes(x=CT, y=Aspartate, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[6:72, ]
fit4 <- fit2[86:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*21.69/65)+0.203
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*24.52/73)-0.293
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*21.69/65)+0.203
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*26.18/73)-0.293
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

campos_Aspartate <- ggplot(data=campos_Aspartate_timecourses, aes(x=CT, y=Aspartate, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Aspartate", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized metabolite levels", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
campos_Aspartate
FinalMaxMin


campos_Glycine_timecourses <- read.csv("campos_Glycine.txt", sep="\t")
ggplot(data=campos_Glycine_timecourses, aes(x=CT, y=Glycine, group=Campo)) +
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

campos_Glycine <- ggplot(data=campos_Glycine_timecourses, aes(x=CT, y=Glycine, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "Glycine", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized metabolite levels", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
campos_Glycine
FinalMaxMin

campos_GABA_timecourses <- read.csv("campos_GABA.txt", sep="\t")
ggplot(data=campos_GABA_timecourses, aes(x=CT, y=GABA, group=Campo)) +
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

campos_GABA <- ggplot(data=campos_GABA_timecourses, aes(x=CT, y=GABA, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = -2.75, label = "GABA", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized metabolite levels", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
campos_GABA
FinalMaxMin

plot_grid(campos_Valine, campos_Leucine, campos_Isoleucine, campos_Citrate, campos_Glycerate, campos_Malate, campos_Aspartate, campos_Glycine, campos_GABA, labels = c("A","B","C","D","E","F","G","H","I"), ncol = 3, align = "none", rel_widths = c(1.15, 1, 1),rel_heights = c(1,1,1.15),label_size = 20)


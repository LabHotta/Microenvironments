library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())

#green
C1 <- "#007849"
C2 <- "#82b135"
both <- "grey"

#C1_line = "longdash"
C1_line = "solid"
C2_line = "solid"

#export 1400 x 1400
#.svg 1440 x 1440
#set folder

#Figure3A_FLD
timecourses_FLD <- read.csv("FLD_timecourses.txt", sep="\t")
ggplot(data=timecourses_FLD, aes(x=CT, y=FLD, group=Campo)) +
  geom_jitter(aes(col=Campo), shape = 3, position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit5 <- cbind(fit3)
fit5 <- data.frame(fit5)
maxC20 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*21.78/79)-1.95
minC20 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*21.78/79)-1.95
FinalMaxMin <- cbind(maxC2, minC2)

FLD <- ggplot(data=timecourses_FLD, aes(x=CT, y=FLD, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo), shape = 17, position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C2))+
  scale_fill_manual(values=c(C2))+
  scale_linetype_manual(values=c(C2_line)) +
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "FLD", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3.2,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position = "none"
  )
FLD
FinalMaxMin


#Figure3B_COP1
timecourses_COP1 <- read.csv("COP1_timecourses.txt", sep="\t")
ggplot(data=timecourses_COP1, aes(x=CT, y=COP1, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*22.08/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*21.78/79)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*22.08/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*21.78/79)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

COP1 <- ggplot(data=timecourses_COP1, aes(x=CT, y=COP1, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "COP1-1", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3.2,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
COP1
FinalMaxMin


#Figure3C_CDF1
timecourses_CDF1 <- read.csv("CDF1_timecourses.txt", sep="\t")
ggplot(data=timecourses_CDF1, aes(x=CT, y=CDF1, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*22.08/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*21.78/79)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*22.08/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*21.78/79)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

CDF1 <- ggplot(data=timecourses_CDF1, aes(x=CT, y=CDF1, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "CDF1", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3.2,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
CDF1
FinalMaxMin


#Figure3D_SVP-1
timecourses_SVP <- read.csv("SVP_1_timecourses.txt", sep="\t")
ggplot(data=timecourses_SVP, aes(x=CT, y=SVP, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*22.08/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*21.78/79)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*22.08/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*21.78/79)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

SVP_1 <- ggplot(data=timecourses_SVP, aes(x=CT, y=SVP, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "SVP-1", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3.2,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position = "none"
  )
SVP_1
FinalMaxMin


#Figure3E_SVP-2
timecourses_SVP <- read.csv("SVP_2_timecourses.txt", sep="\t")
ggplot(data=timecourses_SVP, aes(x=CT, y=SVP, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*16.13/59)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*15.77/59)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*16.13/59)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*15.77/59)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

SVP_2 <- ggplot(data=timecourses_SVP, aes(x=CT, y=SVP, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "SVP-2", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3.2,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
SVP_2
FinalMaxMin


#Figure3F_SVP_3
timecourses_SVP <- read.csv("SVPc_timecourses.txt", sep="\t")
ggplot(data=timecourses_SVP, aes(x=CT, y=SVP, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:70, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*19.01/69)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*21.78/79)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*19.01/69)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*21.78/79)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

SVP_3 <- ggplot(data=timecourses_SVP, aes(x=CT, y=SVP, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1[1], y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "SVP-3", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3.2,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
SVP_3
FinalMaxMin


#FigureG_SOC1
timecourses_SOC1 <- read.csv("SOC1_timecourses.txt", sep="\t")
ggplot(data=timecourses_SOC1, aes(x=CT, y=SOC1, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:150, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*22.08/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*18.78/69)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*22.08/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*18.78/69)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

SOC1 <- ggplot(data=timecourses_SOC1, aes(x=CT, y=SOC1, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2[1], y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "SOC1", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3.2,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position = "none"
  )
SOC1
FinalMaxMin


#FigureH_FT
timecourses_FT <- read.csv("FT_timecourses.txt", sep="\t")
ggplot(data=timecourses_FT, aes(x=CT, y=FT, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*22.08/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*21.78/79)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*22.08/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*21.78/79)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

FT <- ggplot(data=timecourses_FT, aes(x=CT, y=FT, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "FT", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3.2,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
FT
FinalMaxMin


#FigureI_AP1-1
timecourses_AP1 <- read.csv("AP1_timecourses.txt", sep="\t")
ggplot(data=timecourses_AP1, aes(x=CT, y=AP1, group=Campo)) +
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:150, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*22.08/79)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*18.78/69)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*22.08/79)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*18.78/69)-1.95
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)

AP1a <- ggplot(data=timecourses_AP1, aes(x=CT, y=AP1, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "AP1-1", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3.2,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
AP1a
FinalMaxMin


plot_grid(FLD,COP1,CDF1,SVP_1,SVP_2,SVP_3,SOC1,FT,AP1a, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), ncol = 3, rel_widths = c(1.25, 1,1),rel_heights = c(1,1,1.15), label_size = 20)

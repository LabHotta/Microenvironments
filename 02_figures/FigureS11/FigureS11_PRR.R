library(pcaMethods)
library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)

#exportar 1100 x 800

C1 <- "#007849"
C2 <- "#82b135"
C1_line = "solid"
C2_line = "solid"
C5_line = "solid"
C6_line = "solid"
C5 <- "#573280"
C6 <- "#b0228c"
C3_line = "solid"
C4_line = "solid"
C3 <- "#0B3C5D"
C4 <- "#0375B4"


sugarcane4mo9mo_PRR73_timecourses <- read.csv("FigureS11_4mo9mo_PRR73.txt", sep="\t")
sugarcane4mo9mo_PRR73 <- ggplot(data=sugarcane4mo9mo_PRR73_timecourses, aes(x=CT, y=PRR73, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.15, ymax = 1.5, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.15, ymax = 1.5, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  #(aes(group = Campo, colour = Campo, fill=Campo, linetype=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = 13.35, y = 1.4, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = 7.00, y = 1.4, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 21, y = 1.4, label = "italic(PRR73)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,1.5,0.5), name="Normalized Expression", limits=c(-0.15,1.5), labels = scales::number_format(accuracy = 0.01), expand = expansion(mult = c(0, 0.05)))+
  #theme_minimal_grid() +
  theme(panel.grid.major = element_line(colour = "#efefef"), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank())+
  background_grid()
sugarcane4mo9mo_PRR73

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
round(FinalMaxMin)



EW_PRR73_timecourses <- read.csv("FigureS11_EW_PRR73.txt", sep="\t")
EW_PRR73 <- ggplot(data=EW_PRR73_timecourses, aes(x=CT, y=PRR73, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.12, ymax = 0.36, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.12, ymax = 0.36, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.5)+
  scale_colour_manual(values=c(C5, C6))+
  scale_fill_manual(values=c(C5, C6))+
  scale_linetype_manual(values=c(C5_line, C6_line)) +
  annotate("text", x = 7.79, y = 0.35, label = "\u25bc", size = 6, colour=C5)+
  annotate("text", x = 5.49, y = 0.35, label = "\u25bc", size = 6, colour=C6)+
  annotate("text", x = 21, y = 0.35, label = "italic(PRR73)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,0.36,0.12), name="Normalized Expression", limits=c(-0.12,0.36), labels = scales::number_format(accuracy = 0.01), expand = expansion(mult = c(0, 0.05)))+
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
        legend.position = "none",
        legend.title = element_blank()
  )
EW_PRR73

#method to identify the peak and though of the smoothed fit in C3
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*25.95/79)-1.41
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*25.95/79)-1.41
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*25.95/79)-1.41
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*25.95/79)-1.41
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)



muro_PRR73_timecourses <- read.csv("FigureS11_muro_PRR73.txt", sep="\t")
muro_PRR73_timecourses$Campo = factor(muro_PRR73_timecourses$Campo, levels=c("before the wall", "after the wall"))
muro_PRR73 <- ggplot(data=muro_PRR73_timecourses, aes(x=CT, y=PRR73, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.3, ymax = 1.5, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.3, ymax = 1.5, alpha = .5, , fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.5)+
  scale_colour_manual(values=c(C3, C4))+
  scale_fill_manual(values=c(C3, C4))+
  scale_linetype_manual(values=c(C3_line, C4_line)) +
  annotate("text", x = 5.7, y = 1.45, label = "\u25bc", size = 6, colour=C4)+
  annotate("text", x = 6.3, y = 1.45, label = "\u25bc", size = 6, colour=C3)+
  annotate("text", x = 21, y = 1.45, label = "italic(PRR73)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,1.5,0.5), name="Normalized Expression", limits=c(-0.3,1.5), labels = scales::number_format(accuracy = 0.01), expand = expansion(mult = c(0, 0.05)))+
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
        legend.position = "none",
        legend.title = element_blank()
  )
muro_PRR73

#method to identify the peak and though of the smoothed fit in C3
fit2 <- data.frame(fit2)
fit3 <- fit2[1:40, ]
fit4 <- fit2[81:120, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26/79)-1.25
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26/79)-1.25
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26/79)-1.25
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26/79)-1.25
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)


EW_PRR37_timecourses <- read.csv("FigureS11_EW_PRR37.txt", sep="\t")
EW_PRR37 <- ggplot(data=EW_PRR37_timecourses, aes(x=CT, y=PRR37, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.003, ymax = 0.009, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.003, ymax = 0.009, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.5)+
  scale_colour_manual(values=c(C5, C6))+
  scale_fill_manual(values=c(C5, C6))+
  scale_linetype_manual(values=c(C5_line, C6_line)) +
  annotate("text", x = 9.43, y = 0.0086, label = "\u25bc", size = 6, colour=C5)+
  annotate("text", x = 12.7, y = 0.0086, label = "\u25bc", size = 6, colour=C6)+
  annotate("text", x = 21, y = 0.0086, label = "italic(PRR37)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,0.009,0.003), name="Normalized Expression", limits=c(-0.003,0.009), labels = scales::number_format(accuracy = 0.001), expand = expansion(mult = c(0, 0.05)))+
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
        legend.position = "none",
        legend.title = element_blank()
  )
EW_PRR37

#method to identify the peak and though of the smoothed fit in C3
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*25.95/79)-1.41
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*25.95/79)-1.41
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*25.95/79)-1.41
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*25.95/79)-1.41
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)


muro_PRR37_timecourses <- read.csv("FigureS11_muro_PRR37.txt", sep="\t")
muro_PRR37_timecourses$Campo = factor(muro_PRR37_timecourses$Campo, levels=c("before the wall", "after the wall"))
muro_PRR37 <- ggplot(data=muro_PRR37_timecourses, aes(x=CT, y=PRR37, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.007, ymax = 0.024, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.007, ymax = 0.024, alpha = .5, , fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.5)+
  scale_colour_manual(values=c(C3, C4))+
  scale_fill_manual(values=c(C3, C4))+
  scale_linetype_manual(values=c(C3_line, C4_line)) +
  annotate("text", x = 3.36, y = 0.023, label = "\u25bc", size = 6, colour=C4)+
  annotate("text", x = 4.67, y = 0.023, label = "\u25bc", size = 6, colour=C3)+
  annotate("text", x = 21, y = 0.023, label = "italic(PRR37)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,0.024,0.008), name="Normalized Expression", limits=c(-0.007,0.024), labels = scales::number_format(accuracy = 0.001), expand = expansion(mult = c(0, 0.05)))+
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
        legend.position = "none",
        legend.title = element_blank()
  )
muro_PRR37

#method to identify the peak and though of the smoothed fit in C3
fit2 <- data.frame(fit2)
fit3 <- fit2[1:40, ]
fit4 <- fit2[81:120, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26/79)-1.25
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26/79)-1.25
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26/79)-1.25
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26/79)-1.25
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)


plot_grid(EW_PRR73, EW_PRR37, muro_PRR73, muro_PRR37, labels = c("a", "b","c", "d"), ncol = 2, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1.2),label_size = 20)

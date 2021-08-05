library(pcaMethods)
library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)
library(eulerr)
library(magick)

C5_line = "solid"
C6_line = "solid"
C5 <- "#573280"
C6 <- "#b0228c"
C3_line = "solid"
C4_line = "solid"
C3 <- "#0B3C5D"
C4 <- "#0375B4"


#Figure4A
EW_LHY_timecourses <- read.csv("Figure4_EW_LHY.txt", sep="\t")
EW_LHY <- ggplot(data=EW_LHY_timecourses, aes(x=CT, y=LHY, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -1.45, ymax = 7.5, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -1.45, ymax = 7.5, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.5)+
  scale_colour_manual(values=c(C5, C6))+
  scale_fill_manual(values=c(C5, C6))+
  scale_linetype_manual(values=c(C5_line, C6_line)) +
  annotate("text", x = 0.56, y = 6.25, label = "\u25bc", size = 6, colour=C5)+
  annotate("text", x = 1.55, y = 6.25, label = "\u25bc", size = 6, colour=C6)+
  annotate("text", x = 21, y = 7.125, label = "italic(LHY)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,7.5,2.5), name="Normalized Expression", limits=c(-1.45,7.5), labels = scales::number_format(accuracy = 0.001), expand = expansion(mult = c(0, 0.05)))+
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
EW_LHY

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

#FinalMaxMin
#maxFS    minFS    maxAS    minAS
#0.5608861 12.05772 1.546329 15.99949


EW_TOC1_timecourses <- read.csv("Figure4_EW_TOC1.txt", sep="\t")
#EW_TOC1_timecourses$Campo = factor(EW_TOC1_timecourses$Campo, levels=c("4-month old", "9-month old"))
EW_TOC1 <- ggplot(data=EW_TOC1_timecourses, aes(x=CT, y=TOC1, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.001, ymax = 0.006, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.001, ymax = 0.006, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.5)+
  scale_colour_manual(values=c(C5, C6))+
  scale_fill_manual(values=c(C5, C6))+
  scale_linetype_manual(values=c(C5_line, C6_line)) +
  annotate("text", x = 10.92, y = 0.005, label = "\u25bc", size = 6, colour=C5)+
  annotate("text", x = 10.59, y = 0.005, label = "\u25bc", size = 6, colour=C6)+
  annotate("text", x = 21, y = 0.0057, label = "italic(TOC1)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,0.006,0.002), name="Normalized Expression", limits=c(-0.001,0.006), labels = scales::number_format(accuracy = 0.001), expand = expansion(mult = c(0, 0.05)))+
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
EW_TOC1

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

#FinalMaxMin
#maxFS    minFS   maxAS minAS
#11.07228 4.502658 10.7438 -1.41

#Figure4B
photo_east <- cowplot::ggdraw() + cowplot::draw_image("Figure4_photo_east.png", scale = 0.9)
photo_west <- cowplot::ggdraw() + cowplot::draw_image("Figure4_photo_west.png", scale = 0.9)


#Figure4C
muro_LHY_timecourses <- read.csv("Figure4_muro_LHY.txt", sep="\t")
muro_LHY_timecourses$Campo = factor(muro_LHY_timecourses$Campo, levels=c("before the wall", "after the wall"))
muro_LHY <- ggplot(data=muro_LHY_timecourses, aes(x=CT, y=LHY, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -1.05, ymax = 4.2, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -1.05, ymax = 4.2, alpha = .5, , fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.5)+
  scale_colour_manual(values=c(C3, C4))+
  scale_fill_manual(values=c(C3, C4))+
  scale_linetype_manual(values=c(C3_line, C4_line)) +
  annotate("text", x = 2.37, y = 3.85, label = "\u25bc", size = 6, colour=C4)+
  annotate("text", x = 1.05, y = 3.85, label = "\u25bc", size = 6, colour=C3)+
  annotate("text", x = 21, y = 3.99, label = "italic(LHY)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,4.2,1.4), name="Normalized Expression", limits=c(-1.05,4.2), labels = scales::number_format(accuracy = 0.001), expand = expansion(mult = c(0, 0.05)))+
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
muro_LHY

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


#FinalMaxMin
#maxFS    minFS    maxAS    minAS
#2.370253 7.306962 1.053797 9.281646

muro_TOC1_timecourses <- read.csv("Figure4_muro_TOC1.txt", sep="\t")
muro_TOC1_timecourses$Campo = factor(muro_TOC1_timecourses$Campo, levels=c("before the wall", "after the wall"))
muro_TOC1 <- ggplot(data=muro_TOC1_timecourses, aes(x=CT, y=TOC1, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.03, ymax = 0.12, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.03, ymax = 0.12, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.5)+
  scale_colour_manual(values=c(C3, C4))+
  scale_fill_manual(values=c(C3, C4))+
  scale_linetype_manual(values=c(C3_line, C4_line)) +
  annotate("text", x = 10.60, y = 0.11, label = "\u25bc", size = 6, colour=C4)+
  annotate("text", x = 10.93, y = 0.11, label = "\u25bc", size = 6, colour=C3)+
  annotate("text", x = 21, y = 0.114, label = "italic(TOC1)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,0.12,0.04), name="Normalized Expression", limits=c(-0.03,0.12), labels = scales::number_format(accuracy = 0.001), expand = expansion(mult = c(0, 0.05)))+
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
muro_TOC1

#method to identify the peak and though of the smoothed fit in C3
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
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

#Figure4D
photo_wall <- cowplot::ggdraw() + cowplot::draw_image("Figure4_foto_muro.png", scale = 0.9)
cartoon_wall <- cowplot::ggdraw() + cowplot::draw_image("Figure4_cartoon.png", scale = 0.9)


panelBD <- plot_grid(EW_LHY, EW_TOC1, muro_LHY, muro_TOC1, labels = c("b", "","d", ""), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1,1,1.4),label_size = 20)
panelBD

panelAC <- plot_grid(photo_east, photo_west, photo_wall, cartoon_wall, labels = c("a", "","c", ""), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1,1,1.4),label_size = 20)
panelAC

panelABCD <- plot_grid(panelAC, panelBD, labels = c(" ", ""), ncol = 2, align = "none", rel_widths = c(1.2, 1),rel_heights = c(1,1),label_size = 20)
panelABCD

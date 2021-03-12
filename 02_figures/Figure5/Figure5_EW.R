library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)
library(magick)


#export 1400 x 1400

C5_line = "solid"
C6_line = "solid"

#purple
C5 <- "#573280"
C6 <- "#b0228c"

#Figure5A
photo_east <- cowplot::ggdraw() + cowplot::draw_image("Figure5_photo_east.png", scale = 0.9)
photo_west <- cowplot::ggdraw() + cowplot::draw_image("Figure5_photo_west.png", scale = 0.9)


#Figure5B_LHY
EW_LHY_timecourses <- read.csv("Figure5_EW_LHY.txt", sep="\t")
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
        legend.position = "bottom",
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
FinalMaxMin


#Figure5B_TOC1
EW_TOC1_timecourses <- read.csv("Figure5_EW_TOC1.txt", sep="\t")
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
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
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
FinalMaxMin


#Figure5C_cartoon
Ew_cartoon <- cowplot::ggdraw() + cowplot::draw_image("Figure5_cartoon.png", scale = 0.9)


panelA <- plot_grid(photo_east, photo_west, labels = c("A", ""), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelA

panelB <- plot_grid(EW_LHY, EW_TOC1, labels = c("B", ""), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelB

panelAB <- plot_grid(panelA, panelB, labels = c("", ""), ncol = 2, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelAB

panelABC <- plot_grid(panelAB, Ew_cartoon, labels = c("", "C"), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,0.8),label_size = 20)
panelABC

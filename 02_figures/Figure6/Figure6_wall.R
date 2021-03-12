library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)
library(magick)


#export 1100 x 800

C3_line = "solid"
C4_line = "solid"
#blue
C3 <- "#0B3C5D"
C4 <- "#0375B4"

#Figure6A
photo_wall <- cowplot::ggdraw() + cowplot::draw_image("Figure6_photo_wall.png", scale = 0.9)


#Figure6B
wall_LHY_timecourses <- read.csv("Figure6_wall_LHY.txt", sep="\t")
wall_LHY_timecourses$Campo = factor(wall_LHY_timecourses$Campo, levels=c("before the wall", "after the wall"))
wall_LHY <- ggplot(data=wall_LHY_timecourses, aes(x=CT, y=LHY, group=Campo, linetype=Campo)) +
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
  scale_y_continuous(breaks=seq(0,4.2,1.4), name="Normalized Expression", limits=c(-1.05,4.2), labels = scales::number_format(accuracy = 0.01), expand = expansion(mult = c(0, 0.05)))+
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
wall_LHY

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


#Figure6C
cartoon_wall <- cowplot::ggdraw() + cowplot::draw_image("Figure6_cartoon.png", scale = 0.9)

#Figure6D
wall_TOC1_timecourses <- read.csv("Figure6_wall_TOC1.txt", sep="\t")
wall_TOC1_timecourses$Campo = factor(wall_TOC1_timecourses$Campo, levels=c("before the wall", "after the wall"))
wall_TOC1 <- ggplot(data=wall_TOC1_timecourses, aes(x=CT, y=TOC1, group=Campo, linetype=Campo)) +
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
  scale_y_continuous(breaks=seq(0,0.12,0.04), name="Normalized Expression", limits=c(-0.03,0.12), labels = scales::number_format(accuracy = 0.01), expand = expansion(mult = c(0, 0.05)))+
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
wall_TOC1

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

#FinalMaxMin
#maxFS    minFS    maxAS    minAS
#10.5981 1.382911 10.92722 18.16772

panelAC <- plot_grid(photo_wall, cartoon_wall, labels = c("A", "C"), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelAC

panelBD <- plot_grid(wall_LHY, wall_TOC1, labels = c("B", "D"), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelBD

panelABCD <- plot_grid(panelAC, panelBD, labels = c("", ""), ncol = 2, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelABCD
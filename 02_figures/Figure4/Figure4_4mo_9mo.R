library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)
library(magick)


#exportar 1400 x 1400

C1 <- "#007849"
C2 <- "#82b135"
C1_line = "solid"
C2_line = "solid"
C1_fill <- "#7fbba4"
C2_fill <- "#c0d89a"

#Figure 4A
photo_4mo <- cowplot::ggdraw() + cowplot::draw_image("Figure4_photo_4mo.png", scale = 0.9)
photo_9mo <- cowplot::ggdraw() + cowplot::draw_image("Figure4_photo_4mo.png", scale = 0.9)

sugarcane4mo_9mo_LHY_timecourses <- read.csv("Figure4_4mo_9mo_LHY.txt", sep="\t")
sugarcane4mo_9mo_LHY <- ggplot(data=sugarcane4mo_9mo_LHY_timecourses, aes(x=CT, y=LHY, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.75, ymax = 4.5, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.75, ymax = 4.5, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  #(aes(group = Campo, colour = Campo, fill=Campo, linetype=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = 0.20, y = 3, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = 2.69, y = 3, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 21, y = 4.275, label = "italic(LHY)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,4.5,1.5), name="Normalized Expression", limits=c(-0.75,4.5), labels = scales::number_format(accuracy = 0.01), expand = expansion(mult = c(0, 0.05)))+
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
        legend.position = "bottom",
        legend.title = element_blank())+
  background_grid()
sugarcane4mo_9mo_LHY

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

#FinalMaxMin
#maxC1    minC1    maxC2    minC2
#0.203038 16.30481 2.689494 9.980127

sugarcane4mo_9mo_TOC1_timecourses <- read.csv("Figure4_4mo_9mo_TOC1.txt", sep="\t")
#campos_TOC1_timecourses$Campo = factor(campos_TOC1_timecourses$Campo, levels=c("4-month old", "9-month old"))
sugarcane4mo_9mo_TOC1 <- ggplot(data=sugarcane4mo_9mo_TOC1_timecourses, aes(x=CT, y=TOC1, group=Campo, linetype=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.0075, ymax = 0.06, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.0075, ymax = 0.06, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  #stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, linetype=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = 11,04, y = 0.055, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = 11.31, y = 0.055, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 21, y = 0.057, label = "italic(TOC1)", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(0,0.06,0.02), name="Normalized Expression", limits=c(-0.0075,0.06), expand = expansion(mult = c(0, 0.05)))+
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
sugarcane4mo_9mo_TOC1

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

#FinalMaxMin
#maxC1    minC1   maxC2    minC2
#11.04709 3.817722 11.3057 4.677848

#Figure4C
cartoon_4mo <- cowplot::ggdraw() + cowplot::draw_image("Figure4_cartoon_4mo.png", scale = 0.9)
cartoon_9mo <- cowplot::ggdraw() + cowplot::draw_image("Figure4_cartoon_9mo.png", scale = 0.9)


panelA <- plot_grid(photo_4mo, photo_4mo, labels = c("A", ""), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelA

panelB <- plot_grid(sugarcane4mo_9mo_LHY, sugarcane4mo_9mo_TOC1, labels = c("B", ""), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelB

panelAB <- plot_grid(panelA, panelB, labels = c("", ""), ncol = 2, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelAB

panelC <- plot_grid(cartoon_4mo, cartoon_9mo, labels = c("C", ""), ncol = 2, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelC

plot_grid(panelAB, panelC, labels = c("", ""), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,0.5),label_size = 20)


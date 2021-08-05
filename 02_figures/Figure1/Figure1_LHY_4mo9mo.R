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

#Figure1A
sugarcane4mo9mo_LHY_timecourses <- read.csv("Figure1_4mo9mo_LHY.txt", sep="\t")
sugarcane4mo9mo_LHY <- ggplot(data=sugarcane4mo9mo_LHY_timecourses, aes(x=CT, y=LHY, group=Campo, linetype=Campo)) +
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
        legend.position = "none",
        legend.title = element_blank())+
  background_grid()
sugarcane4mo9mo_LHY

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

sugarcane4mo9mo_TOC1_timecourses <- read.csv("Figure1_4mo9mo_TOC1.txt", sep="\t")
sugarcane4mo9mo_TOC1 <- ggplot(data=sugarcane4mo9mo_TOC1_timecourses, aes(x=CT, y=TOC1, group=Campo, linetype=Campo)) +
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
  scale_y_continuous(breaks=seq(0,0.06,0.02), name="Normalized Expression", limits=c(-0.0075,0.06), labels = scales::number_format(accuracy = 0.01), expand = expansion(mult = c(0, 0.05)))+
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
sugarcane4mo9mo_TOC1

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


#Figure1B
foto_campo_C1 <- cowplot::ggdraw() + cowplot::draw_image("Figure1_foto_campo_C1.png", scale = 0.9)
foto_campo_C2 <- cowplot::ggdraw() + cowplot::draw_image("Figure1_foto_campo_C2.png", scale = 0.9)

B <- plot_grid(sugarcane4mo9mo_LHY, sugarcane4mo9mo_TOC1, labels = c("b", " "), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1.2),label_size = 20)

A <- plot_grid(foto_campo_C1, foto_campo_C2, labels = c("a", " "), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)

plot_grid(A, B, labels = c(" ", ""), ncol = 2, align = "none", rel_widths = c(1.1, 1),rel_heights = c(1,1),label_size = 20)



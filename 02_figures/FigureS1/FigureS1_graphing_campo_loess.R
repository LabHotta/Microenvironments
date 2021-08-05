library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())

C1_line = "solid"
C2_line = "solid"

#green
C1 <- "#007849"
C2 <- "#82b135"

#exportar 1400 x 1400

campo = read.csv("FigureS1_campo_temperatura_diaria.txt", sep="\t")

temperature_daily_field <- ggplot(data=campo, aes(x=CT, y=Temperature, group=Sampling)) +
                        annotate("rect", xmin = -2, xmax = 0, ymin = 5, ymax = 50, alpha = .5, fill = "#e3e3e3")+
                        annotate("rect", xmin = 12, xmax = 24, ymin = 5, ymax = 50, alpha = .5, fill = "#e3e3e3")+
                        #geom_line(aes(colour=Sampling, linetype=Sampling), size = 1.2) +
                        geom_jitter(aes(col=Sampling, shape = Sampling),position=position_jitter(0.2), size = 3) +
                        stat_smooth(aes(group = Sampling, colour = Sampling, fill=Sampling, linetype=Sampling, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
                        scale_fill_manual(values=c(C1, C2))+
                        scale_colour_manual(values=c(C1, C2))+
                        scale_linetype_manual(values=c(C1_line, C2_line)) +
                        scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
                        scale_y_continuous(name="Temperature ("~degree~"C)", breaks=seq(5,50,15), limits=c(5,50))+
                        annotate("text", x = 8.30, y = 47.5, label = "\u25bc", size = 6, colour=C1)+
                        annotate("text", x = 9.82, y = 47.5, label = "\u25bc", size = 6, colour=C2)+
                        theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75),
                              text = element_text(size=18), 
                              axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              #axis.text.x = element_text(size=18),
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text.y = element_text(size=18),
                              legend.position="none")
temperature_daily_field

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.96/80)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*26.16/80)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.96/80)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*25.48/80)-0.94
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)
round(FinalMaxMin)
FinalMaxMin


light_daily_field <- ggplot(data=campo, aes(x=CT, y=Light, group=Sampling)) +
                  annotate("rect", xmin = -2, xmax = 0, ymin = -0.4, ymax = 4.5, alpha = .5, fill = "#e3e3e3")+
                  annotate("rect", xmin = 12, xmax = 24, ymin = -0.4, ymax = 4.5, alpha = .5, fill = "#e3e3e3")+
                  #geom_line(aes(colour=Sampling, linetype=Sampling), size = 1.2) +
                  geom_jitter(aes(col=Sampling, shape = Sampling),position=position_jitter(0.2), size = 3) +
                  stat_smooth(aes(group = Sampling, colour = Sampling, fill=Sampling, linetype=Sampling, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
                  scale_fill_manual(values=c(C1, C2))+
                  scale_colour_manual(values=c(C1, C2))+
                  scale_linetype_manual(values=c(C1_line, C2_line)) +
                  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
                  scale_y_continuous(name="Light Intensity (MJ/" ~ m^{2}~")", breaks=seq(0,4.5,1.5), limits=c(-0.4, 4.5))+
                  annotate("text", x = 6.67, y = 4.275, label = "\u25bc", size = 6, colour=C1)+
                  annotate("text", x = 5.90, y = 4.275, label = "\u25bc", size = 6, colour=C2)+
                  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
                  text = element_text(size=18), 
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  #axis.text.x = element_text(size=18),
                  axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.y = element_text(size=18),
                  legend.position="none")
light_daily_field

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.96/80)-1.44
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*26.16/80)-1.95
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.96/80)-1.44
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*25.48/80)-0.94
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)
round(FinalMaxMin)
FinalMaxMin

C4_line = "solid"
C3_line = "solid"
#blue
C3 <- "#0B3C5D"
C4 <- "#0375B4"

muro = read.csv("FigureS1_muro_temperatura_diaria.txt", sep="\t")

temperature_daily_wall <- ggplot(data=muro, aes(x=CT, y=Temperature, group=Sampling)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 5, ymax = 50, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = 5, ymax = 50, alpha = .5, fill = "#e3e3e3")+
  #geom_line(aes(colour=Sampling, linetype=Sampling), size = 1.2) +
  geom_jitter(aes(col=Sampling, shape = Sampling),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Sampling, colour = Sampling, fill=Sampling, linetype=Sampling, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  scale_fill_manual(values=c(C3, C4))+
  scale_colour_manual(values=c(C3, C4))+
  scale_linetype_manual(values=c(C3_line, C4_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(name="Temperature ("~degree~"C)", breaks=seq(5,50,15), limits=c(5, 50))+
  annotate("text", x = 4.93, y = 47.5, label = "\u25bc", size = 6, colour=C3)+
  annotate("text", x = 6.19, y = 47.5, label = "\u25bc", size = 6, colour=C4)+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75),
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")
temperature_daily_wall

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.17/80)-0.42
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*25.17/80)-0.42
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.17/80)-0.42
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*25.17/80)-0.42
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)
round(FinalMaxMin)
FinalMaxMin


light_daily_wall <- ggplot(data=muro, aes(x=CT, y=Light, group=Sampling)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.5, ymax = 4.5, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.5, ymax = 4.5, alpha = .5, fill = "#e3e3e3")+
  #geom_line(aes(colour=Sampling, linetype=Sampling), size = 1.2) +
  geom_jitter(aes(col=Sampling, shape = Sampling),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Sampling, colour = Sampling, fill=Sampling, linetype=Sampling, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  scale_colour_manual(values=c(C3, C4))+
  scale_linetype_manual(values=c(C3_line, C4_line)) +
  scale_fill_manual(values=c(C3, C4))+
  annotate("text", x = 4.61, y = 4.275, label = "\u25bc", size = 6, colour=C3)+
  annotate("text", x = 4.93, y = 4.275, label = "\u25bc", size = 6, colour=C4)+
    scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(name="Light Intensity (MJ/" ~ m^{2}~")", breaks=seq(0,4.5,1.5), limits=c(-0.5, 4.5))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position="none")
light_daily_wall

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.17/80)-0.42
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*25.17/80)-0.42
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.17/80)-0.42
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*25.17/80)-0.42
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)
round(FinalMaxMin)
FinalMaxMin

C5_line = "solid"
C6_line = "solid"

#roxo
C5 <- "#573280"
C6 <- "#b0228c"

margin = read.csv("FigureS1_margens_temperatura_diaria.txt", sep="\t")

temperature_daily_margin <- ggplot(data=margin, aes(x=CT, y=Temperature, group=Sampling)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 5, ymax = 50, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = 5, ymax = 50, alpha = .5, fill = "#e3e3e3")+
  #geom_line(aes(colour=Sampling, linetype=Sampling), size = 1.2) +
  geom_jitter(aes(col=Sampling, shape = Sampling),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Sampling, colour = Sampling, fill=Sampling, linetype=Sampling, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  scale_fill_manual(values=c(C5, C6))+
  scale_colour_manual(values=c(C5, C6))+
  scale_linetype_manual(values=c(C5_line, C6_line)) +
  annotate("text", x = 4.16, y = 47.5, label = "\u25bc", size = 6, colour=C5)+
  annotate("text", x = 7.66, y = 47.5, label = "\u25bc", size = 6, colour=C6)+
    scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.57))+
  scale_y_continuous(name="Temperature ("~degree~"C)", breaks=seq(5,50,15), limits=c(5,50))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75),
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position="none")
temperature_daily_margin

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.48/80)-0.94
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*25.48/80)-0.94
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.48/80)-0.94
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*25.48/80)-0.94
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)
round(FinalMaxMin)
FinalMaxMin



light_daily_margin <- ggplot(data=margin, aes(x=CT, y=Light, group=Sampling)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.5, ymax = 4.5, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.5, ymax = 4.5, alpha = .5, fill = "#e3e3e3")+
  #geom_line(aes(colour=Sampling, linetype=Sampling), size = 1.2) +
  geom_jitter(aes(col=Sampling, shape = Sampling),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Sampling, colour = Sampling, fill=Sampling, linetype=Sampling, outfit=fit2<<-..y..),method="loess",  span = 0.5, size = 1.2)+
  scale_colour_manual(values=c(C5, C6))+
  scale_linetype_manual(values=c(C5_line, C6_line)) +
  scale_fill_manual(values=c(C5, C6))+
  annotate("text", x = 4.79, y = 4.275, label = "\u25bc", size = 6, colour=C5)+
  annotate("text", x = 5.11, y = 4.275, label = "\u25bc", size = 6, colour=C6)+
    scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(name="Light Intensity (MJ/" ~ m^{2}~")", breaks=seq(0,4.5,1.5), limits=c(-0.5, 4.5))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position="none")
light_daily_margin

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxC10 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxC1 <- ((maxC10$row-1)*25.48/80)-0.94
maxC20 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxC2 <- ((maxC20$row-1)*25.48/80)-0.94
minC10 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minC1 <- ((minC10$row-1)*25.48/80)-0.94
minC20 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minC2 <- ((minC20$row-1)*25.48/80)-0.94
FinalMaxMin <- cbind(maxC1, minC1, maxC2, minC2)
round(FinalMaxMin)
FinalMaxMin


plot_grid(light_daily_field, temperature_daily_field, light_daily_margin, temperature_daily_margin, light_daily_wall, temperature_daily_wall, labels = c("A", "B","C", "D","E", "F"), ncol= 2, align = "v",rel_heights = c(1,1,1.15),label_size = 20)

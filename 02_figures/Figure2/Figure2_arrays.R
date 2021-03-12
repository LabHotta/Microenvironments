library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)
library(eulerr)
library(ggrepel)


#export 1400 x 1650
# .svg 1440 x 1700
#set folder first

C1 <- "#007849"
C2 <- "#82b135"
C1_line = "solid"
C2_line = "solid"
C1_fill <- "#7fbba4"
C2_fill <- "#c0d89a"

#Figure 2A_expressed
euler_expressed_C1C2_data  <- euler(c("A" = 69, "B" = 1338, 
                                      "A&B" = 8484))
euler_expressed_C1C2 <- plot(euler_expressed_C1C2_data, 
                             lex = 4,
                             labels = c("4 months", "9 months"), 
                             fill_alpha = 0.5,
                             col = c(C1, C2),
                             border = c(C1, C2),
                             quantities = TRUE,
                             fill = c(C1_fill, C2_fill)
)
euler_expressed_C1C2


#Figure 2A_rhythmic
euler_rhythmic_C1C2_data <- euler(c("A" = 33, "B" = 2616, 
                                    "A&B" = 4110))
euler_rhythmic_C1C2 <- plot(euler_rhythmic_C1C2_data, 
                            lex = 4,
                            labels = c("4 months", "9 months"), 
                            fill_alpha = 0.5,
                            col = c(C1, C2),
                            border = c(C1, C2),
                            quantities = TRUE,
                            fill = c(C1_fill, C2_fill)
)
euler_rhythmic_C1C2


#Figure 2B
phase_data <- read.csv("Figure2_phase_difference.txt", sep="\t")
phase_difference <- ggplot(data=phase_data, aes(x=Phase.difference, y=Counts)) +
  geom_vline(xintercept=-1, linetype="dashed", color = "red", size = 1)+
  geom_vline(xintercept=1, linetype="dashed", color = "red", size = 1)+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  scale_x_continuous(breaks=seq(-12,12,2), name="Phase Difference (h)", limits=c(-12, 12))+
  scale_y_continuous(name="Counts", breaks=seq(0,900,300), limits=c(0, 900))+
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position = "top",
        legend.title = element_blank()
  )
phase_difference


#Figure 2C_VRLK1
timecourses_VRLK1 <- read.csv("Figure2_VRLK1_timecourses_v2.txt", sep="\t")
ggplot(data=timecourses_VRLK1, aes(x=CT, y=VRLK1, group=Campo)) +
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

VRLK1 <- ggplot(data=timecourses_VRLK1, aes(x=CT, y=VRLK1, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "VRLK1", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position = "none"
  )
VRLK1
FinalMaxMin


#Figure 2C_CesA
timecourses_CESA <- read.csv("Figure2_CESA_timecourses_v2.txt", sep="\t")
ggplot(data=timecourses_CESA, aes(x=CT, y=CESA, group=Campo)) +
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

CESA <- ggplot(data=timecourses_CESA, aes(x=CT, y=CESA, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "CesA", size = 6, parse=TRUE)+
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
CESA
FinalMaxMin


#Figure 2C_ERF11
timecourses_ERF11 <- read.csv("Figure2_ERF11_timecourses_v2.txt", sep="\t")
ggplot(data=timecourses_ERF11, aes(x=CT, y=ERF11, group=Campo)) +
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

ERF11 <- ggplot(data=timecourses_ERF11, aes(x=CT, y=ERF11, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "ERF11", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
ERF11
FinalMaxMin


#Figure 2C_POP2
timecourses_POP2 <- read.csv("Figure2_POP2_timecourses_v2.txt", sep="\t")
ggplot(data=timecourses_POP2, aes(x=CT, y=POP2, group=Campo)) +
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

POP2 <- ggplot(data=timecourses_POP2, aes(x=CT, y=POP2, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3.2, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "POP2", size = 6, parse=TRUE)+
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
POP2
FinalMaxMin
 

#Figure 2D_bubble
annotation <- read.csv("Figure2_annotation_bubble.txt", sep="\t")
annotation_bubble <- ggplot(annotation, aes(x=prop_C1, y=prop_C2, size = expressed_all, fill = C1_C2)) +
    geom_vline(xintercept=0.514, colour = C2, alpha=0.5, linetype="dashed", size = 1)+
    geom_hline(yintercept=0.746, colour = C1, alpha=0.5, linetype="dashed", size = 1)+
    geom_point(alpha=0.6, shape=21, color="black")+
    scale_size(range = c(1, 14)) +
    geom_text_repel(aes(label = Pathway, colour = C1_C2), size = 5.5)+
    scale_color_gradientn(colours = c(C1,C2), aesthetics=c("fill", "color"))+
    scale_x_continuous(breaks=seq(0.3,1,0.1), name="Propotion of rhythmic in 4 mo.", limits=c(0.3, 1))+
    scale_y_continuous(breaks=seq(0.5,1,0.1), name="Propotion of rhythmic in 9 mo.", limits=c(0.45, 1))+
    theme(text = element_text(size=18), 
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=18),
          legend.position = "none")
annotation_bubble  


panelABC <- plot_grid(euler_expressed_C1C2, euler_rhythmic_C1C2, phase_difference, labels = c("A", "", "B"), ncol = 3, rel_widths = c(0.5, 0.4,1), label_size = 20)

panelDEFG <- plot_grid(VRLK1, CESA, ERF11, POP2, labels = c("C", "", "", ""), ncol = 4, rel_widths = c(1.3, 1,1,1),label_size = 20)

plot_grid(panelABC, panelDEFG, annotation_bubble, labels = c("", "", "D"), ncol = 1, rel_heights = c(1,1,1.6), label_size = 20)

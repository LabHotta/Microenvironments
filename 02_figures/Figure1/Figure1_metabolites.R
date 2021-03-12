library(pcaMethods)
library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)

#export 1400 x 1650
# .svg 1440 x 1700
#set folder first

C1 <- "#007849"
C2 <- "#82b135"
C1_line = "longdash"
C2_line = "solid"
day = "#ff9505"
night = "#275dad"
outlier_C1 = "#275dad"
outlier_C2 = "#ff9505"

#Figure1A_4mo
C1_PCA0 <- read.csv("Figure1_C1_PCA_t.txt", sep="\t")
C1_Classes <- factor(C1_PCA0$Day_night)
C1_PCA_results <- pca(C1_PCA0[,-2], center = T, nPcs = 2, method = "nipals")
#slplot(C1_PCA_results, scoresLoadings = c(T,F), scol = C1_Classes)
#slplot(C1_PCA_results, sl=as.character(C1_PCA[,1]), scoresLoadings = c(T,F))
C1ggplot2 <- merge(C1_PCA0, scores(C1_PCA_results), by=0)
C1_PCA <- ggplot(C1ggplot2, aes(PC1, PC2, colour=Day_night)) +
  geom_point(size=3) +
  stat_ellipse(level = 0.85)+
  scale_x_continuous(breaks=seq(-4,4,2), name="PC1", limits=c(-4,4), labels=function(x){sprintf("%.1f", x)})+
  scale_y_continuous(breaks=seq(-4,4,2), name="PC2", limits=c(-4,4), labels=function(x){sprintf("%.1f", x)})+
  scale_colour_manual(values=c(day, night, outlier_C1))+
  scale_fill_manual(values=c(day, night, outlier_C1))+
  annotate("text", x = 0.40, y = 0.45, label = "ZT21a", colour=night, size = 3, parse=TRUE)+
  annotate("text", x = 0.60, y = 0.8, label = "ZT21b", colour=night, size = 3, parse=TRUE)+
  annotate("text", x = 0.68, y = 1.05, label = "ZT21c", colour=night, size = 3, parse=TRUE)+
  annotate("text", x = 2.5, y = 3.5, label = "4 months old", size = 6)+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=16), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=16),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=16),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none"
  )


#Figure1A_9mo
C2_PCA0 <- read.csv("Figure1_C2_PCA_t.txt", sep="\t")
C2_Classes <- factor(C2_PCA0$Day_night)
C2_PCA_results <- pca(C2_PCA0[,-2], center = T, nPcs = 2, method = "nipals")
slplot(C2_PCA_results, scoresLoadings = c(T,F), scol = C2_Classes)
#slplot(C2_PCA_results, sl=as.character(C2_PCA[,1]), scoresLoadings = c(T,F))
C2ggplot2 <- merge(C2_PCA0, scores(C2_PCA_results), by=0)
C2_PCA <- ggplot(C2ggplot2, aes(PC1, PC2, colour=Day_night)) +
  stat_ellipse(level = 0.85)+
  geom_point(size=3) +
  annotate("text", x = 0.355, y = 0.282, label = "ZT01a", colour=day, size = 3, parse=TRUE)+
  annotate("text", x = 0.315, y = 0.352, label = "ZT01b", colour=day, size = 3, parse=TRUE)+
  annotate("text", x = 0.4, y = 0.55, label = "9 months old", size = 6)+
  scale_colour_manual(values=c(day, night, outlier_C2))+
  scale_fill_manual(values=c(day, night, outlier_C2))+
  scale_x_continuous(breaks=seq(-0.6,0.6,0.3), name="PC1", limits=c(-0.6,0.6))+
  scale_y_continuous(breaks=seq(-0.6,0.6,0.3), name="PC2", limits=c(-0.6,0.6))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=16), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=16),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=16),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none"
  )


#Figure1B
phase_aas0 <- read.csv("Figure1_phase_aas.txt", sep="\t")
phase_aas0$Metabolites <- factor(phase_aas0$Metabolites, levels = phase_aas0$Metabolites[order(-phase_aas0$Shift)])
phase_shift <- ggplot(phase_aas0) +
  #geom_vline(aes(xintercept = 0), color = "black", size = 0.7) +
  geom_segment(aes(x=0, xend=Shift, y=Metabolites, yend=Metabolites), color="black",size=1) +
  geom_point( aes(x=Shift, y=Metabolites), color= C2, shape = "\u25bc",size=5 )+
  geom_point( aes(x=0, y=Metabolites), color= C1, shape = "\u25bc", size=5 )+
  scale_x_continuous(breaks=seq(-12,12,6), limits=c(-13, 13), labels = function(x) ifelse(x>0, paste0("+", x), x))+
  xlab("Phase Difference")+
  theme(panel.grid.major.x = element_line(colour = "#efefef", size = 1), 
        text = element_text(size=18),
        axis.ticks = element_blank(),
        axis.line.x = element_line(size = 0.7),
        axis.line.y = element_blank(),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=18)
  )
phase_shift


#Figure1C
sugarcane4mo9mo_Sucrose_timecourses <- read.csv("Figure1_4mo9mo_Sucrose.txt", sep="\t")
ggplot(data=sugarcane4mo9mo_Sucrose_timecourses, aes(x=CT, y=Sucrose, group=Campo)) +
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

sugarcane4mo9mo_Sucrose <- ggplot(data=sugarcane4mo9mo_Sucrose_timecourses, aes(x=CT, y=Sucrose, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "Sucrose", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
sugarcane4mo9mo_Sucrose
FinalMaxMin


#Figure1D
sugarcane4mo9mo_Tyrosine_timecourses <- read.csv("Figure1_4mo9mo_Tyrosine.txt", sep="\t")
ggplot(data=sugarcane4mo9mo_Tyrosine_timecourses, aes(x=CT, y=Tyrosine, group=Campo)) +
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

sugarcane4mo9mo_Tyrosine <- ggplot(data=sugarcane4mo9mo_Tyrosine_timecourses, aes(x=CT, y=Tyrosine, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "Tyrosine", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
sugarcane4mo9mo_Tyrosine
FinalMaxMin



#Figure1E
sugarcane4mo9mo_Alanine_timecourses <- read.csv("Figure1_4mo9mo_Alanine.txt", sep="\t")
ggplot(data=sugarcane4mo9mo_Alanine_timecourses, aes(x=CT, y=Alanine, group=Campo)) +
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

sugarcane4mo9mo_Alanine <- ggplot(data=sugarcane4mo9mo_Alanine_timecourses, aes(x=CT, y=Alanine, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "Alanine", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
sugarcane4mo9mo_Alanine
FinalMaxMin


#Figure1G
sugarcane4mo9mo_Glutamate_timecourses <- read.csv("Figure1_4mo9mo_Glutamate.txt", sep="\t")
ggplot(data=sugarcane4mo9mo_Glutamate_timecourses, aes(x=CT, y=Glutamate, group=Campo)) +
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

sugarcane4mo9mo_Glutamate <- ggplot(data=sugarcane4mo9mo_Glutamate_timecourses, aes(x=CT, y=Glutamate, group=Campo)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Campo, shape = Campo),position=position_jitter(0.2), size = 3) +
  stat_smooth(aes(group = Campo, colour = Campo, fill=Campo, outfit=fit2<<-..y..),method="loess",  span = 0.5)+
  scale_colour_manual(values=c(C1, C2))+
  scale_fill_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line, C2_line)) +
  annotate("text", x = maxC1, y = 2.75, label = "\u25bc", size = 6, colour=C1)+
  annotate("text", x = maxC2, y = 2.75, label = "\u25bc", size = 6, colour=C2)+
  annotate("text", x = 22, y = 2.75, label = "Glutamate", size = 6, parse=TRUE)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.75))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Normalized Expression", limits=c(-3,3), labels = scales::number_format(accuracy = 0.1))+
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
sugarcane4mo9mo_Glutamate
FinalMaxMin


#panelA
PCA <- plot_grid(C1_PCA, C2_PCA, labels = c("A", ""), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)

#panelsABC
Upper <- plot_grid(PCA, phase_shift, labels = c(" ", "B"), ncol = 2, align = "none", rel_widths = c(1, 0.9),rel_heights = c(1,1),label_size = 20)

#panelsDEFG
Down <- plot_grid(sugarcane4mo9mo_Sucrose, sugarcane4mo9mo_Tyrosine, sugarcane4mo9mo_Alanine, sugarcane4mo9mo_Glutamate, labels = c("C", "D","E", "F" ), ncol = 2, align = "none", rel_widths = c(1, 0.9),rel_heights = c(1,1.3),label_size = 20)

#all
plot_grid(Upper, Down, labels = c("", ""), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)

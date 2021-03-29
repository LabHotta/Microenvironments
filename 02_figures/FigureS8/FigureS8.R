library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)


#export 1100 x 1350

C1 <- "#007849"
C2 <- "#82b135"
C3 <- "#0B3C5D"
C4 <- "#0375B4"
C5 <- "#573280"
C6 <- "#b0228c"


LHY_levels <- read.csv("LHY_histogram.txt", sep="\t")
LHY_levels$Treatments = factor(LHY_levels$Treatments, levels=c("4 mo.", "9 mo.", "East", "West", "Before", "After"))

LHY_experiments <- ggplot(data=LHY_levels, aes(x=Experiments,Treatments, y=LHY, colour=Treatments)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7, fill="#bdbdbd", size = 1)+
  geom_errorbar(aes(ymin=LHY-STDEV, ymax=LHY+STDEV), width=.2, position=position_dodge(.7), size = 1)+
  scale_colour_manual(values=c(C1, C2, C5, C6, C3, C4))+
  scale_y_continuous(breaks=seq(0,9,3), name="Normalized Expression", limits=c(0,9), labels = scales::number_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.05)))
  LHY_experiments


+ 
  scale_fill_manual(values=c("#383838", "#bdbdbd"))+
  scale_x_discrete(labels=c("leaf +1 circadian" = "Circadian", "leaf +1 C1" = "4 months old", "leaf +1 C2" = "9 months old"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15000), breaks=seq(0,12000, by=2000), name="Counts")+
  theme(text = element_text(size=18), 
        legend.title = element_blank(), 
        legend.position="right", 
        axis.ticks = element_blank(),
        legend.spacing.x = unit(0.05, 'cm'),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.major.y = element_line(colour = "#e5e5e5", size = 0.5),
        panel.grid.major.x = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,-5,-5),
        axis.title = element_text(face="bold", size=14),
        axis.title.x = element_blank()
  ) 
Transcriptome_description 
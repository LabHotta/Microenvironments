library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
theme_set(theme_minimal_grid())
library(png)
library(grid)
library(RColorBrewer)

#exportar 1400 x 1700

C1_line = "solid"
C2_line = "solid"

#green
C1 <- "#007849"
C2 <- "#82b135"

#Figure S6A
ritmicos = read.csv("FigureS6_Expressed_rhythmic.txt", sep="\t")
ritmicos$Organ <- factor(ritmicos$Organ, levels = c("leaf +1 circadian", "leaf +1 C1", "leaf +1 C2"))
names(ritmicos) [names(ritmicos) == "expressed"] <- "expressed"
names(ritmicos) [names(ritmicos) == "rhythmic"] <- "rhythmic"

Transcriptome_description <- ggplot(data=ritmicos, aes(x=Organ, y=Count, fill=Type)) +
  geom_rect(data = ritmicos, aes(x = Organ, y=Count), xmin = as.numeric(ritmicos$Organ[[1]]) - 0.52,
            xmax = as.numeric(ritmicos$Organ[[1]]) + 0.52,
            ymin = 50, ymax = 14600, fill="transparent", linetype= 2, colour="#969696", size = 1)+
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+ 
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
  ) +
  annotate("text", x = 1, y = 14000, label = "circadian", size=7)+
  annotate("text", x = 1, y = 13000, label = "32.1%", size=7)+
  annotate("text", x = 2, y = 13000, label = "48.4%", size=7)+
  annotate("text", x = 3, y = 13000, label = "68.3%", size=7)
Transcriptome_description 

#Figure S6B
phase <- read.csv("FigureS6_phase_distribution.txt", sep="\t")
phase_C1_C2 <- ggplot(data=phase , aes(x=ZT, y=Timecourses, group=Group)) +
  #annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 2000, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Group, linetype=Group), size = 1.5) +
  scale_colour_manual(values=c(C1, C2))+
  scale_linetype_manual(values=c(C1_line,C2_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(0, 24))+
  scale_y_continuous(breaks=seq(0,2000,500), name="Counts", limits=c(0, 2000))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=16), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        legend.position = "none"
  )
phase_C1_C2


#Figure S6C_expressed
annotation <- read.csv("FigureS6_Annotation_expressed_abs.txt", sep="\t")
annotation$Pathway <- factor(annotation$Pathway,levels = c("Transcription Factors", "Protein Degradation", 
                                                           "Protein Synthesis",  "RNA degradation", 
                                                           "RNA synthesis and processing", "DNA replication",
                                                           "Chromatin remodelling", "Vesicle Trafficking", 
                                                           "Transporters", "Redox", "Pigment Synthesis", 
                                                           "Photorespiration", "Photosynthesis", 
                                                           "Nitrogen Metabolism", "Lipid Metabolism", 
                                                           "Light Harvesting", 
                                                           "Cell Wall Synthesis/ Cell Expansion", 
                                                           "Carbohydrate Metabolism", "Amino Acid Metabolism", 
                                                           "Light Signalling", "Jasmonate Signalling", 
                                                           "Gibberellin Signalling", "Flowering", 
                                                           "Ethylene Signalling", "Cytokinin Signalling",
                                                           "Circadian Clock", "Brassinosteroid Signalling",
                                                           "Auxin Signalling", "ABA/drought signalling"))
annotation$Group <- factor(annotation$Group,levels = c("C2 expressed", "C1 expressed"))
annotation_expressed <- ggplot(data=annotation, aes(x=Pathway, y=Counts, group=Group)) +
  geom_bar(stat="identity", aes(fill = Group), position=position_dodge())+
  scale_y_continuous(breaks=seq(0,400, 100), limits=c(0, 400))+
  scale_fill_manual(values=c(C2, C1))+
  theme_minimal_vgrid()+
  coord_flip()+
  theme(text = element_text(size=16), 
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
annotation_expressed 


#Figure S6C_rhythmic
annotation <- read.csv("FigureS6_Annotation_rhythmic_abs.txt", sep="\t")
annotation$Pathway <- factor(annotation$Pathway,levels = c("Transcription Factors", "Protein Degradation", 
                                                           "Protein Synthesis",  "RNA degradation", 
                                                           "RNA synthesis and processing", "DNA replication",
                                                           "Chromatin remodelling", "Vesicle Trafficking", 
                                                           "Transporters", "Redox", "Pigment Synthesis", 
                                                           "Photorespiration", "Photosynthesis", 
                                                           "Nitrogen Metabolism", "Lipid Metabolism", 
                                                           "Light Harvesting", 
                                                           "Cell Wall Synthesis/ Cell Expansion", 
                                                           "Carbohydrate Metabolism", "Amino Acid Metabolism", 
                                                           "Light Signalling", "Jasmonate Signalling", 
                                                           "Gibberellin Signalling", "Flowering", 
                                                           "Ethylene Signalling", "Cytokinin Signalling",
                                                           "Circadian Clock", "Brassinosteroid Signalling",
                                                           "Auxin Signalling", "ABA/drought signalling"))
annotation$Group <- factor(annotation$Group,levels = c("C2 rhythmic", "C1 rhythmic"))
annotation_rhythmic <- ggplot(data=annotation, aes(x=Pathway, y=Counts, group=Group)) +
  geom_bar(stat="identity", aes(fill = Group), position=position_dodge())+
  scale_y_continuous(breaks=seq(0,400, 100), limits=c(0, 400))+
  scale_fill_manual(values=c(C1, C2))+
  theme_minimal_vgrid()+
  coord_flip()+
  theme(text = element_text(size=16), 
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
annotation_rhythmic


#panelsBC
panelAC <- plot_grid(Transcriptome_description, phase_C1_C2, labels = c("A", "C"), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1),label_size = 20)
panelAC

#panelsABC
plot_grid(panelAC, annotation_expressed, labels = c("", "B"), ncol = 2, align = "none", rel_widths = c(1, 0.9),rel_heights = c(1,1),label_size = 20)

plot_grid(Transcriptome_description, phase_C1_C2,annotation_expressed, annotation_rhythmic, labels = c("A", "B", "C", "D"), ncol = 2, align = "none", rel_widths = c(1, 1),rel_heights = c(1,2),label_size = 20)

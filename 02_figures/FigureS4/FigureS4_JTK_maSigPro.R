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


JTK <- "#ca0020"
Dantas20 <- "#0571b0"
JTK_fill <- "#d6604d"
Dantas20_fill <- "#4393c3"

C1 <- "#007849"
C2 <- "#82b135"
C1_fill <- "#7fbba4"
C2_fill <- "#c0d89a"

euler_JTK_C1_data  <- euler(c("A" = 237, "B" = 2987, 
                                      "A&B" = 1156))
euler_JTK_C1 <- plot(euler_JTK_C1_data, 
                             lex = 4,
                             labels = c("JTK-cycle", "Dantas et al. 2020"), 
                             fill_alpha = 0.5,
                             #main = "rhythmic",
                             col = c(JTK, Dantas20),
                             border = c(JTK, Dantas20),
                             quantities = TRUE,
                             fill = c(JTK_fill, Dantas20_fill)
)
euler_JTK_C1


euler_JTK_C2_data  <- euler(c("A" = 334, "B" = 1207, 
                              "A&B" = 5498))
euler_JTK_C2 <- plot(euler_JTK_C2_data, 
                     lex = 4,
                     labels = c("JTK-cycle", "Dantas et al. 2020"), 
                     fill_alpha = 0.5,
                     #main = "rhythmic",
                     col = c(JTK, Dantas20),
                     border = c(JTK, Dantas20),
                     quantities = TRUE,
                     fill = c(JTK_fill, Dantas20_fill)
)
euler_JTK_C2


euler_JTK_C1_C2_data  <- euler(c("A" = 142, "B" = 4483, 
                              "A&B" = 1014))
euler_JTK_C1_C2 <- plot(euler_JTK_C1_C2_data, 
                     lex = 4,
                     labels = c("C1", "C2"), 
                     fill_alpha = 0.5,
                     #main = "rhythmic",
                     col = c(C1, C2),
                     border = c(C1, C2),
                     quantities = TRUE,
                     fill = c(C1_fill, C2_fill)
)
euler_JTK_C1_C2


phase_data <- read.csv("JTK_phase_difference.txt", sep="\t")
phase_difference_JTK <- ggplot(data=phase_data, aes(x=Phase.difference, y=Counts, group=Method, col=Method)) +
  #annotate("rect", xmin = -1, xmax = 1, ymin = 0, ymax = 900, alpha = .5, fill = "#e3e3e3")+
  geom_vline(xintercept=-1, linetype="dashed", color = "red", size = 1)+
  geom_vline(xintercept=1, linetype="dashed", color = "red", size = 1)+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  #geom_bar(stat="identity")+
  scale_colour_manual(values=c(Dantas20,JTK))+
  scale_fill_manual(values=c(Dantas20,JTK))+
  scale_x_continuous(breaks=seq(-12,12,2), name="Phase Difference (h)", limits=c(-12, 12))+
  scale_y_continuous(name="Counts", breaks=seq(0,450,150), limits=c(0, 450))+
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank()
  )
phase_difference_JTK


phase_data <- read.csv("maSigPro_phase_difference.txt", sep="\t")
phase_difference_maSigPro <- ggplot(data=phase_data, aes(x=Phase.difference, y=Counts, group=Method, col=Method)) +
  #annotate("rect", xmin = -1, xmax = 1, ymin = 0, ymax = 900, alpha = .5, fill = "#e3e3e3")+
  geom_vline(xintercept=-1, linetype="dashed", color = "red", size = 1)+
  geom_vline(xintercept=1, linetype="dashed", color = "red", size = 1)+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  #geom_bar(stat="identity")+
  scale_colour_manual(values=c(Dantas20,JTK))+
  scale_fill_manual(values=c(Dantas20,JTK))+
  scale_x_continuous(breaks=seq(-12,12,2), name="Phase Difference (h)", limits=c(-12, 12))+
  scale_y_continuous(name="Counts", breaks=seq(0,450,150), limits=c(0, 450))+
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank()
  )
phase_difference_maSigPro

AB <- plot_grid(euler_JTK_C1, euler_JTK_C2, labels = c("a", "b"), ncol = 2, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1,1),label_size = 20)

DE <- plot_grid(phase_difference_JTK, phase_difference_maSigPro, labels = c("d", "e"), ncol = 2, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1,1),label_size = 20)

ABCDE <- plot_grid(AB, euler_JTK_C1_C2, DE, labels = c(" ", "c", " "), ncol = 1, align = "none", rel_widths = c(1, 1),rel_heights = c(1,1,1.5),label_size = 20)
ABCDE

library(eulerr)
library(png)
library(cowplot)
library(grid)

#save 700x700

C1 <- "#007849"
C2 <- "#82b135"

C1_fill <- "#7fbba4"
C2_fill <- "#c0d89a"


#METABOLITES
euler_detected_metabolites_C1C2_data  <- euler(c("A" = 18, "B" = 12, 
                                                 "A&B" = 39))
euler_detected_metabolites_C1C2 <- plot(euler_detected_metabolites_C1C2_data, 
                                        lex = 4,
                                        labels = c("4 months", "9 months"), 
                                        fill_alpha = 0.5,
                                        #main = "rhythmic",
                                        col = c(C1, C2),
                                        border = c(C1, C2),
                                        quantities = TRUE,
                                        fill = c(C1_fill, C2_fill)
)
euler_detected_metabolites_C1C2

euler_rhythmic_metabolites_C1C2_data  <- euler(c("A" = 11, "B" = 13, 
                                                 "A&B" = 20))
euler_rhythmic_metabolites_C1C2 <- plot(euler_rhythmic_metabolites_C1C2_data, 
                                        lex = 4,
                                        labels = c("4 months", "9 months"), 
                                        fill_alpha = 0.5,
                                        #main = "rhythmic",
                                        col = c(C1, C2),
                                        border = c(C1, C2),
                                        quantities = TRUE,
                                        fill = c(C1_fill, C2_fill)
)
euler_rhythmic_metabolites_C1C2


plot_grid(euler_detected_metabolites_C1C2, euler_rhythmic_metabolites_C1C2, labels = c("A", "B"), ncol = 2, rel_widths = c(1, 1),rel_heights = c(1,1), label_size = 20)

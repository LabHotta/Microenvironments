library("maSigPro")

data.field <- read.csv("data.field.txt", sep="\t")
rownames(data.field) <- data.field[,1]
data.field <- data.field[,-1]

edesign.field <- read.csv("edesign.field.txt", sep="\t")
rownames(edesign.field) <- edesign.field[,1]
edesign.field <- edesign.field[,-1]

design <- make.design.matrix(edesign.field, degree = 2)
#"C2vsC1" "C1"     "C2vsC1" "C1"     "C2vsC1"

fit <- p.vector(data.field, design, Q = 0.05, MT.adjust = "BH", min.obs = 12)
fit$i
#3650

tstep <- T.fit(fit, step.method = "backward", alfa = 0.05)

sigs <- get.siggenes(tstep, rsq = 0.6, vars = "groups")

sigs$sig.genes$C2vsC1$g
#252

see.genes(sigs$sig.genes$C2vsC1, show.fit = T, dis =design$dis,cluster.method="hclust" ,cluster.data = 1, k = 9)

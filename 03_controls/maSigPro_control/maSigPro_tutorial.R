library("maSigPro")

data(data.abiotic)
head(data.abiotic)
#Control_3H_1 Control_3H_2 Control_3H_3 Control_9H_1 Control_9H_2 Control_9H_3 Control_27H_1 Control_27H_2
#STMDF90   0.13735714  -0.36530651  -0.15329448   0.44754535  0.287476796  0.248818724   0.179325865    0.12799310

data(edesign.abiotic)
#Time Replicate Control Cold Heat Salt
#Control_3H_1    3         1       1    0    0    0

design <- make.design.matrix(edesign.abiotic, degree = 2)

design$groups.vector

fit <- p.vector(data.abiotic, design, Q = 0.05, MT.adjust = "BH", min.obs = 20)

tstep <- T.fit(fit, step.method = "backward", alfa = 0.05)

sigs <- get.siggenes(tstep, rsq = 0.6, vars = "groups")

names(sigs$sig.genes)
suma2Venn(sigs$summary[, c(2:4)])

sigs$sig.genes$SaltvsControl$g
see.genes(sigs$sig.genes$ColdvsControl, show.fit = T, dis =design$dis,cluster.method="hclust" ,cluster.data = 1, k = 9)

STMDE66 <- data.abiotic[rownames(data.abiotic)=="STMDE66", ]
PlotGroups (STMDE66, edesign = edesign.abiotic)
PlotGroups (STMDE66, edesign = edesign.abiotic, show.fit = T, dis = design$dis, groups.vector = design$groups.vector)



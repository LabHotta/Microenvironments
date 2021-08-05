library("maSigPro")

data.field <- read.csv("data.field.normalized.txt", sep="\t")
rownames(data.field) <- data.field[,1]
data.field <- data.field[,-1]

edesign.field <- read.csv("edesign.field.normalized.txt", sep="\t")
rownames(edesign.field) <- edesign.field[,1]
edesign.field <- edesign.field[,-1]

design <- make.design.matrix(edesign.field, degree = 4)
#"C2vsC1" "C1"     "C2vsC1" "C1"     "C2vsC1"

fit <- p.vector(data.field, design, Q = 0.05, MT.adjust = "BH", min.obs = 12)
fit$i
#3873
#d4_3924

tstep <- T.fit(fit, step.method = "backward", alfa = 0.05)

sigs <- get.siggenes(tstep, rsq = 0.6, vars = "groups")

sigs$sig.genes$C2vsC1$g
#880
#d4_1539

see.genes(sigs$sig.genes$C2vsC1, show.fit = T, dis =design$dis,cluster.method="hclust" ,cluster.data = 1, k = 9)

results <- sigs$summary$C2vsC1
write.table(results,file=paste("maSigPro_results","xls",sep="."),row.names=F,col.names=T,quote=F,sep="\t")


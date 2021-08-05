source("JTK_CYCLE.R")

project <- "folha1_1cycle"
options(stringsAsFactors=FALSE)
annot <- read.delim("temp/annot_modules_l1.txt")
data <- read.delim("temp/input_modules_l1.txt")

rownames(data) <- data[,1]
data <- data[,-1]
jtkdist(ncol(data))

periods <- 12; interval <- 2;
jtk.init(periods,interval)

cat("JTK analysis started on",date(),"\n")
flush.console()

st <- system.time({
  res <- apply(data,1,function(z) {
    jtkx(z)
    c(JTK.ADJP,JTK.PERIOD,JTK.LAG,JTK.AMP)
  })
  res <- as.data.frame(t(res))
  bhq <- p.adjust(unlist(res[,1]),"BH")
  res <- cbind(bhq,res)
  colnames(res) <- c("BH.Q","ADJ.P","PER","LAG","AMP")
  results <- cbind(annot,res,data)
  results <- results[order(res$ADJ.P,-res$AMP),]
})
print(st)

save(results,file=paste("JTK",project,"rda",sep="."))
write.table(results,file=paste("temp/JTK",project,"xls",sep="."),row.names=F,col.names=T,quote=F,sep="\t")


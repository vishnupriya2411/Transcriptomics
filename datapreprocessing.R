saveRDS(rfiles.new, "c:/Users/USER/Desktop/R/TSdata_cons_noncons_hsa.rds")
rm(rfiles.new)

rfiles.new = readRDS(file = "c:/Users/USER/Desktop/R/TSdata_cons_noncons_hsa.rds")

library(dplyr)

newrfiles.new = rfiles.new %>% group_by(Gene.Symbol, miRNA) %>% 
  summarise(max.contextscore = max(context...score))

wd = "c:/Users/USER/Desktop/Project/Datasets/BA_10/"

setwd(wd)

new.gene = read.table(file = "BA_10genes.txt", header = T, sep = "\t", na.strings = NA)
new.microRNA = read.table(file = "BA_10microRNA.txt", header = T, sep = "\t", na.strings = NA)

gengrn = function(template, de.gene, de.microRNA) {
  template = newrfiles.new
  de.gene = new.gene
  de.microRNA = new.microRNA
  
  ## subsetting microRNA
  dataRNA.subset = subset(de.microRNA, pval <= 0.05)
  
  ## subsetting genes
  datagene.subset = subset(de.gene, pval <= 0.05)
  
  DEmicroRNA = right_join(x = template, y = dataRNA.subset)
  DEgene = right_join(x = DEmicroRNA, y = datagene.subset, by = c("Gene.Symbol" = "Gene.Symbol"))
  
  df = subset(DEgene, select = -c(Gene.Tax.ID,Transcript.ID,weighted.context...score.percentile,weighted.context...score))
  return(df)
}
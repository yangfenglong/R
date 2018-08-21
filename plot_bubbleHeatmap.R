args<-commandArgs(T)
if(length(args)<2){
    cat ("[usage:] <correlation.xls> <out.pdf>\n")
    cat ("Example: corrheatmap.R   correlation.xls  correlation.pdf \n")
    quit("no")
}

library(ggplot2)
library(reshape2)
cor=read.table(args[1],head=T,sep='\t')
melt<-melt(cor,value.name="value")
pdf(args[2])
ggplot(melt, aes(x = X, y = variable)) +
    geom_point(aes(size = abs(value), colour = value)) + 
#    geom_text(hjust = 1, size = 2) +
#    scale_size(range = c(1,15)) +
	labs(title ="", xlab = "", ylab ="") 
    theme_bw()
dev.off()


args <- commandArgs(T)
if(length(args)<2){
    cat("usage: <density.table.txt> <colnumber>  <output: <density.pdf> <density.png>\ncolnumber is the density colum")
    cat ("Example:  /PUBLIC/software/public/System/R-3.2.4/bin/Rscript density_plot.R density.table.txt 2  density.pdf\n")
    quit("no")
}
#=========================

len <- read.table(file=args[1],sep="\t")
col <- as.numeric(args[2])
library(ggplot2)
plot <-  ggplot(len,aes(len[col])) +
  geom_density()+
  theme(
#		title=element_text(family="Arial",size=12,color="black", face="plain",hjust=0.2,lineheight=0.2), 
#Font face ("plain", "italic", "bold", "bold.italic")
        axis.title.x=element_text(size=15,face="plain",color="black",hjust=0.5),
        axis.title.y=element_text(size=15,color="black",hjust=0.5), #angle=45
        axis.text.x=element_text(family="Arial",size=15,color="black"),
        axis.text.y=element_text(family="Arial",size=15,color="black"))


pdf <- paste(args[1],"pdf",sep=".") 
png <- paste(args[1],"png",sep=".") 
cairo_pdf(filename=pdf,height=10,width=12)
plot
png(filename=png,res=600,height=5400,width=7200,type="cairo")
plot
dev.off()

args <- commandArgs(T)
if(length(args)<2){
    cat("usage: <NMDS_scores.txt> <stress> <output:NMDS_scores.summary.txt> <nmds.pdf> <nmds.png> \n")
    cat ("Example:  /PUBLIC/software/public/System/R-2.15.3/bin/Rscript plot.R NMDS_scores.txt.bak 0.227 NMDS_scores.summary.txt nmds.pdf nmds.png\n")
    quit("no")
}

plot.txt <- args[1]
stress <- args[2]
output <- args[3]
pdf <- args[4]
png <- args[5]

df<-read.table(file=plot.txt,header=T)
colnames(df) <- c("sample","MDS1","MDS2","group")

library(plyr)
library(ggplot2)

df.summary <- ddply(df,"group",summarize,
	MDS1.mean=mean(MDS1),
	MDS1.sd=sd(MDS1),
	MDS1.lower=MDS1.mean - MDS1.sd/sqrt(NROW(MDS1)),
	MDS1.upper=MDS1.mean + MDS1.sd/sqrt(NROW(MDS1)),

	MDS2.mean=mean(MDS2),
	MDS2.sd=sd(MDS2),
	MDS2.lower=MDS2.mean - MDS2.sd/sqrt(NROW(MDS2)),
	MDS2.upper=MDS2.mean + MDS2.sd/sqrt(NROW(MDS2))
)
df.summary$group=factor(df.summary$group, levels=c("SG","SE","MG","ME","LG","LE"))
write.table(df.summary,file=output,quote = F,row.names=F, sep = "\t",col.names = T)
plot <- ggplot(df.summary,aes(x=MDS1.mean,y=MDS2.mean)) + 
geom_point(aes(color=group),size=8)+
geom_errorbarh(aes(xmin=MDS1.lower,xmax=MDS1.upper)) +
geom_errorbar(aes(ymin=MDS2.lower,ymax=MDS2.upper))+
#geom_text(aes(label=group),size=5,family="Arial",hjust=0.5,vjust=-0.8)+
xlab(paste("MDS1")) + ylab(paste("MDS2"))+
theme(text=element_text(family="Arial",size=18))+
#geom_vline(aes(x=0,y=0),linetype="dotted")+
#geom_hline(aes(x=0,y=0),linetype="dotted")+
geom_text(aes(x=max(MDS1.upper),y=max(MDS2.upper)),hjust=1,vjust=0,size=5,label= paste("Stress = ",stress),colour="black")+
theme(panel.background = element_rect(fill='white', colour='black'), panel.grid=element_blank(),
	axis.title = element_text(color='black',family="Arial",size=18),
axis.ticks = element_line(color='black'), axis.line = element_line(colour = "black"),
axis.title.x=element_text(colour='black', size=18),axis.title.y=element_text(colour='black', size=18),axis.text=element_text(colour='black',size=18),
legend.title=element_blank(),legend.text=element_text(family="Arial", size=18),legend.key=element_blank(),
plot.title = element_text(size=20,colour = "black",face = "bold"))

cairo_pdf(filename=pdf,height=10,width=12)
plot
png(filename=png,res=600,height=5400,width=7200,type="cairo")
plot
dev.off()

args <- commandArgs(T)
if(length(args)<2){
    cat("usage: <pca.csv> <04.Diversity/group.list>  <pc1> <pc2> <output:pca.csv.summary.txt> <pca.pdf> <pca.png> \n")
    cat ("Example:  /PUBLIC/software/public/System/R-2.15.3/bin/Rscript plot.R pca.csv ../../group.list 14.67 12.95 pca.csv.summary.txt pca.pdf pca.png\n")
    quit("no")
}

pca.csv=args[1]
group.list=args[2]
data = read.table(file=pca.csv, head=T, na.strings=T,row.names=1,sep=",")
groups = read.table(file=group.list, head=F,na.strings=T,colClasses=c("character","character"))

pc1 <- args[3]
pc2 <- args[4]
output <- args[5]
pdf <- args[6]
png <- args[7]

group1=c()
for(i in 1:length(groups$V1)){
    Order=grep(paste0('^',rownames(data)[i],'$'),groups$V1,perl=T)
    group1[i]=groups$V2[Order]
}
group1=factor(group1,levels=unique(group1))

df = data.frame(rownames(data),data[,1],data[,2],group1)
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

plot <- ggplot(df.summary,aes(x=MDS1.mean,y=MDS2.mean)) +
geom_point(aes(color=group),size=8)+
geom_errorbarh(aes(xmin=MDS1.lower,xmax=MDS1.upper)) +
geom_errorbar(aes(ymin=MDS2.lower,ymax=MDS2.upper))+

xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
theme(text=element_text(family="Arial",size=18))+
#geom_vline(aes(x=0,y=0),linetype="dotted")+
#geom_hline(aes(x=0,y=0),linetype="dotted")+
#geom_text(aes(x=max(MDS1.upper),y=max(MDS2.upper)),hjust=1,vjust=0,size=5,label= paste("Stress = ",stress),colour="black")+

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

colnames(df.summary)<- c("group","pca1.mean","pca1.sd","pca1.lower","pca1.upper","pca2.mean","pca2.sd","pca2.lower","pca2.upper")
write.table(df.summary,file=output,quote = F,row.names=F, sep = "\t",col.names = T)


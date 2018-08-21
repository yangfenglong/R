args<-commandArgs(T)
if(length(args)>2){
  cat ("[usage:] <correlation.xls> <out.pdf>\n")
  cat ("Example: corrheatmap.R   correlation.xls  correlation.pdf \n")
  quit("no")
}

library(tidyverse)
cor=read.table(args[1],head=T,sep='\t',row.names=1)
plot<- as.data.frame(cor)

plot %>%
  rownames_to_column() %>% 
  gather(sample,abundance,-rowname) %>%
  separate(sample,c("kos","group"), sep = "_", remove = F) -> tidydata

pdf(args[2])
pic <- ggplot(tidydata,aes(x =sample, y = sqrt(abundance))) +
geom_col(aes(fill=rowname),position="stack")+
guides(fill= guide_legend(ncol = 2))+
theme(
  legend.position='none',
#  legend.title=element_blank(),
  axis.title.x= element_blank(),
  axis.text.x=element_text(size=8,angle=45,hjust=1))

pic
dev.off()

#ggsave(pic, file = "args[2]")


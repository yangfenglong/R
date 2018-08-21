#!/NJPROJ2/MICRO/PROJ/yangfenglong/software/miniconda3/lib/R/bin/R

#######################options########################
suppressPackageStartupMessages(library("optparse"))
option_list <- list(
  
  #input & output
  make_option("--gene_data", help="Gene Data accepts data matrices in tab- or comma-delimited format (txt or csv)"),
  make_option("--cpd_data", help="Compound Data accepts data matrices in tab- or comma-delimited format (txt or csv)"),
  make_option("--gene_id", default="kegg", help="ID type used for the Gene Data [default %default]"),
  make_option("--cpd_id", default="kegg", help="ID type used for the Compound Data [default %default]"),
  make_option("--species", default="ko", help="Either the KEGG code, scientific name or the common name of the target species [default %default]"),
  make_option("--pathview_id", default="00020", help="Alternatives: 'pearson' and 'kendall' [default %default]"),  
  make_option("--suffix", default="pathview", help="specifies the suffix to be added after the pathway name as part of the output graph file name [default %default]"),
  make_option("--kegg_dir", default="/NJPROJ2/MICRO/PROJ/yangfenglong/yanfa/pathview/maps/", help="dir of ko*.xml and ko*.png files [default %default]"),
  
  #graphics
  make_option("--native:", default="TRUE", help="specifies whether to render the pathway as native KEGG graph (.png) or using Graphviz layout engine (.pdf) [default %default]"),
  make_option("--same_layer", default="TRUE", help="if edge/node type legend be plotted in the same page  [Default %default]"),
  make_option("--discrete_gene", default="FALSE", help="whether Gene Data should be treated as discrete [Default %default]"),
  make_option("--discrete_cpd", default="FLASE", help="whether Compound Data should be treated as discrete [Default %default]"),
  make_option("--multistate", default="TRUE", help="specifies whether multiple states (samples or columns) Gene Data or Compound Data should be integrated and plotted in the same graph [Default %default, otherwise, gene or compound nodes will be sliced into multiple pieces corresponding to the number of states in the data]"),
  make_option("--cpd_offset", default="1.0", help="specifies how much compound labels should be put above the default position or node center [Default %default]"),
  make_option("--key_align", default="x", help="specifies how the color keys are aligned [Default %default]"),
  make_option("--key_pos", default="topleft", help="position of color key(s) [Default %default]"),
  make_option("--sign_pos", default="NA", help="position of pathview signature [Default %default]"),

  #coloration
  make_option("--node_sum", default="sum", help="The method name to calculate node summary given that multiple genes or compounds are mapped to it [default: sum, Alternative options: \"mean\", \"median\", \"max\", \"max.abs\" and \"random\"]"),
  make_option("--na_color", default="transparent", help="Color used for NA's or missing values in Gene Data and Compound Data [default: transparent, Alternative: grey]"),
  make_option("--low_gene", default="green", help="specify colors using common names (green, red etc), hex color codes (00FF00, D3D3D3 etc), or the color picker"),
  make_option("--mid_gene", default="gray", help=" Default spectra (low-mid-high) \"green-gray-red\""),
  make_option("--high_gene", default="red", help="specify the color "),
  make_option("--low_cpd", default="blue", help="specify colors using common names (green, red etc), hex color codes (00FF00, D3D3D3 etc), or the color picker"),
  make_option("--mid_cpd", default="gray", help="[Default blue-gray-yellow]"),
  make_option("--high_cpd", default="yellow", help="specify the color "),

  # node size and font size
  make_option("--afactor", default=1, help="for node size fine-tuning [Default %default]"),
  make_option("--text_width", default=15, help="specifying the line width for text wrap [Default %default]"),
  make_option("--cex", default=0.5, help="amount by which plotting text and symbols should be scaled relative to the default 1 [Default %default]")
)

opt <- parse_args(
  OptionParser(
  option_list=option_list,
  usage="/NJPROJ2/MICRO/PROJ/yangfenglong/software/miniconda3/lib/R/bin/Rscript %prog [options] \n",
  description = "Description\n
  Pathview is a tool set for pathway based data integration and
  visualization. It maps and renders user data on relevant pathway
  graphs. All users need is to supply their gene or compound data
  and specify the target pathway. 
  Pathview generates both native KEGG view and Graphviz views for
  pathways. keggview.native and keggview.graph are the two viewer
  functions, and pathview is the main function providing a unified
  interface to downloader, parser, mapper and viewer functions.\n", 
  epilogue = "Example: 
  /NJPROJ1/MICRO/share/software/R-3.3.3/bin/Rscript %prog \\
  --mat1 ko_diff_relative.xls --tans --mat2 phenotype.xls \\
  -m spearman --r 0.6 --p 0.05 --prefix ko_pheno\nReferences:\n
  Luo W, Pant G, Bhavnasi YK, Blanchard SG, Brouwer C. Pathview Web: user friendly pathway visualization and data integration. Nucleic Acids Res, 2017, Web Server issue, doi: 10.1093/ nar/gkx372
  Luo W, Brouwer C. Pathview: an R/Biocondutor package for pathway-based data integration and visualization. Bioinformatics, 2013, 29(14):1830-1831, doi: 10.1093/bioinformatics/btt285 
  You can alse use web_based api, upload your gene and compound data and download the zip result: https://pathview.uncc.edu/analysis\nOutput files:\n
  ko_pheno_qsigr.csv, ko_pheno_qsigp.csv
  \nAuthor(s): Yangfenglong\n"
))

prog <- sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])
if(is.null(opt$gene_data) and is.null(opt$cpd_data)){
  system(paste("Rscript ",prog," -h"))
  quit("no")
}

#######################################main script########################################################
library("pathview")

#gene data 
gdata<- read.csv("ko_pheno_qsigr.csv",head=T,quote="", row.names=1)
cpd<-read.csv("metab_pheno_qsigr.csv",head=T,quote="",row.names=1)

gdata<- read.csv(opt$gene_data,head=T,quote="",row.names=1)

# compoud data
cpd <- read.csv(opt$cpd_data,head=T,quote="",row.names=1)
cpd$id <- cpdname2kegg(row.names(cpd))[,2]
cdata <- cpd[which(cpd$id!="NA"),]
row.names(cdata) <- cdata$id
cdata <- cdata[,-ncol(cpd)]
write.table(cdata,file=paste("keggid_",opt$cpd,sep=""), row.names = TRUE, col.names = TRUE, quote=F)

# map gene and cpd on the kegg map
path_ids <- readLines("/NJPROJ2/MICRO/PROJ/yangfenglong/yanfa/pathview/maps/kegg_maps.list")
pv.out <- pathview(
  # input & output
  gene.data=gdata, 
  cpd.data=cdata,
  gene.idtype=opt$gene_id,
  cpd.idtype=opt$cpd_id,
  pathway.id=opt$pathview_id,
  species=opt$species, 
  out.suffix=opt$suffix,
  kegg.dir=opt$kegg_dir,

  # topology  of map 
  kegg.native=opt$native,
  same.layer=opt$same_layer,
  dsicrete=list(gene=opt$discrete_gene, cpd=opt$discrete_cpd),
  multi.state=opt$multistate,
  cpd.lab.offset=opt$cpd_offset,
  key.align=opt$key_align,
  key.pos=opt$key_pos,
  sign.pos=opt$sign_pos,

  # coloration
  node.sum=opt$node_sum,
  na.col=opt$na_color,
  low=list(gene=opt$low_gene, cpd=opt$low_cpd), 
  mid=list(gene=opt$mid_gene, cpd=opt$mid_cpd), 
  high=list(gene=opt$high_gene, cpd=opt$high_cpd),

  # other opts for " kegg.native= FALSE"
  afactor=opt$afactor,
  text.width=opt$text_width,
  cex=opt$cex
)
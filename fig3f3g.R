####################################################
############fig3F and 3G############################
########Author:songzhihong##########################
####################################################
library(reshape2)
markergenePlot<-function(combined,markgenes,col2){
UMAPPlot(combined,group.by="ANN",label=T,cols=col2,split.by="Pid")+NoAxes()+NoLegend()
  
vln.df=as.data.frame(combined[["integrated"]]@data[markgenes,])
vln.df$gene=rownames(vln.df)
vln.df=melt(vln.df,id="gene")
colnames(vln.df)[c(2,3)]=c("CB","exp")

combined@meta.data$CB<-rownames(combined@meta.data)
anno=combined@meta.data[,c("CB","ANN")]
vln.df=merge(vln.df,anno,by="CB")
vln.df$gene=factor(vln.df$gene,levels =markgenes)

p<-vln.df%>%ggplot(aes(ANN,exp))+geom_violin(aes(fill=ANN),scale = "width")+
  facet_grid(vln.df$gene~.,scales = "free_y")+
  #scale_fill_brewer(palette = "Set3",direction = 1)+
  scale_fill_manual(values = col2)+
  scale_x_discrete("")+scale_y_continuous("")+
  theme_void()+
  theme(
    axis.text.x.bottom = element_text(angle = 45,hjust = 1,vjust = 1),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
    legend.position = "none",axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    strip.background = element_blank(), strip.placement = "outside" #, colour = 'white', size = rel(2), linetype = 2
  )
print(p)
}

markergenePlot(combined,markgenes,col2)

###combined:seurat object file (version 4.0.1;with "ANN":cell type and "Pid":treatment)
###markgenes:vector(cell type marker genes)
###col2:vector(colors of cell types)
####################################################
############fig5A 5D################################
########Author:songzhihong##########################
####################################################
markerplot<-function(obj,genes,col2){
  data0<-obj@assays$integrated@data
  for(i in 1:length(genes)){
    type0<-data0[rownames(data0)==genes[i],]
    type<-as.character(obj@meta.data$ANN)
    type[type0==0]="Noexpress"
    obj@meta.data$tmp<-as.factor(type)
    cols<-c()
    cols["Noexpress"]="#dbebfa"
    colo1<-c(cols,col2)
    obj@meta.data$tmp<-factor(obj@meta.data$tmp,levels = names(colo1))
    
    p<-DimPlot(obj, label = FALSE, pt.size = 1,group.by="tmp",split.by = "Pid",cols = colo1)+labs(title = genes[i]) #, do.return=T,do.hover=T
    print(p)
  }
}

markerplot(PCobj,"Gch1",col2)
markerplot(PCobj,"Scd1",col2)
###PCobj:seurat object file (version 4.0.1;with "ANN":cell type and "Pid":treatment)
###col2:vector(colors of cell types)
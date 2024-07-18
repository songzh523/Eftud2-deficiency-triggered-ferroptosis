####################################################
############fig3K###################################
########Author:songzhihong##########################
####################################################
selfheatmap<-function(PCobj,ords,bk){
mat<-GetAssayData(PCobj,slot = "data")
cluster_info<-sort(PCobj$Pid)
mat<-as.matrix(mat[ords,names(cluster_info)])
ann_colors = list(Treatment = c(KO ="#0BBBB7", Control ="#FB7679"))
annotation_col = data.frame(
  Treatment = c(rep("Control",table(PCobj$Pid)[[1]]),rep("KO",table(PCobj$Pid)[[2]])))
rownames(annotation_col) = colnames(mat)

pheatmap::pheatmap(mat,cluster_cols = F,cluster_rows = F, annotation_col = annotation_col,annotation_colors = ann_colors,breaks = bk,show_colnames = F,scale = "row",gaps_col = table(PCobj$Pid)[[1]],border_color = NA,treeheight_row = 0,color = colorRampPalette(colors = c("blue","#E9EB12","red"))(100)) 
}
selfheatmap(PCobj,ords,bk)
###PCobj:seurat object file (version 4.0.1;with "ANN":cell type and "Pid":treatment)
###ords:vector(gene order)
###bk:vector(color label)


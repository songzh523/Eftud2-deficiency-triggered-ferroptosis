####################################################
############fig3A###################################
########Author:songzhihong##########################
####################################################
### volcano plot
vacanoPlot<-function(res,intgene){
res2 <- res[order(res$padj),] 
results = as.data.frame(dplyr::mutate(as.data.frame(res2), sig=ifelse(res2$padj<0.05 & res2$log2FoldChange > 0.5, "Up",ifelse(res2$padj<0.05 & res2$log2FoldChange < (-0.5), "Down","Other"))), row.names=rownames(res2))

s2<-results[rownames(results) %in% intgene,] 
shows<-s2
testinp0<-as.data.frame(table(results$sig))
testinp<-testinp0[testinp0$Var1 %in% c("Up","Down"),]
colnames(testinp)<-c("Significant","Number")
testinp1<-testinp[c(2,1),]

geom.text.size = 10
theme.size = geom.text.size*5/14

p = ggplot2::ggplot(results, ggplot2::aes(log2FoldChange, -log10(padj))) +ggplot2::xlim(-10,10)+ 
  ggplot2::geom_point(ggplot2::aes(col = sig),size=0.6) +theme_classic()+ theme(axis.text.x  = element_text(size = geom.text.size))+ theme(axis.text.y  = element_text(size = geom.text.size))+
  ggplot2::scale_color_manual(values = c("#38436C","gray","#A02622"))+ theme(legend.position="none",axis.title.x   = element_text(size = 6),axis.title.y  = element_text(size = geom.text.size))+labs(color="Significant")  
p0<-p +xlim(-2.5,2.5)+geom_vline(xintercept = c(0.5,-0.5),lty=2,col="gray",lwd=1)+geom_hline(yintercept = 1.30103,lty=2,col="gray",lwd=1)+ ggrepel::geom_text_repel(data=shows,min.segment.length = 0.1,max.overlaps = 6, ggplot2::aes(label=rownames(shows)),size = theme.size)+ theme(text = element_text(size = geom.text.size))
p1<-ggtexttable(testinp1, rows = NULL, theme = ttheme("light",base_size = geom.text.size))
print(p0 + annotation_custom(ggplotGrob(p1),xmin = 1.3, ymin = 20,xmax = 2))
}

vacanoPlot(res,intgene)
###res:data frame(row:gene;column:foldchange and adjusted p value)
###intgene:vector(genes want to show in the volcano plot)
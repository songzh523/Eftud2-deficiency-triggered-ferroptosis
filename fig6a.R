####################################################
############fig6A###################################
########Author:songzhihong##########################
####################################################
psiplots<-function(sigevent){
wtpsi<-unlist(strsplit(as.character(sigevent$IncLevel1),",",fixed = T))
kopsi<-unlist(strsplit(as.character(sigevent$IncLevel2),",",fixed = T))
df<-as.data.frame(cbind(wtpsi,kopsi))
colnames(df)<-c("Ctrl","Eftud2 cKO")
df$Control<-as.numeric(as.character(df$Control))
df$KO<-as.numeric(as.character(df$KO))
df1<-reshape2::melt(df)
sigres<-ks.test(df$Control, df$KO)
my_comparisons<-list(c("Ctrl","Eftud2 cKO"))
p<-ggboxplot(df1,"variable","value",fill="variable",xlab="",ylab = "PSI")+theme(legend.position="none")
p+stat_compare_means(comparisons = my_comparisons,label = "p.signif")
}

psiplots(sigevent)
###sigevent:data frame(row:event;column:IncLevel1 and IncLevel2).result from rMATS

####################################################
############fig6C###################################
########Author:songzhihong##########################
####################################################
library(VennDiagram)
ventwoplots<-function(a,b,anam,bnam){
  grid.newpage()
  x<-list(a=a,b=b)
  names(x)<-c(anam, bnam)
  venn.plot <- venn.diagram(x,cat.col=c("red","blue"),col=c("red","blue"),filename = NULL)
  grid.draw(venn.plot)
}

ventwoplots(wtevents,koevents,"Ctrl","Eftud2 cKO")
###wtevents:vector(exon skipping events in Ctrl)
###koevents:vector(exon skipping events in Eftud2 cKO)
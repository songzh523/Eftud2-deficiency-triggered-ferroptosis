####################################################
############fig6B###################################
########Author:songzhihong##########################
####################################################
stackplot<-function(percents,fac,mainlab,colcelltype){ 
  inp<-as.data.frame(percents)
  inp$Var1<-factor(inp$Var1,levels = fac)
  ggplot(inp,aes(x=Var2,y=Freq,fill=Var1))+labs(title = mainlab)+geom_bar(stat = "identity",colour=NA,width=0.5,position='stack')+labs(x="",y="AS events proportion")+theme_classic()+  theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14),axis.text.x =element_text(size=10), axis.text.y=element_text(size=10)) +scale_fill_manual(values =colcelltype,name="AS events")+annotate("text",x=1.5,y=75,label=paste("p =",round(0.04947,3)),size=3)
}

stackplot(percents,ord,"significant AS event",brewer.pal(5, "Set3"))
###percents:data frame(row:id;column:Var1(event:factor),Var2(treatment:factor)).
###ord:vector(event factor level)

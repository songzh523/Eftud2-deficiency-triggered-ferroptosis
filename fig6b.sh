####################################################
############fig6B###################################
########Author:heyuanlin############################
####################################################
isoseq collapse  bamfile  collapsed.gff  | tee log.txt &

gffread collapsed.gff -T -o collapsed.gtf &

rmats.py --b1 b1.txt --gtf collapsed.gtf --statoff -t single --variable-read-length --readLength 600 --nthread 20 --od output1 --tmp tmp_output1 &

###R script###
library(ggthemes)
library(tidyverse)
library(magrittr)
library(ggplot2)

psi.as%>% group_by(EventType,group) %>% summarise(count=sum(TotalEventsJCEC)) %>% group_by(group) %>% mutate(sum=sum(count),percent=count/sum) %>% ggplot(aes(x=group,y=percent,fill=EventType))+geom_col(position = 'stack')+geom_text(aes(label=scales::percent(percent,0.01)),size=3,color='black',position = position_stack(vjust = 0.5))+scale_fill_npg()+theme_classic()

###psi.as:data frame(output file from rmats["summary.txt"],with group(treatment),EventType,TotalEventsJCEC

setwd("C:/Users/hrlexd/Dropbox/Otago_2023 (1)/Ben_hairworm/Jeff_work/RNA_extractions_2024/")
library(tidyverse)
single<-read.table('single_infection_worm_lengths_ratio.txt', header =T, row.names=NULL,sep='\t')
dual<-read.table('dual_infection_worm_lengths_ratio.txt',header=T, row.names=NULL,sep='\t')

ratios<-rbind(ratios,dual %>% filter(lineage=='C534') %>% mutate(ratio=length_mm/66) %>% select(ratio))
ratios<-rbind(ratios,dual %>% filter(lineage=='C534') %>% mutate(ratio=length_mm/98) %>% select(ratio))

single %>% slice(-1:-2) %>% head()
single %>% slice(-1:-1) %>% head()
single %>% head()


#shift hashes to switch between dual and single
list_worms<-unique(dual$lineage)
#list_worms<-unique(single$lineage)
ratios<-NULL

for (item in list_worms){
  count<-0
  worm_lin<-dual %>% filter(lineage==item) 
  #worm_lin<-single %>% filter(lineage==item) 
  for (item2 in worm_lin$length_mm){
  count=count+1
  ratios<-rbind(ratios,worm_lin %>% slice(-1:-count) %>% mutate(ratio=length_mm/item2) %>% mutate(comp=lineage) %>% mutate(length1=item2) %>% mutate(length2=length_mm)%>% select(ratio,comp,length1,length2))
  }
}


p <- ggplot(ratios, aes(x=comp, y=ratio,color=comp)) + 
  geom_boxplot() + theme_bw()
p
p<-p+ geom_jitter(shape=16, position=position_jitter(0.2))

p_dual<-p
p_single<-p

p_dual + theme(legend.position="none") + xlab("Worm")+ ylab("Ratio")
p_single+ theme(legend.position="none")+ xlab("Worm")+ ylab("Ratio")

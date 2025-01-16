#R script
#eddy dowle May 2024
#hairworm relatedness analyses


#want to plot 
#R0 vs R1 and kingrobust vs R1
#following the ryan waples paper https://onlinelibrary.wiley.com/doi/full/10.1111/mec.14954
#and Helen's paper has a really nice picture in the supplementary breaking down the different relationship guides

#table from angsd + ngsRelate
setwd("C:/Users/hrlexd/Dropbox/Otago_2023 (1)/Hairworm/GBS_2023/Hairworm_GBS_2024")

library(tidyverse)
library(ggrepel)
ngsrelate_out<-read.table('newres.cleanKING.txt', header =T, row.names=NULL,sep='\t')
samplelist<-read.table('Samplelist.txt',header=T, row.names=NULL,sep='\t')

ngsrelate_out_filter<-ngsrelate_out %>% filter( nSites>= 400)
#these were dud samples but just putting a general filter on 
#ngsrelate_out_filter<-ngsrelate_out_filter %>% filter(!a==7) %>% filter(!a==8) %>% filter(!b==7) %>% filter(!b==8) %>% 
#filter(!a==16) %>% filter(!b==16) %>% filter(!a==21) %>% filter(!b==21)

ggplot(ngsrelate_out_filter, aes(x=R1, y=KING)) +
  geom_point()

ngsrelate_out_filter<-ngsrelate_out_filter %>% mutate(name=paste0(a,b))

#better way to filter
ngsrelate_out_conspecific<-ngsrelate_out_filter %>% filter(a %in% 0:5 & b %in% 1:6|a==7 & b==8|a==11 &b==12|a==13&b==14|a %in% 18:19 & b %in% 19:20|a==35&b==36|a==37&b==38|a==41&b==42|a %in% 43:50 & b %in% 44:51)

#comparisons of interest
#HW14 0:6
#HW17 7:8
#HW24 11:12
#HW29 13:14
#HW_ch11 18:20
#HW_w133 35:36
#HW_w134 37:38
#HW_w41:42
#HW_w58 43:51


ggplot(ngsrelate_out_conspecific, aes(x=R1, y=KING)) +
  geom_point()

ngsrelate_out_filter<-ngsrelate_out_filter %>% mutate(colouring=case_when(name %in% ngsrelate_out_conspecific$name ~ 'firebrick2', !name %in% ngsrelate_out_conspecific$name ~'black' ))

ngsrelate_out_filter<-ngsrelate_out_filter %>% mutate(conspecific=case_when(name %in% ngsrelate_out_conspecific$name ~ 'Conspecific', !name %in% ngsrelate_out_conspecific$name ~'General population' ))

ngsrelate_out_filter<-ngsrelate_out_filter %>% arrange(colouring)
ggplot(ngsrelate_out_filter, aes(x=R1, y=KING)) +
  geom_point(size=4,colour=ngsrelate_out_filter$colouring)+
  theme_bw()+
  geom_hline(yintercept=0.25, linetype="dashed", 
               color = "magenta1", size=1) +
  geom_hline(yintercept=0.125, linetype="dashed", 
             color = "blueviolet", size=1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "dodgerblue", size=1)
#from waples 2019
#Thus using the values in table S1 this means that the expected KING-robust kinship estimate is 1/2
#for MZ, 1/4 for both PO pairs and full siblings, 1/8 for HS, 1/ 16 for C1, 1/64 for C2 and 0 for unrelated pair

#otheroption to just plot the king-robust values

ggplot(ngsrelate_out_filter,aes(y=KING,x=conspecific))+
  geom_boxplot(aes(fill=conspecific))+
  theme_bw()+
geom_hline(yintercept=0.25, linetype="dashed", 
           color = "magenta1", size=1) +
  geom_hline(yintercept=0.125, linetype="dashed", 
             color = "blueviolet", size=1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "dodgerblue", size=1)+
  scale_fill_manual(values=c("firebrick1", "cyan3"))+
  labs(x="Worm type",y='KING-robust')
  
#plots on worm size

size_info_worms<-read.csv('comparisonsofinterest.csv', header =T, row.names=NULL)

#size range of mature worms
size_info_worms_mature<-size_info_worms %>% filter(WormMaturity=='Mature')

#this is all including those collected from Cass that could theoretically be from different hosts
ggplot(size_info_worms_mature %>% filter(!is.na(Size)),aes(y=Size,x=Sex))+
  geom_boxplot(aes(fill=Sex))+
  theme_bw()+
scale_fill_manual(values=c("firebrick1", "cyan3"))+
  labs(x="Sex",y='Size (mm)')

#just worms known to come from cave weta
ggplot(size_info_worms_mature %>% filter(!is.na(Size)) %>% filter(Worms_collected_for!='Jeff_freelivingcass'),aes(y=Size,x=Sex))+
  geom_boxplot(aes(fill=Sex))+
  theme_bw()+
  scale_fill_manual(values=c("firebrick1", "cyan3"))+
  labs(x="Sex",y='Size (mm)')
#only includes four males and 5 females

#ideally I'd do something like this but we dont have the samples
size_info_worms_mature<-size_info_worms_mature %>% mutate(conspecific_name=case_when(dual=='N' ~ 'Single infection',dual=='Y' ~'Multiple infection'))

ggplot(size_info_worms_mature %>% filter(!is.na(Size)) %>% filter(Worms_collected_for!='Jeff_freelivingcass'),aes(y=Size,x=Sex))+
  geom_boxplot(aes(fill=Sex))+
  theme_bw()+
  scale_fill_manual(values=c("firebrick1", "cyan3"))+
  labs(x="Infection type",y='Size (mm)')+
  facet_wrap(~conspecific_name)

#but yeah not enough data


#plots on genome repeat content
repeats<-read.table('TE_classification proportion.txt', header=T, row.names=NULL,sep='\t')
ggplot(repeats, aes(x = "", fill = factor(TE_classification))) +
  geom_bar(stat= "count", width = 1, color = "white") +
  geom_text(aes(label = proportion), stat = 'count', position = position_stack(vjust = .5)) +
  coord_polar("y", start = 0, direction = -1) +
 # scale_fill_manual(values = c("#00BA38", "#619CFF", "#F8766D")) +
  theme_void()

repeats_2 <- repeats %>% 
  arrange((proportion)) %>%
  mutate(prop =proportion *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop, 
         SubSegment = factor(TE_classification, levels=TE_classification[order(-(prop))], ordered=TRUE))

repeats_2[,'prop']=round(repeats_2[,'prop'],1)


gb2020label <- 
  repeats %>% 
  mutate(prop =proportion *100) %>%
  arrange((proportion)) %>% ## arrange in the order of the legend
  mutate(text_y = cumsum(prop) - prop/2,
         SubSegment = factor(TE_classification, levels=TE_classification[order(-(prop))], ordered=TRUE)) ### calculate where to place the text labels

gb2020label[,'prop']=round(gb2020label[,'prop'],1)

ggplot(repeats_2, aes(x="", y=prop, fill=SubSegment)) +
  geom_bar(stat="identity", width=1,color='white',alpha=0.8) +
  coord_polar("y", start=0) +
  theme_void() +
#  theme(legend.position="none") +
#  geom_text(aes(y = ypos, label = prop), size=3) +
  scale_fill_brewer(palette="Set1")# +
#  geom_label_repel(aes(label = labels, y = text_y), 
                #   nudge_x = 0.6, nudge_y = 0.6,
                #   size = 5, show.legend = F) +
  
  ggplot(gb2020label, aes(x="", y=prop, fill=SubSegment)) +
  geom_bar(stat="identity", width=1,color='white',alpha=0.8) +
  coord_polar("y", start=0) +
  theme_void() +
    scale_fill_brewer(palette="Set1") +
  #theme(legend.position="none") +
    geom_label_repel(aes(label = prop, y = text_y), 
                     force=2,nudge_x = 0.8, nudge_y = 0.8,
                     size = 7, show.legend = T) +
    guides(fill=guide_legend(title="TE"))
  
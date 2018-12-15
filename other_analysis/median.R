
# Compare medians of our data with others

moore.df = data.frame(Estrogen = estrogen_names[1:15], 'Moore.median'= c(2.04,0.42,0.22,0.05,0.02,1.62,0.09,0.38,0.36,0.16,0.49,0.12,1.83,0.53,0.2  ))
sampson.df = data.frame(Estrogen = estrogen_names[1:15], 'Sampson.median'= c(5.8,1.2,0.7,0.1,0.1,4.9,0.3,1.2,1.1,0.5,1.5,0.4,5.5,1.6,0.6))

a = df.s %>% select(-s.Total)
colnames(a) = estrogen_names[1:15]
a = a %>% 
  gather(Estrogen,Concentration) %>% 
  inner_join(estrogen_names_df,by="Estrogen") %>%
  group_by(Estrogen,Nomenclature) %>% summarize('Constanza.et.al.2018' = round(median(Concentration,na.rm=TRUE),2)) %>% 
  inner_join(moore.df,by="Estrogen") %>% inner_join(sampson.df,by="Estrogen") %>% 
  mutate(moore.fold = paste0("(",round((Moore.median/Constanza.et.al.2018)*100,0),"%",")") ,
         sampson.fold = paste0("(",round((Sampson.median/Constanza.et.al.2018)*100,0),"%",")"),
         moore.diff =   paste0("[",round(Moore.median - Constanza.et.al.2018,2),"]"),
         sampson.diff =   paste0("[",round(Sampson.median - Constanza.et.al.2018,2),"]"),
         Moore.et.al.2016 = paste(Moore.median,moore.fold,moore.diff),
         Sampson.et.al.2016 = paste(Sampson.median,sampson.fold,sampson.diff)
         ) %>%
  select(-c(moore.fold,sampson.fold,moore.diff,sampson.diff,Moore.median,Sampson.median)  ) %>%
  rename('Moore et al. 2016 (% Difference) [Absolute Difference]' = Moore.et.al.2016,
         'Sampson et al. 2016 (% Difference) [Absolute Difference]' = Sampson.et.al.2016,
         'Constanza et al.' = Constanza.et.al.2018)



sjPlot::tab_df(as.data.frame(a),show.footnote=TRUE,
               title="Table 1: Median estrogen metabolite concentrations for postmenopausal women reported for each study",
               footnote="Note: Units in pmol/mg creatinine.")





#write.xlsx(a,"C:/Users/zhaoi2/Desktop/units_check_version2.xlsx" , sheetName="Sheet1", col.names=TRUE, row.names=FALSE, showNA=FALSE)

############################################################################################################################
# boxplots for all korea snu
setwd("C:/Users/zhaoi2/Desktop")
a = as.data.frame(read_xlsx("korea_snu.xlsx"), na="")

b = a %>% group_by(batch.num) %>% summarize(n=n(), n_cases=sum(as.numeric(case)))

c = a %>% gather(Estrogen,Concentration,7:21) %>% group_by(batch.num, Estrogen) %>% summarize(median=median(Concentration,na.rm=TRUE))

d = a %>%  gather(Estrogen,Concentration,7:21) %>% mutate(batch.num = as.factor(batch.num))

my_comparisons <- list( c("9", "10"), c("10","11"),c("11","12"), 
                        c("13","14"),c("14","15"),c("15","16"),
                        c("9","11"),c("9","12"), c("10","12"),
                        c("13","15"),c("13","16"),c("14","16")
                        )

ggplot(data = d, aes(x=batch.num,y=Concentration)) +
  geom_boxplot(color="black") +
 stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black")+
  theme_classic()+
  #stat_compare_means(comparisons = my_comparisons, method="wilcox.test")+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~ Estrogen, scales="free_y",ncol=5) +
  theme(legend.position="none")+
  geom_vline(aes(xintercept=4.5), linetype="dotted")+
  ggtitle("Batch Number vs. Concentration distribution for Korea (SNU)")+
  xlab("Batch #")+
  ylab("Concentration (pg)")

#####################################################################

#Get median for all estrogens per batch

qc.df = as.data.frame(read_xlsx("./data/qc_analysis.xlsx",na="")) %>% 
  #as.data.frame(read_xlsx("./data/all_controls.xlsx",na="")) %>% 
  filter((ID %in% c("A","B","D","E"))) %>%
  mutate(batch.num=as.character(batch.num)) %>%
  gather(Estrogen,Concentration,estrogen_names) %>%
  mutate(batch.num = as.numeric(batch.num))

ggplot(qc.df,aes(x=batch.num,y=Concentration)) +
  geom_point(aes(color=ID),size=4) +
  #geom_line(aes(color=ID),size=2) +
  stat_smooth(geom="smooth",aes(color=ID),size=1.5, se = FALSE, span=0.3,alpha=0.7) +
  stat_smooth(data=data.frame(qc.df %>% filter(Estrogen=="4OHE1")),geom="smooth",aes(color=ID),size=1.5, se = FALSE, span=0.6,alpha=0.7      )+
  #scale_shape_manual(values=1:nlevels(qc.df$batch.num))+
  theme_classic() +  
  #scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10()+
  ylab("Concentration (pg)")+
  xlab("Batch #")+ 
  labs(color='ID') +
  labs(shape='ID') + 
  scale_x_continuous(breaks=seq(1:16))+
  geom_vline(data = data.frame(Estrogen= sort(rep(unique(qc.df$Estrogen),length(unique(qc.df$batch.num)))), 
                               id.num=rep(1:length(unique(qc.df$batch.num)),length(unique(qc.df$Estrogen)))),
             aes(xintercept=id.num),color="grey10",linetype="dotted")+
  geom_hline(data = data.frame(qc.df %>% group_by(Estrogen,ID) %>% summarize(median = median(Concentration,na.rm=TRUE))), aes(yintercept=median, color=ID), size=1.5,linetype="dashed"   )+
  #geom_text_repel(data=subset(qc_outliers, abs(log.sd.above.mean)>=1.96), aes(x=ID,y=Concentration,label=customer.id))+
  facet_wrap(~ Estrogen,scales="free",ncol=5) 
##################################################################
# number of cases for each batch

a = df %>% select(origin,batch.num,case) %>% mutate(case=as.numeric(case)-1)
b = a %>% group_by(origin,batch.num) %>% summarize(n=n(), n_cases=sum(as.numeric(case),na.rm=TRUE), pct_cases = round(n_cases/n,2)) %>% arrange(batch.num)

sjPlot::tab_df(as.data.frame(b),
               title="Percent of cases in each batch")

##############################################################################
# make dataset for all korea snu with hospital
setwd("C:/Users/zhaoi2/Desktop")
a = as.data.frame(read_xlsx("korea_snu_all.xlsx"), na="") 
b = a %>% select(batch.num,hosp,case) %>% group_by(batch.num,hosp) %>% summarize(n=n())
c = a %>% select(batch.num,hosp,case) %>% group_by(batch.num) %>% summarize(total_n=n())
d=full_join(b,c) %>% mutate(pct = round(n/total_n,2)) 
#%>% arrange(hosp,desc(n))
sjPlot::tab_df(as.data.frame(d),show.footnote=TRUE,
               title="Table 1: Distribution of hospitals among batches",
               footnote="pct = proportion of individuals in batch from hospital")
e = a %>% select(batch.num,hosp,case) %>% group_by(hosp,case) %>% summarize(n=n())
sjPlot::tab_df(as.data.frame(e))


##############################################################################
# boxplot of korea snu and korea kmcc
a = df %>% filter(origin %in% c("Korea (KMCC)","Korea (SNU)")) %>% filter(batch.num != 2) %>%
  select(batch.num,origin,customer.id,estrogen_names) %>%
  gather(Estrogen,Concentration,estrogen_names) %>% 
  mutate(batch.num = factor(as.factor(batch.num),levels = as.character(c(1,9,10,11,12,3,13,14,15,16))))

my_comparisons <- list( c("1","9"),c("9", "10"), c("10","11"),c("11","12"), 
                        c("3","13"),c("13","14"),c("14","15"),c("15","16"))
                       # c("9","11"),c("9","12"), c("10","12"),
                        #c("13","15"),c("13","16"),c("14","16")


ggplot(data = a, aes(x=batch.num,y=Concentration)) +
  geom_boxplot(aes(color=origin))+ 
  #  color="black") +
  #stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "black")+
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, aes(color =origin))+
  theme_classic()+
  stat_compare_means(comparisons = my_comparisons, method="wilcox.test", label.y=4.5, label= "p.signif")+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~ Estrogen, scales="free",ncol=5) +
  theme(legend.position="none")+
  geom_vline(aes(xintercept=5.5), linetype="dotted")+
  xlab("Batch #")+
  ylab("Concentration (pg)")
  #annotate(x=-Inf, y =Inf ,  geom='text', label = "Mostly Cases", size=4, hjust=-0.2,vjust=1)+
  #annotate(x=Inf, y =Inf ,  geom='text', label = "Mostly Controls", size=4, hjust=1,vjust=1)


##################################################################################
# chart of estrogens

qc.df = as.data.frame(read_xlsx("./data/qc_analysis.xlsx",na="")) %>%
  #as.data.frame(read_xlsx("./data/all_controls.xlsx",na="")) %>% 
  filter(!(ID %in% c("A","B","D","E"))) %>%
  #select(-origin,-sample.id,-volume,-injection,-"#") %>%
  mutate(batch.num=as.character(batch.num)) %>%
  gather(Estrogen,Concentration,estrogen_names) %>%
  mutate(batch.num = factor(as.factor(batch.num),levels = as.character(seq(1:16)))) 
qc.df$Concentration[qc.df$Concentration<=0] = 0.01

qc.df = qc.df %>% mutate(Concentration = log(Concentration)) %>% group_by(Estrogen) %>% 
                           summarise(var.error =    round((attr(VarCorr(lmer(Concentration ~ 1+ (1|ID   ))), "sc"))^2,2),
                                     var.resid =   round(VarCorr(lmer(Concentration ~ 1+ (1|ID   )))$ID[1],2),
                                     cv = round(sqrt(var.resid)/mean(Concentration,na.rm=TRUE),2),
                                     icc = round(var.resid/(var.resid+var.error),2),
                                     max = max(Concentration,na.rm=TRUE) ) %>%

  mutate(CV = paste0(round(100*cv,2),"%")  ,
         ICC = paste0(round(100*icc,2),"%")) %>%
select(Estrogen,CV,ICC) 
#qc.stats = as.data.frame(qc.df 
                        


a = df.s 
colnames(a) = estrogen_names
b = a %>% 
  gather(Estrogen,Concentration,estrogen_names) %>% 
  inner_join(estrogen_names_df,by="Estrogen") %>%
  mutate(Estrogen = factor(as.factor(Estrogen),levels = estrogen_names)) %>%
  group_by(Estrogen,Nomenclature) %>% 
  summarize(Median = round(median(Concentration,na.rm=TRUE),2),
            Q25 =  round(quantile(Concentration, probs=0.25, na.rm=TRUE, names=FALSE),2),
            Q75 =  round(quantile(Concentration, probs=0.75, na.rm=TRUE, names=FALSE),2)
  ) %>%
  mutate(IQR = paste0("(",Q25,", ",Q75,")"  )   ) %>%
  select(-Q25,-Q75) %>%
  inner_join(qc.df,by="Estrogen") %>%
rename('Code Name' = Estrogen)


sjPlot::tab_df(as.data.frame(b %>% select('Code Name',Nomenclature, Median, IQR,CV,ICC )),alternate.rows=TRUE,show.footnote=TRUE,
               footnote = "Note: Units in pmol/mg creatinine")



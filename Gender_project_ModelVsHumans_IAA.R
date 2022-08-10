Model_vs_human_data_complete_28pp_NA_removed <- read.csv("~/Desktop/Model_vs_human_data_complete_28pp_NA_removed.csv", sep=";")

Model_vs_human_data_complete_28pp <- read.csv("/Users/nataliaresende/Dropbox/Gender_Model_project_tensorflow/Model_vs_human_data_complete_28pp.csv", sep=";")
dtNA=Model_vs_human_data_complete_28pp_NA_removed
dta=Model_vs_human_data_complete_28pp



View(dt)

library(tidyverse)

names(dt)[names(dt) == "item_pp"] <- "word_pp"
names(dtNA)[names(dt) == "item_pp"] <- "word_pp"
names(dt)

dt$simulation=as.factor(dt$simulation)
dt$item=as.factor(dt$item)
dt$word=as.factor(dt$word)
dt$label=as.numeric(dt$label)
dt$categories=as.factor(dt$categories)
dt$ending=as.factor(dt$ending)
dt$easy_diff=as.factor(dt$easy_diff)
dt$word_pp=as.factor(dt$word_pp)
dt$label_pp=as.numeric(dt$label_pp)
dt$categories_pp=as.factor(dt$categories_pp)
dt$ending_pp=as.factor(dt$ending_pp)
dt$easy_dif_pp=as.factor(dt$easy_dif_pp)



View(dt)
attach(dt)


library(irr)
library(ggplot2)
library(plyr)
library(broom)
library(tibble)
#Regular 

dt$label <- as.factor(ifelse(dt$label == '1', 'Fem', ifelse(dt$label == '0', 'Masc', NA)))
View(dt$label)
dt$label_pp <- as.factor(ifelse(dt$label_pp == '1', 'Fem', ifelse(dt$label_pp == '0', 'Masc', NA)))
View(dt$label_pp)

dtNA$label <- as.factor(ifelse(dtNA$label == '1', 'Fem', ifelse(dtNA$label == '0', 'Masc', NA)))
View(dtNA$label)
dtNA$label_pp <- as.factor(ifelse(dtNA$label_pp == '1', 'Fem', ifelse(dtNA$label_pp == '0', 'Masc', NA)))
View(dtNA$label_pp)
par(mfrow=c(1,2))
attach(dt)
ggplot(dt, aes(x= ending,  group=label)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", x='Form Categories', fill="Form categories") +
  facet_grid(~label) +
  scale_y_continuous(labels = scales::percent)

attach(dtNA)
ggplot(dtNA, aes(x= ending_pp,  group=label_pp)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", x='Form Categories', fill="Form categories") +
  facet_grid(~label_pp) + scale_y_continuous(labels = scales::percent)
  
  


#Model's and subjects' answers for regular nouns
Model_answers_reg=subset(dt, categories=="Reg", select=c(label))
Subj_answers_reg=subset(dt, categories_pp=="Reg", select=c(label_pp))

#table for model's and subject comparison regular nouns
Table_answers_Reg_ModelVsHumans=cbind(Model_answers_reg, Subj_answers_reg)
View(Table_answers_Reg_ModelVsHumans)
Table_answers_Reg_ModelVsHumans
#kappa
kappa2(Table_answers_Reg_ModelVsHumans, 'unweighted') #kappa= 0.00149 z = 0.312 and p-value = 0.975
kappa2(Table_answers_Reg_ModelVsHumans, 'equal') #kappa= 0.00149 z = 0.312 and p-value = 0.975

#Transp/fem

#Model's and subjects' answers for transparent feminine nouns
Model_answers_Transp_fem = subset(dt, categories == "TrFem", select=c(label)) 
Subj_answers_Transp_fem=subset(dt, categories_pp =="Transp/fem", select=c(label_pp)) 

#table for model's and subject comparison transparent feminine nouns
Table_answers_Transp_fem_ModelVsHumans=cbind(Model_answers_Transp_fem, Subj_answers_Transp_fem)
View(Table_answers_Transp_fem_ModelVsHumans)

#Kappa
kappa2(Table_answers_Transp_fem_ModelVsHumans, 'unweighted') #kappa=-0.0603 z = -1.17 p-value = 0.244
kappa2(Table_answers_Transp_fem_ModelVsHumans, 'equal') #kappa=-0.0603 z = -1.17 p-value = 0.244

#Transp/Irr

#Model's and subjects' answers for transparent feminine nouns
Model_answers_Transp_irr = subset(dt, categories=="Transp/Irr", select=c(label))
Subj_answers_Transp_irr=subset(dt, categories_pp=="Transp/Irr", select=c(label_pp))

#table for model's and subject comparison transparent feminine nouns
Table_answers_Transp_irr_ModelVsHumans=cbind(Model_answers_Transp_irr, Subj_answers_Transp_irr)
View(Table_answers_Transp_irr_ModelVsHumans)

#Kappa
kappa2(Table_answers_Transp_irr_ModelVsHumans, 'unweighted') #Kappa = -0.0959 z = -1.75 p-value = 0.0801 
kappa2(Table_answers_Transp_irr_ModelVsHumans, 'equal') #Kappa = -0.0959 z = -1.75 p-value = 0.0801

#Transp/Masc

#Model's and subjects' answers for transparent feminine nouns
Model_answers_Transp_masc = subset(dt, categories=="Transp/Masc", select=c(label))
Subj_answers_Transp_masc=subset(dt, categories_pp=="Transp/Masc", select=c(label_pp))

#table for model's and subject comparison transparent feminine nouns
Table_answers_Transp_masc_ModelVsHumans=cbind(Model_answers_Transp_masc, Subj_answers_Transp_masc)
View(TTable_answers_Transp_masc_ModelVsHumans)

#Kappa
kappa2(Table_answers_Transp_masc_ModelVsHumans, 'unweighted') #Kappa = -0.026 z = -0.74 p-value = 0.459
kappa2(Table_answers_Transp_masc_ModelVsHumans, 'equal') #Kappa = -0.026 z = -0.74 p-value = 0.459


#tables

Model_answers_a=subset(dt, ending=='-a' & easy_diff=="difficult", select=c(label))
table_a=table(Model_answers_a)
table_a #56 (0) e 28(1)
prop.table(table_a)*100

Model_answers_a=subset(dt, ending=='-a' & easy_diff=='easy', select=c(label))
table_a=table(Model_answers_a)
table_a #56 (0) e 28(1)
prop.table(table_a)*100 #56 (0) e 28(1)


Model_answers_o=subset(dt, ending=='-o' & easy_diff=='easy', select=c(label))
table_o=table(Model_answers_o)
table_o #56 e 0
prop.table(table_o)*100

Model_answers_o=subset(dt, ending=='-o' & easy_diff=='difficult', select=c(label))
table_o=table(Model_answers_o)
table_o #84 e 28
prop.table(table_o)

  Model_answers_agem=subset(dt, ending=='-agem' & easy_diff=='difficult', select=c(label))
  table_agem=table(Model_answers_agem)
  table_agem #56 28
  prop.table(table_agem)*100

Model_answers_agem=subset(dt, ending=='-agem' & easy_diff=='easy', select=c(label))
table_agem=table(Model_answers_agem)
table_agem #28 e 56
prop.table(table_agem)*100

Model_answers_ade=subset(dt, ending=='-ade' & easy_diff=='difficult', select=c(label))
table_ade=table(Model_answers_ade)
table_ade #0 e 84
prop.table(table_ade)*100

Model_answers_ade=subset(dt, ending=='-ade' & easy_diff=='easy', select=c(label))
table_ade=table(Model_answers_ade)
table_ade #56 e 28
prop.table(table_ade)*100

Model_answers_ume=subset(dt, ending=='-ume' & easy_diff=='difficult', select=c(label))
table_ume=table(Model_answers_ume)
table_ume #28 e 28
prop.table(table_ume)*100

Model_answers_ume=subset(dt, ending=='-ume' & easy_diff=='easy', select=c(label))
table_ume=table(Model_answers_ume)
table_ume #0 e 112
prop.table(table_ume)*100

Model_answers_or=subset(dt, ending=='-or' & easy_diff=='difficult', select=c(label))
table_or=table(Model_answers_or)
table_or #28 e 56
prop.table(table_or)*100

Model_answers_or=subset(dt, ending=='-or' & easy_diff=='easy', select=c(label))
table_or=table(Model_answers_or)
table_or #56 e 28
prop.table(table_or)*100

Model_answers_ema=subset(dt, ending=='-ema' & easy_diff=='difficult', select=c(label))
table_ema=table(Model_answers_ema)
table_ema 
prop.table(table_ema)*100

Model_answers_ema=subset(dt, ending=='-ema' & easy_diff=='easy', select=c(label))
table_ema=table(Model_answers_ema)
table_ema 
prop.table(table_ema)*100


Model_answers_ao=subset(dt, ending=='-??o' & easy_diff=='easy', select=c(label))
table_ao=table(Model_answers_ao)
table_ao 
prop.table(table_ao)*100

Model_answers_ao=subset(dt, ending=='-o' & easy_diff=='difficult', select=c(label))
table_ao=table(Model_answers_ao)
table_ao 
prop.table(table_ao)*100

#kappa to check agreement for each noun ending
library(irr)
#Model's and subjects' answers for regular nouns
Model_answers_a=subset(dt, ending=='-a' & easy_diff=='difficult', select=c(label))
Subject_answers_a=subset(dt, ending_pp=='-a' & easy_dif_pp=='difficult', select=c(label_pp))
Table_answers_a=cbind(Model_answers_a, Subject_answers_a)

agree(Table_answers_a) #39%
kappa2(Table_answers_a, 'unweighted')


Model_answers_a=subset(dt, ending=='-a' & easy_diff=='easy', select=c(label))
Subject_answers_a=subset(dt, ending_pp=='-a' & easy_dif_pp=='easy', select=c(label_pp))
Table_answers_a=cbind(Model_answers_a, Subject_answers_a)

agree(Table_answers_a) #41%
kappa2(Table_answers_a, 'unweighted')

Model_answers_o=subset(dt, ending=='-o' & easy_diff=='difficult', select=c(label))
Subject_answers_o=subset(dt, ending_pp=='-o' & easy_dif_pp=='difficult', select=c(label_pp))
Table_answers_o=cbind(Model_answers_o, Subject_answers_o)

agree(Table_answers_o) #67.9
kappa2(Table_answers_o, 'unweighted')

Model_answers_o=subset(dt, ending=='-o' & easy_diff=='easy', select=c(label))
Subject_answers_o=subset(dt, ending_pp=='-o' & easy_dif_pp=='easy', select=c(label_pp))
Table_answers_o=cbind(Model_answers_o, Subject_answers_o)

agree(Table_answers_o) #98.2
kappa2(Table_answers_o, 'unweighted')


Model_answers_or=subset(dt, ending=='-or' & easy_diff=='difficult', select=c(label))
Subject_answers_or=subset(dt, ending_pp=='-or' & easy_dif_pp=='difficult', select=c(label_pp))
Table_answers_or=cbind(Model_answers_or, Subject_answers_or)

agree(Table_answers_or) #40.5
kappa2(Table_answers_or, 'unweighted')

Model_answers_or=subset(dt, ending=='-or' & easy_diff=='easy', select=c(label))
Subject_answers_or=subset(dt, ending_pp=='-or' & easy_dif_pp=='easy', select=c(label_pp))
Table_answers_or=cbind(Model_answers_or, Subject_answers_or)

agree(Table_answers_or) #63.1
kappa2(Table_answers_or, 'unweighted')

summary(dt$ending)



Model_answers_ade=subset(dtNA, ending=='-ade' & easy_diff=='difficult', select=c(label))
Subject_answers_ade=subset(dtNA, ending_pp=='-ade' & easy_dif_pp=='difficult', select=c(label_pp))
Table_answers_ade=cbind(Model_answers_ade, Subject_answers_ade)

agree(Table_answers_ade) #71.1
kappa2(Table_answers_ade, 'unweighted') #no answer

Model_answers_ade=subset(dt, ending=='-ade' & easy_diff=='easy', select=c(label))
Subject_answers_ade=subset(dt, ending_pp=='-ade' & easy_dif_pp=='easy', select=c(label_pp))
Table_answers_ade=cbind(Model_answers_ade, Subject_answers_ade)

agree(Table_answers_ade) #58.3
kappa2(Table_answers_ade, 'unweighted') #k=-0,0194, z=-0.184, p=0.854

Model_answers_agem=subset(dt, ending=='-agem' & easy_diff=='difficult', select=c(label))
Subject_answers_agem=subset(dt, ending_pp=='-agem' & easy_dif_pp=='difficult', select=c(label_pp))
Table_answers_agem=cbind(Model_answers_agem, Subject_answers_agem)

agree(Table_answers_agem) #41.7
kappa2(Table_answers_agem, 'unweighted') 

Model_answers_agem=subset(dt, ending=='-agem' & easy_diff=='easy', select=c(label))
Subject_answers_agem=subset(dt, ending_pp=='-agem' & easy_dif_pp=='easy', select=c(label_pp))
Table_answers_agem=cbind(Model_answers_agem, Subject_answers_agem)


agree(Table_answers_agem) #56%

Model_answers_ao=subset(dt, ending=='-??o' & easy_diff=='difficult', select=c(label))
Subject_answers_ao=subset(dt, ending_pp=='-??o' & easy_dif_pp=='difficult', select=c(label_pp))
Table_answers_ao=cbind(Model_answers_ao, Subject_answers_ao)

agree(Table_answers_ao) #341
kappa2(Table_answers_ao, 'unweighted')

Model_answers_ao=subset(dt, ending=='-??o' & easy_diff=='easy', select=c(label))
Subject_answers_ao=subset(dt, ending_pp=='-??o' & easy_dif_pp=='easy', select=c(label_pp))
Table_answers_ao=cbind(Model_answers_ao, Subject_answers_ao)

agree(Table_answers_ao) #42.2
kappa2(Table_answers_ao, 'unweighted')


Model_answers_ema=subset(dt, ending=='-ema' & easy_diff=='difficult', select=c(label))
Subject_answers_ema=subset(dt, ending_pp=='-ema' & easy_dif_pp=='difficult', select=c(label_pp))
Table_answers_ema=cbind(Model_answers_ema, Subject_answers_ema)

agree(Table_answers_ema) #48.2
kappa2(Table_answers_ema, 'unweighted') 


Model_answers_ema=subset(dt, ending=='-ema' & easy_diff=='easy', select=c(label))
Subject_answers_ema=subset(dt, ending_pp=='-ema' & easy_dif_pp=='easy', select=c(label_pp))
Table_answers_ema=cbind(Model_answers_ema, Subject_answers_ema)


agree(Table_answers_ema) #61.4
kappa2(Table_answers_ema, 'unweighted')

Model_answers_ume=subset(dt, ending=='-ume' & easy_diff=='difficult', select=c(label))
Subject_answers_ume=subset(dt, ending_pp=='-ume' & easy_dif_pp=='difficult', select=c(label_pp))
Table_answers_ume=cbind(Model_answers_ume, Subject_answers_ume)

agree(Table_answers_ume) #58.9
kappa2(Table_answers_ume, 'unweighted')

Model_answers_ume=subset(dt, ending=='-ume' & easy_diff=='easy', select=c(label))
Subject_answers_ume=subset(dt, ending_pp=='-ume' & easy_dif_pp=='easy', select=c(label_pp))
Table_answers_ume=cbind(Model_answers_ume, Subject_answers_ume)

kappa2(Table_answers_ume, 'unweighted')

agree(Table_answers_ume) #21.6
kappa2(Table_answers_ume, 'unweighted')
#collapsed

Model_answers_a=subset(dtNA, ending=='-a', select=c(label))
Subject_answers_a=subset(dtNA, ending_pp=='-a', select=c(label_pp))


agree(Model_answers_a,Subject_answers_a)

Model_answers_o=subset(dtNA, ending=='-o', select=c(label))
Subject_answers_o=subset(dtNA, ending_pp=='-o', select=c(label_pp))


agree(Model_answers_o,Subject_answers_o)

Model_answers_agem=subset(dtNA, ending=='-agem', select=c(label))
Subject_answers_agem=subset(dtNA, ending_pp=='-agem', select=c(label_pp))
Table=cbind(Model_answers_agem,Subject_answers_agem)
names(Table)

agree(Model_answers_agem,Subject_answers_agem, tolerance = 0)






names(dtNA)
attach(dtNA)

library("ggpubr")
ggscatter(dt, x = "label", y = "label_pp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Model choices", ylab = "Participants' choices")

#interaction plots





Gender_data_28_simulations <- read.csv("~/Desktop/Book_chapter_Gender_simulation/Gender_data_28_simulations.csv", sep=";")

dt=Gender_data_28_simulations



dt$categories=ifelse(dt$categories=='Transp/fem','Transparent',
                     ifelse(dt$categories=='Transp/Masc', 'Transparent',
                            ifelse(dt$categories=='Transp/Irr', 'Irregular',
                                   ifelse(dt$categories=='Reg', 'Reg',
                                   as.factor(dt$categories)))))


dt$ending_frequency=as.numeric(dt$ending_frequency)
is.numeric(dt$ending_frequency)
dt$neighborFrquency=as.numeric(dt$neighborFrquency)

is.numeric(dt$neighborFrquency)
dt$neighborDensity=as.numeric(dt$neighborDensity)
is.numeric(dt$neighborDensity)

attach(dt)
View(dt)


ggplot() +
  aes(x = GenderDecision, color = categories, group = categories, y = neighborFrquency ) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")



ggplot() +
  aes(x = GenderDecision, color = categories, group = categories, y = neighborFrquency ) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

ggplot() +
  aes(x = dta$, color = categories, group = categories, y = neighborFrquency ) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

View(dta)

#GLMERs 

library(lme4)

dataset=dt

dt$GenderDecision=ifelse(dt$GenderDecision=='yes','1',
                              ifelse(dt$GenderDecision=='no', '0',
                                     as.numeric(dt$GenderDecision)))

View(dt)
dt$GenderDecision=as.numeric(dt$GenderDecision)
is.numeric(dt$GenderDecision)

dt$neighborFrquency=scale(dt$neighborFrquency, center= TRUE, scale = TRUE)
dt$neighborDensity=scale(dt$neighborDensity, center= TRUE, scale = TRUE)
dt$ending_frequency=as.numeric(dt$ending_frequency, center= TRUE, scale = TRUE)

attach(dt)
gm_all <- all_fit(gm1)
m1 <- glmer(GenderDecision ~ ending_frequency+categories+neighborDensity+(1|simulation) + (1|item),family=binomial(link = "logit"), data = dt)
summary(m1)
m1.1 <- glmer(GenderDecision ~ ending_frequency+categories+neighborDensity+(1 |simulation) + (1|item),family=binomial(link = "logit"), data = dt, control=glmerControl(optCtrl=list(maxfun=2000000)))

m2 <- glmer(GenderDecision ~ ending_frequency+categories+neighborDensity+neighborFrquency+(1 |simulation) + (1|item),family=binomial(link = "logit"), data = dt, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

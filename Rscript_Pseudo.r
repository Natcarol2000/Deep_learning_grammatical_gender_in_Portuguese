PP=Pseudo_PP16
attach(PP)
names(PP)
#selecionando colunas
dataset=data.frame(Subject,TaskBlock1.RESP,TaskBlock1.RT,WordsBlock1)
View(dataset)

#excluindo linhas

Sujeito=Subject[-c(1:10)]
Resposta=TaskBlock1.RESP[-c(1:10)]
Lat??ncia=TaskBlock1.RT[-c(1:10)]
Itens=WordsBlock1[-c(1:10)]

#Criando data frame

dataset=data.frame(Sujeito,Itens,Resposta,Lat??ncia)
View(dataset)

#adicionando colunas 'Sexo' e 'OrdemTarefa'
dataset['Sexo']='Fem'
#ou
dataset['Sexo']='Masc'
dataset['OrdemTarefa']='Primeira'
#ou
dataset['OrdemTarefa']='Segunda'

View(dataset)

#adicionando colunas para vari??veis
dataset['Categoria']=NA
dataset['FreqTermina????o']=NA
dataset['FreqPalavrasVizinhas']=NA
dataset['Grafemas']=NA
dataset['Termina????o']=NA
dataset['Dificuldade']=NA

dataset$Sujeito=ifelse(Sujeito==16,'PP16',
                       as.factor(dataset$Sujeito))

#adicionando termina????o das pseudo
dataset$Termina????o=as.factor(ifelse(dataset$Itens %in% c('Famida','Fot??cia','Eferta', 'Cetoda','Bam??lia','Detuca'),'-a',
                                  ifelse(dataset$Itens %in% c('Damume','Liume','Merfume','Ligume','Datume','Vostume'),'-ume',
                                         ifelse(dataset$Itens %in% c('Galodor','Fensador','Pobedor','Nor','Fanodor','Sor'), '-or',
                                                ifelse(dataset$Itens %in% c('Cetafo','Tomento','Damibo','Muturo','Cecuto','Fafeco'), '-o',
                                                       ifelse(dataset$Itens %in% c('Cetade','Lontade','Imade','Secade','Vomade','Vabade'), '-ade',
                                                              ifelse(dataset$Itens %in% c('Maragem','Voragem','Decagem','Vomagem','Cabagem','Tantagem'), '-agem',
                                                                     ifelse(dataset$Itens %in% c('Afen????o','Ronstru????o','Docot??o','Pria????o','Fomad??o','Tetad??o'), '-??o',
                                                                            ifelse(dataset$Itens %in% c('Evema','Cilema','Sisfema','Cedema','Favema','Titema'), '-ema',
                                                                                   as.character(dataset$Itens))))))))))

#adicionando grau de dificuldade
dataset$Dificuldade=as.factor(ifelse(dataset$Itens %in% c('Fot??cia','Eferta','Bam??lia','Liume','Merfume','Ligume','Vostume','Fensador','Tomento','Muturo','Cetade','Lontade','Imade','Voragem','Maragem','Tantagem','Afen????o','Ronstru????o','Pria????o','Cilema','Sisfema','Evema','Nor','Sor'),'f??cil',
                                    ifelse(dataset$Itens %in% c('Famida','Cetoda','Detuca','Damume','Datume','Galodor','Pobedor','Fanodor','Cetafo','Damibo','Cecuto','Fafeco','Secade','Vomade','Vabade','Decagem','Cabagem','Vomagem','Docot??o','Fomad??o','Tetad??o','Cedema','Favema','Titema'),'Dif??cil',
                                           as.character(dataset$Itens))))

#adicionando n??mero de grafemas
dataset$Grafemas=as.numeric(ifelse(dataset$Itens %in% c('Famida'),6,
                                    ifelse(dataset$Itens %in% c('Fot??cia'),7,
                                           ifelse(dataset$Itens %in% c('Eferta'),6,
                                                  ifelse(dataset$Itens %in% c('Cetoda'),6,
                                                         ifelse(dataset$Itens %in% c('Bam??lia'),7,
                                                                ifelse(dataset$Itens %in% c('Detuca'),6,
                                                                       ifelse(dataset$Itens %in% c('Damume'),6,
                                                                              ifelse(dataset$Itens %in% c('Liume'),5,
                                                                                     ifelse(dataset$Itens %in% c('Merfume'),7,
                                                                                            ifelse(dataset$Itens %in% c('Ligume'),6,
                                                                                                   ifelse(dataset$Itens %in% c('Datume'),6,
                                                                                                          ifelse(dataset$Itens %in% c('Vostume'),7,
                                                                                                                 ifelse(dataset$Itens %in% c('Galodor'),7,
                                                                                                                        ifelse(dataset$Itens %in% c('Fensador'),8,
                                                                                                                               ifelse(dataset$Itens %in% c('Pobedor'),7,
                                                                                                                                      ifelse(dataset$Itens %in% c('Nor'),3,
                                                                                                                                             ifelse(dataset$Itens %in% c('Fanodor'),7,
                                                                                                                                                    ifelse(dataset$Itens %in% c('Sor'),3,
                                                                                                                                                           ifelse(dataset$Itens %in% c('Cetafo'),6,
                                                                                                                                                                  ifelse(dataset$Itens %in% c('Tomento'),7,
                                                                                                                                                                         ifelse(dataset$Itens %in% c('Damibo'),6,
                                                                                                                                                                                ifelse(dataset$Itens %in% c('Muturo'),6,
                                                                                                                                                                                       ifelse(dataset$Itens %in% c('Cecuto'),6,
                                                                                                                                                                                              ifelse(dataset$Itens %in% c('Fafeco'),6,
                                                                                                                                                                                                     ifelse(dataset$Itens %in% c('Cetade'),6,
                                                                                                                                                                                                            ifelse(dataset$Itens %in% c('Lontade'),7,
                                                                                                                                                                                                                   ifelse(dataset$Itens %in% c('Imade'),5,
                                                                                                                                                                                                                          ifelse(dataset$Itens %in% c('Secade'),6,
                                                                                                                                                                                                                                 ifelse(dataset$Itens %in% c('Vomade'),6,
                                                                                                                                                                                                                                        ifelse(dataset$Itens %in% c('Vabade'),6,
                                                                                                                                                                                                                                               ifelse(dataset$Itens %in% c('Maragem'),7,
                                                                                                                                                                                                                                                      ifelse(dataset$Itens %in% c('Voragem'),7,
                                                                                                                                                                                                                                                             ifelse(dataset$Itens %in% c('Decagem'),7,
                                                                                                                                                                                                                                                                    ifelse(dataset$Itens %in% c('Vomagem'),7,
                                                                                                                                                                                                                                                                           ifelse(dataset$Itens %in% c('Cabagem'),7,
                                                                                                                                                                                                                                                                                  ifelse(dataset$Itens %in% c('Tantagem'),8,
                                                                                                                                                                                                                                                                                         ifelse(dataset$Itens %in% c('Afen????o'),7,
                                                                                                                                                                                                                                                                                                ifelse(dataset$Itens %in% c('Ronstru????o'),10,
                                                                                                                                                                                                                                                                                                       ifelse(dataset$Itens %in% c('Docot??o'),7,
                                                                                                                                                                                                                                                                                                              ifelse(dataset$Itens %in% c('Pria????o'),7,
                                                                                                                                                                                                                                                                                                                     ifelse(dataset$Itens %in% c('Fomad??o'),7,
                                                                                                                                                                                                                                                                                                                            ifelse(dataset$Itens %in% c('Tetad??o'),7,
                                                                                                                                                                                                                                                                                                                                   ifelse(dataset$Itens %in% c('Evema'),5,
                                                                                                                                                                                                                                                                                                                                          ifelse(dataset$Itens %in% c('Cilema'),6,
                                                                                                                                                                                                                                                                                                                                                 ifelse(dataset$Itens %in% c('Sisfema'),7,
                                                                                                                                                                                                                                                                                                                                                        ifelse(dataset$Itens %in% c('Cedema'),6,
                                                                                                                                                                                                                                                                                                                                                               ifelse(dataset$Itens %in% c('Favema'),6,
                                                                                                                                                                                                                                                                                                                                                                      ifelse(dataset$Itens %in% c('Titema'),6,
                                                                                                                                                                                                                                                                                                                                                                             as.numeric(dataset$Grafemas))))))))))))))))))))))))))))))))))))))))))))))))))

#adicionando categorias
dataset$Categoria=as.factor(ifelse(dataset$Itens %in% c('Famida','Fot??cia','Eferta', 'Cetoda','Bam??lia','Detuca'),'Reg',
                                    ifelse(dataset$Itens %in% c('Damume','Liume','Merfume','Ligume','Datume','Vostume'),'Transp/Masc',
                                           ifelse(dataset$Itens %in% c('Galodor','Fensador','Pobedor','Nor','Fanodor','Sor'), 'Transp/Masc',
                                                  ifelse(dataset$Itens %in% c('Cetafo','Tomento','Damibo','Muturo','Cecuto','Fafeco'), 'Reg',
                                                         ifelse(dataset$Itens %in% c('Cetade','Lontade','Imade','Secade','Vomade','Vabade'), 'Transp/fem',
                                                                ifelse(dataset$Itens %in% c('Maragem','Voragem','Decagem','Vomagem','Cabagem','Tantagem'), 'Transp/fem',
                                                                       ifelse(dataset$Itens %in% c('Afen????o','Ronstru????o','Docot??o','Pria????o','Fomad??o','Tetad??o'), 'Transp/Irr',
                                                                              ifelse(dataset$Itens %in% c('Evema','Cilema','Sisfema','Cedema','Favema','Titema'), 'Transp/Irr',
                                                                                     as.character(dataset$Itens))))))))))

#adicionando frequ??ncia dos palavras 'vizinhas'
dataset$FreqPalavrasVizinhas=ifelse(dataset$Itens %in% c('Fot??cia'),179,
                                    ifelse(dataset$Itens %in% c('Eferta'),113,
                                           ifelse(dataset$Itens %in% c('Bam??lia'),571,
                                                  ifelse(dataset$Itens %in% c('Liume'),25,
                                                         ifelse(dataset$Itens %in% c('Merfume'),31,
                                                                ifelse(dataset$Itens %in% c('Ligume'),13,
                                                                       ifelse(dataset$Itens %in% c('Vostume'),80,
                                                                              ifelse(dataset$Itens %in% c('Fensador'),24,
                                                                                     ifelse(dataset$Itens %in% c('Nor'),223,
                                                                                            ifelse(dataset$Itens %in% c('Sor'),142,
                                                                                                   ifelse(dataset$Itens %in% c('Tomento'),7,
                                                                                                          ifelse(dataset$Itens %in% c('Muturo'),431,
                                                                                                                 ifelse(dataset$Itens %in% c('Lontade'),216,
                                                                                                                        ifelse(dataset$Itens %in% c('Imade'),218,
                                                                                                                               ifelse(dataset$Itens %in% c('Cetade'),172,
                                                                                                                                      ifelse(dataset$Itens %in% c('Voragem'),64,
                                                                                                                                             ifelse(dataset$Itens %in% c('Maragem'),4,
                                                                                                                                                    ifelse(dataset$Itens %in% c('Tantagem'),188,
                                                                                                                                                           ifelse(dataset$Itens %in% c('Afen????o'),236,
                                                                                                                                                                  ifelse(dataset$Itens %in% c('Ronstru????o'),248,
                                                                                                                                                                         ifelse(dataset$Itens %in% c('Pria????o'),290,
                                                                                                                                                                                ifelse(dataset$Itens %in% c('Evema'),3,
                                                                                                                                                                                       ifelse(dataset$Itens %in% c('Cilema'),401,
                                                                                                                                                                                              ifelse(dataset$Itens %in% c('Sisfema'),731,
                                                                                                                                                                                                     ifelse(dataset$Itens %in% c('Favema'),128,
                                                                                                                                                                                                            as.numeric(dataset$FreqPalavrasVizinhas))))))))))))))))))))))))))



#adicionando frequ??ncia da Termina????o
dataset$FreqTermina????o=ifelse(dataset$Itens %in% c('Famida','Fot??cia','Eferta','Cetoda','Bam??lia','Detuca'),70251,
                              ifelse(dataset$Itens %in% c('Damume','Liume','Merfume','Ligume','Datume','Vostume'),629,
                                     ifelse(dataset$Itens %in% c('Galodor','Fensador','Pobedor','Nor','Fanodor','Sor'),22178,
                                            ifelse(dataset$Itens %in% c('Cetafo','Tomento','Damibo','Muturo','Cecuto','Fafeco'),52478,
                                                   ifelse(dataset$Itens %in% c('Cetade','Lontade','Imade','Secade','Vomade','Vabade'),17005,
                                                          ifelse(dataset$Itens %in% c('Maragem','Voragem','Decagem','Vomagem','Cabagem','Tantagem'),3421,
                                                                 ifelse(dataset$Itens %in% c('Afen????o','Ronstru????o','Docot??o','Pria????o','Fomad??o','Tetad??o'),51114,
                                                                        ifelse(dataset$Itens %in% c('Evema','Cilema','Sisfema','Cedema','Favema','Titema'),1254,
                                                                               as.numeric(dataset$FreqTermina????o)))))))))



View(dataset)  





setwd("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo")

write.table(dataset, file='Pseudo_PP16.txt', sep='\t', row.names=FALSE)

Data_Pseudo_complete=rbind(Pseudo_PP1,Pseudo_PP2,Pseudo_PP3,Pseudo_PP5,Pseudo_PP6,Pseudo_PP7,Pseudo_PP8,Pseudo_PP9,Pseudo_PP10,Pseudo_PP11,Pseudo_PP12,Pseudo_PP14,Pseudo_PP15,Pseudo_PP16,Pseudo_PP17,Pseudo_PP18,Pseudo_PP19,Pseudo_PP20,Pseudo_PP21,Pseudo_PP22,Pseudo_PP23,Pseudo_PP24,Pseudo_PP25,Pseudo_PP26,Pseudo_PP27,Pseudo_PP28,Pseudo_PP29,Pseudo_PP30)
write.table(Data_Pseudo_complete, file='Data_Pseudo_complete.txt',sep='\t', row.names=FALSE)
# estat??stica erros -------------------------------------------------------------------

Data_Pseudo_complete <- read.delim("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/Data_Pseudo_complete.txt")
Gender_simulation_Ndensity <- read.csv("~/Desktop/Book_chapter_Gender_simulation/Gender_simulation_Ndensity.csv", sep=";")
dataset=Data_Pseudo_complete
dataset=Gender_simulation_Ndensity

View(dataset)
names(dataset)
attach(dataset)

#counting number of errors per item endings


aF=subset(dataset, Termina????o=='-a'& Dificuldade=='f??cil', select=c(Resposta)) 

aD=subset(dataset, Termina????o=='-a'& Dificuldade=='Dif??cil', select=c(Resposta)) 
aD
oF=subset(dataset, Termina????o=='-o'& Dificuldade=='f??cil', select=c(Resposta))#19erros
oF
oD=subset(dataset, Termina????o=='-o'& Dificuldade=='Dif??cil',select=c(Resposta))#19erros
oD


agemF=subset(dataset, Termina????o=='-agem'& Dificuldade=='f??cil', select=c(Resposta))#26erros
agemF
agemD=subset(dataset, Termina????o=='-agem'& Dificuldade=='Dif??cil', select=c(Resposta))#26erros
agemD


adeF=subset(dataset, Termina????o=='-ade'& Dificuldade=='f??cil', select=c(Resposta))#42erros
adeF
adeD=subset(dataset, Termina????o=='-ade'& Dificuldade=='Dif??cil', select=c(Resposta))#25erros
adeD
umeF=subset(dataset, Termina????o=='-ume'& Dificuldade=='f??cil', select=c(Resposta))#45erros
umeF 

umeD=subset(dataset, Termina????o=='-ume'& Dificuldade=='Dif??cil', select=c(Resposta))#45erros
umeD 

orF=subset(dataset, Termina????o=='-or'& Dificuldade=='f??cil', select=c(Resposta))
orF 

orD=subset(dataset, Termina????o=='-or'& Dificuldade=='Dif??cil', select=c(Resposta))
orD 





#counting number os entries for 'Fem' or 'Masc' 

??oF=subset(dataset, Termina????o=='-??o'& Dificuldade=='f??cil', select=c(Resposta)) #69 fem e #96 masculinas 3NA
??oF
??oD=subset(dataset, Termina????o=='-??o'& Dificuldade=='Dif??cil', select=c(Resposta)) #69 fem e #96 masculinas 3NA
??oD

emaF=subset(dataset, Termina????o=='-ema'& Dificuldade=='f??cil',select=c(Resposta))#56masc e #112fem 2NA
emaF

emaD=subset(dataset, Termina????o=='-ema'& Dificuldade=='Dif??cil', select=c(Resposta))#56masc e #112fem 2NA
emaD


#counting number os entries for 'Fem' or 'Masc' for each of the 'easy' pseudoword category

Fot??cia=subset(dataset, Itens=='Fot??cia', select=c(Resposta))
Fot??cia # 3 erros
Eferta=subset(dataset, Itens=='Eferta', select=c(Resposta))
Eferta # 3 erros
Bam??lia=subset(dataset, Itens=='Bam??lia', select=c(Resposta))
Bam??lia # 1 erro
Liume=subset(dataset, Itens=='Liume', select=c(Resposta))
Liume # 10 erros
Merfume=subset(dataset, Itens=='Merfume', select=c(Resposta))
Merfume #8 erros
Ligume=subset(dataset, Itens=='Ligume', select=c(Resposta))
Ligume #4 erros
Vostume=subset(dataset, Itens=='Vostume', select=c(Resposta))
Vostume #2 erros
Fensador=subset(dataset, Itens=='Fensador', select=c(Resposta))
Fensador # erros
Nor=subset(dataset, Itens=='Nor', select=c(Resposta))
Nor # 8 erros
Sor=subset(dataset, Itens=='Sor', select=c(Resposta))
Sor #7 erros

Tomento=subset(dataset, Itens=='Tomento', select=c(Resposta))
Tomento # 1 erro
Muturo=subset(dataset, Itens=='Muturo', select=c(Resposta))
Muturo # 0 erro
Cetade=subset(dataset, Itens=='Cetade', select=c(Resposta))
Cetade #7 erros
Lontade=subset(dataset, Itens=='Lontade', select=c(Resposta))
Lontade # 5 erros
Imade=subset(dataset, Itens=='Imade', select=c(Resposta))
Imade # 7 erros
Voragem=subset(dataset, Itens=='Voragem', select=c(Resposta))
Voragem # 6 erros
Maragem=subset(dataset, Itens=='Maragem', select=c(Resposta))
Maragem #3 erros
Tantagem=subset(dataset, Itens=='Tantagem', select=c(Resposta))
Tantagem #4 erros


Decagem=subset(dataset, Itens=='Decagem', select=c(Resposta))
Decagem # 3 erros
Vomagem=subset(dataset, Itens=='Vomagem', select=c(Resposta))
Vomagem #6 erros
Cabagem=subset(dataset, Itens=='Cabagem', select=c(Resposta))
Cabagem #4 erros





Afen????o=subset(dataset, Itens=='Afen????o', select=c(Resposta))
Afen????o #4 erros
Ronstru????o=subset(dataset, Itens=='Ronstru????o', select=c(Resposta))
Ronstru????o # 8 erros
Pria????o=subset(dataset, Itens=='Pria????o', select=c(Resposta))
Pria????o #10 erros
Cilema=subset(dataset, Itens=='Cilema', select=c(Resposta))
Cilema #14 erros
Sisfema=subset(dataset, Itens=='Sisfema', select=c(Resposta))
Sisfema #16 erros
Evema=subset(dataset, Itens=='Evema', select=c(Resposta))
Evema #22 erros



# correlation -------------------------------------------------------------

data_correlation <- read.csv("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/data_correlation.csv", sep=";")
attach(data_correlation)
#excluding -??o e -ema
x=freq[c(1:6)]
y=erros[c(1:6)]
#excluding -??o
w=freq[c(1:7)]
z=erros[c(1:7)]

#plots
library(lattice)
library(psych)
library(car)
scatterplot(y ~ x, data=data_correlation, legend.coords="topleft")
scatterplot(erros ~ termina????o, data=data_correlation)


boxplot(erros~categoria, data=data_correlation,
        col=(c("blue")),
        main="Quantidade de erros por categoria", xlab="Categorias", ylab="Quantidade de erros por categoria", outline=FALSE)



#correlation tests Erros*Frequ??ncia de Termina????o
cor.test(x, y, method = "kendall") #0.02172 tau=-0.82 -excluding termina????o -??o e -ema
cor.test(z, w, method = "kendall") #0.015 tau=-0.78 - excluding Termina????o em -??o

#data:PseudopalavrasFaceis

data_PseudoFaceis <- read.csv("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/data_PseudoFaceis.csv", sep=";")
data=data_PseudoFaceis
names(data)
attach(data)

#plots:PseudopalavrasFaceis

scatterplot(erros ~ categoria, data=data, legend.coords="topleft")
scatterplot(erros ~ termina????o, data=data)


boxplot(erros~Termina????o, data=data, notch=TRUE,
        col=(c("blue")),
        main="Quantidade de erros por Termina????o (F??ceis)", xlab="Categorias", ylab="Quantidade de erros por Termina????o (F??ceis)", outline=FALSE)

boxplot(erros~categoria, data=data, notch=TRUE,
        col=(c("blue")),
        main="Quantidade de erros por categoria (F??ceis)", xlab="Categorias", ylab="Quantidade de erros por categoria (F??ceis)", outline=FALSE)


#correlation tests Erros*Frequ??ncia de Termina????o
cor.test(freqPalavra, erros, method = "kendall")
cor.test(FreqTermina????o, erros, method = "kendall") 





# stats- velocidade de resposta -------------------------------------------

Data_Pseudo_complete <- read.delim("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/Data_Pseudo_complete.txt")
data_pseudo_30pp_complete_simulation <- read.csv("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/data_pseudo_30pp_complete_simulation.csv", sep=";")
dataset=Data_Pseudo_complete
dataset=data_pseudo_30pp_complete_simulation
attach(dataset)
names(dataset)
View(dataset)
#excluding 'NA' rows and Latencies below 300ms
dataset=subset(dataset, Lat??ncia>300)
dataset=subset(dataset, Resposta>0)

#amalgamando Categories 'Transp/masc' and 'Transp/fem' 

dataset$Categories=ifelse(dataset$Categories=='Transp/fem','Transp',
                         ifelse(dataset$Categories=='Transp/Masc', 'Transp',
                                ifelse(dataset$Categories=='Transp/Irr', 'Irr',
                                       as.character(dataset$Categories))))


View(dataset)

#get descriptives

library(psych)
names(dataset)
with(dataset, describeBy(RT, group=list(Categories), mat=TRUE))
with(dataset, describeBy(RT, group=list(Endings), mat=TRUE))
with(dataset, describeBy(RT, group=list(Endings, Difficulty), mat=TRUE))




#understanding distribution
library(lattice)
with(dataset, densityplot(Lat??ncia))#left skewed
library(ggplot2)
ggplotRT<- ggplot(dataset, aes(Lat??ncia)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Lat??ncia", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(dataset$Lat??ncia, na.rm = TRUE), sd = sd(dataset$Lat??ncia, na.rm = TRUE)), colour = "black", size = 1) 
ggplotRT
skew(dataset$Lat??ncia)

qqplot.RT <- qplot(sample = dataset$Lat??ncia, stat = "qq")
qqplot.RT


#checking for outliers
library(pastecs)
stat.desc(dataset$Lat??ncia, basic = FALSE, norm = TRUE)

#z-score to check for outliers

dataset$Lat??nciaz<-scale(dataset$Lat??ncia)
describe(dataset$Lat??nciaz)
stat.desc(dataset$Lat??nciaz)
sortcategorybyLat??nciaz <- dataset[order(dataset$Lat??nciaz),] #sort categories by z-score of RT
table(dataset$Lat??nciaz > 2.5 | dataset$Lat??nciaz < -2.5) #28 
tail(sortcategorybyLat??nciaz, n = 28)

#log transformation

dataset$logLat??ncia <- log(1+dataset$Lat??ncia)
skew(dataset$logLat??ncia)
skew(dataset$Lat??ncia)
describe(dataset$logLat??ncia)
densityplot(dataset$logLat??ncia) #right skewed
hist.logLat??ncia <- ggplot(dataset, aes(logLat??ncia)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Lat??ncia (log)", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(dataset$logLat??ncia, na.rm = TRUE), sd = sd(dataset$logLat??ncia, na.rm = TRUE)), colour = "red", size = 1) 
hist.logLat??ncia #right skewed
with(dataset, densityplot(~ logLat??ncia| Categoria)) 
ggplot(dataset, aes(x= logLat??ncia, fill = Categoria)) + geom_density(alpha = 0.2)
stat.desc(dataset$logLat??ncia, basic = FALSE, norm = TRUE)
describe(dataset$logLat??ncia)
hist.logLat??ncia<- ggplot(dataset, aes(logLat??ncia)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Lat??ncia", y = "Densidade") + stat_function (fun = dnorm, args = list(mean = mean(dataset$logLat??ncia, na.rm = TRUE), sd = sd(dataset$logLat??ncia, na.rm = TRUE)), colour = "black", size = 1) 
hist.logLat??ncia #much better

# check outliers in log transformation via z-scores
boxplot(dataset$logLat??ncia, xlab = "Todas as categorias", ylab = "Lat??ncia (log)")

dataset$logLat??nciaz<-scale(dataset$logLat??ncia)
describe(dataset$logLat??nciaz)
stat.desc(dataset$logLat??nciaz)
sortcategorybylogLat??nciaz <- dataset[order(dataset$logLat??nciaz),] #sort categories by z-score of RT
table(dataset$logLat??nciaz > 2.5 | dataset$logLat??nciaz < -2.5) # 18
tail(sortcategorybylogLat??nciaz, n = 18)



#removing outliers from variable 'logLat??ncia' that are 2,5 sd away from mean
dataset_logtrimmed = dataset[abs(scale(dataset$logLat??ncia)) < 2.5,] #10 outliers removed 
densityplot(dataset_logtrimmed$logLat??ncia)
hist.logLat??ncia <- ggplot(dataset_logtrimmed, aes(logLat??ncia)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Lat??ncia (log)", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(dataset_logtrimmed$logLat??ncia, na.rm = TRUE), sd = sd(dataset_logtrimmed$logLat??ncia, na.rm = TRUE)), colour = "red", size = 2) 
hist.logLat??ncia #Great
skew(dataset_logtrimmed$logLat??ncia)
attach(dataset_logtrimmed)

shapiro.test(dataset_logtrimmed$logLat??ncia) 

library(Hmisc)
# separate per condition
with(dataset, densityplot(~ logLat??ncia| Categoria)) # all categories are positively skewed


write.table(dataset_logtrimmed, file='data_logtrimmed_pseudo.txt', sep='\t', row.names=FALSE)




# lme4 models -------------------------------------------------------------
data_logtrimmed_pseudo <- read.delim("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/data_logtrimmed_pseudo.txt")
dataset=data_logtrimmed_pseudo

library(lme4)

names(dataset)

model1=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria+Termina????o+Dificuldade+(1|Sujeito)+(1|Itens), data=dataset)
summary(model1, cor=FALSE)
model2=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria+Termina????o*Dificuldade+(1|Sujeito)+(1|Itens), data=dataset)
summary(model2, cor=FALSE)
model3=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria*Termina????o*Dificuldade+(1|Sujeito)+(1|Itens), data=dataset)
summary(model3, cor=FALSE)
model4=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria*Termina????o*Dificuldade+(1+Categoria|Sujeito)+(1|Itens), data=dataset)
summary(model4, cor=FALSE)
model5=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria*Dificuldade+Termina????o+(1+Categoria|Sujeito)+(1|Itens), data=dataset)
summary(model5, cor=FALSE) 
model6=lmer(logLat??ncia~Sexo+Grafemas+Categoria*Dificuldade+Termina????o+OrdemTarefa+(1|Sujeito)+(1|Itens), data=dataset)
summary(model6, cor=FALSE)
model7=lmer(logLat??ncia~Sexo+Grafemas+Categoria*Termina????o*Dificuldade*OrdemTarefa+(1+Dificuldade|Sujeito)+(1|Itens), data=dataset)
summary(model7, cor=FALSE)
model8=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria+Termina????o+Dificuldade+(1+Categoria|Sujeito)+(1|Itens), data=dataset)
summary(model8, cor=FALSE)


#models REML= FALSE

model1_REML=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria+Termina????o+FreqTermina????o+Dificuldade+(1|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model1, cor=FALSE)
model2_REML=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria+Termina????o*FreqTermina????o+Dificuldade+(1|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model2, cor=FALSE)
model3_REML=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria*FreqTermina????o*Termina????o*Dificuldade+(1|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model3, cor=FALSE)
model4_REML=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria*FreqTermina????o*Termina????o*Dificuldade+(1+Categoria|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model4, cor=FALSE)
model6_REML=lmer(logLat??ncia~Sexo+OrdemTarefa+Grafemas+Categoria*FreqTermina????o*Termina????o*Dificuldade+(1+Dificuldade|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model6, cor=FALSE)
model7_REML=lmer(logLat??ncia~Sexo+Grafemas+Categoria*FreqTermina????o*Termina????o*Dificuldade*OrdemTarefa+(1+Dificuldade|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model7, cor=FALSE)


#likelihood ratio tests with anova() = Compara????o dos modelos

library(car)
anova(model1_REML,model2_REML)
anova(model1_REML,model3_REML)
anova(model2_REML,model3_REML)
anova(model1_REML,model4_REML)
anova(model2_REML,model4_REML)
anova(model1, model6)
anova(model1_REML, model7_REML)

#ANOVA para extra????o do P-valor
library(car)


Anova(model6, type = 3, test = 'Chisq')

Anova(model1, type = 3, test = 'Chisq')

# make diagnostic plots of the model

plot(model1) # fitted vs. residuals
library(lattice)
plot(model1)
densityplot(resid(model1)) # distribution of the resicuals
plot(data_logtrimmed_pseudo$logLat??ncia, fitted(model1)) # raw vs. fitted

#glmer models

Gender_data_28_simulations <- read.csv("~/Desktop/Book_chapter_Gender_simulation/Gender_data_28_simulations.csv", sep=";")
Gender_simulation_Ndensity <- read.csv("~/Desktop/Book_chapter_Gender_simulation/Gender_simulation_Ndensity.csv", sep=";")
dataset=Gender_data_28_simulations
dataset=Gender_simulation_Ndensity
View(dataset)
library(lme4)
names(dataset)
attach(dataset)


dataset$acerto=ifelse(dataset$acerto=='yes','1',
                      ifelse(dataset$acerto=='no', '0',
                             as.numeric(dataset$acerto)))

dataset$GenderDecision=ifelse(dataset$GenderDecision=='yes','1',
                      ifelse(dataset$GenderDecision=='no', '0',
                             as.numeric(dataset$GenderDecision)))



dataset$acerto=as.numeric(dataset$acerto)
dataset$GenderDecision=as.numeric(dataset$GenderDecision)
is.numeric(dataset$acerto)
is.numeric(dataset$GenderDecision)


dataset$ending_frequency=as.numeric(dataset$ending_frequency)
dataset$EndingFrequency=as.numeric(dataset$EndingFrequency)
dataset$EndingFrequency=scale(dataset$EndingFrequency, center = TRUE)
dataset$NeighborFrequency=scale(dataset$NeighborFrequency, center= TRUE)
dataset$neighborFrquency=scale(dataset$neighborFrquency, center= TRUE)
dataset$neighborDensity=scale(dataset$neighborDensity, center= TRUE)
dataset$N_D=scale(dataset$N_D, center = TRUE)
names(dataset)

library(ggplot2)
ggplot() +
  aes(x = dataset$N_D, color = dataset$Endings, group = dataset$Endings, y = dataset$acerto) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

boxplot(dataset$acerto~dataset$N_D)

m1 <- glmer(acerto ~ EndingFrequency+Categories+NeighborFrequency+N_D+(1 |Subject) + (1|Items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m1)

m1 <- glmer(acerto ~ EndingFrequency+Categories+NeighborFrequency+N_D+(1 |Subject) + (1|Items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


m2 <- glmer(acerto ~ EndingFrequency*Categories+N_D+NeighborFrequency+(1 |Subject) + (1|Items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m2)

anova(m2,m1)

m3 <- glmer(acerto ~ EndingFrequency*Categories*N_D+Difficulty+NeighborFrequency+(1 |Subject) + (1|Items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m3)

anova(m3,m2)

m4 <- glmer(acerto ~ EndingFrequency*Categories*N_D+Difficulty+NeighborFrequency+(1 + Difficulty|Subject) + (1|Items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(m4)


anova(m4,m3)

m5 <- glmer(acerto ~ EndingFrequency*Categories*N_D+NeighborFrequency+(1 + N_D |Subject) + (1|Items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m5)

m6 <- glmer(acerto ~ EndingFrequency*Categories*N_D+NeighborFrequency+(1  |Subject) + (1|Items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m6)
anova(m5, m6)
# Plots -------------------------------------------------------------------
data_logtrimmed_pseudo <- read.delim("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/data_logtrimmed_pseudo.txt")
dataset=data_logtrimmed_pseudo
attach(dataset)
View(dataset)
names(dataset)

boxplot(logLat??ncia~Categoria, data=dataset, notch=TRUE,
        col=c('blue'),
        main="Boxplots da velocidade de resposta por categoria", xlab="Categorias", ylab="(log) Velocidade de Resposta", outline=FALSE)

boxplot(logLat??ncia~Termina????o, data=dataset, notch=TRUE,
        col=c('blue'),
        main="Boxplots da velocidade de resposta por Termina????o", xlab="termina????o", ylab="(log) Velocidade de Resposta", outline=FALSE)

boxplot(logLat??ncia~Termina????o*Dificuldade, data=dataset, notch=TRUE,
        col=c('blue'),
        names=c('-a','-ade','-agem','-??o','-ema','-o','-or','-ume','-a','-ade','-agem','-??o','-ema','-o','-or','-ume'),
        main="Boxplots da velocidade de resposta por termina????o", xlab="Dificuldade (Dif??cil x F??cil)", ylab="(log) Velocidade de Resposta", outline=FALSE)




interaction.plot(dataset$Dificuldade, Termina????o,dataset$logLat??ncia, xlab='Dificuldade (Dif??cil x F??cil)', ylab='(log) Velocidade de Resposta',leg.bty = "o", legend=TRUE, col = 1:5)
interaction.plot(dataset$Dificuldade, dataset$Categoria ,dataset$logLat??ncia, xlab='Dificuldade (Dif??cil x F??cil)', ylab='(log) Velocidade de Resposta',leg.bty = "o", legend=TRUE, col = 1:5)






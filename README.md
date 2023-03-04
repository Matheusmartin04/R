# Análise de dados de saúde usando linguagem R

rm(list = ls())

### CARREGANDO BIBLIOTECAS
library(knitr)  
library(readxl)
library(readxl)  
library(tidyverse)  
library(ggthemes)  
library(egg)  
library(plotly)  
library(scales)  
library(ggrepel)  

setwd("C:\\Users\\tarcisio\\Documents\\FACULDADE\\R")

dados <- read.csv('dados.csv')

dados <- dados %>% select('X','IDADE', 'SEXO','CATEGORIA_NOME','DISTRITO_NOME','VALOR_REIVINDICACAO','Mortality')

View(head(dados,10))
#### TRATAMENTO DE DADOS

## RENOMEANDO COLUNAS
#dados3 <- dados2 %>% rename(IDADE = AGE,
SEXO=SEX,CATEGORIA_CODIGO=CATEGORY_CODE, 
CATEGORIA_NOME=CATEGORY_NAME,
CIRURGIA_CODIGO=SURGERY_CODE, 
CIRURGIA=SURGERY, VILA=VILLAGE, 
MANDAL_NAME=MANDAL_NAME, 
DISTRITO_NOME=DISTRICT_NAME,
#PRE_AUTH_DATA=PREAUTH_DATE, 
VALOR_PRE_AUTH=PREAUTH_AMT, 
DATA_REIVINDICACAO=CLAIM_DATE, 
VALOR_REIVINDICACAO=CLAIM_AMOUNT,
#HOSP_LOCALIZACAO=HOSP_LOCATION, 
HOSP_DISTRITO=HOSP_DISTRICT,
#CIRURGIA_DATA=SURGERY_DATE, 
DATA_QUITA??O=DISCHARGE_DATE, 
DATA_MORTE=MORTALITY_DATE)

dados <- as.factor(c(dados$SEXO, dados$CATEGORIA_NOME, dados$DISTRITO_NOME, dados$Mortality.Y...N))

#### VARIAVEL SEXO

dados[dados$SEXO == "Female", ]$SEXO = "Feminino"
dados[dados$SEXO == "Male", ]$SEXO = "Masculino"
dados[dados$SEXO == "Female(Child)", ]$SEXO = "Feminino(crian?a)"
dados[dados$SEXO == "Male(Child)", ]$SEXO = "Masculino(crian?a)"
dados$IDADE = as.integer(dados$IDADE)

#### variavel categoria nome
dados[dados$CATEGORIA_NOME =='CARDIAC AND CARDIOTHORACIC SURGERY', ]$CATEGORIA_NOME='CIRURGIA CARDIACA E CARDIOTORACICA'  
dados[dados$CATEGORIA_NOME =='CARDIOLOGY', ]$CATEGORIA_NOME='CARDIOLOGIA'  
dados[dados$CATEGORIA_NOME =='COCHLEAR IMPLANT SURGERY', ]$CATEGORIA_NOME='CIRURGIA DE IMPLANTE COCLEAR'  
dados[dados$CATEGORIA_NOME =='CRITICAL CARE', ]$CATEGORIA_NOME='CUIDADOS INTENSIVOS'  
dados[dados$CATEGORIA_NOME =='DERMATOLOGY', ]$CATEGORIA_NOME='DERMATOLOGIA'  
dados[dados$CATEGORIA_NOME =='ENDOCRINOLOGY', ]$CATEGORIA_NOME='ENDOCRINOLO'  
dados[dados$CATEGORIA_NOME =='ENT  SURGERY', ]$CATEGORIA_NOME='CIRURGIA ORL'  
dados[dados$CATEGORIA_NOME =='GASTROENTEROLOGY', ]$CATEGORIA_NOME='GASTROENTEROLOGIA'  
dados[dados$CATEGORIA_NOME =='GENERAL MEDICINE', ]$CATEGORIA_NOME='MEDICINA GERAL'  
dados[dados$CATEGORIA_NOME =='GENERAL SURGERY', ]$CATEGORIA_NOME='CIRURGIA GERAL'  
dados[dados$CATEGORIA_NOME =='GENITO URINARY SURGERIES', ]$CATEGORIA_NOME='GENITO CIRURGIAS URINARIAS'  
dados[dados$CATEGORIA_NOME =='GYNAECOLOGY AND OBSTETRICS  SURGERY', ]$CATEGORIA_NOME='CIRURGIA DE GINECOLOGIA E OBSTETRICIA'  
dados[dados$CATEGORIA_NOME =='MEDICAL ONCOLOGY', ]$CATEGORIA_NOME='ONCOLOGIA MEDICA'  
dados[dados$CATEGORIA_NOME =='NEPHROLOGY', ]$CATEGORIA_NOME='NEFROLOGIA'  
dados[dados$CATEGORIA_NOME =='NEUROLOGY', ]$CATEGORIA_NOME='NEUROLOGIA'  
dados[dados$CATEGORIA_NOME =='NEUROSURGERY', ]$CATEGORIA_NOME='NEUROCIRURGIA'  
dados[dados$CATEGORIA_NOME =='OPHTHALMOLOGY  SURGERY', ]$CATEGORIA_NOME='CIRURGIA OFTALMOLOGICA'  
dados[dados$CATEGORIA_NOME =='ORTHOPEDIC  SURGERY AND PROCEDURES', ]$CATEGORIA_NOME='CIRURGIA ORTOPEDICA E PROCEDIMENTOS'  
dados[dados$CATEGORIA_NOME =='PEDIATRIC SURGERIES', ]$CATEGORIA_NOME='CIRURGIAS PEDIATRICAS'  
dados[dados$CATEGORIA_NOME =='PEDIATRICS', ]$CATEGORIA_NOME='PEDIATRIA'  
dados[dados$CATEGORIA_NOME =='PLASTIC SURGERY', ]$CATEGORIA_NOME='CIRURGIA PLASTICA'  
dados[dados$CATEGORIA_NOME =='POLY TRAUMA', ]$CATEGORIA_NOME='POLI TRAUMA'  
dados[dados$CATEGORIA_NOME =='PULMONOLOGY', ]$CATEGORIA_NOME='PULMONOLOGIA'  
dados[dados$CATEGORIA_NOME =='RADIATION ONCOLOGY', ]$CATEGORIA_NOME='ONCOLOGIA DE RADIACAO'  
dados[dados$CATEGORIA_NOME =='RHEUMATOLOGY', ]$CATEGORIA_NOME='REUMATOLOGIA'  
dados[dados$CATEGORIA_NOME =='SURGICAL GASTRO ENTEROLOGY', ]$CATEGORIA_NOME='GASTRO ENTEROLOGIA CIRURGICA'  
dados[dados$CATEGORIA_NOME =='SURGICAL ONCOLOGY', ]$CATEGORIA_NOME='ONCOLOGIA CIRURGICA'  

#### TRANFORMA??ES NAS VARIAVEIS
dados3$SEXO  = as.factor(dados3$SEXO)
dados3$VALOR_REIVINDICACAO  = as.numeric(dados3$VALOR_REIVINDICACAO)

#### preço médio do valor de reivendicação por distritos
df <- dados %>% group_by(DISTRITO_NOME) %>% summarise(MEDIA = mean(VALOR_REIVINDICACAO)) %>% view()
kable(df, col.names = c("DISTRITO_NOME", "MEDIA")) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#### preço médio do valor de reivendicação por categoria de cirurgia
dd <-dados %>% group_by(CATEGORIA_NOME) %>% summarise(MEDIA = mean(VALOR_REIVINDICACAO)) %>% view()
kable(dd, col.names = c("CATEGORIA_NOME", "MEDIA"))%>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive"))



## visualização de dados
### sexo
x = table(dados$SEXO)
y = prop.table(table(dados$SEXO))
dd = data.frame(x,round(y,2)) %>% rename(SEXO=Var1,Frequ?ncia_absoluta=Freq,Frequ?ncia_relativa=Freq.1)
dd <- dd[-3]
kable(dd, col.names = c("Var1", "Freq", "Freq.1")) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
stargazer(dd, summary = F)
ggplot(dd,aes(y = reorder(Var1,Freq), x = Freq))+
  geom_col(col = 'black',fill = 'grey', width = 0.5)+ 
  scale_x_continuous(expand = c(.01, 0), limits = c(0,12000), breaks = seq(0,12000,1500))+
  theme_bw()+
  labs(x= 'FREQU?NCIA',y = 'SEXO' )+
  theme(text = element_text(family = 'serif',face = 'bold'))+
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))

### idade
ggplot(dados, aes(x = "", y = dados$IDADE)) +
  geom_errorbar(stat = "boxplot", width = 0.2)+
  geom_boxplot(show.legend = F, alpha = .5, width = 0.6) +
  geom_point(stat = "summary", fun = "mean")+
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  theme_bw() +
  labs(x= '',y = 'IDADE', title = "Boxplot Da Vari?vel Idade" )+
  theme(text = element_text(family = 'serif',face = 'bold'))+
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))

### HISTROGRAMA DA IDADE

ggplot(dados, aes(x = as.numeric(dados$IDADE)))+
  geom_histogram(col = 'black', alpha=1,position = 'identity', fill = 'grey')+
  #scale_fill_viridis_d()+  
  labs(x = "Nota", y = "Frequencia")+
  theme_bw()+
  theme(text = element_text(family = 'serif',face = 'bold'))+
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))+
  scale_y_continuous(expand = c(.01, 0), limits = c(0,2000),breaks = seq(0,2000,200))



### idade x sexo
ggplot(dados, aes(x = dados$SEXO, y = dados$IDADE, fill = dados$SEXO)) +
  geom_errorbar(stat = "boxplot", width = 0.6)+
  geom_boxplot(show.legend = F, alpha = .5) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  theme_bw() +
  labs(x= 'SEXO',y = 'IDADE' )+
  theme(text = element_text(family = 'serif',face = 'bold'))+
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))


dados %>% group_by(CATEGORIA_NOME) %>% count(SEXO == 'Masculino') %>%  view()


### frequencia da variavel categoria de nomes cirurgia
x = table(dados$CATEGORIA_NOME)
y = prop.table(table(dados$CATEGORIA_NOME))
dd <- data.frame(x,y)


ggplot(dd, aes(y =reorder(dd$Var1, dd$Freq), x = dd$Freq))+
  geom_col(col = 'black',fill = 'grey', width = 0.5)+ 
  theme_bw()+
  labs(y= 'Categoria de  Cirurgia',x = 'Frequencia' )+
  theme(text = element_text(family = 'serif',face = 'bold'))+
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))+
  scale_x_continuous(expand = c(.01, 0),limits = c(0,6000), breaks = seq(0,6000,500))


### frequencia da variavel categoria de nomes cirurgia
x = table(dados$Mortality)
y = prop.table(table(dados$Mortality))
dd <- data.frame(x,y)


ggplot(dd, aes(y =dd$Var1, x = dd$Freq))+
  geom_col(col = 'black',fill = 'grey', width = 0.5)+ 
  theme_bw()+
  labs(y= 'Houve óbito?',x = 'Frequencia' )+
  theme(text = element_text(family = 'serif',face = 'bold'))+
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))+
  scale_x_continuous(expand = c(.01,0), breaks = seq(0,18000,1000))

# Analyse des données du COVID19 - Données Nationales
# Alain Lucas
# 10-04-2020
# Source : 
# https://www.data.gouv.fr/en/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/


###################################################################################################
# Options et librairies
rm(list=ls())
options(width=150)

library(dplyr)
library(ggplot2)

###################################################################################################
# Chargement des données
URL = "https://www.data.gouv.fr/en/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7"
readLines(URL,n=10)

dataset = read.table(URL,header=TRUE,sep=";",quote="\"")
head(dataset)
str(dataset)
tail(dataset)

# dep : département
# sexe : 0 = total 1 = hommes 2 = femmes
# Jour : date de notification
# hosp : nombre de patients hospitalisés
# rea : nombre de patients en réanimation ou soins intensifs
# rad : nombre cumulé de patients retournés au domicile
# dc : nombre cumulé de patients décédés à l'hôpital


###################################################################################################
# Préparation des données
dataset %>% mutate(sexe = factor(sexe,labels=c("Total","Homme","Femme")),
                   jour = as.Date(jour,format="%Y-%m-%d")) -> dataset



###################################################################################################
# Analyse Hospitalisation + Réanimation en France métropolitaine

dataset %>% filter(sexe == "Total") %>% group_by(jour) %>% 
  summarize(hosp = sum(hosp), rea = sum(rea)) %>% mutate(Prop = round(100*rea/hosp,2)) %>% 
  ggplot(aes(x=jour))+
  geom_line(aes(y=hosp,color="col1"),size=1.3)+
  geom_line(aes(y=rea,color="col2"),size=1.3)+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_colour_manual(name="Légende",values = c("col1"="#FF0000","col2"="#0000FF","col3"="black"),
                      labels=c("Hôpital","Réanimation","7000 lits en réanimation"))+
  labs(title="Nombre de patients hospitalisés et en réanimation",
       x="Jour",y="")+
  scale_x_date(date_labels="%d/%m",date_breaks = "2 day")+
  geom_hline(aes(yintercept=7000,color="col3"),size=1.3)
  
  
  
###################################################################################################
# Analyse Hospitalisation + réanimation par département en Normandie

dataset %>% filter(sexe == "Total" & dep %in% c("14","50","61","27","76")) %>% 
  group_by(dep,jour) %>% summarize(hosp = sum(hosp), rea = sum(rea)) %>% 
  ggplot(aes(x=jour))+
  geom_line(aes(y=hosp,color="col1"),size=1.2)+
  geom_line(aes(y=rea,color="col2"),size=1.2)+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_colour_manual(name="Légende",values = c("col1"="#FF0000","col2"="#0000FF"),
                      labels=c("Hôpital","Réanimation"))+
  labs(title="Nombre de patients hospitalisés et en réanimation par département",
       x="Jour",y="")+
  scale_x_date(date_labels="%d/%m",date_breaks = "2 day")+
  facet_wrap(~dep)

###################################################################################################
# Analyse nombre de décès par jour en France métropolitaine

dataset %>% filter(sexe=="Total") %>% group_by(jour) %>% 
  summarize(dc = sum(dc)) %>% mutate(dc_jour = dc - lag(dc)) %>% na.omit() %>% 
  ggplot(aes(x=jour))+
  geom_bar(stat="identity",aes(y=dc_jour),color="black",fill="#2E86C1")+
  scale_x_date(date_labels="%d/%m",date_breaks = "2 day")+
  labs(title="Nombre de décès par jour",x="",y="Effectif")+
  geom_text(aes(x=jour,y=dc_jour,label=paste(dc_jour)),vjust=-0.3)







###################################################################################################
# Analyse nombre de décès par jour en Normandie

Département = c("14"="Calvados","27"="Eure","76"="Seine-maritime",
                "61"="Orne","50"="Manche")

dataset %>% filter(sexe == "Total" & dep %in% c("14","50","61","27","76")) %>%
  group_by(dep) %>% select(dep,jour,dc) %>% mutate(dc_jour = dc - lag(dc)) %>% 
  na.omit() %>% ungroup() %>% 
  ggplot(aes(x=jour))+
  geom_bar(stat="identity",aes(y=dc_jour),color="black",fill="#E67E22")+
  scale_x_date(date_labels="%d/%m",date_breaks = "2 day")+
  labs(title="Nombre de décès par jour par département",x="",y="Effectif")+
  geom_text(aes(x=jour,y=dc_jour,label=paste(dc_jour)),vjust=-0.3)+
  facet_wrap(~dep,labeller=as_labeller(Département))




# Analyse des données du COVID19 - International
# Alain Lucas
# 10-04-2020


##################################################################################################
# Options et librairies
rm(list=ls())
options(width=150)

library(dplyr)
library(ggplot2)

##################################################################################################
# Chargement des données
URL = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
readLines(URL,n=10)

dataset = read.table(URL,header=TRUE,sep=",")
head(dataset)
str(dataset)


##################################################################################################
# Préparation des données
dataset %>% select(dateRep,cases,deaths,countriesAndTerritories,popData2018) %>% 
  mutate(date=as.Date(dateRep,format="%d/%m/%Y")) %>% mutate(dateRep=NULL) %>% 
  rename(country = countriesAndTerritories,pop2018 = popData2018) -> covid



##################################################################################################
# Dernière mise à jour

covid %>% select(date) %>% summarize(MAJ = max(date))



##################################################################################################
# Enregistrement de covid
save(covid,file="covid.RData") ; rm(covid)
load("covid.RData")
write.table(covid,file="covid19-international.txt",sep=";",
            row.names=FALSE,col.names=TRUE,quote=FALSE)


##################################################################################################
# Nombre de décès par pays

covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium",
                                "Switzerland","United_Kingdom","United_States_of_America")) %>%
  group_by(country) %>% summarize(total_deaths = sum(deaths)) %>%  
  ggplot(aes(x=reorder(country,-total_deaths),y=total_deaths,fill=country))+
  geom_bar(stat="identity",col="black")+
  theme_bw()+
  geom_text(aes(y=total_deaths,label=total_deaths),vjust=-0.3)+
  labs(title="Nombre de décès",x="",y="Effectifs")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position="none") -> graph1

covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium",
                                "Switzerland","United_Kingdom","United_States_of_America")) %>%
  group_by(country) %>% 
  summarize(total_deaths = sum(deaths)) %>% 
  arrange(desc(total_deaths)) %>%
  rename(Country = country, Total_Deaths = total_deaths) %>% 
  ggtexttable(theme=ttheme("mOrange"),rows=NULL) -> table1
  
paste("Note : Pour les Etats-Unis, la courbe de l'évolution de \n 
      l'épidémie est beaucoup plus rapide que pour les pays européens")-> text1

ggparagraph(text1,face="italic",size=11,color="red")->paragraph1



##################################################################################################
# Taux de décès par pays

covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium",
                "Switzerland","United_Kingdom","United_States_of_America")) %>%
  group_by(country) %>% summarize(Total_deaths = sum(deaths),pop2018 = first(pop2018)) %>% 
  mutate(taux = (Total_deaths /pop2018)*10000) %>% 
  ggplot(aes(x=reorder(country,-taux),y=taux,fill=country))+
  geom_bar(stat="identity",col="black")+
  theme_bw()+
  geom_text(aes(label=round(taux,2)),vjust=-0.3)+
  labs(title="Taux de mortalité par COVID19 pour 10 000 habitants",x="",y="Taux")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(legend.position = "none") -> graph2



##################################################################################################
# Evolution du nombre cumulé de décès au cours du temps

covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium",
                  "Switzerland","United_Kingdom","United_States_of_America")) %>%
  filter(date >= as.Date("2020-03-29")) %>% 
  group_by(country) %>% arrange(date) %>% mutate(Cum_deaths = cumsum(deaths)) %>% 
  ggplot(aes(x=date,y=Cum_deaths,color=country))+
  geom_line(size=1.5)+
  theme_bw()+
  labs(title="Nombre cumulé de décès",x="Date",y="Effectif")+
  scale_x_date(date_labels = "%d %B",date_breaks = "5 days")+
  scale_color_discrete(name="Pays")+
  theme(legend.position="bottom") -> graph3

ggplotly(graph3) %>% layout(legend = list(x=0.05,y=0.95))

ggplotly(graph3) %>% layout(legend = list(orientation = "h",y=-0.15))




# Source : http://www.sthda.com/english/articles/32-r-graphics-essentials/126-combine-multiple-ggplots-in-one-graph/

library(ggpubr)

ggarrange(graph3,ggarrange(graph1,graph2,ncol=2),nrow=2,labels="A",
          common.legend = TRUE,legend = "bottom")

ggarrange(graph3,table1,paragraph1,ncol=1,nrow=3,heights=c(1,0.5,0.2))

library(gridExtra)

grid.arrange(arrangeGrob(graph1,graph2,ncol=2),graph3,nrow=2)



##################################################################################################
# Nombre de décès par jour en France

dataset %>% select(countriesAndTerritories,deaths,dateRep) %>% 
  mutate(date = as.Date(dateRep,format="%d/%m/%Y")) %>% 
  mutate(dateRep = NULL) %>% 
  rename(Pays = countriesAndTerritories) %>%  
  filter(Pays=="France") -> France

#-- Evolution du nombre de décès

France %>% filter(date >= as.Date("2020-03-01")) %>% 
ggplot(aes(x=date,y=deaths))+
  geom_bar(stat="identity",fill="lightblue",col="black")+
  labs(title="Nombre de décès par jour en France",xlab="",ylab="Effectif")+
  scale_x_date(date_labels = "%d %B",date_breaks="2 days")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

#-- Taux de décès journalier

France %>% mutate(taux = round(100*(deaths - lag(deaths))/lag(deaths),2)) -> France

France %>% filter(date >= as.Date("2020-03-01")) %>% na.omit() %>% 
  filter(taux < Inf) %>% mutate(color = ifelse(taux >0,"positif","négatif")) %>%
 ggplot(aes(x=date,y=taux))+
  geom_bar(stat="identity",aes(fill=color),col="black")+
  labs(title="Evolution du taux de décès journalier en France",xlab="",ylab="Pourcentage")+
  scale_x_date(date_labels = "%d %B",date_breaks="2 days")+
  scale_y_continuous(limits=c(-150,300))+
  scale_fill_manual(values=c("positif"="pink","négatif"="lightblue"))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(legend.position = "none")

summary(France)


##################################################################################################
# Evolution du nombre de décès journalier au cours du temps

library(plotly)

covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium",
                                "Switzerland","United_Kingdom","United_States_of_America")) %>%
  mutate(country=factor(country)) %>% 
  filter(date >= as.Date("2020-03-01"))  %>% group_by(country,date) %>%  
  summarize(totalcases = sum(cases),totaldeaths = sum(deaths)) %>% 
  plot_ly(x=~country,y=~totaldeaths) %>% 
  add_bars(color=~country,frame=~as.character(date),
           hoverinfo="text",text=~paste(format(date,"%d-%m-%Y"),":",totaldeaths)) %>% 
   layout(showlegend=FALSE,title="Evolution du nombre de décès par jour",
          xaxis=list(title=""),yaxis=list(title="Effectif")) %>% 
   animation_slider( currentvalue = list(prefix = "Date: ")) %>% 
   animation_opts(frame=1000,redraw=TRUE)
  


##################################################################################################
# Evolution du nombre total de décès au cours du temps

covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium","Netherlands",
                                "Switzerland","United_Kingdom","United_States_of_America")) %>%
  mutate(country=factor(country)) %>% 
  filter(date >= as.Date("2020-03-01"))  %>% group_by(country) %>% 
  arrange(country,date) %>% mutate(cumdeaths = cumsum(deaths)) %>% 
  plot_ly(x=~country,y=~cumdeaths) %>% 
  add_bars(color=~country,frame=~as.character(date),
           hoverinfo="text",text=~paste(format(date,"%d-%m-%Y"),":",cumdeaths)) %>% 
  layout(showlegend=FALSE,title="Evolution du cumule de décès par jour",
         xaxis=list(title=""),yaxis=list(title="Effectif")) %>% 
  animation_slider( currentvalue = list(prefix = "Date: ")) %>% 
  animation_opts(frame=1000,redraw=TRUE)


covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium",
                                "Switzerland","United_Kingdom","United_States_of_America")) %>%
  filter(date >= as.Date("2020-03-29")) %>% 
  group_by(country) %>% arrange(date) %>% mutate(Cum_deaths = cumsum(deaths)) %>%
  plot_ly(x=~date,y=~Cum_deaths,color=~country) %>% 
  add_markers(frame=~as.character(date)) %>%  
  layout(showlegend=FALSE) %>% 
  layout(xaxis = list(
    range = 
      c(as.numeric(as.POSIXct("2020-03-29", format="%Y-%m-%d"))*1000,
        as.numeric(as.POSIXct("2020-04-25", format="%Y-%m-%d"))*1000),
    type = "date"))

library(purrr)
library(RColorBrewer)
covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium",
                                "Switzerland","United_Kingdom","United_States_of_America")) %>%
  filter(date >= as.Date("2020-03-29")) %>% 
  group_by(country) %>% arrange(date) %>% mutate(Cum_deaths = cumsum(deaths)) %>%
  split(f=.$date) %>% accumulate(.,~bind_rows(.x,.y)) %>% bind_rows(.id = "frame") %>% 
  plot_ly(x=~date,y=~Cum_deaths,color=~country,
          colors = brewer.pal(n=8,"Dark2")) %>% 
  add_lines(frame=~frame,ids =~country) %>%  
  layout(showlegend=FALSE) %>% 
  layout(xaxis = list(
    range = 
      c(as.numeric(as.POSIXct("2020-03-29", format="%Y-%m-%d"))*1000,
        as.numeric(as.POSIXct("2020-04-25", format="%Y-%m-%d"))*1000),
    type = "date"))
  




##################################################################################################
# Evolution du nombre de décès en fonction du nombre de cas au cours du temps 

covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium","Netherlands",
                                "Switzerland","United_Kingdom","United_States_of_America")) %>%
  mutate(country=factor(country)) %>% 
  filter(date >= as.Date("2020-03-01")) %>% 
  plot_ly(x=~cases,y=~deaths) %>% 
  add_markers(color=~country,frame=~as.character(date),size=~pop2018,
              hoverinfo="text",text=~paste(format(date,"%d-%m-%Y"),":",deaths,"deaths")) %>% 
  animation_slider( currentvalue = list(prefix = "Date: ")) %>% 
  layout(title="Nombre de décès en fonction du nombre de cas",
         xaxis=list(title="Nombre de cas"),
         yaxis=list(title="Nombre de décès"))


##################################################################################################
# 

library(crosstalk)

covid %>% filter(country %in% c("France","Italy","Germany","Spain","Belgium","Netherlands",
                                "Switzerland","United_Kingdom","United_States_of_America")) %>%
  mutate(country=factor(country),pop2018=NULL) %>% 
  filter(date >= as.Date("2020-03-01")) %>% group_by(country) %>% arrange(country,date) %>%  
  mutate(cumdeaths = cumsum(deaths)) -> data

shared_data = data %>% SharedData$new()  

shared_data %>% plot_ly(x=~cumdeaths,y=~country) %>% 
  add_bars(orientation="h",color=~country,frame=~as.character(date),
           hoverinfo="text",text=~paste(format(date,"%d-%m-%Y"),":",cumdeaths)) %>% 
  layout(title="Décès cummulés",showlegend=FALSE,
         xaxis=list(title="Nombre de décès"),
         yaxis=list(title="")) -> graph1

data %>% plot_ly(x=~cases,y=~deaths) %>% 
  add_markers(color = ~country, frame=~as.character(date)) -> graph2

bscols(graph1,graph2)  



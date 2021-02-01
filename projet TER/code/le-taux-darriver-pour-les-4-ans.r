#########################     figure10   ######################################################

##################Taux d'arrivée durant l'année 2007 pour########## 
##################### departement Emergency Internal ##############
####################### Medicine Unit #############################
dbDisconnect(con) 
library(RMySQL)
library(ggplot2)
#faire une connexion a la base de donner mysql rambam 
con=dbConnect(MySQL(),username='root',dbname='rambam',host='localhost')
#############             2004               ##################
#NOMBRE DES PATIENTS POUR CHAQUE MOIS PENDANT LANNEE 2004
result=dbSendQuery(con,"SELECT COUNT(*) as duration,date(entry_date) As WeekDay,MONTH( entry_date ) as mois FROM visits 
WHERE YEAR(entry_date)=2004 and  
first_department= 1 AND HOUR(entry_date)
GROUP BY MONTH( entry_date )")

#recuperer le resultat
data.frame=fetch(result)
#affectation du résultat a req4
req4=data.frame

#############             2005               ##################
#NOMBRE DES PATIENTS POUR CHAQUE MOIS PENDANT LANNEE 2005
result=dbSendQuery(con,"SELECT COUNT(*) as duration,date(entry_date) As WeekDay,MONTH( entry_date ) as mois FROM visits 
WHERE YEAR(entry_date)=2005 and  
first_department= 1 AND HOUR(entry_date)
GROUP BY MONTH( entry_date )")
data.frame=fetch(result)
req5=data.frame

#############             2006               ##################
#NOMBRE DES PATIENTS POUR CHAQUE MOIS PENDANT LANNEE 2006
result=dbSendQuery(con,"SELECT COUNT(*) as duration,date(entry_date) As WeekDay,MONTH( entry_date ) as mois FROM visits 
WHERE YEAR(entry_date)=2006 and  
first_department= 1 AND HOUR(entry_date)
GROUP BY MONTH( entry_date )")
data.frame=fetch(result)
req6=data.frame

#############             2007               ##################
#NOMBRE DES PATIENTS POUR CHAQUE MOIS PENDANT LANNEE 2007
result=dbSendQuery(con,"SELECT COUNT(*) as duration,date(entry_date) As WeekDay,MONTH( entry_date ) as mois FROM visits 
WHERE YEAR(entry_date)=2007 and  
first_department= 1 AND HOUR(entry_date)
GROUP BY MONTH( entry_date )")

#recuperer le resultat
data.frame=fetch(result)
#affectation du résultat a req7
req7=data.frame
# data4 CONTIEN LE NUMERO DE MOIS ET LE NOM DE MOIS
data4<-data.frame(mois=req4$mois,m=c("janvier", "février", "mars", "avril", "mai","juin","juillet", "aout", "septembre", "octobre","novembre","décembre"))
#FAIRE LE MERGE ENTRE REQ4 et data4 PARAPORT AU MOIS 
req4<-merge(req4, data4, by ="mois")
#AFFICHER data4
data4

req4$m<- factor(req4$m, levels= c("janvier", "février", "mars", "avril", "mai","juin","juillet", "aout", "septembre", "octobre","novembre","décembre"))

# data5 CONTIEN LE NUMERO DE MOIS ET LE NOM DE MOIS
data5<-data.frame(mois=req5$mois,m=c("janvier", "février", "mars", "avril", "mai","juin","juillet", "aout", "septembre", "octobre","novembre","décembre"))

#FAIRE LE MERGE ENTRE req4 et data5 PARAPORT AU MOIS 
req5<-merge(req5, data5, by ="mois")
req5$m<- factor(req5$m, levels= c("janvier", "février", "mars", "avril", "mai","juin","juillet", "aout", "septembre", "octobre","novembre","décembre"))
# data6 CONTIEN LE NUMERO DE MOIS ET LE NOM DE MOIS
data6<-data.frame(mois=req6$mois,m=c("janvier", "février", "mars", "avril", "mai","juin","juillet", "aout", "septembre", "octobre","novembre","décembre"))
#FAIRE LE MERGE ENTRE req6 et data6 PARAPORT AU MOIS 
req6<-merge(req6, data6, by ="mois")
req6$m<- factor(req6$m, levels= c("janvier", "février", "mars", "avril", "mai","juin","juillet", "aout", "septembre", "octobre","novembre","décembre"))
# data6 CONTIEN LE NUMERO DE MOIS ET LE NOM DE MOIS
data7<-data.frame(mois=req7$mois,m=c("janvier", "février", "mars", "avril", "mai","juin","juillet", "aout", "septembre", "octobre"))
#FAIRE LE MERGE ENTRE req6 et data6 PARAPORT AU MOIS 
req7<-merge(req7, data7, by ="mois")
req7$m<- factor(req7$m, levels= c("janvier", "février", "mars", "avril", "mai","juin","juillet", "aout", "septembre", "octobre"))

#le graphe de taux d'arrivée durant l'année 2004 pour 
#departement Emergency Internal Medicine Unit pour chaque moi
a<-ggplot(data = req4, aes(x = req4$m,y=req4$duration)) + 
  geom_histogram(stat = "identity",fill = "#660099")+
  ggtitle("Taux d'arrivée durant l'année 2004 pour 
       departement Emergency Internal 
                       Medicine Unit") +
  xlab("Mois") +
  ylab("Nombe d'arrive")+
  coord_flip()+
  geom_text(aes( y = req4$duration, label = req4$duration),position = position_stack(vjust = 0.5) ,colour="white")

#le graphe de taux d'arrivée durant l'année 2005 pour 
#departement Emergency Internal Medicine Unit pour chaque moi
b<-ggplot(data = req5, aes(x = req5$m,y=req5$duration)) + 
  geom_histogram(stat = "identity",fill = "#990099")+
  ggtitle("Taux d'arrivée durant l'année 2005 pour 
       departement Emergency Internal 
                       Medicine Unit") +
  xlab("Mois") +
  ylab("Nombe d'arrive")+
  coord_flip()+
  geom_text(aes( y = req5$duration, label = req5$duration),position = position_stack(vjust = 0.5) ,colour="white")

#le graphe de taux d'arrivée durant l'année 2006 pour 
#departement Emergency Internal Medicine Unit pour chaque moi
c<-ggplot(data = req6, aes(x = req6$m,y=req6$duration)) + 
  geom_histogram(stat = "identity",fill = "#CC0099")+
  ggtitle("Taux d'arrivée durant l'année 2006 pour 
       departement Emergency Internal 
                       Medicine Unit") +
  xlab("Mois") +
  ylab("Nombe d'arrive")+
  coord_flip()+
  geom_text(aes( y = req6$duration, label = req6$duration),position = position_stack(vjust = 0.5) ,colour="white")
#le graphe de taux d'arrivée durant l'année 2007 pour 
#departement Emergency Internal Medicine Unit pour chaque moi
d<-ggplot(data = req7, aes(x = req7$m,y=req7$duration)) + 
  geom_histogram(stat = "identity",fill = "#FF0099",width = 0.5)+
  ggtitle("Taux d'arrivée durant l'année 2007 pour 
       departement Emergency Internal 
                       Medicine Unit") +
  xlab("Mois") +
  ylab("Nombe d'arrive")+
  coord_flip()+
  geom_text(aes( y = req7$duration, label = req7$duration),position = position_stack(vjust = 0.5) ,colour="white")



#REGROUPER LES 4 graphe a b c d on leur donne les noms A B C D 
#les 4 graphes pour les 4 annees 
plot_grid(a, b, c,d, labels=c("A", "B","C","D"))

###############################################################################
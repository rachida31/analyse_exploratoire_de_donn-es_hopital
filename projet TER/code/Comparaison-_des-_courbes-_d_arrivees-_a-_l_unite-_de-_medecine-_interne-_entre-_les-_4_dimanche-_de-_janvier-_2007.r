#deconnecter si il y a déjà une connexion
dbDisconnect(con)
#importation de packet dbConnect pour realiser la connecxion
library(dbConnect)
#importation de packet ggplot2 pour realiser la les différents graphe
library(ggplot2)
#importation de packet ggplot2 pour pouvoir regrouper plusieures graphe
library(gridExtra)

#connection a la base de donnée
con = dbConnect(MySQL(),dbname='hopital',user='root',password='Racheletmoi2',host='localhost')
#**********************************nombre de patient pour le departement 1 pour les 4 dimanches de mois de 01/2007 *****************
rq0=dbGetQuery(con," SELECT DATE_FORMAT(v.entry_date, \"%k\") as heure, COUNT(*)FROM visit_details v WHERE  YEAR(v.entry_date)=2007 and v.department=1 and month(v.entry_date)=1 and  dayofweek(v.entry_date)=1 and  DATE_FORMAT(v.entry_date, \"%U\") =1
 GROUP by DATE_FORMAT(v.entry_date, \"%k\") order by heure;")
rq1=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \"%k\") as heure, COUNT(*)FROM visit_details v WHERE  YEAR(v.entry_date)=2007 and v.department=1 and month(v.entry_date)=1 and  dayofweek(v.entry_date)=1 and  DATE_FORMAT(v.entry_date, \"%U\") =2
 GROUP by DATE_FORMAT(v.entry_date, \"%k\") order by heure;")
rq2=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \"%k\") as heure, COUNT(*)FROM visit_details v WHERE  YEAR(v.entry_date)=2007 and v.department=1 and month(v.entry_date)=1 and  dayofweek(v.entry_date)=1 and  DATE_FORMAT(v.entry_date, \"%U\") =3
 GROUP by DATE_FORMAT(v.entry_date, \"%k\") order by heure;")
rq3=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \"%k\") as heure, COUNT(*)FROM visit_details v WHERE  YEAR(v.entry_date)=2007 and v.department=1 and month(v.entry_date)=1 and  dayofweek(v.entry_date)=1 and  DATE_FORMAT(v.entry_date, \"%U\") =4
 GROUP by DATE_FORMAT(v.entry_date, \"%k\") order by heure;")
rq0$`COUNT(*)`<-cumsum(rq0$`COUNT(*)`)
rq1$`COUNT(*)`<-cumsum(rq1$`COUNT(*)`)
rq2$`COUNT(*)`<-cumsum(rq2$`COUNT(*)`)
rq3$`COUNT(*)`<-cumsum(rq3$`COUNT(*)`)
rq0$heure<-sort(as.integer(rq0$heure))
rq1$heure<-sort(as.integer(rq1$heure))
rq2$heure<-sort(as.integer(rq2$heure))
rq3$heure<-sort(as.integer(rq3$heure))
p<-ggplot() + 
  geom_line(data=rq0,aes(heure,`COUNT(*)`, colour = "07/01/2007"), size=1)+ 
  geom_line(data=rq1,aes(heure,`COUNT(*)`, colour = "17/01/2007"), size=1)+
  geom_line(data=rq2,aes(heure,`COUNT(*)`, colour = "21/01/2007"), size=1)+ 
  geom_line(data=rq3,aes(heure,`COUNT(*)`, colour = "28/01/2007"), size=1)+
  scale_x_continuous(name="Minutes") +scale_y_continuous(name="Nombre de patients ") +ggtitle("Nombres d'arrivÃ©es pour les 4 dimanches \n de janvier 2007 par heures")+ theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )
  p+scale_color_discrete(name="Courbe")




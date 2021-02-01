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
#********************************* nombre de patient accumuler pour le 19/01/2007 et 21/01/2007 avec leurs moyennes par heure pour les trois départements*******************
rq0=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %k\") as heure,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=1 and month(v.entry_date)=1  and DATE_FORMAT(v.entry_date, \" %e\")=19 GROUP by DATE_FORMAT(v.entry_date, \" %k\") order by heure
")
#fonction qui accumule le nombre d'arrivée pour chaque heure.
rq0$`COUNT(*)`<-cumsum(rq0$`COUNT(*)`)
rq1=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %k\") as heure,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=1 and month(v.entry_date)=1  and DATE_FORMAT(v.entry_date, \" %e\")=21 GROUP by DATE_FORMAT(v.entry_date, \" %k\") order by heure
")
rq1$`COUNT(*)`<-cumsum(rq0$`COUNT(*)`)
#calcule de la moyenne entre les deux jours
s<-rowMeans(data.frame(rq0$`COUNT(*)`,rq1$`COUNT(*)`))

rq0$heure<-sort(as.integer(rq0$heure))
rq1$heure<- sort(as.integer(rq1$heure))
p1<-ggplot() +
  geom_line(data=rq0,aes(heure,`COUNT(*)`, colour = "vendredi 19/01/2007"), size=1)+ 
  geom_line(data=rq1,aes(heure,`COUNT(*)`, colour = "dimanche 21/01/2007"), size=1)+ 
  geom_line(data=rq1,aes(heure,s, colour = "La courbe moyenne"), size=1)+
  scale_x_continuous(name="Heures") +scale_y_continuous(name="Nombre de patients ")+
  ggtitle("A-Emergency Internal Medicine Unit")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold"))

p1+scale_color_brewer(type = 'qual',palette="Paired",name="Courbe")
rrq0=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %k\") as heure,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=9 and month(v.entry_date)=1  and DATE_FORMAT(v.entry_date, \" %e\")=19 GROUP by DATE_FORMAT(v.entry_date, \" %k\") order by heure
")
rq0$`COUNT(*)`<-cumsum(rq0$`COUNT(*)`)
rq1=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %k\") as heure,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=9 and month(v.entry_date)=1  and DATE_FORMAT(v.entry_date, \" %e\")=21 GROUP by DATE_FORMAT(v.entry_date, \" %k\") order by heure
")
rq1$`COUNT(*)`<-cumsum(rq1$`COUNT(*)`)
h1<-c(rq2$heure,NA,NA)
count1<-c(rq1$`COUNT(*)`,NA,NA)
d<-data.frame(rq0$`COUNT(*)`,count1)
s<-rowMeans(d)
s<-s[c(-18,-19)]rq0$heure<-sort(as.integer(rq0$heure))
rq1$heure<- sort(as.integer(rq1$heure))
p3<-ggplot() +
  geom_line(data=rq0,aes(heure,`COUNT(*)`, colour = "vendredi 19/01/2007"), size=1)+ 
  geom_line(data=rq1,aes(heure,`COUNT(*)`, colour = "dimanche 21/01/2007"), size=1)+ 
  geom_line(data=rq1,aes(heure,s, colour = "La courbe moyenne"), size=1)+
  scale_x_continuous(name="Heures") +scale_y_continuous(name="Nombre de patients ") +
  ggtitle("C-Pediatric Emergency Unit")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold"))

p3+scale_color_discrete(name="Courbe")

rq0=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %k\") as heure,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=11 and month(v.entry_date)=1  and DATE_FORMAT(v.entry_date, \" %e\")=19 GROUP by DATE_FORMAT(v.entry_date, \" %k\") order by heure
")
rq0$`COUNT(*)`<-cumsum(rq0$`COUNT(*)`)
rq1=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %k\") as heure,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=11 and month(v.entry_date)=1  and DATE_FORMAT(v.entry_date, \" %e\")=21 GROUP by DATE_FORMAT(v.entry_date, \" %k\") order by heure
")
rq1$`COUNT(*)`<-cumsum(rq1$`COUNT(*)`)
s<-rowMeans(data.frame(rq0$`COUNT(*)`,rq1$`COUNT(*)`))
rrq0$heure<-sort(as.integer(rq0$heure))
rq1$heure<- sort(as.integer(rq1$heure))
p4<-ggplot() +
  geom_line(data=rq0,aes(heure,`COUNT(*)`, colour = "vendredi 19/01/2007"), size=1)+ 
  geom_line(data=rq1,aes(heure,`COUNT(*)`, colour = "dimanche 21/01/2007"), size=1)+ 
  geom_line(data=rq1,aes(heure,s, colour = "La courbe moyenne"), size=1)+
  scale_x_continuous(name="Heures") +scale_y_continuous(name="Nombre de patients ") +
  ggtitle("D-Emergency Maternity Unit")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold"))

p4+scale_color_brewer(palette="Paired",name="Courbe")


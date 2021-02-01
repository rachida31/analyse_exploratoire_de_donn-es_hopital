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
#**********************************nombre de patient pour le 01/2007 sur le departement 1,3,9,11 *****************
#requete sql a executer et recuperer le resultat dans un data frame rq0 
rq0=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %e\") as jour,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=1 and month(v.entry_date)=1 GROUP by DATE_FORMAT(v.entry_date, \" %e\") order by jour
")
#affichage de resultat de la requete.
print(rq0)
#convertir le numéro de jour en un integer car la reqête renvois des caractaires.
rq0$jour<-as.integer(rq0$jour)
#aprés convertion des numéro de jour le verteur est désordonné donc on l'ordonne avec la fonction sort
rq0$jour<-sort(rq0$jour)
# le graphe avec bar pour le département Emergency Maternity Unit
p1<-ggplot(data=rq0, aes(x=jour, y=`COUNT(*)`)) +
  geom_bar(stat="identity" ,width=0.5, fill="#F80673")+
  scale_x_continuous(name="Jours") +scale_y_continuous(name="Nombre de patients ")+ theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )
rq0=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %e\") as jour,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=3 and month(v.entry_date)=1 GROUP by DATE_FORMAT(v.entry_date, \" %e\") order by jour
")
rq0$jour<-sort(as.integer(rq0$jour))
p2<-ggplot(data=rq0, aes(x=jour, y=`COUNT(*)`)) +
  geom_bar(stat="identity" ,width=0.5, fill="#FFD700")+
  scale_x_continuous(name="Jours") +scale_y_continuous(name="Nombre de patients ") + theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )
rq0=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %e\") as jour,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=9 and month(v.entry_date)=1 GROUP by DATE_FORMAT(v.entry_date, \" %e\") order by jour
")
rq0$jour<-sort(as.integer(rq0$jour))
p3<-ggplot(data=rq0, aes(x=jour, y=`COUNT(*)`)) +
  geom_bar(stat="identity" ,width=0.5, fill="#A04EF2")+
  scale_x_continuous(name="Jours") +scale_y_continuous(name="Nombre de patients ")+ theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )
p3
rq0=dbGetQuery(con,"SELECT DATE_FORMAT(v.entry_date, \" %e\") as jour,COUNT(*) FROM visit_details v where YEAR(v.entry_date)=2007 and v.department=11 and month(v.entry_date)=1 GROUP by DATE_FORMAT(v.entry_date, \" %e\") order by jour
")
rq0$jour<-sort(as.integer(rq0$jour))
p4<-ggplot(data=rq0, aes(x=jour, y=`COUNT(*)`)) +
  geom_bar(stat="identity" ,width=0.5,  fill="#7FFFD4")+
  scale_x_continuous(name="Jours") +scale_y_continuous(name="Nombre de patients ") + theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

plot_grid(p1, p2, p3,p4, labels=c( "A-Emergency Internal Medicine Unit","B-Emergency Traumatology Unit","C-Pediatric Emergency Unit","D-Emergency Maternity Unit"))

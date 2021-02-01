
#########################     figure13   ######################################################

#faire une connexion a la base de donner mysql rambam 
con=dbConnect(MySQL(),username='root',dbname='rambam',host='localhost')
#AFFICHER Le temps d'attente moyenne(h) pour chaque départements
rq=dbSendQuery(con,"SELECT *, 
#le temps d'attente moyenne 
               ((sum(hospitalization_time - entry_time) / count(*))/3600)as temps
               FROM visits 
#la contraine temps d'attente > 0H
               where hospitalization_time - entry_time > 0
#la contraine temps d'attente < 24H
               and (hospitalization_time - entry_time)<86400 
#année 2007
               and YEAR(entry_date)=2007 
#POUR CHAQUE DEPARTMENTS 
               group by first_department")

#recuperer le resultat
data.frame=fetch(rq)
#affectation du résultat a r
r<-data.frame
library(ggplot2)

#affectation pour d1 les departements avec leur temps dattente moyenne
d1<-data.frame(first_department=r$first_department,
               temps=c(r$temps))
#afficher d1
d1
#AFFECTER pour chaque departement son nom dans laxe x et a droite 
#d2 se compose de departements avec numeros et les deux noms et le temps dattente moyenne pour chaque departements  
d2<-data.frame(first_department=c("Emergency Internal Medicine Unit ","Emergency Surgery Unit ",
                                  "Emergency Traumatology Unit ","Emergency Otorhinolaryngology Unit ",
                                  "Emergency Ophthalmology Unit ","Emergency Psychiatry Unit ",
                                  "Emergency Gynecology Unit ","Pediatric Emergency Unit ",
                                  "Pediatric Surgery Emergency Unit ","Emergency Maternity Unit "),
               first_departments=c("E I M U ","E S U ",
                                  "E T U ","E Ot U ",
                                  "E Op U ","E P U ",
                                  "E G U ","P E U ",
                                  "P S E U ","E M U "),
               temps=c(r$temps))
#AFFICHER d2
d2
#AFFICHER LE GRAPHE AVEC 10 departemts et le temps moyen dattente (heure)
#avec les noms des departements
#ranger le text grace geom_text
a5 <- ggplot(d2, aes(x = first_departments, y = temps, fill = first_department)) + 
  geom_col(width = 0.1) + 
  ggtitle("Le temps moyen d'attente par départment ") +
  xlab("numéro de départment") +
  ylab("Nombre d'heurs")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=round(temps,  digits = 1),
                vjust=0.3, 
                hjust=ifelse(temps>5, 1.3,-0.3),
                angle = 45)) 
           
a5
###############################################################################

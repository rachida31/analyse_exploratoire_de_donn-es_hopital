#########################     figure15   ######################################################
install.packages("cowplot")
library(cowplot)
###################################################
library(RMySQL)
library(ggplot2)

#faire une connexion a la base de donner rambam 
con=dbConnect(MySQL(),username='root',dbname='rambam',host='localhost')

##################   1/1/2007  ##############
#une requête qui renvoie le nombre des arrivées ,
#le temps d'attends moyen ,pour chaque heure de 
#la journée 1/1/2007 sur le département 1
result=dbSendQuery(con,"SELECT COUNT(patient_id) as nbr,
HOUR(entry_date) as heurARRIVER,
DAY(entry_date) as day,
MONTH(entry_date) as mois,
               (count(DAY(entry_date)))as nbrday,
#La moyen de temps d'attente               
               ((sum(hospitalization_time - entry_time) / count(patient_id))/3600)as temps 
#la contraine temps d'attente > 0               
               FROM visits where hospitalization_time - entry_time > 0 
##la contraine temps d'attente < 24H
               and (hospitalization_time - entry_time)<86400 
               and YEAR(entry_date)=2007 
               and MONTH( entry_date )=01 
               and DAY(entry_date)=1 
                   AND first_department=1
#mettre le resultat pour chaque heur d'arriver 
                   group by heurARRIVER")
#recuperer le resultat
data.frame=fetch(result)
#affectation du résultat a res1 c'est pour l'utiliser aprés 
res1<-data.frame
#afficher le resultat 
res1
##################   8/1/2007 ##############

#la meme requête  le nombre des arrivées ,
#le temps d'attends moyen ,pour chaque heure de 
#la journée 8/1/2007 sur le département 1
#Avec les meme contraines que avant 
result=dbSendQuery(con,"SELECT COUNT(patient_id) as nbr, HOUR(entry_date) as heurARRIVER,DAY(entry_date) as day,MONTH(entry_date) as mois
               ,(count(DAY(entry_date)))as nbrday,
               ((sum(hospitalization_time - entry_time) / count(patient_id))/3600)as temps 
               FROM visits where hospitalization_time - entry_time > 0 
               and (hospitalization_time - entry_time)<86400
               and YEAR(entry_date)=2007 
               and MONTH( entry_date )=1 
               and DAY(entry_date)=8
                   AND first_department=1
                   group by heurARRIVER")
data.frame=fetch(result)
#affectation du résultat a res2 c'est pour l'utiliser aprés
res2<-data.frame
##################   15/1/2007 ##############
#la meme requête le nombre des arrivées ,
#le temps d'attends moyen ,pour chaque heure de 
#la journée 15/1/2007 sur le département 1
#Avec les meme contraines que avant 
result=dbSendQuery(con,"SELECT COUNT(patient_id) as nbr, HOUR(entry_date) as heurARRIVER,DAY(entry_date) as day,MONTH(entry_date) as mois
               ,(count(DAY(entry_date)))as nbrday,
               ((sum(hospitalization_time - entry_time) / count(patient_id))/3600)as temps 
               FROM visits where hospitalization_time - entry_time > 0 
               and (hospitalization_time - entry_time)<86400 
               and YEAR(entry_date)=2007 
               and MONTH( entry_date )=01 
               and DAY(entry_date)=15 
                   AND first_department=1
                   group by heurARRIVER")
data.frame=fetch(result)
#affectation du résultat a res2 c'est pour l'utiliser aprés
res3<-data.frame
#AFFICHER les 3 data frame res3 res2 res1
res3
res2
res1
#Utiliser merge pour marger les deux res1 et res2 dans data.fram1
#heur d'arriver et nombre de personne en attente
data.fram1<-merge(select(res1, heurARRIVER, nbr), select(res2, heurARRIVER, nbr), by ="heurARRIVER") 
#Utiliser merge pour marger les deux res3 et data.fram2 dans data.fram2
#heur d'arriver et nombre de personne en attente
data.fram2<-merge(select(res3, heurARRIVER, nbr),data.fram1, by ="heurARRIVER") 
#Afficher le merger des trois res1 res2 res3
#AVEC heur d'arriver et nombre de personne en attente
data.fram2
#AFFecter
moyenne <-vector("numeric",23)
#boucle for
for(i in 1:23) {   
  data<-c(data.fram2$nbr[i],data.fram2$nbr.x[i],data.fram2$nbr.y[i])
  moyenne[i]<-mean(data) 
}
#afficher
moyenne
#afficter data.fram2e et moyenne a cc
cc<-data.frame(heurARRIVER=data.fram2$heurARRIVER,nbr=moyenne)
#afficher heure arriver
cc$heurARRIVER
#faire le merge parport a heurARRIVER
data.fram3<-merge(cc,data.fram2, by ="heurARRIVER")
#afficher le resultat de marge
data.fram3
#affictation
Personne_attente=res1$nbr
#affictation
heure=res1$h
#affictation
heurees=data.fram2
#a c'est 4courbes qui contient Le nombre de personne en attentes
#pour le departement Emergency Internal Medicine Unit pandant les 24heurs de la journée
#avec les 3 lundi le 1,8,et 15 de janvier 2007 
#avec une courbe moyenne des 3 courbe (1,8,15)

a<-ggplot()+
  geom_line(data=res1,aes(y=Personne_attente,x= heure,colour="black"),size=0.5 )+
  geom_line(data=res2,aes(y=res2$nbr,x= res2$h,colour="blue"),size=0.5 )+
  geom_line(data=res3,aes(y=res3$nbr,x= res3$h,colour="green"),size=0.5 )+
  geom_line(data=cc,aes(y=cc$nbr,x= cc$heurARRIVER,colour="red"),size=1 )+
  labs(title="Le nombre de personne en attentes pour le 
          departement Emergency Internal 
                  Medicine Unit")+
  scale_color_discrete(name="jour",labels = c("LUN 01-01-07","LUN 08-01-07","LUN 15-01-07","Courbe moyenne"))
#Utiliser merge pour marger les deux res1 et res3 dans data1
#heur d'arriver et le temps d'attente moyen
data1<-merge(select(res2, heurARRIVER, temps), select(res3, heurARRIVER, temps), by ="heurARRIVER") 
#Utiliser merge pour marger les deux res1 et data1 dans data2
#heur d'arriver et le temps d'attente moyen
data2<-merge(select(res1, heurARRIVER, temps),data1, by ="heurARRIVER") 
#Afficher le merger des trois res1 res2 res3
#AVEC heur d'arriver et LE Temps d'atente moyen
data2

moyenne2 <-vector("numeric",23)
#UTILISER UNE BOUCLE FOR 
for(i in 1:23) {   
  data3<-c(data2$temps[i],data2$temps.x[i],data2$temps.y[i])
  moyenne2[i]<-mean(data3) 
}
#AFFICHER LA moyenne2
moyenne2
#AFFECTATION 
moyenne_Temps<-data.frame(heurARRIVER=data.fram2$heurARRIVER,tesmp=moyenne2)
#AFFECTATION 
temps_attentes=res1$temps
#AFFECTATION 
heures=res1$h
#AFFECTATION 
temps_moyenne<-moyenne_Temps$tesmp
#AFFECTATION 
heur<-moyenne_Temps$heurARRIVER
#AFFECTATION 
moyenne_Temps$heurARRIVER
#AFFICHER
heur
#AFFICHER
res1
#b c'est 4courbes qui contient Temps d'attentes moyen pour le 
#departement Emergency Internal Medicine Unit pendant les 24heurs de la journée
#avec les 3 lundi le 1,8,et 15 de janvier 2007 
#avec une courbe moyenne des 3 courbes (1,8,15)
b<-ggplot()+
  geom_line(data=res1,aes(y=temps_attentes,x= heures,colour="black"),size=0.5 )+
  geom_line(data=res2,aes(y=res2$temps,x= res2$h,colour="bleu"),size=0.5 )+
  geom_line(data=res3,aes(y=res3$temps,x= res3$h,colour="green"),size=0.5 )+
  geom_line(data=moyenne_Temps,aes(y=temps_moyenne,x= heur,colour="red"),size=1 )+
  labs(title="Temps d'attentes moyen pour le 
     departement Emergency Internal 
            Medicine Unit")+
  scale_color_discrete(name="Courbe",labels = c("LUN 01-01-07","LUN 08-01-07","LUN 15-01-07","Courbe moyenne"))
#c c'est 2courbes qui contient le temps d'attentes moyen 
#et nombre de personne moyen en attente durant les trois lundis
#la moyenne des trois lundis 
c<-ggplot()+
  geom_line(data=moyenne_Temps,aes(y=temps_moyenne,x= heur,colour="green"),size=0.5 )+
  geom_line(data=cc,aes(y=cc$nbr,x= cc$heurARRIVER,colour="magenta"),size=0.5 )+
  labs(title="Temps d'attentes moyen et nombre de personne moyen en
                     attente durant les trois jour ")+
  scale_color_discrete(name="Courbe",labels = c("Temps d'attentes","nombre de personne en attente"))
# plot_grid pour regrouper les 3 graphes a b et c dans un seul graphe
plot_grid(
  a, NULL, NULL, b, c, NULL,
  ncol = 2,
  label_size = 12,
  align = "v"
)
#ranger bien les 3 graphes
plot_grid((a + b), c)
bottom_row <- plot_grid(a, b, labels = c('A', 'B'), label_size = 12)
plot_grid(bottom_row, c, labels = c('', 'C'), label_size = 12, ncol = 1)

###############################################################################
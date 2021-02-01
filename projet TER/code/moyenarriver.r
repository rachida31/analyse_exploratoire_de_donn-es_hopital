#########################     figure12   ######################################################

dbDisconnect(con)
library(RMySQL)
library(dbConnect) 
#faire une connexion a la base de donner mysql rambam 
con=dbConnect(MySQL(),username='root',dbname='rambam',host='localhost')

#################### 1 JOUR DE LA SEMAINE  avg1  #########################

#nombre des patiens on indique la semaine heure jour 
#POUR l'année 2007 ,mois janvier,Département 1 ,1er jour de la semaine
result=dbSendQuery(con,"SELECT

#utiliser le format de la semaine (il nous retourne le nemero de la semaine)
DATE_FORMAT(v.entry_date,  \"%U\") as semaine,
#pour retourner format heure exmpl 00 ,12,23
DATE_FORMAT(v.entry_date,  \"%H\") as heur,
#pour retourner le format de jour SUN
DATE_FORMAT(v.entry_date,  \"%a\") as jour,
# nombre DE patient
COUNT(*) as nombre_patient
FROM visit_details v 
where 
YEAR(v.entry_date)=2007 
and v.department=1 
#le 1 jour de la semaine 
and  dayofweek(v.entry_date)=1 
and month(v.entry_date)=1 
GROUP by DATE_FORMAT(v.entry_date, \"%H\"),DATE_FORMAT(v.entry_date,  \"%U\")
")
#recuperer le resultat
data.frame=fetch(result)
#afficher
print(data.frame)
#affectation du résultat  a res1 c'est pour l'utiliser aprés
res1<-data.frame
#avg1 nombre des patiens moyen pendant les 24h
avg1<-aggregate(nombre_patient ~ heur, data = res1 , mean)
#################### 2 JOUR DE LA SEMAINE avg2  ########################
#LE MEME TRATMENT QUE LE 1 JOUR DE LA SEMAINE
result=dbSendQuery(con,"SELECT DATE_FORMAT(v.entry_date,  \"%U\") as semaine,
DATE_FORMAT(v.entry_date,  \"%H\") as heur,
DATE_FORMAT(v.entry_date,  \"%a\") as jour,
COUNT(*) as nombre_patient
FROM visit_details v where 
YEAR(v.entry_date)=2007 
and v.department=1 
and  dayofweek(v.entry_date)=2 and month(v.entry_date)=1 
GROUP by DATE_FORMAT(v.entry_date, \"%H\"),DATE_FORMAT(v.entry_date,  \"%U\")
")
data.frame=fetch(result)
print(data.frame)
res2<-data.frame
avg2<-aggregate(nombre_patient ~ heur, data = res2 , mean)

#################### 3 JOUR DE LA SEMAINE  avg3 #########################
#LE MEME TRATMENT QUE LE 1 JOUR DE LA SEMAINE

result=dbSendQuery(con,"SELECT DATE_FORMAT(v.entry_date,  \"%U\") as semaine,
DATE_FORMAT(v.entry_date,  \"%H\") as heur,
DATE_FORMAT(v.entry_date,  \"%a\") as jour,
COUNT(*) as nombre_patient
FROM visit_details v where 
YEAR(v.entry_date)=2007 
and v.department=1 
and  dayofweek(v.entry_date)=3 and month(v.entry_date)=1 
GROUP by DATE_FORMAT(v.entry_date, \"%H\"),DATE_FORMAT(v.entry_date,  \"%U\")
")
data.frame=fetch(result)
print(data.frame)
res3<-data.frame
avg3<-aggregate(nombre_patient ~ heur, data = res3 , mean)

#################### 4 JOUR DE LA SEMAINE avg4  #########################
#LE MEME TRATMENT QUE LE 1 JOUR DE LA SEMAINE

result=dbSendQuery(con,"SELECT DATE_FORMAT(v.entry_date,  \"%U\") as semaine,
DATE_FORMAT(v.entry_date,  \"%H\") as heur,
DATE_FORMAT(v.entry_date,  \"%a\") as jour,
COUNT(*) as nombre_patient
FROM visit_details v where 
YEAR(v.entry_date)=2007 
and v.department=1 
and  dayofweek(v.entry_date)=4 and month(v.entry_date)=1 
GROUP by DATE_FORMAT(v.entry_date, \"%H\"),DATE_FORMAT(v.entry_date,  \"%U\")
")
data.frame=fetch(result)
print(data.frame)
res4<-data.frame
avg4<-aggregate(nombre_patient ~ heur, data = res4 , mean)

#################### 5 JOUR DE LA SEMAINE avg5  #########################
#LE MEME TRATMENT QUE LE 1 JOUR DE LA SEMAINE

result=dbSendQuery(con,"SELECT DATE_FORMAT(v.entry_date,  \"%U\") as semaine,
DATE_FORMAT(v.entry_date,  \"%H\") as heur,
DATE_FORMAT(v.entry_date,  \"%a\") as jour,
COUNT(*) as nombre_patient
FROM visit_details v where 
YEAR(v.entry_date)=2007 
and v.department=1 
and  dayofweek(v.entry_date)=5 and month(v.entry_date)=1 
GROUP by DATE_FORMAT(v.entry_date, \"%H\"),DATE_FORMAT(v.entry_date,  \"%U\")
")
data.frame=fetch(result)
print(data.frame)
res5<-data.frame
avg5<-aggregate(nombre_patient ~ heur, data = res5 , mean)

#################### 6 JOUR DE LA SEMAINE avg6  #########################
#LE MEME TRATMENT QUE LE 1 JOUR DE LA SEMAINE

result=dbSendQuery(con,"SELECT DATE_FORMAT(v.entry_date,  \"%U\") as semaine,
DATE_FORMAT(v.entry_date,  \"%H\") as heur,
DATE_FORMAT(v.entry_date,  \"%a\") as jour,
COUNT(*) as nombre_patient
FROM visit_details v where 
YEAR(v.entry_date)=2007 
and v.department=1 
and  dayofweek(v.entry_date)=6 and month(v.entry_date)=1 
GROUP by DATE_FORMAT(v.entry_date, \"%H\"),DATE_FORMAT(v.entry_date,  \"%U\")
")
data.frame=fetch(result)
print(data.frame)
res6<-data.frame
avg6<-aggregate(nombre_patient ~ heur, data = res6 , mean)

#################### 7 JOUR DE LA SEMAINE avg7  #########################
#LE MEME TRATMENT QUE LE 1 JOUR DE LA SEMAINE

result=dbSendQuery(con,"SELECT DATE_FORMAT(v.entry_date,  \"%U\") as semaine,
DATE_FORMAT(v.entry_date,  \"%H\") as heur,
DATE_FORMAT(v.entry_date,  \"%a\") as jour,
COUNT(*) as nombre_patient
FROM visit_details v where 
YEAR(v.entry_date)=2007 
and v.department=1 
and  dayofweek(v.entry_date)=7 and month(v.entry_date)=1 
GROUP by DATE_FORMAT(v.entry_date, \"%H\"),DATE_FORMAT(v.entry_date,  \"%U\")
")
data.frame=fetch(result)
print(data.frame)
res7<-data.frame
avg7<-aggregate(nombre_patient ~ heur, data = res7 , mean)

#UTILISER les 7 resultats de avg pour dissiner un graphe
#qui regroupe les 7 courbes 
#le graphe contient 7 courbes pour chaque jour de la semaine 
#l'axe x c'est l heure d arriver et laxe y c'est nombre des patients qui sont entree au departement 1
#geom_line pour une line contunier avec x ,y la colour et la taille de la line
# title pour faire un titre 
#pour faire le sous titre a droite
ggplot()+
  geom_line(data=avg1,aes(y=nombre_patient,x= heur,colour="a",group = 1),size=0.5 )+
  geom_line(data=avg2,aes(y=nombre_patient,x= heur,colour="b",group = 1),size=0.5 )+
  geom_line(data=avg3,aes(y=nombre_patient,x= heur,colour="c",group = 1),size=0.5 )+
  geom_line(data=avg4,aes(y=nombre_patient,x= heur,colour="d",group = 1),size=0.5 )+
  geom_line(data=avg5,aes(y=nombre_patient,x= heur,colour="e",group = 1),size=0.5 )+
  geom_line(data=avg6,aes(y=nombre_patient,x= heur,colour="f",group = 1),size=0.5 )+
  geom_line(data=avg7,aes(y=nombre_patient,x= heur,colour="g",group = 1),size=0.5 )+
  
  labs(title="
Nombre moyen de patients par jour de semaine dans le departement 
                      Emergency Internal Medicine Unit")+
  scale_color_discrete(name="Courbe",labels = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"))
###############################################################################


#=====================Calcul de corrélation============================

#*********************************entre nombre d'arrivée et le jour d'arrivée*****************************
rq0=dbGetQuery(con," SELECT DATE_FORMAT(v.entry_date, \"%j\") as jour,
               COUNT(*)FROM visit_details v 
               WHERE  YEAR(v.entry_date)=2007 and 
               v.department=1 and month(v.entry_date)=1 
               GROUP by DATE_FORMAT(v.entry_date, \"%j\") 
               order by jour;")
rq0$jour<-sort(as.integer(rq0$jour))
#la fo,ction de calcul de corrélation
res<-cor.test(rq0$jour,rq0$`COUNT(*)`, method="pearson")
ggplot(rq0, aes(x=jour, y=`COUNT(*)`)) + geom_point(shape=20, color="#00BFFF",size=2)+
  geom_smooth(method=lm, se=FALSE,color="#DC143C")+ scale_x_continuous(name="jours") +scale_y_continuous(name="Nombre d'arrivées ") +
  ggtitle("Corrélation entre nombres d'arrivée et  le mois de janvier 2007")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold"))
#*********************************entre nombre d'arrivée et l'age*****************************
rq0=dbGetQuery(con,"SELECT v.age_years as age, COUNT(*) FROM visits v WHERE
               v.entry_group='1' AND YEAR(v.entry_date)=2007 and 
               v.first_department=1 and month(v.entry_date)=1 and  
               v.age_years<100 and v.age_years<>0 group by v.age_years")
res<-cor.test(rq0$age,rq0$`COUNT(*)`, method="pearson")
p1<-ggplot(rq0, aes(x=age, y=`COUNT(*)`))+ geom_point(shape=20, color="#00BFFF",size=2)+
  geom_smooth(method=lm, se=FALSE,color="#DC143C")+ scale_x_continuous(name="Age") +scale_y_continuous(name="Nombre d'arrivées ") +
  ggtitle("Corrélation entre nombres d'arrivée et leurs ages")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold"))

#*********************************entre nombre d'arrivée et le temps d'attante *****************************
rq0=dbGetQuery(con,"SELECT count(*) as count,DAY(entry_date) as day,MONTH(entry_date) as moins
               FROM visits where hospitalization_time - entry_time > 0 
               and (hospitalization_time - entry_time)<86400 and YEAR(entry_date)=2007 and  
                   MONTH( entry_date )=1 and first_department=1  group by day")

rq1=dbGetQuery(con,"SELECT DAY(entry_date) as day,MONTH(entry_date) as moins
               ,(count(DAY(entry_date)))as nbrday,
               ((sum(hospitalization_time - entry_time) / count(patient_id))/3600)as temps 
               FROM visits where hospitalization_time - entry_time > 0 
               and (hospitalization_time - entry_time)<86400 and YEAR(entry_date)=2007 and  
#                   MONTH( entry_date )=1 and first_department=1  group by day")


d<-data.frame(rq0$count,rq1$temps,rq0$day)
p1<-ggplot(d, aes(y=d$rq1.temps, x=d$rq0.count))+ geom_point(shape=20, color="#00BFFF",size=2)+
  geom_smooth(method=lm, se=FALSE,color="#DC143C")+ scale_x_continuous(name="Nombre d'arrivées") +scale_y_continuous(name=" Temps d'attente ") +
  ggtitle("Corrélation entre nombres d'arrivée et leurs temps d'attente")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="grey", size=14, face="bold",hjust = 0.5),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold"))
p1


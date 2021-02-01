
con=dbConnect(MySQL(),user='root',dbname='rambam2007',host='localhost')

rq=dbSendQuery(con,"SELECT COUNT(*) FROM xrays_visits  ")
data.frame=fetch(rq)
r<-data.frame
print(r)


rq=dbSendQuery(con,"SELECT COUNT(*) FROM xrays_visits WHERE  age_years>=0 AND age_years<20")
data.frame=fetch(rq)
r1<-(data.frame*100)/r
print(r1)

rq=dbSendQuery(con,"SELECT COUNT(*) FROM xrays_visits WHERE  age_years>=20 AND age_years<40")
data.frame=fetch(rq)
r2<-(data.frame*100)/r
print(r2)

rq=dbSendQuery(con,"SELECT COUNT(*) FROM xrays_visits WHERE  age_years>=40 AND age_years<60")
data.frame=fetch(rq)
r3<-(data.frame*100)/r
print(r3)

rq=dbSendQuery(con,"SELECT COUNT(*) FROM xrays_visits WHERE  age_years>=60 AND age_years<80")
data.frame=fetch(rq)
r4<-(data.frame*100)/r
print(r4)


rq=dbSendQuery(con,"SELECT COUNT(*) FROM xrays_visits WHERE  age_years>=80 AND age_years<100")
data.frame=fetch(rq)
r5<-(data.frame*100)/r
print(r5)


H<-c(13,21,39,24,3)
print(H)

M<-c("0-20","20-40","40-60","60-80","80-100")


print(M)

pie(H,labels = paste(M,"\n",round(prop.table(H)*100,1),"%") ,  radius =1.02,cex=0.8,main="XRAYS_VISITS TABLE",sub="TRANCHES D'AGES DES PATIENTS AYANT EFFECTUE DES RADIOS

    											EN 2007 ")
###############################################################################################

on.exit(dbDisconnect(con)
        con=dbConnect(MySQL(),user='root',dbname='rambam2004',host='localhost')
        
        rq=dbSendQuery(con,"SELECT COUNT(*) FROM xrays_visits ")
        data.frame=fetch(rq)
        r<-data.frame
        print(r)
        
        rq=dbSendQuery(con,"SELECT COUNT(*) FROM xrays_visits WHERE gender='1'")
        data.frame=fetch(rq)
        r11<-(data.frame*100)/r
        print(r11)
        rq=dbSendQuery(con,"SELECT COUNT(*) FROM xrays_visits WHERE gender='2'")
        data.frame=fetch(rq)
        r12<-(data.frame*100)/r
        print(r12)
        
        H<-c(48,52)
        print(H)
        M<-c("Female","Mal")
        pie(H,labels = paste(M,"\n",round(prop.table(H)*100,1),"%") ,  radius =1.02,cex=0.8,main="VISITS TABLE",sub="TAUX D'INDIVIDUS AYANT FAIT DES VISITES
											 EN 2007 SELON LE SEXE ")
        
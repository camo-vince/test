
#install.packages("readxl")
library("readxl")
# xls files
setwd('D://Téléchargements vrac//sysnav')
rem_im=read_excel("2020-02-11 - Données Test - Data Scientist.xlsx",sheet=1)  #1ere 
rem_km_serv=as.data.frame(read_excel("2020-02-11 - Données Test - Data Scientist.xlsx",sheet=2))
rem_km_prest=as.data.frame(read_excel("2020-02-11 - Données Test - Data Scientist.xlsx",sheet=3))

#détecter les anomalies
#calculer les distances par journée de rem_km_serv
km_serveur=c()
diff=c()
diff_pcent=c()
n_photo=c()
rem_km=rem_km_prest
for (i in 1:nrow(rem_km_prest)){
  d_h_vehicle=subset(rem_km_serv,vehicle_id==rem_km_prest[i,3] & datemission==rem_km_prest[i,1])
  d_journee_vehicle=sum(d_h_vehicle[,3])
  km_serveur=append(km_serveur,d_journee_vehicle/1000)
  diff=append(diff,rem_km_prest[i,2]-d_journee_vehicle/1000)
  diff_pcent=append(diff_pcent,(rem_km_prest[i,2]-d_journee_vehicle/1000)/rem_km_prest[i,2])
  p_h_vehicle=subset(rem_im,vehicle_id==rem_km_prest$Vehicle_id[i] & datemission==rem_km_prest$datemission[i])
  n_photo=append(n_photo,sum(p_h_vehicle$count_images))
}
rem_km=cbind(rem_km,km_serveur)
rem_km=cbind(rem_km,diff)
rem_km=cbind(rem_km,diff_pcent)
rem_km=cbind(rem_km,n_photo)

#détecter lorsque le syst est en panne
rem_km_pannesyst=subset(rem_km,rem_km$km_serveur==0) #tableau panne syst
rem_km=subset(rem_km,rem_km$km_serveur!=0) #rem_km sans les valeurs zeros
tb_panne= table(rem_km_pannesyst$datemission,rem_km_pannesyst$Vehicle_id)#jours de panne par voiture
somme_jours_panne=as.data.frame(colSums(tb_panne)) #somme jours de panne par voiture


hist(rem_km$diff,50,main="histogramme écarts mesurés",xlab="écarts km serveur km système",ylab="fréquence")

hist(rem_km$diff_pcent,50,main="histogramme écarts mesurés en %",xlab="écarts km serveur km système",ylab="fréquence")



# classification via gaussienne 3 sigmas
nb_ok=0
moyenne_val=mean(rem_km$diff_pcent)
trois_sigma=3*sqrt(var(rem_km$diff_pcent))
classification=c()
for (i in 1:nrow(rem_km)){
  if ((moyenne_val-trois_sigma)<rem_km$diff_pcent[i] & rem_km$diff_pcent[i]<(moyenne_val+trois_sigma)){
    nb_ok=nb_ok+1
    classification=append(classification,FALSE)
  }
  else{
    classification=append(classification,TRUE)
  }
}
nb_pb=nrow(rem_km)-nb_ok

rem_km=cbind(rem_km,classification) 
j=0
for (k in 1:length(classification)){
  if (classification[k]==TRUE){
    j=j+1}
}

#j nbre de valeurs problématiques

rem_km_ok=subset(rem_km,rem_km$classification==FALSE) #data ou les valeurs sont dans l'intervalle tolérance
rem_km_pb=subset(rem_km,rem_km$classification==TRUE) #data en dehors de l'intervalle de tolérance

#histogramme des valeurs problématiques et de celles correctes
to.plot = data.frame(x = rem_km$diff_pcent,
                     label =rem_km$classification)
ggplot(to.plot, aes(x, color = label, fill = label))+xlab("Ecart en pourcentage")+ylab("Nombre d'occurence")+
  geom_histogram(alpha=0.5, position="identity", bins = 50)+
  theme(legend.position = "bottom")

#histogramme de la fréquence des problèmes en fonction du véhicule
tb_pb_instal=as.data.frame(table(rem_km_pb$Vehicle_id))
tb_pb_instal_j=table(rem_km_pb$datemission,rem_km_pb$Vehicle_id) #table des erreurs croisant la date et la voiture concernée

hist(rem_km_pb$Vehicle_id,nrow(tb_pb_instal))
moyenne_pb=mean(tb_pb_instal$Freq)

#histogramme de la frequence des problèmes en fonction de la date d'émission
tb_pb_date=as.data.frame(table(rem_km_pb$datemission))
hist(rem_km_pb$datemission,nrow(tb_pb_date),freq=TRUE,main="frequence des problèmes",xlab="date")

library(MASS)


#plotter nb photo valeurs heures par heures 

km=c()#nombre de km parcourus heure par heure
for (i in 1:nrow(rem_im)){
  tab_km=subset(rem_km_serv,vehicle_id==rem_im$vehicle_id[i] & datemission==rem_im$datemission[i] & hour==rem_im$hour[i])
  km=append(km,sum(tab_km$surveyed_roads_meters)/1000)
  
}
km=round(km,2)
rem_im=cbind(rem_im,km)

class=c()
for (i in 1:nrow(rem_im)){
  cl=subset(rem_km,datemission==rem_im$datemission[i] & Vehicle_id==rem_im$vehicle_id[i])
  class=append(class,cl$classification[1])
}
rem_im=cbind(rem_im,class)


#date en abscisse, nb de photo / jour en ordonnée
data3 = data.frame(x1 = rem_km$datemission,x2 = rem_km$n_photo, label=rem_km$classification)

library(ggplot2)
library(ggExtra)
library(ggpubr)
p=ggplot(data3, aes(x1, x2, color = label))+
  geom_point() +xlab("date")+ylab("nombre de photo/jour")
  theme(legend.position = "bottom")
p1 = ggMarginal(p, groupFill= TRUE,groupColour = TRUE,type = "histogram",margins='y')
ggarrange(p1)
  


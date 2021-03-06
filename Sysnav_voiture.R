
#install.packages("readxl")
library("readxl")
# xls files
setwd('D://T�l�chargements vrac//sysnav')
rem_im=read_excel("2020-02-11 - Donn�es Test - Data Scientist.xlsx",sheet=1)  #1ere 
rem_km_serv=as.data.frame(read_excel("2020-02-11 - Donn�es Test - Data Scientist.xlsx",sheet=2))
rem_km_prest=as.data.frame(read_excel("2020-02-11 - Donn�es Test - Data Scientist.xlsx",sheet=3))

#d�tecter les anomalies
#calculer les distances par journ�e de rem_km_serv
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

#d�tecter lorsque le syst est en panne
rem_km_pannesyst=subset(rem_km,rem_km$km_serveur==0) #tableau panne syst
rem_km=subset(rem_km,rem_km$km_serveur!=0) #rem_km sans les valeurs zeros
tb_panne= table(rem_km_pannesyst$datemission,rem_km_pannesyst$Vehicle_id)#jours de panne par voiture
somme_jours_panne=as.data.frame(colSums(tb_panne)) #somme jours de panne par voiture


hist(rem_km$diff,50,main="histogramme �carts mesur�s",xlab="�carts km serveur km syst�me",ylab="fr�quence")

hist(rem_km$diff_pcent,50,main="histogramme �carts mesur�s en %",xlab="�carts km serveur km syst�me",ylab="fr�quence")

library(ggplot2)
library(mvtnorm)
library(radiant)
library(reshape)


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

#j nbre de valeurs probl�matiques

rem_km_ok=subset(rem_km,rem_km$classification==FALSE) #data ou les valeurs sont dans l'intervalle tol�rance
rem_km_pb=subset(rem_km,rem_km$classification==TRUE) #data en dehors de l'intervalle de tol�rance

#histogramme des valeurs probl�matiques et de celles correctes
to.plot = data.frame(x = rem_km$diff_pcent,
                     label =rem_km$classification)
ggplot(to.plot, aes(x, color = label, fill = label))+xlab("Ecart en pourcentage")+ylab("Nombre d'occurence")+
  geom_histogram(alpha=0.5, position="identity", bins = 50)+
  theme(legend.position = "bottom")

#histogramme de la fr�quence des probl�mes en fonction du v�hicule
tb_pb_instal=as.data.frame(table(rem_km_pb$Vehicle_id))
tb_pb_instal_j=table(rem_km_pb$datemission,rem_km_pb$Vehicle_id) #table des erreurs croisant la date et la voiture concern�e

hist(rem_km_pb$Vehicle_id,nrow(tb_pb_instal))
moyenne_pb=mean(tb_pb_instal$Freq)

#histogramme de la frequence des probl�mes en fonction de la date d'�mission
tb_pb_date=as.data.frame(table(rem_km_pb$datemission))
hist(rem_km_pb$datemission,nrow(tb_pb_date),freq=TRUE,main="frequence des probl�mes",xlab="date")

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


#date en abscisse, nb de photo / jour en ordonn�e
data3 = data.frame(x1 = rem_km$datemission,x2 = rem_km$n_photo, label=rem_km$classification)

library(ggplot2)
library(ggExtra)
library(ggpubr)
p=ggplot(data3, aes(x1, x2, color = label))+
  geom_point() +xlab("date")+ylab("nombre de photo/jour")
  theme(legend.position = "bottom")
p1 = ggMarginal(p, groupFill= TRUE,groupColour = TRUE,type = "histogram",margins='y')
ggarrange(p1)
  


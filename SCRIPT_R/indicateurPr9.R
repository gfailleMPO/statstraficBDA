#############################################################
#############################################################
#########script indicateur PR9           ####################
##############################################FROBIN02#######

# les library

library(dplyr)
library(reshape2)
library("readxl")
library(stringr )
library(common)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(Hmisc)

# le chemin  

allo<-"Y:/AMP_Refuges/Projets/BdA/Donnees/Traffic_maritime/InovJAN2024/livrable-clean-2024/Transits"
analyse<-"Y:/AMP_Refuges/Projets/BdA/Donnees/Traffic_maritime/AnalysesRepSciencDec2023/V2_jan2024"
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### concatenation  du jeu de données  ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
Querry1 <-function(){
  q1<-yesno::yesno2("\n Voulez concatener les fichiers (1) (il doivent contenir mpo_data.csv) ou Charger (2) le fichier complet")
  if(q1==TRUE){
     cat(paste("Attention tous les fichier sont dans ",allo,"\n en cours de traitement"))
    setwd(allo)
    files= list.files(pattern="mpo_data.csv")
  ind<-list(NULL)
for (i in files){
  X<- read.csv2(i,head=T,sep=",")
ind[[i]]<-X
}
IND<-do.call("rbind",ind)
setwd(analyse)

write.csv(IND,"IndicPr9_ALL_DATA_transit.csv",row.names=FALSE)
cat(" \n c'est concatener dans IndicPr9_ALL_DATA_transit.csv.csv et chargé pour le script")
}else{
    setwd(analyse)
   IND<- read.csv2("IndicPr9_ALL_DATA_transit.csv",header=T,sep=",")
   cat(" \n le fichier  IndicPr9_ALL_DATA_transit.csv.csv est chargé pour le script")
}
  return(IND)  
}
IND<-Querry1()

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Date limitation vitesse  baleine noire           ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

setwd("Y:/AMP_Refuges/Projets/BdA/Donnees/Traffic_maritime")
tp <-read_excel("Dates_limites_vitesse_baleineN.xlsx", sheet = 1)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Formatage du jeux de données            ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


names(IND)[c(1,5:6,11:14)]<-c("Annee","Category_Type","Category_Longueur","Duree_en_jours","Vitesse_moyenne","Vitesse_min","Vitesse_max") # Attention si l'ordre des variables change
IND$Category_Type<-gsub("Opération maritimes","Opérations maritimes",IND$Category_Type)#correction du 's'
IND$Category_Type2<-ifelse(IND$Category_Type== "Marchands" | IND$Category_Type=="Passagers","Commercial",IND$Category_Type) # creation d un nouveau champ 
cat( "n\ Les Marchands et Passagers sont compris dans Commercial") # crée une catégorie qui met passaers + marchands dans commercial

IND$UP10<-ifelse(as.numeric(IND$Vitesse_max)>10,"SPEED","SLOW")# pas vite vs trop vite
IND$Debut<-as.POSIXct(IND$Début,format("%Y-%m-%d %H:%M"),tz = "UTC")

for (i in 1:dim(IND)[1]){
 if(IND$Annee[i]%in%tp$Annee==F){
   IND$PERIOD[i]<-"out"
 }else{
  test<- vector()
  for(j in 1:dim(tp)[1]){
  test[j]<-between(as.numeric(as.Date(IND$Debut[i])),
                as.numeric(as.Date(tp$Deb[j]),format=format("%Y-%m-%d")),
                as.numeric(as.Date(tp$fin[j]),format=format("%Y-%m-%d")))}
  test<-sum(ifelse(test==TRUE,1,0))
  
  if(test==1){
    IND$PERIOD[i]<-"in"
  }else {IND$PERIOD[i]<-"out"}
  }}### PERIOD : creation in et out periode baleine noire

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### creation un jeu de données journalier des jours avec limitation ### ### ### car ligne avec plusieurs années ;) 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
timeserie  <-as.data.frame(seq(as.Date("2012-01-01"), as.Date(Sys.time()),"days"))
names(timeserie)<-c("date")
for (i in 1:dim(timeserie)[1]){
  find<- vector()
  
  for(j in 1:dim(tp)[1]){
    find[j]<-between(as.numeric(as.Date(timeserie$date[i])),
                     as.numeric(as.Date(tp$Deb[j]),format=format("%Y-%m-%d")),
                     as.numeric(as.Date(tp$fin[j]),format=format("%Y-%m-%d")))}
timeserie$IN[i]<-sum(ifelse(find==TRUE,1,0))
}
timeserie$Annee<-year(as.Date(timeserie$date))
nombrein <-as.data.frame(tapply(timeserie$IN,list(timeserie$Annee),sum))
nombrein$Annee<-row.names(nombrein)
names(nombrein)[1]<-c("nombre.in")
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

IND$GROUPYEAR<-paste(IND$Category_Type2,IND$Annee,sep="XX")### code de flemard pour factoriser plusieurs variables

# test <-as.data.frame(tapply(IND$UP10,list(IND$GROUPYEAR,IND$UP10),length))
# test$SUM <-rowSums(test)
# test$pourc10<- test$SPEED/test$SUM*100
# all <-cbind(test,str_split_fixed(rownames(test),"XX",2))
# names(all)[5:6]<-c("Category_Type","Annee")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### creation du tableau in out periode et speed up to 10 ## ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
IND$N<-1 # Génère un efectif 1/ligne
test2 <-as.data.frame(tapply(IND$N,list(IND$GROUPYEAR,IND$PERIOD,IND$UP10),sum,na.rm=T)) # Création d'une liste style tableau croisé dynamique
test2$sum <-rowSums(test2,na.rm=T) # Ajoute une variable somme du nombre de passages annuels (in, out, vite pas vite) 
test2 <-cbind(test2,str_split_fixed(rownames(test2),"XX",2)) # Déconcaténer le facteur de flemard òù il y a xx pour ravoir catégorie + année séparées
names(test2)[6:7]<-c("Category_Type","Annee")
test2[is.na(test2)]<-0
test2$in.prop.speed<-round(c(test2$in.SPEED/c(test2$in.SLOW+test2$in.SPEED))*100,0) # proportion en %
test2$out.prop.speed<-round(c(test2$out.SPEED/c(test2$out.SLOW+test2$out.SPEED))*100,0) # proportion en %
test2$totup10<-c(test2$in.SPEED+test2$out.SPEED)
test2<-powerjoin::power_full_join(test2,nombrein, fill = NA) # Fait une jointure avec le fichier timeseries (nombrein) nombre de jours dans l'année sous restriction
test2 <-test2[c("Category_Type","Annee","nombre.in", "sum", "totup10", "in.SLOW","in.SPEED","in.prop.speed","out.SLOW","out.SPEED","out.prop.speed")]
setwd(analyse) # On se replace dans le fichier analyse
test2$in.prop.speed[is.nan(test2$in.prop.speed)]<-0
test2$out.prop.speed[is.nan(test2$out.prop.speed)]<-0
# test2 <-droplevels(subset(test2,Category_Type%nin%NA))
test2 <-droplevels(subset(test2,Category_Type%in%c("AOM",  "Commercial", "Inconnu",  "Opérations maritimes", "Pêches", "Plaisanciers"))) # En attendant de résoudre le problème lié au package Hmisc
write.csv(test2,"IndicPr9_TAB_Vitesse_up10.csv",row.names=FALSE)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### creation du tableau vitesse moyenne annuelle des vitesses moyennes  ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

test <-as.data.frame(tapply(as.numeric(IND$Vitesse_moyenne),list(IND$Annee,IND$Category_Type2),mean,na.rm=T))
test[is.na(test)]<-0
test<-round(test,1)
test<-cbind(rownames(test),test)
names(test)[1]<-c("Annee")

setwd(analyse)

write.csv(test,"IndicPr9_TAB_Vitesse_moy.csv",row.names=FALSE)

teststd <-as.data.frame(tapply(as.numeric(IND$Vitesse_moyenne),list(IND$Annee,IND$Category_Type2),sd,na.rm=T))


#Packages utilises
library(tibble)
library(tidyr)
library(dplyr)
library(stats)
library(ggplot2)

#Chemin vers les donnees depuis mon ordinateur
setwd("/Users/victorfischer/Desktop/Q12/Mémoire/code")

#Recuperer les bases de donnnees 
dataNO2 <- read.csv("per-capita-nitrous-oxide.csv")
dataCarbon <- read.csv("co-emissions-per-capita.csv")

#Liste des pays etudies 
listePays=c("Norway", "Sweden", "Russia", "Poland", "Switzerland", "Luxembourg", "Turkey", "Bulgaria", "Iceland", "Denmark", "Serbia", "Romania")


taille=72 #minimum de colonnes parmi listePays, pour qu'il y ait les memse annees pour tout le monde

#Le nom du label pour les ordonnees (va servir pour les graphes)
#ylabel="Emissions [t eq CO2/hab/an]"
#ylabel="Emissions [t/hab/an]"

#filtre est une fonction qui prend une base de donnees en argument et renvoie les 72 derniers elements (en fonction de taille)
#Elle est utilisee dans clean
filtre = function(data){
  if(nrow(data)-taille==0){
    data
  }
  else{
    data[-c(1:(nrow(data)-taille)), ]
  }
}

#clean est une fonction qui prend une base de donnees en arguement et renvoie un dataframe avec les donnees qui nous interessent
clean = function(data){
  m=list() #liste qui va contenir toutes les donnees 
  for (i in 1:length(listePays)){
    dataPays=filter(data, Entity == listePays[i]) #recuperer les donnees d'un pays
    dataPays=filtre(dataPays) #recuperer les 72 dernieres donnees
    dataPays=dataPays[,-2] #enlever la colonne inutile
    m=rbind(m,dataPays) #ajouter les donnees de ce pays a m
  }
  #Transformation en data.frame en nommant les colonnes et les lignes
  as.data.frame(m)
  colnames(m)=c("Country", "Year", "Emissions")
  rownames(m)=1:(dim(m)[1])
  m$Country = factor(m$Country, levels = listePays) #garantit que les niveaux sont ordonnes selon l'ordre de listePays
  m
}

#check est une petite fonction verifiant que les donnees ont bien ete recuperees 
check = function(data){
  print(table(factor(data$Country, levels = listePays))==taille) #Est ce qu'il y a bien "taille" donnees pour chaque pays ? 
  for (i in 0:11){
    print(data[taille*i+1,2]==1950) #Est ce que les donnees commencent bien en 1950 ? 
  }
}

#regression est une fonction appliquant la methode Difference in Difference a data, en fonction de l'annee
#Le code a ete tres largement inspire de ce code : https://www.princeton.edu/~otorres/DID101R.pdf
regression = function(data, annee){
  data$time = ifelse(data$Year >= annee, 1, 0) 
  data$treated = ifelse(data$Country == "Sweden" |
                                data$Country == "Poland" |
                                data$Country == "Luxembourg" |
                                data$Country == "Bulgaria" |
                                data$Country == "Denmark" |
                                data$Country == "Romania", 1,0)
  
  didreg = lm(Emissions ~ treated*time, data = data) #Regression en tant que telle
  print(summary(didreg))
}

#plotSum est une fonction donnant la somme des emissions des 2 groupes sur le meme graphe, avec name le nom du fichier contenant le graphe
plotSum=function(data, name){
  m=matrix(nrow = taille, ncol=2)
  for(i in 1:taille){
    medIn=c()
    medOut=c()
    for (j in 1:6){
      medOut=c(medOut,data[(j*taille)+i,3])
      medIn=c(medIn,data[((j+1)*taille)+i,3])
    }
    m[i,1]=sum(medOut)
    m[i,2]=sum(medIn)
  }
  #Data.frame contenant toutes les donnees 
  dataMedian=data.frame(Country=c(rep("Non", taille), rep("Oui", taille)), 
                        Year=c(rep(1950:2021,2)), 
                        Emissions=c(m[,1], m[,2]))
  
  #Graphe avec ses options
  g2=ggplot(data = dataMedian) + 
    aes(x=Year,y=Emissions,color=Country) + 
    ylab(ylabel) + 
    geom_line() + 
    scale_color_manual(values = c("red", "chartreuse4")) +
    theme_bw() + 
    theme(axis.title.x=element_blank()) + 
    geom_vline(xintercept = 2005, color="black") +
    geom_vline(xintercept = 2008, color="black") +
    geom_vline(xintercept = 2013, color="black") +
    labs(color = "Dans le SEQE ?") + 
    theme(
      axis.title = element_text(size = 18),  
      axis.text = element_text(size = 13),  
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),  
    )
  print(g2)
  ggsave(name, plot=last_plot(), width=11, height = 5) #sauver le graphe 
}

#plotGraphs est une fonction non-utilisee dans ce rapport mais qui permet de comparer les emissions d'un pays face a son equivalent
plotGraphs=function(data){
  for (i in 1:6){
    #Graphes avec ses options
    g2 = ggplot(data = data[(2*(i-1)*taille+1):(2*i*taille),]) +
      aes(x = Year, y = Emissions, color = Country) +
      ylab(ylabel) +
      scale_color_manual(values =  c("red", "chartreuse4")) +
      geom_line() +
      theme_bw() +
      theme(axis.title.x=element_blank()) + 
      geom_vline(xintercept = 2005, color="black")
    print(g2)
  }
}
#plotGraphs(dataNO2)

#plotOneGraphs est une fonction renvoyant les emissions de tous les pays en fonction de leur groupe, avec name le nom du fichier contenant le graphe
plotOneGraphs=function(data, name){
  data$treated = ifelse(data$Country == "Sweden" |
                                data$Country == "Poland" |
                                data$Country == "Luxembourg" |
                                data$Country == "Bulgaria" |
                                data$Country == "Denmark" |
                                data$Country == "Romania", "Oui", "Non")
  #Graphe avec ses options
  g2 <- ggplot(data = data) +
    aes(x = Year, y = Emissions, group = Country) +
    geom_path(aes(color = treated)) +  
    ylab(ylabel) + 
    scale_color_manual(values = c("red", "chartreuse4")) + 
    theme_bw() +
    theme(axis.title.x=element_blank()) + 
    geom_vline(xintercept = 2008, color="black") + 
    labs(color = "Dans le SEQE ?") + 
    theme(
      axis.title = element_text(size = 18),  
      axis.text = element_text(size = 13),  
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),  
    )
  
  
  ggsave(name, plot=last_plot(), width=11, height = 5)
  print(g2)
}
#plotOneGraphs(dataNO2, "Global_emissions_NO2.png")

#ecart est une fonction calculant l'effet du traitement en fonction de l'annee 
#Technique prise de ce site : https://thetarzan.wordpress.com/2011/06/20/differences-in-differences-estimation-in-r-and-stata/
ecart = function(data,annee){
  data$time = ifelse(data$Year >= annee, 1, 0)
  data$treated = ifelse(data$Country == "Sweden" |
                                data$Country == "Poland" |
                                data$Country == "Luxembourg" |
                                data$Country == "Bulgaria" |
                                data$Country == "Denmark" |
                                data$Country == "Romania", 1,0)
  a = sapply(subset(data, time == 0 & treated == 0, select=Emissions), mean)
  b = sapply(subset(data, time == 0 & treated == 1, select=Emissions), mean)
  c = sapply(subset(data, time == 1 & treated == 0, select=Emissions), mean)
  d = sapply(subset(data, time == 1 & treated == 1, select=Emissions), mean)
  (d-c)-(b-a)
}

#piechart est une fonction qui renvoie un graphique "en camembert", avec name le nom du fichier contenant le graphe
#utilisation : differentes variables calculees, proportion : proportion de utilisation, mycols : couleur du graphe
#Le code s'est largement inspire de ce site : https://www.datanovia.com/en/fr/blog/comment-creer-un-camembert-dans-r-en-utilisant-ggplot2/
piechart = function(name, utilisation , proportion, mycols){
  #Creation d'un data.frame
  count.data <- data.frame(
    Origine = utilisation,
    prop = proportion
  )
  
  #Pour la position des chiffres dans le graphe
  count.data <- count.data %>%
    arrange(desc(Origine)) %>%
    mutate(lab.ypos = cumsum(prop) - 0.5*prop)
  count.data
  
  #Graphe avec ses options
  g=ggplot(count.data, aes(x = "", y = prop, fill = Origine)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = prop), color = "white")+
    scale_fill_manual(values = mycols) +
    theme_void()
  print(g)
  ggsave(name, plot=last_plot(), width=5, height = 3)
}

#Executions des fonctions pour la base de donnees des emissions de CO2 
dataCarbon=clean(dataCarbon) 
check(dataCarbon)
regression(dataCarbon, 2005)
plotSum(dataCarbon, "sum_carbon.png")
plotOneGraphs(dataCarbon, "Global_emissions_carbon.png")
print(c(ecart(dataCarbon,2005), ecart(dataCarbon,2008), ecart(dataCarbon,2013)))
piechart("carbon_origine.png", c("Utilisation de combustibles fossiles", "Changements dans l'utilisation des sols", "Procedes industriels"), c(87, 9, 4), c("#EFC000FF", "#CD534CFF", "#0073C2FF"))

#Executions des fonctions pour la base de donnees des emissions de N2O 
dataCarbon=clean(dataN2O) 
check(dataN2O)
regression(dataN2O, 2008)
plotSum(dataN2O, "sum_N2O.png")
plotOneGraphs(dataN2O, "Global_emissions_N2O.png")
print(c(ecart(dataN2O,2008), ecart(dataN2O,2013)))
piechart("N2O_origine.png", c("Agriculture", "Combustibles fossiles et procedes industriels", "Brûlage de biomasse", "Depôts atmospheriques", "Eaux usees humaines"), c(67,10,10,9,3), c("#0073C2FF", "#EFC000FF", "#CD534CFF", "#868686FF", "#60BD68"))

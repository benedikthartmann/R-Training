# Quelle: https://www.inwt-statistics.de/blog-artikel-lesen/Clusteranalyse_in_R.html
#Notwendige Pakete
require(dplyr)
require(broom)
require(ggplot2)
require(ggdendro)
require(cluster)

#installieren/nur einmal
install.packages("dplyr")
install.packages("broom")
install.packages("ggplot2")
install.packages("ggdendro")
install.packages("cluster")
install.packages("tidyverse")

#libaray jedes mal aktivieren
library("dplyr")
library("broom")
library("ggplot2")
library("ggdendro")
library("cluster")
library("tidyverse")

#Datenbank aus Paket
mtcars

#Auswahl
cars=select(mtcars,mpg,disp)
ggplot(cars,aes(mpg,disp))+geom_point()

#Clusterzusammenhang
h.cluster<-cars%>%dist(.,method="euclidean")%>%hclust(.,method="ward.D")
ggdendrogram(h.cluster)

#screenplot
multi.clust<-data.frame(k = 1:6)%>%group_by(k)%>%do(clust = kmeans(cars,.$k))
sumsq.clust<-multi.clust%>%group_by(k)%>%do(glance(.$clust[[1]]))
ggplot(sumsq.clust,aes(k,tot.withinss))+ geom_line()+geom_point()

# k-means-Algorithmus erhaltene Gruppenstruktur für K=2
p.cluster<-cars%>%kmeans(.,2)
# Benedikt: der nachfolgende Befehl beschädigt dir für sil das p.cluster:
p.cluster$clusterKmeans<-as.factor(p.cluster$cluster)
ggplot(cars,aes(mpg,disp,label=rownames(cars)))+scale_fill_discrete(name ="Cluster")+xlim(9,35)+geom_label(aes(fill=p.cluster$clusterKmeans),colour="white",fontface="bold",size=2)

#An diesem Punkt wäre es interessant, vergleichsweise die 
#Gruppenstruktur auch für k=1,k=2,K=3,K=4 anzusehen

multi.clust<-data.frame(k = 1:6)%>%group_by(k)%>%do(clust=kmeans(cars,.$k))
multi.k<-multi.clust%>%group_by(k)%>%do(augment(.$clust[[1]],cars))
ggplot(multi.k,aes(mpg,disp))+geom_point(aes(color=.cluster))+facet_wrap(~k)

#Beurteilung Gruppenzuordnung mit Siholettenplot

sil<-cars%>%dist(.)%>%silhouette(p.cluster$cluster,.)
sil.data<-data.frame(cluster=factor(sil[,1]),sil_width = sil[,3])
ggplot(sil.data,aes(x=row.names(sil.data),y=sil_width,fill=cluster))+geom_bar(stat="identity",width=0.5)+coord_flip()+labs(x="")+scale_x_discrete(limits=row.names(sil.data[order(sil.data$cluster,sil.data$sil_width),])) 

#Überprüfung
mean(sil.data$sil_width)

#Interpretation der Clusterlösung
clustcars=cbind(mtcars,cars=p.cluster$cluster)
ggplot(sil.data,aes(x=row.names(sil.data),y=sil_width,fill=cluster))+geom_bar(stat="identity",width=0.5)+coord_flip()+labs(x="")+scale_x_discrete(limits=row.names(sil.data[order(sil.data$cluster,sil.data$sil_width),]))

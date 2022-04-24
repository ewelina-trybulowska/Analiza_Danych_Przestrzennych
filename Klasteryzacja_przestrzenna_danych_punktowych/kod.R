
library("dbscan")
library("sf")
library("devtools")
library("rgdal")
library("ggplot2")


#wczytanie danych:

Data<-st_read("zestaw8_XYTableToPoint_Proje.shp")
Data<-st_drop_geometry(Data) #pozbywam siê kolumny geometry
Data <- subset( Data, select = -c(1,2) ) #wybieram potrzebne mi kolumny z danymi
class(Data)
head(Data)


#mapa dzielnic krakowa z naniesionymi danymi################################################
shp <- readOGR(dsn ="osiedla.shp", stringsAsFactors = F)
map <- ggplot() +
  geom_point(aes(x=Data$POINT_X, y=Data$POINT_Y),colour = 'red')+
  geom_polygon(data = shp, aes(x = long, y = lat, group = group),fill = NA, colour = "black")+
  ggtitle("Mapa zarejestrowanych wykroczen ta terenie Krakowa")+
  theme(plot.title = element_text(size=10, hjust = 0.5))+
  labs(x="dlugosc geograficzna",y="szerokosc geograficzna")+coord_fixed()
map

#map+ theme_void()


#############################################################################################

#DBSCAN
dim(Data)
## Find suitable DBSCAN parameters using KNN:
#k=2*dim-1
#min Pts=k+1
kNNdistplot(Data, k = 5)
abline(h=450, col = "green", lty=2)

db <- dbscan(Data, eps =50, minPts = 10)
db
#eps – maksymalny promieñ s¹dziedztwa.
#minPts – minimalna liczba obiektów w regionie okreœlonego eps. Domyœlnie 5 punktów.


cluster_dbscan<- db$cluster
groups_dbscan<-Data[cluster_dbscan != 0,]
noise_dbscan<-Data[cluster_dbscan == 0,]
clusters_dbscan<-as.factor(cluster_dbscan)
color_dbscan<-clusters_dbscan[clusters_dbscan!=0]


ggplot() + 
  geom_polygon(data = shp, aes(x = long, y = lat, group = group),fill=NA, color = "black")+
  theme_void()+
  geom_point(aes(groups_dbscan$POINT_X, groups_dbscan$POINT_Y, color = color_dbscan),pch=16,size=2)+
  geom_point(aes(noise_dbscan$POINT_X, noise_dbscan$POINT_Y),pch=20,size=2,color='grey' )+
  labs(color="klastry")+ggtitle("Wykres klasteryzacji DBSCAN z parametrami eps=50 min Pts=10")+
  theme(plot.title = element_text(size=10, hjust = 0.5))+coord_fixed()


                                                                                                                              



#HDBSCAN
#paramer k musi spelniac warunek k>=1
#minpts=4 24klastry
#5-31
#6--2 cluster(s) and 30 noise points
#16--contains 15 cluster(s) and 652 noise points.
#20--9 cluster(s) and 807 noise points.
#25--5 cluster(s) and 699 noise points
#34--6 cluster(s) and 1183 noise points.

#5
#10
#60
#100

hdbscan<-hdbscan(Data, minPts=15)
hdbscan

cluster_hdbscan <- hdbscan$cluster
groups_hdbscan<-Data[cluster_hdbscan != 0,]
noise_hdbscan<-Data[cluster_hdbscan == 0,]
clusters_hdbscan<-as.factor(hdbscan$cluster)
color_hdbscan<-clusters_hdbscan[clusters_hdbscan!=0]


ggplot() + 
  geom_polygon(data = shp, aes(x = long, y = lat, group = group),fill=NA, color = "black")+
  theme_void()+
  geom_point(aes(noise_hdbscan$POINT_X, noise_hdbscan$POINT_Y),pch=20,size=2,color='grey' )+
  geom_point(aes(groups_hdbscan$POINT_X, groups_hdbscan$POINT_Y, color = color_hdbscan),pch=16,size=2)+
  labs(color="klastry")+ggtitle("Wykres klasteryzacji HDBSCAN z parametrem minPts=15")+
  theme(plot.title = element_text(size=10, hjust = 0.5))+coord_fixed()



#OPTICS
#extractDBSCAN() extracts a clustering from an OPTICS ordering
optics<-optics(Data, minPts =10)
optics <- extractDBSCAN(optics, eps_cl = 100)
optics


cluster_optics<- optics$cluster
groups_optics<-Data[cluster_optics != 0,]
noise_optics<-Data[cluster_optics == 0,]
clusters_optics<-as.factor(cluster_optics)
color_optics<-clusters_optics[clusters_optics!=0]


ggplot() + 
  geom_polygon(data = shp, aes(x = long, y = lat, group = group),fill=NA, color = "black")+
  theme_void()+
  geom_point(aes(groups_optics$POINT_X, groups_optics$POINT_Y, color = color_optics),pch=16,size=2)+
  #geom_point(aes(noise_optics$POINT_X, noise_optics$POINT_Y),pch=20,size=2,color='grey' )+
  labs(color="klastry")+ggtitle("Wykres klasteryzacji OPTICS z parametrem minPts=10 eps_cl=100")+
  theme(plot.title = element_text(size=10, hjust = 0.5))+coord_fixed()






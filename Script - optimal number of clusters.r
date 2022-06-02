#Characterization of Pan social systems reveals in-group out-group distinction and out-group tolerance in bonobos
#Samuni, Langergraber, Surbeck

#code to evaluate the optimal number of clusters in Kokolopori

data1=read.csv("Data1 - Characterization of Pan social systems reveals in-group out-group distinction and out-group tolerance in bonobos.csv", header=TRUE)

##Determining optimal number of clusters
####Cunducting hierarchical clustering
library(cluster)

##2017
##Transformation - larger values represent less closely associating individuals
SRI17=1-(sr.2017^(2/3))  
SRI17<-as.dist(SRI17, diag=F, upper = F)#A symmetric matrix - therefore keeping one diagonal of the matrix 
#clustering using the 'ward' method
cluster17 <-agnes(SRI17, metric = 'euclidean', method="ward")

####Calculating the Silhouette Coefficient for varying values of k

####getting the mean silhouette coef for different K values 
sum.sil17=c()
for(k in 2:10){
	ss=silhouette(cutree(cluster17, k), SRI17)
	dff=data.frame(ss[1:nrow(ss), 1:3])
	dff$k=k
	sum.sil17=rbind(sum.sil17, dff)
}

tapply(sum.sil17$sil_width, sum.sil17$k, mean)
#The highest silhouette with lowest mismatch is k=2


##2018
##Transformation - larger values represent less closely associating individuals
SRI18=1-(sr.2018^(2/3))  
SRI18<-as.dist(SRI18, diag=F, upper = F)#A symmetric matrix - therefore keeping one diagonal of the matrix 
#clustering using the 'ward' method
cluster18 <-agnes(SRI18, metric = 'euclidean', method="ward")

####Calculating the Silhouette Coefficient for varying values of k

####getting the mean silhouette coef for different K values 
sum.sil18=c()
for(k in 2:10){
	ss=silhouette(cutree(cluster18, k), SRI18)
	dff=data.frame(ss[1:nrow(ss), 1:3])
	dff$k=k
	sum.sil18=rbind(sum.sil18, dff)
}

tapply(sum.sil18$sil_width, sum.sil18$k, mean)
#The highest silhouette with lowest mismatch is k=4



##2019
##Transformation - larger values represent less closely associating individuals
SRI19=1-(sr.2019^(2/3))  
SRI19<-as.dist(SRI19, diag=F, upper = F)#A symmetric matrix - therefore keeping one diagonal of the matrix 
#clustering using the 'ward' method
cluster19 <-agnes(SRI19, metric = 'euclidean', method="ward")

####Calculating the Silhouette Coefficient for varying values of k

####getting the mean silhouette coef for different K values 
sum.sil19=c()
for(k in 2:10){
	ss=silhouette(cutree(cluster19, k), SRI19)
	dff=data.frame(ss[1:nrow(ss), 1:3])
	dff$k=k
	sum.sil19=rbind(sum.sil19, dff)
}

tapply(sum.sil19$sil_width, sum.sil19$k, mean)
#The highest silhouette with lowest mismatch is k=4


#####plot the Silhouette Coefficient for all years
cwes <- c("#D8B70A", "#972D15", "#81A88D", "#02401B", "#A2A475")
op <- par(mfrow= c(1,3), oma= c(2,0, 2, 0),
          mgp= c(2.5,0.8,0), mar= .1+c(4.2,.9,2,3), cex=0.8)

plot(silhouette(cutree(cluster17, 2), SRI17), main = paste("k = ",2), do.n.k=FALSE, col = c("#D8B70A", "#972D15"))
mtext(text="Kokolopori 2017", at=0.5, line=2, side=3, cex=1.2)

#plot silhouette 2018
plot(silhouette(cutree(cluster18, 4), SRI18), main = paste("k = ",4), do.n.k=FALSE, col = c("#D8B70A", "#972D15", "#02401B", "#81A88D"))
mtext(text="Kokolopori 2018", at=0.5, line=2, side=3, cex=1.2)

#plot silhouette 2019
plot(silhouette(cutree(cluster19, 4), SRI19), main = paste("k = ",4), do.n.k=FALSE, col = c("#D8B70A", "#972D15", "#02401B", "#81A88D"))
mtext(text="Kokolopori 2019", at=0.5, line=2, side=3, cex=1.2)



##Modularity
library(igraph)
library(graph4lg)

##creating columns with individual ids
data1$ind1=matrix(unlist(strsplit(as.character(data1$Dyad), split="_", fixed=T)), ncol=2, byrow=T)[,1]
data1$ind2=matrix(unlist(strsplit(as.character(data1$Dyad), split="_", fixed=T)), ncol=2, byrow=T)[,2]


#create matrices
sr.2017=df_to_pw_mat(data1[data1$Year=="2017",], from="ind1", to="ind2", value="SRI.observed")
sr.2018=df_to_pw_mat(data1[data1$Year=="2018",], from="ind1", to="ind2", value="SRI.observed")
sr.2019=df_to_pw_mat(data1[data1$Year=="2019",], from="ind1", to="ind2", value="SRI.observed")

network17 <-graph.adjacency(sr.2017, mode="undirected", weighted=TRUE, diag=FALSE)
network18 <-graph.adjacency(sr.2018, mode="undirected", weighted=TRUE, diag=FALSE)
network19 <-graph.adjacency(sr.2019, mode="undirected", weighted=TRUE, diag=FALSE)

#Calculate yearly modularity values based on the SRI observed 
modularity17=cluster_louvain(network17)
modularity17

modularity18=cluster_louvain(network18)
modularity18

modularity19=cluster_louvain(network19)
modularity19




#Affinity propogation
library(apcluster)

d.apclus17 <- apcluster(negDistMat(r=2), sr.2017)
d.apclus17@clusters
d.apclus18 <- apcluster(negDistMat(r=2), sr.2018)
d.apclus18@clusters
d.apclus19 <- apcluster(negDistMat(r=2), sr.2019)
d.apclus19@clusters









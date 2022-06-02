#Characterization of Pan social systems reveals in-group out-group distinction and out-group tolerance in bonobos
#Samuni, Langergraber, Surbeck

#code to evaluate the optimal number of clusters in Ngogo

data1=read.csv("Ngogo Data.csv", header=TRUE)


#create matrices
library(graph4lg)

sr.2011=df_to_pw_mat(data1[data1$year=="2011",], from="ind1", to="ind2", value="SRI.observed")
sr.2012=df_to_pw_mat(data1[data1$year=="2012",], from="ind1", to="ind2", value="SRI.observed")
sr.2013=df_to_pw_mat(data1[data1$year=="2013",], from="ind1", to="ind2", value="SRI.observed")


##Determining optimal number of clusters
####Cunducting hierarchical clustering
library(cluster)

##2011
##Transformation - larger values represent less closely associating individuals
SRI11=1-(sr.2011^(2/3))  
SRI11<-as.dist(SRI11, diag=F, upper = F)#A symmetric matrix - therefore keeping one diagonal of the matrix 
#clustering using the 'ward' method
cluster11 <-agnes(SRI11, metric = 'euclidean', method="ward")

####Calculating the Silhouette Coefficient for varying values of k

####getting the mean silhouette coef for different K values 
sum.sil11=c()
for(k in 2:10){
	ss=silhouette(cutree(cluster11, k), SRI11)
	dff=data.frame(ss[1:nrow(ss), 1:3])
	dff$k=k
	sum.sil11=rbind(sum.sil11, dff)
}

tapply(sum.sil11$sil_width, sum.sil11$k, mean)
#The highest silhouette with lowest mismatch is k=3


##2018
##Transformation - larger values represent less closely associating individuals
SRI12=1-(sr.2012^(2/3))  
SRI12<-as.dist(SRI12, diag=F, upper = F)#A symmetric matrix - therefore keeping one diagonal of the matrix 
#clustering using the 'ward' method
cluster12 <-agnes(SRI12, metric = 'euclidean', method="ward")

####Calculating the Silhouette Coefficient for varying values of k

####getting the mean silhouette coef for different K values 
sum.sil12=c()
for(k in 2:10){
	ss=silhouette(cutree(cluster12, k), SRI12)
	dff=data.frame(ss[1:nrow(ss), 1:3])
	dff$k=k
	sum.sil12=rbind(sum.sil12, dff)
}

tapply(sum.sil12$sil_width, sum.sil12$k, mean)
#The highest silhouette with lowest mismatch is k=2



##2019
##Transformation - larger values represent less closely associating individuals
SRI13=1-(sr.2013^(2/3))  
SRI13<-as.dist(SRI13, diag=F, upper = F)#A symmetric matrix - therefore keeping one diagonal of the matrix 
#clustering using the 'ward' method
cluster13 <-agnes(SRI13, metric = 'euclidean', method="ward")

####Calculating the Silhouette Coefficient for varying values of k

####getting the mean silhouette coef for different K values 
sum.sil13=c()
for(k in 2:10){
	ss=silhouette(cutree(cluster13, k), SRI13)
	dff=data.frame(ss[1:nrow(ss), 1:3])
	dff$k=k
	sum.sil13=rbind(sum.sil13, dff)
}

tapply(sum.sil13$sil_width, sum.sil13$k, mean)
#The highest silhouette with lowest mismatch is k=2


##Modularity
library(igraph)

#social network
network11 <-graph.adjacency(sr.2011, mode="undirected", weighted=TRUE, diag=FALSE)
network12 <-graph.adjacency(sr.2012, mode="undirected", weighted=TRUE, diag=FALSE)
network13 <-graph.adjacency(sr.2013, mode="undirected", weighted=TRUE, diag=FALSE)

#Calculate yearly modularity values based on the SRI observed 
modularity11=cluster_louvain(network11)
modularity11

modularity12=cluster_louvain(network12)
modularity12

modularity13=cluster_louvain(network13)
modularity13


#Affinity propogation
library(apcluster)

d.apclus11 <- apcluster(negDistMat(r=2), sr.2011)
d.apclus11@clusters
d.apclus12 <- apcluster(negDistMat(r=2), sr.2012)
d.apclus12@clusters
d.apclus13 <- apcluster(negDistMat(r=2), sr.2013)
d.apclus13@clusters









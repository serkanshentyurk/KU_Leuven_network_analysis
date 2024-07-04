setwd('/Users/Serkan/Desktop/Academia/KUL/2023 - 2024/2024 Spring/Network Analysis/Code/Project/')

## Loading data
affective_w1 = as.matrix(read.csv("RECENS_data/6400_affective_w1.csv",
                                   header=TRUE, row.names=1, sep=","))
sex = as.matrix(read.csv("RECENS_data/6400_sex.csv",
                          header=TRUE, row.names=1, sep=","))
affective_w2 = as.matrix(read.csv("RECENS_data/6400_affective_w2.csv",
                                   header=TRUE, row.names=1, sep=","))
drink = as.matrix(read.csv("RECENS_data/6400_drink.csv",
                            header=TRUE, row.names=1, sep=","))


trust_w1 = as.matrix(read.csv("RECENS_data/6400_trust_w1.csv",
                                  header=TRUE, row.names=1, sep=","))
trust_w2 = as.matrix(read.csv("RECENS_data/6400_trust_w2.csv",
                                  header=TRUE, row.names=1, sep=","))

drink_w1 = drink[,1]
drink_w2 = drink[,2]

## turning affective into binary matrix: like/dislike
friendship_w1 = affective_w1
friendship_w1[friendship_w1 %in% c(-2:1)] = 0
friendship_w1[friendship_w1 == 2] = 1

friendship_w2 = affective_w2
friendship_w2[friendship_w2 %in% c(-2:1)] = 0
friendship_w2[friendship_w2 == 2] = 1

## brief data inspection
(class_size = dim(friendship_w1)) # 36 students
(gender_comp = table(sex)) # 10 males 26 females

## missing value inspection

friend_miss_w1_matrix = is.na(friendship_w1)
(friend_miss_w1_num = sum(friend_miss_w1_matrix)) # 71 missing values including diagonal
(friend_miss_w1_index = which(rowSums(friend_miss_w1_matrix)>30)) # student6412 - row 12 is missing
(friend_miss_w1_ratio = friend_miss_w1_num / ( nrow(friendship_w1) * (ncol(friendship_w1) - 1))) # 5.6% of data is missing

friend_miss_w2_matrix = is.na(friendship_w2) 
(friend_miss_w2_num = sum(friend_miss_w2_matrix)) # 44 missing values including diagonal
(friend_miss_w2_index = which(rowSums(friend_miss_w2_matrix)>30)) # there is no missing student
(friend_miss_w2_ratio = friend_miss_w2_num / ( nrow(friendship_w2) * (ncol(friendship_w2) - 1))) # 3.4% of data is missing
detach(package:igraph)
library(sna)

## Density
friend_dens_w1 <- gden(friendship_w1)
friend_dens_w1
#Density in the first time point is 0.2882
friend_dens_w2 <- gden(friendship_w2)
friend_dens_w2
#Density in the second time point is 0.2045

## Reciprocity
friend_rec_w1 <- grecip(friendship_w1, measure="dyadic.nonnull")
friend_rec_w1
#Proportion of dyads that are reciprocated in first time point is 0.4016
friend_rec_w2 <- grecip(friendship_w2, measure="dyadic.nonnull")
friend_rec_w2
#Proportion of dyads that are reciprocated in second time point is 0.3710

## Below we plot histograms of the in and out degrees in the network
friend_ind_w1 <- degree(friendship_w1, diag=F, cmode="indegree")
friend_outd_w1 <- degree(friendship_w1, diag=F, cmode="outdegree")

friend_ind_w2 <- degree(friendship_w2, diag=F, cmode="indegree")
friend_outd_w2 <- degree(friendship_w2, diag=F, cmode="outdegree")

par(mfrow = c(2, 2))
hist(friend_ind_w1, main = 'Indegree distribution in wave 1', xlab = 'In-degree wave 1', xlim = c(0,20), breaks = 6, col="blue")
hist(friend_outd_w1, main = 'Outdegree distribution in wave 1', xlab = 'Out-degree wave 1', xlim = c(0,25), breaks = 5, col="red")
hist(friend_ind_w2, main = 'Indegree distribution in wave 2', xlab = 'In-degree wave 2', xlim = c(0,20), breaks = 6, col="blue")
hist(friend_outd_w2, main = 'Outdegree distribution in wave 2', xlab = 'Out-degree wave 2', xlim = c(0,25), breaks = 5, col="red")
par(mfrow = c(1, 1))


detach(package:sna)
library(igraph)


# igraph object
graph_w1 = graph_from_adjacency_matrix(friendship_w1)
graph_w2 = graph_from_adjacency_matrix(friendship_w2)
graph_w1t = graph_from_adjacency_matrix(trust_w1)
graph_w2t = graph_from_adjacency_matrix(trust_w2)

# handling missing values
drink_w1[is.na(drink_w1)] = median(drink_w1, na.rm = TRUE)
drink_w2[is.na(drink_w2)] = median(drink_w2, na.rm = TRUE)

## Gender homophily
assortativity_nominal(graph_w1, sex) # assortativity in wave 1: 0.1153
assortativity_nominal(graph_w2, sex) # assortativity in wave 2: 0.0899

## Degree homophily
assortativity_degree(graph_w1) # -0.0557
assortativity_degree(graph_w2) # -0.0809

# Drink homophily
assortativity(graph_w1, drink_w1) # 0.0338
assortativity(graph_w2, drink_w2) # 0.1689


## plotting

graph.12_friendship <- graph.adjacency(friendship_w1 + friendship_w2)
myLayout_friendship <- layout.fruchterman.reingold(graph.12_friendship)

par(mfrow = c(1, 2))
plot(graph_w1t,
     vertex.color = ifelse(sex == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.05,
     vertex.size = drink_w1*4,
     vertex.label = "",
     layout = myLayout_friendship,
     main = "Trust network - wave 1")
plot(graph_w2t,
     vertex.color = ifelse(sex == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.05,
     vertex.size = drink_w1*4,
     vertex.label = "",
     layout = myLayout_friendship,
     main = "Trust network - wave 2")
par(mfrow=c(1,1))

### Friend selection tables

detach(package:igraph)
library(sna)

## sex
# wave 1
gg.1_friend <- friendship_w1[sex==2, sex==2]
gb.1_friend <- friendship_w1[sex==2, sex==1]
bb.1_friend <- friendship_w1[sex==1, sex==1]
bg.1_friend <- friendship_w1[sex==1, sex==2]
friend.selection_1 <- matrix(NA, 2, 2)
rownames(friend.selection_1) <- c("girl", "boy")
colnames(friend.selection_1) <- c("girl", "boy")

friend.selection_1[1,1] <- gden(gg.1_friend, diag=FALSE)
friend.selection_1[1,2] <- gden(gb.1_friend, diag=TRUE)
friend.selection_1[2,2] <- gden(bb.1_friend, diag=FALSE)
friend.selection_1[2,1] <- gden(bg.1_friend, diag=TRUE)

friend.selection_1.norm <- friend.selection_1 / gden(friendship_w1)
friend.selection_1.norm

# wave 2
gg.2_friend <- friendship_w2[sex==2, sex==2]
gb.2_friend <- friendship_w2[sex==2, sex==1]
bb.2_friend <- friendship_w2[sex==1, sex==1]
bg.2_friend <- friendship_w2[sex==1, sex==2]
friend.selection_2 <- matrix(NA, 2, 2)
rownames(friend.selection_2) <- c("girl", "boy")
colnames(friend.selection_2) <- c("girl", "boy")

friend.selection_2[1,1] <- gden(gg.2_friend, diag=FALSE)
friend.selection_2[1,2] <- gden(gb.2_friend, diag=TRUE)
friend.selection_2[2,2] <- gden(bb.2_friend, diag=FALSE)
friend.selection_2[2,1] <- gden(bg.2_friend, diag=TRUE)

friend.selection_2.norm <- friend.selection_2 / gden(friendship_w2)
friend.selection_2.norm

## degrees
# wave 1

friend.selection_1_degree <- matrix(NA, 2, 2)
rownames(friend.selection_1_degree) <- c("girl", "boy")
colnames(friend.selection_1_degree) <- c("girl", "boy")

friend.selection_1_degree[1,1] <- mean(degree(gg.1_friend, diag=FALSE, cmode="outdegree"))
friend.selection_1_degree[1,2] <- mean(degree(gb.1_friend, diag=TRUE, cmode="outdegree"))
friend.selection_1_degree[2,2] <- mean(degree(bb.1_friend, diag=FALSE, cmode="outdegree"))
friend.selection_1_degree[2,1] <- mean(degree(bg.1_friend, diag=TRUE, cmode="outdegree"))


friend.selection_1_degree.norm <- friend.selection_1_degree / gden(friendship_w1)
friend.selection_1_degree.norm

# wave 2
friend.selection_2_degree <- matrix(NA, 2, 2)
rownames(friend.selection_2_degree) <- c("girl", "boy")
colnames(friend.selection_2_degree) <- c("girl", "boy")

friend.selection_2_degree[1,1] <- mean(degree(gg.2_friend, diag=FALSE, cmode="outdegree"))
friend.selection_2_degree[1,2] <- mean(degree(gb.2_friend, diag=TRUE, cmode="outdegree"))
friend.selection_2_degree[2,2] <- mean(degree(bb.2_friend, diag=FALSE, cmode="outdegree"))
friend.selection_2_degree[2,1] <- mean(degree(bg.2_friend, diag=TRUE, cmode="outdegree"))

friend.selection_2_degree.norm <- friend.selection_2_degree / gden(friendship_w2)
friend.selection_2_degree.norm


### Distances

## Hamming distance
hamming <- hdist(friendship_w1, friendship_w2)
hamming # 260

hamming.prop <- hamming/nties(friendship_w1)
hamming.prop # 0.2063

matching <- 1 - hamming.prop
matching # 0.7937

###Jaccard index:
A <- sum((friendship_w1 * friendship_w2)==1, na.rm=TRUE) # #ties that exist in both networks
BplusC <- sum((friendship_w1 + friendship_w2)==1, na.rm=TRUE) # #ties that exist in only one network
jaccard <- A/(A+BplusC)
jaccard # 0.3897


### QAP

same.sex <- sex %*% t(sex)

same.sex[same.sex==2] <- 0
same.sex[same.sex==4] <- 1

# the relation between wave2 and wave1 friendships
(qap1 <- netlogit(friendship_w2, friendship_w1, nullhyp="qap", reps=100))
# the relation between wave2 and (wave1 friendships + sex)
(qap2 <- netlogit(friendship_w2, list(friendship_w1, same.sex), nullhyp = "qap", reps = 1000))

# the relation between wave2 and (wave1 friendships + sex + sender sex + reciever sex)
# Create an empty n x n matrix
n <- length(sex)
sender.sex <- matrix(0, nrow = n, ncol = n)
reciever.sex <- matrix(0, nrow = n, ncol = n)
sender_drink <- matrix(0, nrow = n, ncol = n)
reciever_drink <- matrix(0, nrow = n, ncol = n)
same_drink = matrix(0, nrow = n, ncol = n)
# Fill the matrix based on gender
sender.sex[sex == 2, ] <- 1  # For males, set all values in the row to 1
reciever.sex[, sex == 2] <- 1
sender_drink[drink_w1 == 1,] = 1
sender_drink[drink_w1 == 2,] = 2
sender_drink[drink_w1 == 3,] = 3
sender_drink[drink_w1 == 4,] = 4
reciever_drink[, drink_w1 == 1] = 1
reciever_drink[, drink_w1 == 2] = 2
reciever_drink[, drink_w1 == 3] = 3
reciever_drink[, drink_w1 == 4] = 4
same_drink[drink_w1 ==1, drink_w1 == 1] = 1
same_drink[drink_w1 ==2, drink_w1 == 2] = 2
same_drink[drink_w1 ==3, drink_w1 == 3] = 3
same_drink[drink_w1 ==4, drink_w1 == 4] = 4


(qap3 <- netlogit(friendship_w2, list(friendship_w1, same.sex, sender.sex, reciever.sex), nullhyp="qap", reps=100))
(qap4 <- netlogit(friendship_w2, 
                  list(friendship_w1, 
                       same.sex, sender.sex, reciever.sex, 
                       same_drink, sender_drink, reciever_drink), 
                  nullhyp="qap", reps=100))
# friendship_w1, sender.sex, 

### Dyad/Triad/Cycle Cencus 
#Dyad
(dyad_count_1 <- dyad.census(friendship_w1))
(dyad_count_2 <- dyad.census(friendship_w2))
#Triad
(triad_count_1 <- triad.census(friendship_w1))
(triad_count_2 <- triad.census(friendship_w2))
#k-cycle
temp_friendship_1 = friendship_w1[-which(rowSums(is.na(friendship_w1)) > 1), -which(rowSums(is.na(friendship_w1)) > 1)]
diag(temp_friendship_1) <- 0
kcycle.census(temp_friendship_1, maxlen=5, tabulate.by.vertex=FALSE)
temp_friendship_2 = friendship_w2[-which(rowSums(is.na(friendship_w2)) > 1), -which(rowSums(is.na(friendship_w2)) > 1)]
diag(temp_friendship_2) <- 0
kcycle.census(temp_friendship_2, maxlen=5, tabulate.by.vertex=FALSE) # we removed a lot data tho

### Generate random graphs
library(vioplot)
## Generate
# wave 1 
net1.size <- nrow(friendship_w1)
net1.dens <- gden(friendship_w2)

random1.nets <- rgraph(net1.size, 200, net1.dens)
random1.dens <- gden(random1.nets)
random1.triad <- triad.census(random1.nets)
random1.dyad <- dyad.census(random1.nets)
# wave 2
net2.size <- nrow(friendship_w2)
net2.dens <- gden(friendship_w2)

random2.nets <- rgraph(net2.size, 200, net2.dens)
random2.dens <- gden(random2.nets)
random2.triad <- triad.census(random2.nets)
random2.dyad <- dyad.census(random2.nets)

## Visualise
#Dyads
par(mfrow = c(1,2))
vioplot(random1.dyad,
        names = colnames(random1.dyad),
        col = "transparent",
        main = 'Dyad Counts Wave 1')
points(1:3,
       dyad_count_1,
       col='red',
       type = 'b',
       pch = 15)

vioplot(random2.dyad,
        names = colnames(random2.dyad),
        col = "transparent",
        main = 'Dyad Counts Wave 2')
points(1:3,
       dyad_count_2,
       col='red',
       type = 'b',
       pch = 15)

#Triads
par(mfrow = c(1,2))
vioplot(random1.triad[,9],                 
        random1.triad[,10],
        random1.triad[,11],
        random1.triad[,12],
        random1.triad[,13],
        random1.triad[,14],
        random1.triad[,15],
        random1.triad[,16],
        names=colnames(random1.triad)[c(9:16)],  
        col="transparent",
        main = 'Triad Counts Wave 1')               
points(1:8,
       triad_count_1[c(9,10,11,12,13,14,15,16)],
       col="red",
       type="b",
       pch=15)

vioplot(random2.triad[,9],                 
        random2.triad[,10],
        random2.triad[,11],
        random2.triad[,12],
        random2.triad[,13],
        random2.triad[,14],
        random2.triad[,15],
        random2.triad[,16],
        names=colnames(random2.triad)[c(9:16)],  
        col="transparent",
        main = 'Triad Counts Wave 2')               
points(1:8,
       triad_count_2[c(9,10,11,12,13,14,15,16)],
       col="red",
       type="b",
       pch=15)
par(mfrow = c(1,1))


### Structural Equivalence
## Wave 1
par(mfrow = c(1,1))
equiv.w1 <- equiv.clust(temp_friendship_1, cluster.method="ward.D2", method="hamming")
plot(equiv.w1) # 6 is selected

bm.w1 <- blockmodel(temp_friendship_1, equiv.w1, k=6)

par(mfrow=c(1,2))
plot.sociomatrix(temp_friendship_1, diaglab=FALSE)
plot.sociomatrix(bm.w1$blocked.data, diaglab=FALSE)
par(mfrow = c(1,1))

block1.members <- bm.w1$block.membership[order(bm.w1$order.vector)]
gplot(temp_friendship_1, vertex.col=block1.members)

# indirect blockmodelling with the largest component
cfriend.w1 <- component.largest(temp_friendship_1, connected="weak", result="graph")
gplot(cfriend.w1)
cequiv.w1 <- equiv.clust(cfriend.w1, cluster.method="ward.D2", method="hamming")
plot(cequiv.w1) # 6 is selected

cbm.w1 <- blockmodel(cfriend.w1, cequiv.w1, k=6)
par(mfrow=c(1,2))
plot.sociomatrix(cfriend.w1, diaglab=FALSE)
plot.sociomatrix(cbm.w1$blocked.data, diaglab=FALSE)
par(mfrow=c(1,1))


## Wave 2
equiv.w2 <- equiv.clust(temp_friendship_2, cluster.method="ward.D2", method="hamming")
plot(equiv.w2) # 7 is selected

bm.w2 <- blockmodel(temp_friendship_2, equiv.w2, k=6)
par(mfrow=c(1,2))
plot.sociomatrix(temp_friendship_2, diaglab=FALSE)
plot.sociomatrix(bm.w2$blocked.data, diaglab=FALSE)
par(mfrow = c(1,1))
block2.members <- bm.w2$block.membership[order(bm.w2$order.vector)]
gplot(temp_friendship_2, vertex.col=block2.members)

# indirect blockmodelling with the largest component
cfriend.w2 <- component.largest(temp_friendship_2, connected="weak", result="graph")
gplot(cfriend.w2)
cequiv.w2 <- equiv.clust(cfriend.w2, cluster.method="ward.D2", method="hamming")
plot(cequiv.w2) # 7 is selected

cbm.w2 <- blockmodel(cfriend.w2, cequiv.w2, k=7)
par(mfrow=c(1,2))
plot.sociomatrix(cfriend.w2, diaglab=FALSE)
plot.sociomatrix(cbm.w2$blocked.data, diaglab=FALSE)
par(mfrow=c(1,1))

### Direct Blockmodelling
library(blockmodeling)

res <- optRandomParC(M = temp_friendship_1, k = 2, rep = 20,
                     approaches = "hom", homFun = "ss", # 
                     blocks = "com")
plot.mat(temp_friendship_1, clu=res$best$best1$clu, main="Direct approach")  # error 217.0598

## comparison with prespecified model

# nul com
# nul nul


B <- array(NA, dim = c(  2, 2))
B[  , ] <- "nul"
B[ 1, 2] <- "com"
res <- optRandomParC(M = temp_friendship_1, k = 2, rep = 15,
                     approaches = "hom", homFun = "ss", blocks = B) # 275.4126

plot.mat(temp_friendship_1, clu=res$best$best1$clu, main="Direct approach with prespecified model") 

# com nul
# nul com

B <- array(NA, dim = c(2, 2))
B[ 1, 1 ] <- "nul"
B[ 2, 1 ] <- "nul"
B[ 1, 2] <- "com"
B[ 2, 2] <- "com"

res <- optRandomParC(M = temp_friendship_1, k = 2, rep = 30,
                     approaches = "hom", homFun = "ss", blocks = B) # 232.4708

plot.mat(temp_friendship_1, clu=res$best$best1$clu, main="") 

gplot(temp_friendship_1,  vertex.col=res$best$best1$clu+2, label =colnames(temp_friendship_1),vertex.sides=sex+3)


### Cliques and Communities
detach(package:sna)
library(igraph)

## Wave 1

# Components
friendship_w1_undirected <- temp_friendship_1 + t(temp_friendship_1) 
friendship_w1_undirected[friendship_w1_undirected==2] <- 1

friend1 <- graph.adjacency(friendship_w1_undirected)
friend1 <- as.undirected(friend1)

is_connected(friend1) # connected

components <- decompose(friend1)
main_component = components[[1]]

vcount(components[[1]])
table(sapply(components, vcount))

myLayout <- layout.fruchterman.reingold(friend1)

plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout)


mean_distance(main_component) # average path length - 1.6017
diameter(main_component) # length of longest path - 3
transitivity(main_component) # ratio of closed to open triads - 0.5453


random.nets <- sna::rgraph(33, 200, edge_density(main_component, loops=F), mode="graph")
rnGdist<-numeric();for (i in 1:200){rnGdist<-c(rnGdist,max(sna::geodist(random.nets[i,,])$gdist))}
rnGtrans<-sna::gtrans(random.nets, mode="graph")

mean(rnGdist[!rnGdist%in%Inf]) # 2.52 - mean distance is shoerter than the observed
mean(rnGtrans) # 0.4065 - transivity is lower than the observed

# Cliques
cliques <- cliques(friend1)
length(cliques) # 2932
table(sapply(max_cliques(friend1), length))

# k-cores
cores <- coreness(friend1)

# and we can plot the network with node colors showing k-cores
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= cores,
     layout=myLayout)


# communities
communities <- cluster_fast_greedy(friend1)
length(communities) # how many communities were identified by the algorithm?
sizes(communities) # shat are their sizes?
membership(communities) # who belongs to which community?

plot(communities, friend1,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout)

# comparison of the methods
par(mfrow=c(2,2))
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color="skyblue",
     layout=myLayout,
     main="original network")
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= block1.members,
     layout=myLayout,
     main="blockmodel")
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= cores,
     layout=myLayout,
     main="k-cores")
# the fast-greedy communities
plot(communities, friend1,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout,
     main="fast'n'greedy")
par(mfrow=c(1,1))

## Wave 2

friendship_w2_undirected <- friendship_w2 + t(friendship_w2) 
friendship_w2_undirected[friendship_w2_undirected==2] <- 1

friend2 <- graph.adjacency(friendship_w2_undirected)
friend2 <- as.undirected(friend2)

is_connected(friend2) # connected

components <- decompose(friend2)
main_component = components[[1]]

vcount(components[[1]])
table(sapply(components, vcount))

myLayout <- layout.fruchterman.reingold(friend2)

par(mfrow=c(1,1))
plot(friend2,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout)


mean_distance(main_component) # average path length - 1.7524
diameter(main_component) # length of longest path - 3
transitivity(main_component) # ratio of closed to open triads - 0.4080


random.nets <- sna::rgraph(33, 200, edge_density(main_component, loops=F), mode="graph")
rnGdist<-numeric();for (i in 1:200){rnGdist<-c(rnGdist,max(sna::geodist(random.nets[i,,])$gdist))}
rnGtrans<-sna::gtrans(random.nets, mode="graph")

mean(rnGdist[!rnGdist%in%Inf]) # 3.005 - average distance is longer than the observed
mean(rnGtrans) # 0.2928 - transivity is lower than the observed

# Cliques
cliques <- cliques(friend2)
length(cliques) # 668
table(sapply(max_cliques(friend1), length))

# k-cores
cores <- coreness(friend2)

# and we can plot the network with node colors showing k-cores
plot(friend2,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= cores,
     layout=myLayout)


# communities
communities <- cluster_fast_greedy(friend2)
length(communities) # how many communities were identified by the algorithm?
sizes(communities) # shat are their sizes?
membership(communities) # who belongs to which community?

plot(communities, friend2,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout)

# comparison of the methods
par(mfrow=c(2,2))
plot(friend2,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color="skyblue",
     layout=myLayout,
     main="original network")
plot(friend2,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= block2.members,
     layout=myLayout,
     main="blockmodel")
plot(friend2,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= cores,
     layout=myLayout,
     main="k-cores")
# the fast-greedy communities
plot(communities, friend2,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout,
     main="fast'n'greedy")
par(mfrow=c(1,1))


### ERGMS

graph1 <- graph.adjacency(friendship_w1)
graph2 <- graph.adjacency(friendship_w2)
graph12 <- graph.adjacency(friendship_w1 + friendship_w2)
myLayout <- layout.fruchterman.reingold(graph12)

par(mfrow = c(1, 2))
plot(graph_w1,
     vertex.color = ifelse(sex == 1, "pink", "darkblue"),
     vertex.shape = ifelse(sex == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = drink_w1 *4,
     vertex.label = NA,
     layout = myLayout,
     main = "Network wave 1")
plot(graph_w2,
     vertex.color = ifelse(sex == 1, "pink", "darkblue"),
     vertex.shape = ifelse(sex == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = drink_w2 * 4,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
par(mfrow = c(1, 1))

detach(package:igraph)
library(statnet)

sex_temp = sex - 1
sex_temp = c(sex_temp)
drink_w1_temp = c(drink_w1)
drink_w2_temp = c(drink_w2)


friend1 <- network(friendship_w1)
friend1 %v% "sex" <- sex_temp
friend1 %v% "drink" <- drink_w1_temp

friend2 <- network(friendship_w2)
friend2 %v% "sex" <- sex_temp
friend2 %v% "drink" <- drink_w2_temp

ergm1 <- ergm(friend1~edges, verbose = TRUE)
summary(ergm1) # this is basically a Bernoulli / Erdos-Renyi model (independence!)
plogis(coef(ergm1))


ergm2 <- ergm(friend1~edges+mutual, verbose=TRUE)
summary(ergm2)
plogis(coef(ergm1))

ergm3 <- ergm(friend1~edges+mutual+gwesp(decay=0, fixed=TRUE))
mcmc.diagnostics(ergm3)

ergm3b <- ergm(friend1~edges+mutual+gwesp(0, fixed=T), control=control.ergm(init=ergm3$coef))
mcmc.diagnostics(ergm3b)

ergm3c <- ergm(friend1~edges+mutual+gwesp(0, fixed=T), control=control.ergm(init=ergm3b$coef))
mcmc.diagnostics(ergm3c)
summary(ergm3c)

# ergm4 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star) # didn't work
# mcmc.diagnostics(ergm4) 
# summary(ergm4)

ergm5 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T) +
                nodematch("sex")+nodeicov("sex")+nodeocov("sex"))
ergm5b <- ergm(friend1~edges+mutual+gwesp(0, fixed=T) +
                nodematch("sex")+nodeicov("sex")+nodeocov("sex"), control=control.ergm(init=ergm5$coef))
mcmc.diagnostics(ergm5b)
summary(ergm5b)

ergm6 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T) + 
                nodematch("sex")+nodeicov("sex")+nodeocov("sex")+nodematch("drink") + 
                nodeicov("drink") + nodeocov("drink"))
ergm6b <- ergm(friend1~edges+mutual+gwesp(0, fixed=T) + 
                nodematch("sex")+nodeicov("sex")+nodeocov("sex")+nodematch("drink") + 
                nodeicov("drink") + nodeocov("drink"), control = control.ergm(init=ergm6$coef))
mcmc.diagnostics(ergm6b)
summary(ergm6b)


#goodness of fit test
ergm1_gof <- gof(ergm1) # the empty model
ergm2_gof <- gof(ergm2)  
ergm3c_gof <- gof(ergm3c) 
ergm5b_gof <- gof(ergm5b) 
ergm6_gof <- gof(ergm6b) 

# # save the gof output

# pdf("ergm1_gof.pdf")
# plot(ergm1_gof)
# dev.off()
# 
# pdf("ergm2_gof.pdf")
# plot(ergm2_gof)
# dev.off()
# 
# pdf("ergm3c_gof.pdf")
# plot(ergm3c_gof)
# dev.off()
# 
# pdf("ergm5b_gof.pdf")
# plot(ergm5b_gof)
# dev.off()
# 
# pdf("ergm6_gof.pdf")
# plot(ergm6_gof)
# dev.off()

### SIENA
detach(package:statnet)
library(RSiena)
library(igraph)

# create dependent network variable
nActors <- dim(friendship_w1)[1]
friendship.dependent <- sienaDependent(array(c(friendship_w1, friendship_w2),
                                             dim=c(nActors, nActors, 2)))

# create constant actor covariates
drink.coCovar <- coCovar(drink_w1)
gender.coCovar <- coCovar(sex[,1])


mySienaData <- sienaDataCreate(friendship.dependent,
                               drink.coCovar,
                               gender.coCovar)

#Specify SIENA model
mySienaEffects <- getEffects(mySienaData)
# network effects
mySienaEffects <- includeEffects(mySienaEffects, transTrip, cycle3)
mySienaEffects <- includeEffects(mySienaEffects, inPop)
# homophily effects and ego alter control
mySienaEffects <- includeEffects(mySienaEffects, egoX, altX, sameX, interaction1="gender.coCovar")
mySienaEffects <- includeEffects(mySienaEffects, egoX, altX, sameX, interaction1="drink.coCovar")
#Create SIENA algorithm
mySienaAlgorithm <- sienaAlgorithmCreate(projname="reccens_network_1",
                                         MaxDegree=c(friendship.dependent=100))
#Estimate
result <- siena07(mySienaAlgorithm,
                  data=mySienaData,
                  effects=mySienaEffects)
result


####Second SIENNA
#Create longitudinal object
myNetwork <- sienaDependent( array( c(friendship_w1, friendship_w2), 
                                    dim = c(nActors, nActors, 2 ) ) )
myNetwork

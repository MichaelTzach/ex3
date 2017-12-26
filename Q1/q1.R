workspaceDir = "/Users/michaeltzach/Developer/ex3/Q1"
setwd(workspaceDir)

#requires
if(!require(igraph)) {
  install.packages('igraph')
}
library(igraph)
set.seed(86)

#load files
edgelist = read.csv('ga_edgelist.csv')
actorlist = read.csv('ga_actors.csv')

#helper functions
colorDataGraphVerticesForGender = function(dataGraph) {
  V(dataGraph)$color = 'white'
  
  femaleVerticeIndexes = which(V(dataGraph)$gender=="F")
  V(dataGraph)[femaleVerticeIndexes]$color = 'pink'
  
  maleVerticeIndexes = which(V(dataGraph)$gender=="M")
  V(dataGraph)[maleVerticeIndexes]$color = 'gray'
  
  return(dataGraph)
}

#create graph from all the data. We want to color the vertices by color
dataGraph = graph.data.frame(edgelist, directed=FALSE, vertices=actorlist)
dataGraph = colorDataGraphVerticesForGender(dataGraph)
plot(dataGraph)

#create graph that includes only the biggest component
dataGraphComponents = components(dataGraph)
dataGraphComponentSizes = dataGraphComponents$csize
dataGraphComponentMemberships = dataGraphComponents$membership

biggestComponentSizeIndex = which.max(dataGraphComponentSizes)
biggestComponentVerticeIndexes = which(dataGraphComponentMemberships==biggestComponentSizeIndex)
biggestComponentVertices = V(dataGraph)[biggestComponentVerticeIndexes]

biggestComponentDataGraph = induced.subgraph(graph=dataGraph, v=biggestComponentVertices)
biggestComponentDataGraph = colorDataGraphVerticesForGender(biggestComponentDataGraph)
plot(biggestComponentDataGraph)

#find highest betweeness
betweenness = betweenness(biggestComponentDataGraph)
print(betweenness[which.max(betweenness)])

#find highest closeness
closeness <- closeness(biggestComponentDataGraph)
print(closeness[which.max(closeness)])

#find highest eigen vector
eigenvector = spectrum(biggestComponentDataGraph)$vectors
eigenvectorMaxCharacter = V(biggestComponentDataGraph)[which.max(eigenvector)]
eigenvectorMaxValue = eigenvector[which.max(eigenvector)]
print(paste("character:", eigenvectorMaxCharacter[[1]]$name, "| vector value:", eigenvectorMaxValue))

#Louvain - http://igraph.org/r/doc/cluster_louvain.html
louvainCluster = cluster_louvain(dataGraph)
louvainClusterCharacterMembership = membership(louvainCluster)

plot(dataGraph, vertex.color = louvainClusterCharacterMembership)

louvainClusterNumberOfCommunities = length(louvainCluster)
print(paste("louvain cluster number of communities: ", louvainClusterNumberOfCommunities))

louvainClusterModularity = modularity(louvainCluster)
print(paste("louvain cluster modularity: ", louvainClusterModularity))

#walk trap - http://igraph.org/r/doc/cluster_walktrap.html
walktrapCluster = cluster_walktrap(dataGraph)
walktrapClusterCharacterMembership = membership(walktrapCluster)

plot(dataGraph, vertex.color = walktrapClusterCharacterMembership)

walktrapClusterNumberOfCommunities = length(walktrapCluster)
print(paste("walktrap cluster number of communities: ", walktrapClusterNumberOfCommunities))

walktrapClusterModularity = modularity(walktrapCluster)
print(paste("walktrap cluster modularity: ", walktrapClusterModularity))


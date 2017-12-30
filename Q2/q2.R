knitr::opts_chunk$set(echo = TRUE)

library(twitteR)
library(jsonlite)
library(httr)

## Setting OAuth ready and signning:

source("twitterOAuth.R")
myapp = oauth_app("twitter", key=consumer_key, secret=consumer_secret)
sign = sign_oauth1.0(myapp, token=access_token, token_secret=access_secret)

## Getting friends list of Donald Trump account:

url = "https://api.twitter.com/1.1/friends/list.json?cursor=-1&screen_name=realDonaldTrump&count=50"
presidentFriends = GET(url, sign)
json = jsonlite::fromJSON(jsonlite::toJSON(httr::content(presidentFriends)))
friendsList = json$users$screen_name

length(friendsList)
head(friendsList)

## Initializing an empty grapg and adding vertices:

library(igraph)
g <- make_empty_graph(n = 0, directed = FALSE) #New empty graph
g <- add.vertices(g, 30) #Adding 30 vertices
V(g)$name = friendsList[1: 30] #Adding names to the vertices
plot(g)


## Getting the data for the edges:

df = data.frame(V1="", V2="")
i = 0 #To count the requests and postpone the process every 15 requests 
url = "https://api.twitter.com/1.1/friendships/show.json?source_screen_name="
idx = 2

for (v1 in friendsList[1: 29]) {
  for (v2 in friendsList[idx: 30]) {
    
    fullURL = paste(url, v1, "&target_screen_name=", v2, sep = "")
    res = GET(fullURL, sign)
    json = jsonlite::fromJSON(jsonlite::toJSON(httr::content(res)))
    
    if (json$relationship$source$following && json$relationship$source$followed_by) {
      df = rbind(df, data.frame(V1=v1, V2=v2))
    }
    
    i = i + 1
    if (i == 15) {
      Sys.sleep(60 * 15.05) # sleep 15.1 min every 15 requests (API constraint :/)
      i = 0
    }
  }
  idx = idx + 1
}

## Adding edges to the graph and plotting 

g = graph.data.frame(d=df, directed = FALSE)
tkplot(g, layout = layout.kamada.kawai)

## Answering question 1:
## Getting the biggest component of the graph

gComponents = components(g)
gComponentSizes = gComponents$csize
gComponentMemberships = gComponents$membership

biggestComponentSizeIndex = which.max(gComponentSizes)
biggestComponentVerticeIndexes = which(gComponentMemberships==biggestComponentSizeIndex)
biggestComponentVertices = V(g)[biggestComponentVerticeIndexes]

biggestComponentGraph = induced.subgraph(graph=g, v=biggestComponentVertices)

biggestComponentGraph$layout <- layout.circle(biggestComponentGraph)

## Calculating betweenness:

betweenness = betweenness(biggestComponentGraph)
print(betweenness[which.max(betweenness)])
plot(biggestComponentGraph, vertex.size=10)

## Calculating closeness:

closeness <- closeness(biggestComponentGraph)
print(closeness[which.max(closeness)])

## Calculating eigenvector:

eigenvector = spectrum(biggestComponentGraph)$vectors
eigenvectorMaxCharacter = V(biggestComponentGraph)[which.max(eigenvector)]
eigenvectorMaxValue = eigenvector[which.max(eigenvector)]
print(paste("Account:", eigenvectorMaxCharacter[[1]]$name, "| vector value:", eigenvectorMaxValue))

## Louvain clustering:

louvainCluster = cluster_louvain(g)
louvainClusterCharacterMembership = membership(louvainCluster)

## Plotting the clusters:

g$layout <- layout.circle(g)
plot(g, vertex.size=10, vertex.color = louvainClusterCharacterMembership)

## Communities number:

louvainClusterNumberOfCommunities = length(louvainCluster)
print(paste("louvain cluster number of communities: ", louvainClusterNumberOfCommunities))

## Modularity:

modularity = modularity(louvainCluster)
print(paste("Modularity: ", modularity))

## Walk trap clustering:

walktrapCluster = cluster_walktrap(g)
walktrapClusterCharacterMembership = membership(walktrapCluster)

## Ploting the clusters:

plot(g, vertex.size=10, vertex.color = walktrapClusterCharacterMembership)

## Communities number:

walktrapClusterNumberOfCommunities = length(walktrapCluster)
print(paste("walktrap cluster number of communities: ", walktrapClusterNumberOfCommunities))

## Modularity:

modularity = modularity(walktrapCluster)
print(paste("Modularity: ", modularity))
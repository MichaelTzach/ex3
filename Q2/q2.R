library(twitteR)
library(jsonlite)
library(httr)

source("twitterOAuth.R")
myapp = oauth_app("twitter", key=consumer_key, secret=consumer_secret)
sign = sign_oauth1.0(myapp, token=access_token, token_secret=access_secret)

url = "https://api.twitter.com/1.1/friends/list.json?cursor=-1&screen_name=realDonaldTrump&count=50"
presidentFriends = GET(url, sign)
json = jsonlite::fromJSON(jsonlite::toJSON(httr::content(presidentFriends)))
friendsList = json$users$screen_name

library(igraph)
g <- make_empty_graph(n = 0, directed = TRUE)
g <- add.vertices(g, length(friendsList))
V(g)$name = friendsList

i = 0
url = "https://api.twitter.com/1.1/friendships/show.json?source_screen_name="
for (follower in friendsList) {
  for (name in setdiff(friendsList, c(follower))) {
    fullURL = paste(follower, "&target_screen_name=", name, sep = "")
    res = GET(fullURL, sign)
    json = jsonlite::fromJSON(jsonlite::toJSON(httr::content(res)))
    if (json$relationship$source$following) {
      g = add.edges(g, c(follower, name))
    }
    i = i + 1
  }
  if (i == 15) {
    Sys.sleep(60 * 15.1) # sleep 15.1 min every 15 requests (API constraint :/)
    i = 0
  }
}

plot(g)
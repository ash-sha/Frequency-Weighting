library(twitteR)
library("devtools")
devtools::install_github("hadley/httr")
library("httr")
install_github("twitteR", username="geoffjentry")

api_key <- "b2afVUp4PNsYFNe1wEXso2aeO" 
api_secret <- "L1zdn8WcYOCaCjfiMXrIdkQkpKHLQgWLj36IpLLWhRGeBD852D" 
token <- "2749482174-SNrfb90408RF8ZJAlF5JbVng0LQd0t6GY86A7ei"  
token_secret <- "5jpwWLv79V7wrQ4LdbJyyBGZHU3WPBXcl3WW4IoYd0UzF" 
setup_twitter_oauth(api_key, api_secret, token, token_secret)
tweets <- searchTwitter("medicines OR #ontology OR cancer OR #cancer", n = 200, lang = "en")
tweets.df <-twListToDF(tweets)
write.csv(tweets.df, "/Users/aswath/RProjects/exc/paper2/tweets.csv")

# sign on to Twitter with R

library("httr")
library(twitteR)
API_Key="b2afVUp4PNsYFNe1wEXso2aeO"
API_Secret="L1zdn8WcYOCaCjfiMXrIdkQkpKHLQgWLj36IpLLWhRGeBD852D"
accessToken="2749482174-SNrfb90408RF8ZJAlF5JbVng0LQd0t6GY86A7ei"
accessSecret="5jpwWLv79V7wrQ4LdbJyyBGZHU3WPBXcl3WW4IoYd0UzF"

setup_twitter_oauth(API_Key, API_Secret, accessToken, accessSecret)

rm(API_Key,API_Secret,accessToken,accessSecret) # don't leave them in the workspace

## example of searching for tweets online

tweets <- searchTwitter('#beer', n=50)
head(tweets)

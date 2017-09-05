library(twitteR)
setwd("/Users/apple/Desktop")
# You need to login using your twitter account to dev.twitter.com
# And go to "Manage your App" -> create new app -> get keys
consumer_key <- 
consumer_secret <- 
access_token <- 
access_secret <- 
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# Search tweets with certain key word
tw_search <- searchTwitter("#apple", n=15)
tw_search
data <- tw_search[[1]]
data$text # tweet content
data$screenName # user who posts the tweet

# Note! twitter api has limitation every day/hour/account
# You can check it
getCurRateLimitInfo()
#-

clean.text <- function(some_txt)
{
    some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    some_txt = gsub("@\\w+", "", some_txt)
    some_txt = gsub("[[:punct:]]", "", some_txt)
    some_txt = gsub("[[:digit:]]", "", some_txt)
    some_txt = gsub("http\\w+", "", some_txt)
    some_txt = gsub("[ \t]{2,}", "", some_txt)
    some_txt = gsub("^\\s+|\\s+$", "", some_txt)
    some_txt = gsub("amp", "", some_txt)
    # define "tolower error handling" function
    try.tolower = function(x)
    {
        y = NA
        try_error = tryCatch(tolower(x), error=function(e) e)
        if (!inherits(try_error, "error"))
            y = tolower(x)
        return(y)
    }
    
    some_txt = sapply(some_txt, try.tolower)
    some_txt = some_txt[some_txt != ""]
    names(some_txt) = NULL
    return(some_txt)
}

getSentiment <- function (text, key){
    
    text <- URLencode(text);
    
    #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
    text <- str_replace_all(text, "%20", " ");
    text <- str_replace_all(text, "%\\d\\d", "");
    text <- str_replace_all(text, " ", "%20");
    
    if (str_length(text) > 360){
        text <- substr(text, 0, 359);
    }
    ##########################################
    
    data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
    
    js <- fromJSON(data, asText=TRUE);
    
    # get mood probability
    sentiment = js$output$result
    
    ###################################
    return(list(sentiment=sentiment))
}


tweet = searchTwitter("Trump", 10000, lang="en")
str(tweet)
tweets_text <- sapply(tweet ,function(x)x$getText())
clean_text = clean.text(tweets_text)
tweets_corpus <- Corpus(VectorSource(clean_text))
tdm = TermDocumentMatrix(tweets_corpus, control = list(removePunctuation = TRUE,stopwords = stopwords("SMART"), removeNumbers = TRUE, tolower = TRUE))
tdm <- as.matrix(tdm)

word_freqs = sort(rowSums(tdm), decreasing=TRUE) #now we get the word orders in decreasing order
dm = data.frame(word=names(word_freqs), freq=word_freqs)

wordcloud(dm$word, dm$freq, min.freq=2,random.order=F,colors=brewer.pal(9, "Blues"),  random.color= T, max.words = 150)
png("wordcloud_packages.png", width=800,height=800,res=300)
wordcloud(dm$word, dm$freq, min.freq=2,random.order=F,
          colors=brewer.pal(9, "Blues"),  random.color= T, max.words = 1000)
dev.off()


## This uses the Twitter api to grab the last n Tweets about a certain topic, then will create a wordcloud
## based on those tweets.  The sessio in R must be authenticated using OAuth before this functionality can be used

cloudmaker <- function(searchTerm,lastNTweets){
  # twitteR api call to grab last n tweets with a certain search term
      tweets <-searchTwitter(searchTerm, n=lastNTweets)
  # sapply allows us to travers the dataset and perform operations on indiviual data,
  # here where are grabbing the text from each tweet object
      nerd_text = sapply(tweets, function(x) x$getText())
  # uses 'tm' package aka Text Mining, a corpus can be though of a collection of text objects
      nerd_corpus = Corpus(VectorSource(nerd_text))
  # create matrix of out word corpus
      tdm = TermDocumentMatrix(nerd_corpus, 
                               control = list(removePunctuation = TRUE,
                               stopwords = c(searchTerm, stopwords("english")),
                               removeNumbers = TRUE, tolower = TRUE))
      m = as.matrix(tdm)
  
      word_freq = sort(rowSums(m), decreasing = TRUE)
      dm = data.frame(word=names(word_freq), freq=word_freq)
  #after sorting we created a data fram and used the 'wordcloud' package to create 
     wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
  
 }

cloudmaker("The Nerdery", 100)


## server.R calls this .R file which contain the guts of the lookup mechanism
library(shiny); library(stringr) 

# load in lookup tables
 load(".data/blogs.biDT7.RData"); load(".data/blogs.triDT7.RData"); load(".data/blogs.tetraDT7.RData")
    setkey(blogs.biDT7, gram); setkey(blogs.triDT7, bigram); setkey(blogs.tetraDT7, trigram)
  load(".data/news.biDT7.RData"); load(".data/news.triDT7.RData"); load(".data/news.tetraDT7.RData")
    setkey(news.biDT7, gram); setkey(news.triDT7, bigram); setkey(news.tetraDT7, trigram)
  load(".data/tweets.biDT7.RData"); load(".data/tweets.triDT7.RData"); load(".data/tweets.tetraDT7.RData")
    setkey(tweets.biDT7, gram); setkey(tweets.triDT7, bigram); setkey(tweets.tetraDT7, trigram)

# load profanity list to clean string
  profanity_list <- readLines("http://www.bannedwordlist.com/lists/swearWords.txt", warn = FALSE)

# string coming in from ui.R is cleaned prior to lookup (INCOMING VARIABLE MUST BE CALLED "STRING")
# outgoing variable is called "string.5"
  string.0 <-tolower(string); string.1 <- str_replace_all(string.0, "[^[:alnum:][:space:]'|’]", ""); 
  string.2 <- iconv(string.1, from="UTF-8", to="ascii", sub=""); string.3 <- iconv(string.2, to="ASCII//TRANSLIT"); 
  string.4 <- str_replace_all(string.3, "[[:digit:]]+", ""); 
  string.5 <- str_replace_all(string.4, paste(profanity_list, collapse = "|"), replacement = "")

# determine corpus type for lookup
 if (input$corpus == "blogs") {lookup.blogs(string.5)
 } else {
   if (input$corpus == "news") {
     lookup.news(string.5)
    } else {
      lookup.tweets(string.5)
    }
 }


# function subsetting largest n-gram, performing simple backoff lookup
lookup.blogs <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {blogs.tetra <- blogs.tetraDT7[list(gram)][1]
  return(blogs.tetra)
  } else {
    if(gram.n == 2){
      blogs.tri <- blogs.triDT7[list(gram)][1]
      return(blogs.tri)
    } else {
      blogs.bi <- blogs.biDT7[list(gram)][1]
      return(blogs.bi)
    }
  }
}


lookup.news <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {news.tetra <- news.tetraDT7[list(gram)][1]
                    return(news.tetra)
  } else {
    if(gram.n == 2){
      news.tri <- news.triDT7[list(gram)][1]
      return(news.tri)
    } else {
      news.bi <- news.biDT7[list(gram)][1]
      return(news.bi)
    }
  }
}


lookup.tweets <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {tweets.tetra <- tweets.tetraDT7[list(gram)][1]
                    return(tweets.tetra)
  } else {
    if(gram.n == 2){
      tweets.tri <- tweets.triDT7[list(gram)][1]
      return(tweets.tri)
    } else {
      tweets.bi <- tweets.biDT7[list(gram)][1]
      return(tweets.bi)
    }
  }
}
################################################################################
### Title: server.R                                                          ###
###                                                                          ###
### Project: Coursera Data Science Capstone Project                          ###
###                                                                          ###
### Version: 0.1 - 04/14/2015                                                ###
###                                                                          ###
### Description: Server Script for Text Prediction UI in Shiny               ###
###                                                                          ###
### Authors: Christopher Stewart <stewart.christophermichael@gmail.com>      ###
###                                                                          ###
### Maintainer: Christopher Stewart <stewart.christophermichael@gmail.com>   ###
###                                                                          ###
### Versions:                                                                ###
###     > 0.1 - 04/14/2015 - 14:36:33:                                       ###
###         creation                                                         ###
###                                                                          ###
################################################################################
###

#rm(list = ls()); gc(reset = TRUE)

library(shiny); library(stringr); library(data.table); library()

# load in lookup tables
load(".data/blogs.biDT7.RData"); load(".data/blogs.triDT7.RData"); load(".data/blogs.tetraDT7.RData")
setkey(blogs.biDT7, gram); setkey(blogs.triDT7, bigram); setkey(blogs.tetraDT7, trigram)
load(".data/news.biDT7.RData"); load(".data/news.triDT7.RData"); load(".data/news.tetraDT7.RData")
setkey(news.biDT7, gram); setkey(news.triDT7, bigram); setkey(news.tetraDT7, trigram)
load(".data/tweets.biDT7.RData"); load(".data/tweets.triDT7.RData"); load(".data/tweets.tetraDT7.RData")
setkey(tweets.biDT7, gram); setkey(tweets.triDT7, bigram); setkey(tweets.tetraDT7, trigram)

# functions performing simple backoff lookup for largest possible n-gram stub

lookup.blogs <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {tri <- word(gram, -3:-1); tri <- paste(tri, collapse = ' ')
                    tetra <<- blogs.tetraDT7[tri][1]
                    if (is.na(tetra$target)) {return(tetra)
                    } else if (gram.n == 2) {bi <- word(gram, -2:-1); bi <- paste(bi, collapse = ' ')
                      tri <<- blogs.triDT7[bi][1]
                                        if (is.na(tri$target)) {return(tri)
                                        } else {      									  
                                          if (gram.n == 1) {bi <<- blogs.biDT7[gram][1] 
                                                            return(bi)}
                                               }
                                            }
                    }
                  }


lookup.blogs <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {tri <- word(gram, -3:-1)
                    tri <- paste(tri, collapse = ' ')
                    tetra <<- blogs.tetraDT7[tri][1]
                    if (!is.na(tetra$target)) {return(tetra)
                    } else {bi <- word(gram, -2:-1)
                            bi <- paste(bi, collapse = ' ')
                            tri <<- blogs.triDT7[bi][1]
                                    if (!is.na(tri$target)) {return(tri)
                                    } else {bi <<- blogs.biDT7[gram][1] 
                                                                 return(bi)}
                                    }
                                  }
                              }






lookup.news <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {
    tri <- word(gram, -3:-1)
    tri <- paste(tri, collapse = ' ')
    tetra <<- news.tetraDT7[tri][1]
    if (!is.na(tetra$p)) {
      return(tetra)
    }
  }		
  else if (gram.n == 2) {
    bi <- word(gram, -2:-1); bi <- paste(bi, collapse = ' ')
    tri <<- news.triDT7[bi][1]
    if (!is.na(tri$p)) {
      return(tri)
    } 
  }
  else if (gram.n == 1) {
    bi <<- news.biDT7[gram][1]
    return(bi)
  }
}


lookup.tweets <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {
    tri <- word(gram, -3:-1)
    tri <- paste(tri, collapse = ' ')
    tetra <<- tweets.tetraDT7[tri][1]
    if (!is.na(tetra$p)) {
      return(tetra)
    }
  }  	
  else if (gram.n == 2) {
    bi <- word(gram, -2:-1); bi <- paste(bi, collapse = ' ')
    tri <<- tweets.triDT7[bi][1]
    if (!is.na(tri$p)) {
      return(tri)
    } 
  }
  else if (gram.n == 1) {
    bi <<- tweets.biDT7[gram][1]
    return(bi)
  }
}






lookup.news <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {trigram <- word(gram, -3:-1)
                    trigram <- paste(trigram, collapse = ' ')
                    news.tetra <<- news.tetraDT7[trigram][1]
                    if (!is.na(news.tetra$target) {return(news.tetra)}
                    else bigram <- word(gram, -2:-1)
                         bigram <- paste(bigram, collapse = ' ')
                         news.tri <<- news.triDT7[bigram][1]
                         if (!is.na(news.tri$target) {return(news.tri)} 
                             else {news.bi <<- news.biDT7[gram][1]
                                  return(news.bi)}
                                  }
                    }




  } else {
    if(gram.n == 2){bigram <- word(gram, -2:-1); bigram <- paste(bigram, collapse = ' ')
                    news.tri <- news.triDT7[bigram][1]
                    return(news.tri)
    } else {
      news.bi <- news.biDT7[gram][1]
      return(news.bi)
    }
  }
}


lookup.tweets <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {trigram <- word(gram, -3:-1); trigram <- paste(trigram, collapse = ' ')
                    tweets.tetra <- tweets.tetraDT7[trigram][1]
                    return(tweets.tetra)
  } else {
    if(gram.n == 2){bigram <- word(gram, -2:-1); bigram <- paste(bigram, collapse = ' ')
                    tweets.tri <- tweets.triDT7[bigram][1]
                    return(tweets.tri)
    } else {
      tweets.bi <- tweets.biDT7[gram][1]
      return(tweets.bi)
    }
  }
}


shinyServer(function(input, output) {
  values <- reactiveValues() 
  observe({
    corpus <- userinput$corpus
    string <- userinput$stub
  })
    ## Clean incoming string
    # load profanity list to clean string
    profanity_list <- readLines("http://www.bannedwordlist.com/lists/swearWords.txt", warn = FALSE)
    
    # string coming in from ui.R is cleaned prior to lookup (INCOMING VARIABLE MUST BE CALLED "STRING")
    string.0 <-tolower(string); string.1 <- str_replace_all(string.0, "[^[:alnum:][:space:]'|’]", ""); 
    string.2 <- iconv(string.1, from="UTF-8", to="ascii", sub=""); string.3 <- iconv(string.2, to="ASCII//TRANSLIT"); 
    string.4 <- str_replace_all(string.3, "[[:digit:]]+", ""); 
    string.5 <- str_replace_all(string.4, paste(profanity_list, collapse = "|"), replacement = "")
    
    
    # determine corpus type for lookup
    if (corpus == "blogs") {lookup.blogs(string.5)
    } else {
      if (corpus == "news") {
        lookup.news(string.5)
      } else {
        lookup.tweets(string.5)
      }
    }
    
    # get targets
  if (exists("blogs.tetra") == TRUE) {target.blogs <- blogs.tetra
  } else {
    if (exists("blogs.tri") == TRUE) {target.blogs <- blogs.tri
    } else {target.blogs <- blogs.bi
    }
  }
  
  if (exists("news.tetra") == TRUE) {target.news <- news.tetra
  } else {
    if (exists("news.tri") == TRUE) {target.news <- news.tri
    } else {target.news <- news.bi
    }
  }
  
  if (exists("tweets.tetra") == TRUE) {target.tweets <- tweets.tetra
  } else {
    if (exists("tweets.tri") == TRUE) {target.tweets <- tweets.tri
    } else {target.tweets <- tweets.bi
    }
  }
  
    # output prediction
    output$prediction <- renderText({
      paste("My prediction for the next word in your sentence is: ", values$ext)
    })
    
})})
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

library(shiny); library(stringr); library(data.table)

# load in lookup tables
blogs.biDT7.sorted <- readRDS("./data/blogs.biDT7.sorted.rds")
blogs.triDT7.sorted <- readRDS("./data/blogs.triDT7.sorted.rds")
blogs.tetraDT7.sorted <- readRDS("./data/blogs.tetraDT7.sorted.rds")
blogs.biDT7.sorted[order(p, decreasing = TRUE)]; blogs.triDT7.sorted[order(bigram, -rank(p))]
blogs.tetraDT7.sorted[order(trigram, -rank(p))]

setkey(blogs.biDT7.sorted); setkey(blogs.triDT7.sorted); setkey(blogs.tetraDT7.sorted)


news.biDT7.sorted <- readRDS("./data/news.biDT7.sorted.rds")
news.triDT7.sorted <- readRDS("./data/news.triDT7.sorted.rds")
news.tetraDT7.sorted <- readRDS("./data/news.tetraDT7.sorted.rds")
news.biDT7.sorted[order(gram, -rank(p))]; news.triDT7.sorted[order(bigram, -rank(p))]
news.tetraDT7.sorted[order(trigram, -rank(p))]
setkey(news.biDT7.sorted); setkey(news.triDT7.sorted); setkey(news.tetraDT7.sorted)

tweets.biDT7.sorted <- readRDS("./data/tweets.biDT7.sorted.rds")
tweets.triDT7.sorted <- readRDS("./data/tweets.triDT7.sorted.rds")
tweets.tetraDT7.sorted <- readRDS("./data/tweets.tetraDT7.sorted.rds")
tweets.biDT7.sorted[order(gram, -rank(p))]; tweets.triDT7.sorted[order(bigram, -rank(p))]
tweets.tetraDT7.sorted[order(trigram, -rank(p))]
setkey(tweets.biDT7.sorted); setkey(tweets.triDT7.sorted); setkey(tweets.tetraDT7.sorted)


# functions performing simple backoff lookup for largest possible n-gram stub
## BLOGS
lookup.blogs <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {
    tetra <- word(gram, -3:-1); tetra <- paste(tetra, collapse = ' ')
    tetra <- blogs.tetraDT7.sorted[tetra][1]
    
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- blogs.triDT7.sorted[tri][1]
    
    bi <- word(gram, -1)
    bi <- blogs.biDT7.sorted[bi][1]
    
    ifelse (!is.na(tetra$target), tetra.target <<- tetra$target, tetra.target <- tri$target)
    ifelse (!is.na(tetra.target), tetra.target <<- tri$target, tetra.target <<- bi$target)
  }
  
  if (gram.n == 2) {
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- blogs.triDT7.sorted[tri][1]
    
    bi <- word(gram, -1)
    bi <- blogs.biDT7.sorted[bi][1]
    
    ifelse (!is.na(tri$target), tri.target <<- tri$target, tri.target <<- bi$target)
  }
  
  if (gram.n == 1) {
    bi <- blogs.biDT7.sorted[gram][1]
    bi.target <<- bi$target
  }
}


## NEWS
lookup.news <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {
    tetra <- word(gram, -3:-1); tetra <- paste(tetra, collapse = ' ')
    tetra <- news.tetraDT7.sorted[tetra][1]
    
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- news.triDT7.sorted[tri][1]
    
    bi <- word(gram, -1)
    bi <- news.biDT7.sorted[bi][1]
    
    ifelse (!is.na(tetra$target), tetra.target <<- tetra$target, tetra.target <- tri$target)
    ifelse (!is.na(tetra.target), tetra.target <<- tri$target, tetra.target <<- bi$target)
  }
  
  if (gram.n == 2) {
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- news.triDT7.sorted[tri][1]
    
    bi <- word(gram, -1)
    bi <- news.biDT7.sorted[bi][1]
    
    ifelse (!is.na(tri$target), tri.target <<- tri$target, tri.target <<- bi$target)
  }
  
  if (gram.n == 1) {
    bi <- news.biDT7.sorted[gram][1]
    bi.target <<- bi$target
  }
}


## TWEETS
lookup.tweets <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {
    tetra <- word(gram, -3:-1); tetra <- paste(tetra, collapse = ' ')
    tetra <- tweets.tetraDT7.sorted[tetra][1]
    
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- tweets.triDT7.sorted[tri][1]
    
    bi <- word(gram, -1)
    bi <- tweets.biDT7.sorted[bi][1]
    
    ifelse (!is.na(tetra$target), tetra.target <<- tetra$target, tetra.target <- tri$target)
    ifelse (!is.na(tetra.target), tetra.target <<- tri$target, tetra.target <<- bi$target)
  }
  
  if (gram.n == 2) {
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- tweets.triDT7.sorted[tri][1]
    
    bi <- word(gram, -1)
    bi <- tweets.biDT7.sorted[bi][1]
    
    ifelse (!is.na(tri$target), tri.target <<- tri$target, tri.target <<- bi$target)
  }
  
  if (gram.n == 1) {
    bi <- tweets.biDT7.sorted[gram][1]
    bi.target <<- bi$target
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
    
    
    # lookup and isolate highest possible n-gram from lookup
    if (corpus == "blogs") {lookup.blogs(string.5)
                            ifelse (exists("tetra.target"), target <<- tetra.target, target <<- tri.target)
                            ifelse (exists("tri.target"), target <<- tri.target, target <<- bi.target)
    } else if (corpus == "news") {lookup.news(string.5)
                               ifelse (exists("tetra.target"), target <<- tetra.target, target <<- tri.target)
                               ifelse (exists("tri.target"), target <<- tri.target, target <<- bi.target)
    } else {lookup.news(string.5) 
                                ifelse (exists("tetra.target"), target <<- tetra.target, target <<- tri.target)
                                ifelse (exists("tri.target"), target <<- tri.target, target <<- bi.target)
           }
  
    # output prediction
    output$prediction <- renderText({
      paste("My prediction for the next word in your sentence is: ", values$target)
    })
    
})})
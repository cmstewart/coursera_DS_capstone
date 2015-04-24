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
profanity_list <- readLines("http://www.bannedwordlist.com/lists/swearWords.txt", warn = FALSE)

# load in lookup tables
blogs.bi_finDT <- readRDS("data/blogs.bi_finDT.rds")
blogs.tri_finDT <- readRDS("data/blogs.tri_finDT.rds")
blogs.tetra_finDT <- readRDS("data/blogs.tetra_finDT.rds")

news.bi_finDT <- readRDS("data/news.bi_finDT.rds")
news.tri_finDT <- readRDS("data/news.tri_finDT.rds")
news.tetra_finDT <- readRDS("data/news.tetra_finDT.rds")

tweets.bi_finDT <- readRDS("data/tweets.bi_finDT.rds")
tweets.tri_finDT <- readRDS("data/tweets.tri_finDT.rds")
tweets.tetra_finDT <- readRDS("data/tweets.tetra_finDT.rds")


# Lookup and isolate highest possible n-gram from lookup
lookup <- function (corpus.type, string) {
  if (corpus.type == "blogs") {lookup.blogs(string)
                               if (exists("tetra.target")) {target <<- tetra.target
                               } else if (exists("tri.target")) {target <<- tri.target
                               } else if (exists("bi.target")) {target <<- bi.target
                               }
                            }
                                   
  if (corpus.type == "news") {lookup.news(string)
                               if (exists("tetra.target")) {target <<- tetra.target
                               } else if (exists("tri.target")) {target <<- tri.target
                               } else if (exists("bi.target")) {target <<- bi.target
                               }
                            }
                                     
  if (corpus.type == "tweets") {lookup.tweets(string)
                               if (exists("tetra.target")) {target <<- tetra.target
                               } else if (exists("tri.target")) {target <<- tri.target
                               } else if (exists("bi.target")) {target <<- bi.target
                               }
                            }
                        }


# Functions performing simple backoff lookup for largest possible n-gram stub
## BLOGS
lookup.blogs <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {
    tetra <- word(gram, -3:-1); tetra <- paste(tetra, collapse = ' ')
    tetra <- blogs.tetra_finDT[tetra][1]
    
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- blogs.tri_finDT[tri][1]
    
    bi <- word(gram, -1)
    bi <- blogs.bi_finDT[bi][1]
    
    ifelse (!is.na(tetra$target), tetra.target <<- tetra$target, tetra.target <- tri$target)
    ifelse (!is.na(tetra.target), tetra.target <<- tri$target, tetra.target <<- bi$target)
  }
  
  if (gram.n == 2) {
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- blogs.tri_finDT[tri][1]
    
    bi <- word(gram, -1)
    bi <- blogs.bi_finDT[bi][1]
    
    ifelse (!is.na(tri$target), tri.target <<- tri$target, tri.target <<- bi$target)
  }
  
  if (gram.n == 1) {
    bi <- blogs.bi_finDT[gram][1]
    bi.target <<- bi$target
  }
}


## NEWS
lookup.news <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {
    tetra <- word(gram, -3:-1); tetra <- paste(tetra, collapse = ' ')
    tetra <- news.tetra_finDT[tetra][1]
    
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- news.tri_finDT[tri][1]
    
    bi <- word(gram, -1)
    bi <- news.bi_finDT[bi][1]
    
    ifelse (!is.na(tetra$target), tetra.target <<- tetra$target, tetra.target <- tri$target)
    ifelse (!is.na(tetra.target), tetra.target <<- tri$target, tetra.target <<- bi$target)
  }
  
  if (gram.n == 2) {
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- news.tri_finDT[tri][1]
    
    bi <- word(gram, -1)
    bi <- news.bi_finDT[bi][1]
    
    ifelse (!is.na(tri$target), tri.target <<- tri$target, tri.target <<- bi$target)
  }
  
  if (gram.n == 1) {
    bi <- news.bi_finDT[gram][1]
    bi.target <<- bi$target
  }
}


## TWEETS
lookup.tweets <- function(gram) {
  gram.n <- length(strsplit(gram,' ')[[1]])
  if (gram.n >= 3) {
    tetra <- word(gram, -3:-1); tetra <- paste(tetra, collapse = ' ')
    tetra <- tweets.tetra_finDT[tetra][1]
    
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- tweets.tri_finDT[tri][1]
    
    bi <- word(gram, -1)
    bi <- tweets.bi_finDT[bi][1]
    
    ifelse (!is.na(tetra$target), tetra.target <<- tetra$target, tetra.target <- tri$target)
    ifelse (!is.na(tetra.target), tetra.target <<- tri$target, tetra.target <<- bi$target)
  }
  
  if (gram.n == 2) {
    tri <- word(gram, -2:-1); tri <- paste(tri, collapse = ' ')
    tri <- tweets.tri_finDT[tri][1]
    
    bi <- word(gram, -1)
    bi <- tweets.bi_finDT[bi][1]
    
    ifelse (!is.na(tri$target), tri.target <<- tri$target, tri.target <<- bi$target)
  }
  
  if (gram.n == 1) {
    bi <- tweets.bi_finDT[gram][1]
    bi.target <<- bi$target
  }
}



shinyServer(function(input, output) {

  observe ({
  # Clean incoming string
    string.0 <- tolower(input$stub); string.1 <- str_replace_all(string.0, "[^[:alnum:][:space:]'|’]", ""); 
    string.2 <- iconv(string.1, from="UTF-8", to="ascii", sub=""); string.3 <- iconv(string.2, to="ASCII//TRANSLIT"); 
    string.4 <- str_replace_all(string.3, "[[:digit:]]+", ""); string.4.5 <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", string.4)
    string.5 <- str_replace_all(string.4.5, paste(profanity_list, collapse = "|"), replacement = "")
  
    rm(string.0); rm(string.1); rm(string.2); rm(string.3); rm(string.4); rm(string.4.5)
    
  output$prediction <- renderText ({
    if (input$submission == 0)
      return ()
    else 
      
    lookup(input$corpus, string.5)
      
    paste("Your word:", target, ".")
    })
  })
})
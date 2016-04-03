require(tm)
require(data.table)

# read in n-grams

df.1g <- fread("data/final/onegrams.csv")
df.2g <- fread("data/final/twograms.csv")
df.3g <- fread("data/final/threegrams.csv")
df.4g <- fread("data/final/fourgrams.csv")

ngraminator <- function(x, spaces) {
  
  temp <- strsplit(x," ")
  
  # bigram
  if(spaces == 1){
    
    # print("bigrams")
    # probability of first word
    prob_a <- subset(df.1g,grepl(paste("^",temp[[1]][1],sep=""),token))[1,]
    # bigram with probability
    bigrams <- subset(df.2g,grepl(paste("^",x,sep=""),token))
    
    # check if they exist
    if(!is.na(prob_a$token) & length(bigrams$token)>0) {
      # conditional probability of bigrams with backoff smoothing
      bigrams$conditional <- 0.4 * (as.numeric(bigrams$prob)/prob_a$prob)
      bigrams <- bigrams[order(-bigrams$conditional),]
      result <- strsplit(as.character(head(bigrams$token,1))," ")
      print(result[[1]][2])
    } else {
      # print("backoff 1")
      unigrams <- subset(df.1g,grepl(paste("^",temp[[1]][2],sep=""),token))
      unigrams$conditional <- 0.4 * 0.4 * (as.numeric(unigrams$prob))
      unigrams <- unigrams[order(-unigrams$conditional),]
      result <- head(unigrams$token,1)
      print(result)
    }
  }
  
  # trigrams
  if(spaces == 2){
    
    print("trigrams")
    # probability of first two words
    prob_a <- subset(df.2g,grepl(paste("^",
                                       paste(temp[[1]][1:2], collapse=" ")
                                       ,sep=""),token))[1,]
    # trigram with probability
    trigrams <- subset(df.3g,grepl(paste("^",x,sep=""),token))
    
    # check if they exist
    if(!is.na(prob_a$token) & length(trigrams$token)>0) {
      # conditional probability of trigrams with backoff smoothing
      trigrams$conditional <- 0.4 * (as.numeric(trigrams$prob)/prob_a$prob)
      trigrams <- trigrams[order(-trigrams$conditional),]
      result <- strsplit(as.character(head(trigrams$token,1))," ")
      print(result[[1]][3])
    } else {
      print("backoff 1")
      prob_a <- subset(df.1g,grepl(paste("^",temp[[1]][2],sep=""),token))[1,]
      bigrams <- subset(df.2g,grepl(paste("^",
                                          paste(temp[[1]][2:3], collapse=" "),
                                          sep=""),token))
      if(!is.na(prob_a$token) & length(bigrams$token)>0) {        
        bigrams$conditional <- 0.4 * 0.4 * (as.numeric(bigrams$prob)/prob_a$prob)
        bigrams <- bigrams[order(-bigrams$conditional),]
        result <- strsplit(as.character(head(bigrams$token,1))," ")
        print(result[[1]][2])
      } else {
        print("backoff 2")
        unigrams <- subset(df.1g,grepl(paste("^",temp[[1]][3],sep=""),token))
        unigrams$conditional <- 0.4 * 0.4 * (as.numeric(unigrams$prob))
        unigrams <- unigrams[order(-unigrams$conditional),]
        result <-head(unigrams$token,1)
        print(result)
      }
    }
  }
  
  # quadgrams
  if(spaces == 3){
    
    print("quadgrams")
    # probability of first three words
    prob_a <- subset(df.3g,grepl(paste("^",
                                       paste(temp[[1]][1:3], collapse=" ")
                                       ,sep=""),token))[1,]
    # quadgram with probability
    quadgrams <- subset(df.4g,grepl(paste("^",x,sep=""),token))
    
    # check if they exist
    if(!is.na(prob_a$token) & length(quadgrams$token)>0) {
      # conditional probability of quadgrams with backoff smoothing
      quadgrams$conditional <- 0.4 * (as.numeric(quadgrams$prob)/prob_a$prob)
      quadgrams <- quadgrams[order(-quadgrams$conditional),]
      result <- strsplit(as.character(head(quadgrams$token,1))," ")
      print(result[[1]][4])
    } else {    
      # probability of last two words
      prob_a <- subset(df.2g,grepl(paste("^",
                                         paste(temp[[1]][2:3], collapse=" "),
                                         sep=""),token))[1,]
      # trigram with probability
      trigrams <- subset(df.3g,grepl(paste("^",
                                           paste(temp[[1]][2:4], collapse=" "),
                                           sep=""),token))
      
      # check if they exist
      if(!is.na(prob_a$token) & length(trigrams$token)>0) {
        # conditional probability of trigrams with backoff smoothing
        trigrams$conditional <- 0.4 * 0.4 * (as.numeric(trigrams$prob)/prob_a$prob)
        trigrams <- trigrams[order(-trigrams$conditional),]
        result <- strsplit(as.character(head(trigrams$token,1))," ")
        print(result[[1]][3])
      } else {
        print("backoff 1")
        prob_a <- subset(df.1g,grepl(paste("^",temp[[1]][3],sep=""),token))[1,]
        bigrams <- subset(df.2g,grepl(paste("^",
                                            paste(temp[[1]][3:4], collapse=" "),
                                            sep=""),token))
        if(!is.na(prob_a$token) & length(bigrams$token)>0) {        
          bigrams$conditional <- 0.4 * 0.4 * 0.4 * (as.numeric(bigrams$prob))
          bigrams <- bigrams[order(-bigrams$conditional),]
          result <- strsplit(as.character(head(bigrams$token,1))," ")
          print(result[[1]][2])
        } else {
          print("backoff 2")
          unigrams <- subset(df.1g,grepl(paste("^",temp[[1]][4],sep=""),token))
          unigrams$conditional <- 0.4 * 0.4 * 0.4 * (as.numeric(unigrams$prob))
          unigrams <- unigrams[order(-unigrams$conditional),]
          result <- head(unigrams$token,1)
          print(result)
        }
      }
    }  
  }
}

bea <- function(x) {
  # convert to lowercase
  x <- tolower(x)
  # unigram
  if(!grepl(" ",x) >= 1) {
    search <- subset(df.1g,grepl(paste("^",x,sep=""),token))
    print(head(search$token,1))    
  } 
  
  # everything else
  else {
    
    spaces <- length(gregexpr("\\s",x)[[1]])
    
    # handling opening words of the sentences
    if (spaces <= 3) {
      ngraminator(x,spaces)
    } 
    
    # handling longer sentences
    else {
      is_blank <- substr(x, nchar(x),nchar(x))
      temp <- strsplit(x," ")
      if (is_blank == " ") {
        x <- paste(paste(temp[[1]][(length(temp[[1]])-2):length(temp[[1]])],collapse=" "),"")
        print(x)
        ngraminator(x,3)
      } else { 
        x <- paste(temp[[1]][(length(temp[[1]])-2):length(temp[[1]])],collapse=" ")
        print(x)
        ngraminator(x,2)
      }
    }

  }        
}
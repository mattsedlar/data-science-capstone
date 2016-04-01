require(tm)
require(data.table)

# read in n-grams

df.1g <- fread("data/final/onegrams.csv")
df.2g <- fread("data/final/twograms.csv")
df.3g <- fread("data/final/threegrams.csv")
df.4g <- fread("data/final/fourgrams.csv")

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
    
    temp <- strsplit(x," ")
    spaces <- length(gregexpr("\\s",x)[[1]])
    
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
        bigrams <- bigrams[order(-conditional),]
        result <- strsplit(head(bigrams$token,1)," ")
        print(result[[1]][2])
      } else {
        # print("backoff 1")
        unigrams <- subset(df.1g,grepl(paste("^",temp[[1]][2],sep=""),token))
        unigrams$conditional <- 0.4 * 0.4 * (as.numeric(unigrams$prob))
        unigrams <- unigrams[order(-conditional),]
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
        trigrams <- trigrams[order(-conditional),]
        result <- strsplit(head(trigrams$token,1)," ")
        print(result[[1]][3])
      } else {
        print("backoff 1")
        prob_a <- subset(df.1g,grepl(paste("^",temp[[1]][2],sep=""),token))[1,]
        bigrams <- subset(df.2g,grepl(paste("^",
                                            paste(temp[[1]][2:3], collapse=" "),
                                            sep=""),token))
        if(!is.na(prob_a$token) & length(bigrams$token)>0) {        
          bigrams$conditional <- 0.4 * 0.4 * (as.numeric(bigrams$prob))
          bigrams <- bigrams[order(-conditional),]
          result <- strsplit(head(bigrams$token,1)," ")
          print(result[[1]][2])
        } else {
          print("backoff 2")
          unigrams <- subset(df.1g,grepl(paste("^",temp[[1]][3],sep=""),token))
          unigrams$conditional <- 0.4 * 0.4 * (as.numeric(unigrams$prob))
          unigrams <- unigrams[order(-conditional),]
          result <- head(unigrams$token,1)
          print(result)
        }
      }
    }
    
  }        
}
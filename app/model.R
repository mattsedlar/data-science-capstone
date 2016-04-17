require(tm)
require(data.table)
require(dplyr)

# read in n-grams
df.1g <- fread("data/onegrams.csv")
df.2g <- fread("data/twograms.csv")
df.3g <- fread("data/threegrams.csv")
df.4g <- fread("data/fourgrams.csv")

ngraminator <- function(x, spaces) {
  
  temp <- strsplit(x," ")
  
  # bigram
  if(spaces == 1){
    # probability of first word
    prob_a <- subset(df.1g,grepl(paste("^",temp[[1]][1],sep=""),token))[1,]
    # bigram with probability
    bigrams <- subset(df.2g,grepl(paste("^",x,sep=""),token))
    
    # check if they exist
    if(!is.na(prob_a$token) & length(bigrams$token)>0) {
      # conditional probability of bigrams with backoff smoothing
      bigrams$conditional <- 0.4 * (as.numeric(bigrams$prob)/prob_a$prob)
      bigrams <- bigrams[order(-bigrams$conditional),]
      result <- strsplit(as.character(head(bigrams$token,3))," ")
      for (i in result){
        cat("<span class='label label-success'>",i[2],"</span>",sep="",fill=T)
      }
      
    } else {
      unigrams <- subset(df.1g,grepl(paste("^",
                                           paste(gsub(" ","",sub(temp[[1]][1],"",x)),
                                                 sep=""),sep=""),token))
      unigrams$conditional <- 0.4 * 0.4 * (as.numeric(unigrams$prob))
      unigrams <- unigrams[order(-unigrams$conditional),]
      result <- head(unigrams$token,3)
      for (i in result){
        cat("<span class='label label-success'>",i,"</span>",sep="",fill=T)
      }
    }
  }
  
  # trigrams
  if(spaces == 2){
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
      result <- strsplit(as.character(head(trigrams$token,3))," ")
      for (i in result){
        cat("<span class='label label-success'>",i[3],"</span>",sep="",fill=T)
      }
      
    } else {
      prob_a <- subset(df.1g,grepl(paste("^",temp[[1]][2],sep=""),token))[1,]
      bigrams <- subset(df.2g,grepl(paste("^",
                                          trimws(sub(temp[[1]][1],"",x)),
                                          sep=""),token))
      if(!is.na(prob_a$token) & length(bigrams$token)>0) {        
        bigrams$conditional <- 0.4 * 0.4 * (as.numeric(bigrams$prob)/prob_a$prob)
        bigrams <- bigrams[order(-bigrams$conditional),]
        result <- strsplit(as.character(head(bigrams$token,3))," ")
        for (i in result){
          cat("<span class='label label-success'>",i[2],"</span>",sep="",fill=T)
        }
        
      } else {
        unigrams <- subset(df.1g,grepl(paste("^",paste(gsub(" ",
                                                  "",
                                                  sub(paste(temp[[1]][1:2],collapse=" "),
                                                      "",x)),sep=""),sep=""),token))
        unigrams$conditional <- 0.4 * 0.4 * (as.numeric(unigrams$prob))
        unigrams <- unigrams[order(-unigrams$conditional),]
        result <- head(unigrams$token,3)
        for (i in result){
          cat("<span class='label label-success'>",i,"</span>",sep="",fill=T)
        }
        
      }
    }
  }
  
  # quadgrams
  if(spaces == 3){
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
      result <- strsplit(as.character(head(quadgrams$token,3))," ")
      for (i in result){
        cat("<span class='label label-success'>",i[4],"</span>",sep="",fill=T)
      }
      
    } else {    
      # probability of last two words
      prob_a <- subset(df.2g,grepl(paste("^",
                                         paste(temp[[1]][2:3], collapse=" "),
                                         sep=""),token))[1,]
      # trigram with probability
      trigrams <- subset(df.3g,grepl(paste("^",
                                           paste(gsub(" ",
                                                      "",
                                                      sub(temp[[1]][1],"",x)),sep=""),
                                           sep=""),token))
      
      # check if they exist
      if(!is.na(prob_a$token) & length(trigrams$token)>0) {
        # conditional probability of trigrams with backoff smoothing
        trigrams$conditional <- 0.4 * 0.4 * (as.numeric(trigrams$prob)/prob_a$prob)
        trigrams <- trigrams[order(-trigrams$conditional),]
        result <- strsplit(as.character(head(trigrams$token,3))," ")
        for (i in result){
          cat("<span class='label label-success'>",i[3],"</span>",sep="",fill=T)
        }
        
      } else {
        prob_a <- subset(df.1g,grepl(paste("^",temp[[1]][3],sep=""),token))[1,]
        bigrams <- subset(df.2g,grepl(paste("^",paste(gsub(" ",
                                                 "",
                                                 sub(paste(temp[[1]][1:2],
                                                           collapse=" "),
                                                           "",x)),sep=""),sep=""),token))
        if(!is.na(prob_a$token) & length(bigrams$token)>0) {        
          bigrams$conditional <- 0.4 * 0.4 * 0.4 * (as.numeric(bigrams$prob))
          bigrams <- bigrams[order(-bigrams$conditional),]
          result <- strsplit(as.character(head(bigrams$token,3))," ")
          for (i in result){
            cat("<span class='label label-success'>",i[2],"</span>",sep="",fill=T)
          }
          
        } else {
          unigrams <- subset(df.1g,grepl(paste("^",paste(gsub(" ",
                                                             "",
                                                             sub(paste(temp[[1]][1:3],
                                                                       collapse=" "),
                                                                 "",x)),sep=""),sep=""),token))
          unigrams$conditional <- 0.4 * 0.4 * 0.4 * (as.numeric(unigrams$prob))
          unigrams <- unigrams[order(-unigrams$conditional),]
          result <- head(unigrams$token,3)
          for (i in result){
            cat("<span class='label label-success'>",i,"</span>",sep="",fill=T)
          }
        }
      }
    }  
  }
}

phraseinator <- function(x) {
  if(nchar(x) >= mean(nchar(df.2g$token))) {
    # unigrams
    tokens <- unlist(strsplit(x,"[^a-z]+"))
    tokens <- tokens[tokens != " "]
    # bigrams
    tokens2 <- c(tokens[-1],".")
    tokens2 <- paste(tokens,tokens2)
    tokens2 <- tokens2[1:length(tokens2)-1]
 
    # frequencies
    freq2 <- sort(table(tokens2),decreasing = T)
    
    withProgress(message="Adding Phrase", {
      df2 <- data.frame(token=c(names(freq2),df.2g$token),freq=c(freq2,df.2g$freq))
      incProgress(1/4,detail="merging data")
      df2 <- df2 %>% group_by(token) %>% summarize(freq = sum(freq))
      incProgress(1/2,detail="calculating")
      df2 <- df2 %>% mutate(prob = freq/sum(freq)) %>% arrange(desc(prob))
      incProgress(3/4,detail="writing")
      write.csv(df2,"data/twograms.csv",row.names = F)
    })
    return("Thanks for improving my model!")
  } else { return("This phrase is too short.") }
}

betty <- function(x) {
  # unigram
  if(!grepl(" ",x) >= 1) {
    search <- subset(df.1g,grepl(paste("^",x,sep=""),token))
    result <- head(search$token,3)
    for (i in result){
      cat("<span class='label label-success'>",i,"</span>",sep="",fill=T)
    }
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
        ngraminator(x,3)
      } else { 
        x <- paste(temp[[1]][(length(temp[[1]])-2):length(temp[[1]])],collapse=" ")
        ngraminator(x,2)
      }
    }

  }
}
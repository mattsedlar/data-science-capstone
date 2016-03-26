require(tm)

# read in n-grams

df.1g <- read.csv("data/final/onegrams.csv")
df.2g <- read.csv("data/final/twograms.csv")
df.3g <- read.csv("data/final/threegrams.csv")
df.4g <- read.csv("data/final/fourgrams.csv")

# pronouns as well as 'the' and 'and'

pronouns <- c("all","and", "another","any", "anybody","anyone","anything",
              "both", "each", "either","everybody","everyone","everything",
              "few","he","her","herself","him","himself","his",
              "I","it","its","itself","many","me","mine","more","most","much","myself",
              "neither","nobody","none","nothing","one","other","others","ours","ourselves",
              "several","she","some","somebody","something","that","the","their","theirs",
              "themselves","these","they","this","those","us","we","what","whatever",
              "whichever","who","whoever","whom","whomever","whose","you","your",
              "yours","yourself","yourselves")

## start model

model <- function(x) {
  
  # handling the first word
   if(!grepl(" ",x) >= 1) {
  
    index <- x == substr(pronouns[1:length(pronouns)],1,nchar(x))
    result <- pronouns[which(index==TRUE)]
    
    if(length(result)== 0) { 
      search <- subset(df.1g,substr(token,1,nchar(x))==x)
      print(head(search$token,1))
    }
    
    else { 
      if (length(result) > 1) {
        #df.1g as tie-breaker
        search <- subset(df.1g, grepl(paste("(",paste(result,collapse="|"),")"),token))
        print(head(search$token,1))
        
      } else {
        print(result)
      } 
    }
    
   }
  # 2-grams
  else if (grep(" ",x) == 1) {
    temp <- strsplit(x, " ")
    if(length(temp[[1]]) == 1) {
      search <- subset(df.2g,grepl(paste("^",x,sep=""),token))
      print(head(search$token,1))      
    }
    # 3-grams
    if(length(temp[[1]]) == 2) {    
      search <- subset(df.3g, grepl(paste("^",
                                          paste(temp[[1]],collapse=" "),
                                          sep=""),token))
      print(head(search$token,1))
    }
    # 4-grams
    if(length(temp[[1]]) == 3) {    
      search <- subset(df.4g, grepl(paste("^",
                                          paste(temp[[1]],collapse=" "),
                                          sep=""),token))
      print(head(search$token,1))
    }
    # 5-Inf-grams
    if(length(temp[[1]]) >= 4) {    
      search <- subset(df.4g, grepl(paste("^",
                                          paste(temp[[1]][((length(temp[[1]]) - 3) + 1):length(temp[[1]])],
                                                collapse=" "),
                                          sep=""),token))
      print(paste(paste(temp[[1]][1:(length(temp[[1]]) - 3)], collapse=" "),
                  head(search$token,1)))
    }
    
  }

}
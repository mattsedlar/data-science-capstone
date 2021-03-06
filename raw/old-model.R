require(tm)
require(data.table)

# read in n-grams

df.1g <- fread("data/final/onegrams.csv")
df.2g <- fread("data/final/twograms.csv")
df.3g <- fread("data/final/threegrams.csv")
df.4g <- fread("data/final/fourgrams.csv")
df.5g <- fread("data/final/fivegrams.csv")

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

punctuation <- c(".","!","?")

## start model

model <- function(x) {
  
  # check for punctuation
  if (grepl("[.|!|?]",x) == FALSE) {
    
    # handling the first word
    if(!grepl(" ",x) >= 1) {
      index <- x == substr(pronouns[1:length(pronouns)],1,nchar(x))
      result <- pronouns[which(index==TRUE)]
      
      if(length(result) == 0) { 
        search <- subset(df.1g,substr(token,1,nchar(x))==x)
        print(head(search$token,1))
      }
      
      else { 
        if (length(result) > 1) {
          #df.1g as tie-breaker
          search <- subset(df.1g, grepl(paste("(",
                                              paste(result,collapse="|")
                                              ,")", sep=""),token))
          print(head(search$token,1))
          
        } else {
          print(result)
        } 
      }
      
    }
    
    # 2-grams
    else if (grep(" ",x) == 1) {
      
      temp <- strsplit(x, " ")
      spaces <- gregexpr("\\W",x)
      
      if(length(spaces[[1]]) == 1) {
        
        search <- subset(df.2g,grepl(paste("^",x,sep=""),token))
        
        result <- head(search$token,1)
        
        if (length(result) > 0) {
          result <- strsplit(result," ")
          print(result[[1]][length(result[[1]])])
        } else { print(paste(temp[[1]],collapse = " ")) }
        
      }
      
      # 3-grams
      if(length(spaces[[1]]) == 2) {    
        search <- subset(df.3g, grepl(paste("^",
                                            paste(temp[[1]],collapse=" "),
                                            sep=""),token))
        result <- head(search$token,1)
        
        if (length(result) > 0) {
          result <- strsplit(result," ")
          print(result[[1]][length(result[[1]])])
        } else { print(paste(temp[[1]],collapse = " ")) }
        
      }
      
      # 4-grams
      if(length(spaces[[1]]) == 3) {    
        search <- subset(df.4g, grepl(paste("^",
                                            paste(temp[[1]],collapse=" "),
                                            sep=""),token))
        result <- head(search$token,1)
        
        if (length(result) > 0) {
          result <- strsplit(result," ")
          print(result[[1]][length(result[[1]])])
        } else { print(paste(temp[[1]],collapse = " ")) }
        
      }
      
      # 5-grams
      if(length(spaces[[1]]) == 4) {    
        search <- subset(df.5g, grepl(paste("^",
                                            paste(temp[[1]],collapse=" "),
                                            sep=""),token))
        result <- head(search$token,1)
        
        if (length(result) > 0) {
          result <- strsplit(result," ")
          print(result[[1]][length(result[[1]])])
        } else { print(paste(temp[[1]],collapse = " ")) }
        
      }
      
      if(length(spaces[[1]]) >= 5) {
        
        x <- paste(temp[[1]][((length(temp[[1]]) - 3)):length(temp[[1]])],
                      collapse=" ")
        temp <- strsplit(x, " ")

        search <- subset(df.5g, grepl(paste("^",
                                            paste(temp[[1]],collapse=" "),
                                            sep=""),token))
        result <- head(search$token,1)
        
        if(length(result) > 0) {
          result <- strsplit(result," ")
          print(result[[1]][length(result[[1]])])          
        } else {
          
          x <- paste(temp[[1]][(length(temp[[1]]) - 2):length(temp[[1]])],
                     collapse=" ")
          temp <- strsplit(x, " ")         
          
          search <- subset(df.4g, grepl(paste("^",
                                              paste(temp[[1]],collapse=" "),
                                              sep=""),token))
          result <- head(search$token,1)
          
          if(length(result) > 0) {
            result <- strsplit(result," ")
            print(result[[1]][length(result[[1]])])  
          } else {
            
            search <- subset(df.3g, grepl(paste("^",
                                                paste(temp[[1]][(length(temp[[1]]) - 2):length(temp[[1]])],
                                                      collapse=" "),
                                                sep=""),token))
            result <- head(search$token,1)        
            
            if(length(result) > 0) {
              
              print(paste(paste(temp[[1]][1:(length(temp[[1]]) - 1)], collapse=" "),
                          result)) 
            } else {
              
              search <- subset(df.2g, grepl(paste("^",temp[[1]][length(temp[[1]])],
                                                  sep=""),token))
              
              result <- head(search$token,1)
              
              if(length(result) > 0) {
                print(paste(paste(temp[[1]][1:length(temp[[1]])-1], collapse=" "),
                            result))
              } else { print(paste(temp[[1]],collapse = " ")) }        
              
            }
            
          }
          
        }
      }
    }
  } else { print(x) }
}
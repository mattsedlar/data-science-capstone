require(tm)

# read in n-grams

df.1g <- read.csv("data/final/onegrams.csv")
df.2g <- read.csv("data/final/twograms.csv")
df.3g <- read.csv("data/final/threegrams.csv")

pronouns <- c("all","another","any", "anybody","anyone","anything",
              "both", "each", "either","everybody","everyone","everything",
              "few","he","her","herself","him","himself","his",
              "I","it","its","itself","many","me","mine","more","most","much","myself",
              "neither","nobody","none","nothing","one","other","others","ours","ourselves",
              "several","she","some","somebody","something","that","their","theirs",
              "themselves","these","they","this","those","us","we","what","whatever",
              "whichever","who","whoever","whom","whomever","whose","you","your",
              "yours","yourself","yourselves")

## start model

model <- function(x) {
  
  # handling the first word
   if(!grepl(" ",x) >= 1) {
  
    index <- x == substr(pronouns[1:length(pronouns)],1,nchar(x))
    
    if(length(pronouns[which(index==TRUE)])==0) { 
      search <- subset(df.1g,substr(token,1,nchar(x))==x)
      print(head(search$token,1))
    }
    
    else { 
      print(pronouns[which(index==TRUE)]) 
    }
    
  } else if (grep(" ",x) == 1) {
    search <- subset(df.2g, grepl(paste("^",x,sep=""),token))
    print(head(search$token,1))
  }
}
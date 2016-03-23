require(tm)

# read in n-grams

df.1g <- read.csv("data/final/onegrams.csv")
df.2g <- read.csv("data/final/twograms.csv")

pronouns <- c("I", "me", "he",
              "she", "herself", "you",
              "it", "that", "they", "each", 
              "few", "many", "who",
              "whoever", "whose", "someone",
              "everybody")

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
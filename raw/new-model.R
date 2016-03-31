require(tm)
require(data.table)

# read in n-grams

df.1g <- fread("data/final/onegrams.csv")
df.2g <- fread("data/final/twograms.csv")
df.3g <- fread("data/final/threegrams.csv")
df.4g <- fread("data/final/fourgrams.csv")

bea <- function(x) {
  # unigram
  if(!grepl(" ",x) >= 1) {
    search <- subset(df.1g,grepl(paste("^",x,sep=""),token))
    print(head(search$token,1))    
  } else {
  # bigram
    temp <- strsplit(x," ")
    # probability of first word
    prob_a <- subset(df.1g,grepl(paste("^",temp[[1]][1],sep=""),token))[1,]
    bigrams <- subset(df.2g,grepl(paste("^",x,sep=""),token))
    # conditional probability of bigrams with backoff smoothing
    bigrams$conditional <- 0.4 * (as.numeric(bigrams$prob)/prob_a$prob)
    bigrams <- bigrams[order(-conditional),]
    print(head(bigrams,1))
  }
}
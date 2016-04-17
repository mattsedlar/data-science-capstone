require(tm)
require(R.utils)
require(LaF)
require(SnowballC)
require(quanteda)

options(mc.cores = 1)
# count lines first to speed up process
twitter.n <- countLines("data/en_US.twitter.txt")
blog.n <- countLines("data/en_US.blogs.txt")
news.n <- countLines("data/en_US.news.txt")

# read lines randomly into a data table column
set.seed(1234)

docs <- c(data.frame(sample_lines("data/en_US.twitter.txt", .0275 * twitter.n,nlines=twitter.n)),
          data.frame(sample_lines("data/en_US.blogs.txt", .05 * blog.n, nlines=blog.n)),
          data.frame(sample_lines("data/en_US.news.txt", .05 * news.n, nlines=news.n)),
          data.frame(readLines("data/en_US.academic.txt", encoding="UTF-8")),
          data.frame(readLines("data/en_US.entertainment.txt", encoding = "UTF-8")))

# # corpora with tm
c <- VCorpus(VectorSource(docs))
 
# eliminate whitespace
c <- tm_map(c, stripWhitespace)
(f <- content_transformer(function(x,pattern,sub) gsub(pattern,sub,x)))
# to lower case
# c <- tm_map(c, content_transformer(tolower))
# remove numbers
c <- tm_map(c, content_transformer(removeNumbers))

# profanity filtering
bad.words <- c()
bad.nouns <- c("ass","nigg","dick","slut","pussy","cunt","cooch","panties","titt","\\bhell\\b","dildo")
bad.verbs <- c("fuck","shit","suck","fart","piss","bitch")

bad.words <- c(bad.words,
               paste(bad.nouns,"(a|as|es|er?)?s?|",sep=""),
               paste(bad.verbs,"(a|e|ed|er|ty|ing?)?s?|",sep=""))

bad.words <- paste(bad.words,collapse="")
bad.words <- substr(bad.words,1,nchar(bad.words)-1)
c <- tm_map(c, f, bad.words,"")

# punctuation
punctuation <- "[^[:alnum:][:space:]'-]"
double.hyphen <- "--"
c <- tm_map(c, f, punctuation,"")
c <- tm_map(c, f, double.hyphen,"")

# # TOKENIZERS
c.quant <- corpus(c)
rm(c)

#unigrams
tokens <- tokenize(c.quant)
dfm <- dfm(tokens)
dfm <- trim(dfm, minDoc=2)
rm(tokens)

#bigrams
tokens2 <- tokenize(c.quant,ngrams=2, concatenator=" ")
dfm2 <- dfm(tokens2)
dfm2 <- trim(dfm2, minDoc = 2)
rm(tokens2)

#trigrams
tokens3 <- tokenize(c.quant,what="fasterword",ngrams=3, concatenator=" ")
dfm3 <- dfm(tokens3)
dfm3 <- trim(dfm3, minDoc = 2)
rm(tokens3)

#quadgrams
tokens4 <- tokenize(c.quant,what="fastestword",ngrams=4, concatenator=" ")
dfm4 <- dfm(tokens4)
dfm4 <- trim(dfm4, minDoc = 2)
rm(tokens4)

# frequency for calculating probabilities 
frequency.1g <- colSums(dfm)
frequency.2g <- colSums(dfm2)
frequency.3g <- colSums(dfm3)
frequency.4g <- colSums(dfm4)

# 
frequency.1g <- sort(frequency.1g, decreasing = T)
frequency.2g <- sort(frequency.2g, decreasing = T)
frequency.3g <- sort(frequency.3g, decreasing = T)
frequency.4g <- sort(frequency.4g, decreasing = T)

# create data frames with probabilities

df.1g <- data.frame(token=names(frequency.1g), freq=frequency.1g, prob=frequency.1g/sum(frequency.1g))
df.2g <- data.frame(token=names(frequency.2g), freq=frequency.2g, prob=frequency.2g/sum(frequency.2g))
df.3g <- data.frame(token=names(frequency.3g), freq=frequency.3g, prob=frequency.3g/sum(frequency.3g))
df.4g <- data.frame(token=names(frequency.4g), freq=frequency.4g, prob=frequency.4g/sum(frequency.4g))

write.csv(df.1g,"data/final/onegrams.csv",row.names = F)
write.csv(df.2g,"data/final/twograms.csv",row.names = F)
write.csv(df.3g,"data/final/threegrams.csv",row.names = F)
write.csv(df.4g,"data/final/fourgrams.csv",row.names = F)

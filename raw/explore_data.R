require(tm)
require(R.utils)
require(LaF)
require(SnowballC)

# count lines first to speed up process
twitter.n <- countLines("data/en_US.twitter.txt")
blog.n <- countLines("data/en_US.blogs.txt")
news.n <- countLines("data/en_US.news.txt")

# read lines randomly into a data table column
set.seed(1234)

docs <- c(data.frame(sample_lines("data/en_US.twitter.txt", .016 * twitter.n,nlines=twitter.n)),
          data.frame(sample_lines("data/en_US.blogs.txt", .016 * blog.n, nlines=blog.n)),
          data.frame(sample_lines("data/en_US.news.txt", .016 * news.n, nlines=news.n)))

# # corpora
c <- Corpus(VectorSource(docs))

# check it out
inspect(c)
 
# eliminate whitespace
c <- tm_map(c, stripWhitespace)
# remove all punctuation except apostrophes, fix 'I'm'
(f <- content_transformer(function(x,pattern,sub) gsub(pattern,sub,x)))
c <- tm_map(c, f, "[^[:alnum:][:space:]']","")
# to lower case
c <- tm_map(c, content_transformer(tolower))
# remove numbers
c <- tm_map(c, content_transformer(removeNumbers))

# profanity filtering
bad.words <- c()
bad.nouns <- c("ass","nigg","dick","slut","pussy","cunt","cooch","panties","titt","hell\\b")
bad.verbs <- c("fuck","shit","suck","fart","piss","bitch")

bad.words <- c(bad.words,
               paste(bad.nouns,"(a|as|es|er?)?s?|",sep=""),
               paste(bad.verbs,"(a|e|ed|er|ty|ing?)?s?|",sep=""))

bad.words <- paste(bad.words,collapse="")
bad.words <- substr(bad.words,1,nchar(bad.words)-1)
c <- tm_map(c, f, bad.words,"")

writeCorpus(c,"data")

# # TOKENIZERS

BigramTokenizer <- function(x) {
 unlist(lapply(ngrams(words(x),2),paste,collapse=" "), use.names = F)
}
TrigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x),3),paste,collapse=" "), use.names = F)
}
QuatrogramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x),4),paste,collapse=" "), use.names = F)
}
# 
# # term document matrix for 1-4grams
tdm.1g <- TermDocumentMatrix(c)
tdm.2g <- TermDocumentMatrix(c, control=list(tokenize=BigramTokenizer))
tdm.3g <- TermDocumentMatrix(c, control=list(tokenize=TrigramTokenizer))
tdm.4g <- TermDocumentMatrix(c, control=list(tokenize=QuatrogramTokenizer))
# 
# # removing sparse terms in 1-gram
tdm.1g.common20 <- removeSparseTerms(tdm.1g,.20)
tdm.2g.common50 <- removeSparseTerms(tdm.2g,.50)
tdm.3g.common50 <- removeSparseTerms(tdm.3g,.50)
tdm.4g.common50 <- removeSparseTerms(tdm.4g,.50)
# 
# # finding most-common terms
tdm.1g.common20.matrix <- as.matrix(tdm.1g.common20)
tdm.2g.common50.matrix <- as.matrix(tdm.2g.common50)
tdm.3g.common50.matrix <- as.matrix(tdm.3g.common50)
tdm.4g.common50.matrix <- as.matrix(tdm.4g.common50)


# frequency for calculating probabilities 
frequency.1g <- rowSums(tdm.1g.common20.matrix)
frequency.2g <- rowSums(tdm.2g.common50.matrix)
frequency.3g <- rowSums(tdm.3g.common50.matrix)
frequency.4g <- rowSums(tdm.4g.common50.matrix)

# 
frequency.1g <- sort(frequency.1g, decreasing = T)
frequency.2g <- sort(frequency.2g, decreasing = T)
frequency.3g <- sort(frequency.3g, decreasing = T)
frequency.4g <- sort(frequency.4g, decreasing = T)


df.1g <- data.frame(token=names(frequency.1g), freq=frequency.1g)
df.2g <- data.frame(token=names(frequency.2g), freq=frequency.2g)
df.3g <- data.frame(token=names(frequency.3g), freq=frequency.3g)
df.4g <- data.frame(token=names(frequency.4g), freq=frequency.4g)

write.csv(df.1g,"data/final/onegrams.csv",row.names = F)
write.csv(df.2g,"data/final/twograms.csv",row.names = F)
write.csv(df.3g,"data/final/threegrams.csv",row.names = F)
write.csv(df.4g,"data/final/fourgrams.csv",row.names = F)

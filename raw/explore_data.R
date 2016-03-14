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

docs <- c(data.frame(sample_lines("data/en_US.twitter.txt", .015 * twitter.n)),
          data.frame(sample_lines("data/en_US.blogs.txt", .015 * blog.n)),
          data.frame(sample_lines("data/en_US.news.txt", .015 * news.n)))

# # corpora
c <- Corpus(VectorSource(docs))

# check it out
inspect(c)
 
# eliminate whitespace
c <- tm_map(c, stripWhitespace)
# remove all punctuation except apostrophes, fix 'I'm'
(f <- content_transformer(function(x,pattern,sub) gsub(pattern,sub,x)))
c <- tm_map(c, f, "[^[:alnum:][:space:]']","")
c <- tm_map(c, f, "(i'm)","I'm")
# to lower case
c <- tm_map(c, content_transformer(tolower))
# remove numbers
c <- tm_map(c, content_transformer(removeNumbers))

writeCorpus(c,"data")

# TOKENIZER
BigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x),2),paste, collapse=" "), use.names = F)
}

# term document matrix
tdm <- TermDocumentMatrix(c, control=list(tokenize=BigramTokenizer))

# removing sparse terms
tdm.common40 <- removeSparseTerms(tdm,.4)

# finding most-common terms
tdm.common40.matrix <- as.matrix(tdm.common40)

frequency <- rowSums(tdm.common40.matrix)

frequency <- sort(frequency, decreasing = T)

head(frequency)

# wordcloud

library(wordcloud)

words <- names(frequency)

wordcloud(words[1:100], frequency[1:100], scale=c(10,.75))

## Ngrams
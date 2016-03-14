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

# term document matrix, 1-gram and 2-gram
tdm.1g <- TermDocumentMatrix(c)
tdm.2g <- TermDocumentMatrix(c, control=list(tokenize=BigramTokenizer))

# removing sparse terms
tdm.1g.common20 <- removeSparseTerms(tdm.1g,.20)
tdm.2g.common20 <- removeSparseTerms(tdm.2g,.20)

# finding most-common terms
tdm.1g.common20.matrix <- as.matrix(tdm.1g.common20)
tdm.2g.common20.matrix <- as.matrix(tdm.2g.common20)

frequency.1g <- rowSums(tdm.1g.common20.matrix)
frequency.2g <- rowSums(tdm.2g.common20.matrix)

frequency.1g <- sort(frequency.1g, decreasing = T)
frequency.2g <- sort(frequency.2g, decreasing = T)

# wordclouds

library(wordcloud)
library(RColorBrewer)

pal <- brewer.pal(3,"Greys")

words.1g <- names(frequency.1g)
words.2g <- names(frequency.2g)

png("images/wordcloud-1g.png",width=480, height=480)
wordcloud(words.1g[1:200], frequency.1g[1:200], scale=c(10,.5),random.order = F,colors=pal)
dev.off()

png("images/wordcloud-2g.png",width=480, height=480)
wordcloud(words.2g[1:200], frequency.2g[1:200], scale=c(10,.5),random.order = F,colors=pal)
dev.off()

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

docs <- c(data.frame(sample_lines("data/en_US.twitter.txt",.015*twitter.n)),
          data.frame(sample_lines("data/en_US.blogs.txt", .015 * blog.n)),
          data.frame(sample_lines("data/en_US.news.txt", .015 * news.n)))

# # corpora
c <- Corpus(VectorSource(docs))

# check it out
inspect(c)
 
# eliminate whitespace
c <- tm_map(c, stripWhitespace)
# convert to lowercase
c <- tm_map(c, content_transformer(tolower))
# remove punctuation
c <- tm_map(c, content_transformer(removePunctuation))
# remove numbers
c <- tm_map(c, content_transformer(removeNumbers))
# stem words
# c <- tm_map(c, stemDocument)

# document term matrix
tdm <- TermDocumentMatrix(c)
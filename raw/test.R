require(tm)
require(R.utils)
require(LaF)
require(SnowballC)

blog.n <- countLines("data/en_US.blogs.txt")

set.seed(4321)

doc <- data.frame(sentences=sample_lines("data/en_US.blogs.txt", .0002 * blog.n, nlines=blog.n))

doc$sentences <- as.character(doc$sentences)

punctuation <- "[^[:alnum:][:space:]'-]"

doc$sentences <- gsub(punctuation,"",doc$sentences)

test <- function(x) {
  words <- strsplit(x, " ")
  return(words[[1]][length(words[[1]])])
}

doc$lastword <- sapply(doc$sentences,test)
doc$lastword <- gsub("\r","",doc$lastword)

prediction <- function(x) {
  words <- strsplit(x, " ")
  text <- words[[1]][1:(length(words[[1]])-1)]
  text <- paste(paste(text,collapse = " ")," ",sep="")
  return(paste(capture.output(betty(text))[1],capture.output(betty(text))[2],sep=","))
}

doc$prediction <- sapply(doc$sentences,prediction)

doc$prediction <- gsub("<span class='label label-success'>|</span>","",doc$prediction)

doc$correct <- doc$lastword == doc$prediction

summary <- table(doc$correct)

efficacy <- summary[[2]]/margin.table(summary)

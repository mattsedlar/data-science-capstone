require(tm)
require(R.utils)
require(LaF)
require(SnowballC)

twitter.n <- countLines("data/en_US.twitter.txt")
blog.n <- countLines("data/en_US.blogs.txt")
news.n <- countLines("data/en_US.news.txt")

options(mc.cores=1)
set.seed(4321)

doc <- data.frame(sentences=c(sample_lines("data/en_US.blogs.txt", .0001 * blog.n, nlines=blog.n),
                              sample_lines("data/en_US.news.txt", .0001 * blog.n, nlines=news.n),
                              sample_lines("data/en_US.twitter.txt",.0001 * twitter.n, nlines=twitter.n)))

doc$sentences <- as.character(doc$sentences)

punctuation <- "[^[:alnum:][:space:]'-]"
numbers <- "[0-9]"
pagebreak <- "\r"

doc$sentences <- gsub(punctuation,"",doc$sentences)
doc$sentences <- gsub(numbers,"",doc$sentences)
doc$sentences <- gsub(pagebreak,"",doc$sentences)
doc$sentences <- trimws(doc$sentences)

test <- function(x) {
  words <- strsplit(x, " ")
  return(words[[1]][length(words[[1]])])
}

doc$lastword <- sapply(doc$sentences,test)

prediction <- function(x,first=TRUE) {
  words <- strsplit(x, " ")
  text <- words[[1]][1:(length(words[[1]])-1)]
  text <- paste(paste(text,collapse = " "),substr(words[[1]][length(words[[1]])],1,1),sep=" ")
# text <- paste(paste(text,collapse = " ")," ",sep="")  
  if(first==T){
    return(capture.output(betty(text))[[1]])  
  } else {
    return(paste(capture.output(betty(text)),collapse = ", "))
  }
}

doc$prediction <- sapply(doc$sentences,prediction)

doc$prediction <- gsub("<span class='label label-success'>|</span>","",doc$prediction)

doc$correct <- doc$lastword == doc$prediction

summary <- table(doc$correct)

efficacy <- summary[[2]]/margin.table(summary)

doc$prediction.two <- sapply(doc$sentences,prediction,first=F)

doc$prediction.two <- gsub("<span class='label label-success'>|</span>","",doc$prediction.two)

doc$correct.two <- mapply(grepl,pattern=doc$lastword, x=doc$prediction.two)

summary.two <- table(doc$correct.two)

efficacy.two <- summary.two[[2]]/margin.table(summary.two)

# efficacy of prediction with no leading letter
# one word, 0.07; two word, 0.11
# efficacy of prediction with leading letter
# one word, 0.20; two word, 0.29

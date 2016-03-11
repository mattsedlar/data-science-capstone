require(koRpus)

tt <- list(path="~/Applications/treetagger",
           preset="en")

files <- list.files(path="data", pattern="[1-3].txt", full.names=T,recursive=F)

tagged.text <- lapply(files,treetag,treetagger="manual",lang="en",TT.options=tt)


tagged.df <- rbind(taggedText(tagged.text[[1]]),
                   taggedText(tagged.text[[2]]),
                   taggedText(tagged.text[[3]]))

# adding frequency
tagged.df <- transform(tagged.df, freq.token= ave(seq(nrow(tagged.df)), token, FUN=length))

# removing duplicates
tagged.df <- tagged.df[!duplicated(tagged.df$token),]

# order by frequency
tagged.df <- tagged.df[order(-tagged.df$freq.token),]

# reorder rownames
rownames(tagged.df) <- c(1:nrow(tagged.df))

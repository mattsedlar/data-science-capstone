require(koRpus)

tt <- list(path="~/Applications/tree-tagger-linux-3/",
           preset="en")

# files <- list.files(path="data", pattern="[1-3].txt", full.names=T,recursive=F)

twitter.df <- read.table("data/3.txt", sep="\t")
twitter.df$V1 <- sub(" .*","",twitter.df$V1)
write.table(twitter.df,"data/tagtest.txt",quote = F, row.names = F, col.names = F)

tagged.text <- treetag("data/tagtest.txt",
                                 treetag,
                                 treetagger="manual",
                                 lang="en",TT.options=tt)


tagged.df <- data.frame(taggedText(tagged.text))

# removing duplicates
tagged.df <- tagged.df[!duplicated(tagged.df$token),]

# remove unknonws
tagged.df <- subset(tagged.df, lemma != "<unknown>")

# reorder rownames
rownames(tagged.df) <- c(1:nrow(tagged.df))

require(dplyr)

stats <- tagged.df %>% group_by(desc) %>% summarize(sum = n()) 

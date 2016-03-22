require(tm)
require(koRpus)

file_list <- DirSource(director="data", pattern="[1-3].txt")

tt <- list(path="~/Applications/tree-tagger-linux-3",
           preset="en")

for (file in file_list$filelist){
  if(!exists("dataset")) {
    dataset <- read.table(file,sep="\t")
  } else {
    temp_data <- read.table(file,sep="\t")
    dataset <- rbind(dataset,temp_data)
    rm(temp_data)
  }
}

write.table(dataset,"data/final/final_data.txt")

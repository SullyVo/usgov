#Sulli Vo
setwd("/Users/sullivo/Documents/STA141C")
zip_file_path = "/Users/sullivo/Documents/STA141C/awards.zip"
files = unzip(zip_file_path,list = TRUE)
library(data.table)
install.packages('tm',dependencies = TRUE)
install.packages('NLP')
library(tm)
my_files = list.files("/Users/sullivo/Documents/STA141C/", pattern = '*.csv')
my_files = my_files[-1]
#function return the word changes onto the description
word_change<- function(fn){
  dt_temp <- fread(fn, sep=",",fill = FALSE , 
                   select = c("total_obligation", "description",'funding_agency_id'))
  dt_temp= na.omit(dt_temp)
  for (col in c('description', 'total_obligation')) dt_temp[is.na(get(col)), (col) := .(list(NULL))]
  dt_temp$description = gsub("[[:punct:]]", "",dt_temp$description)
  dt_temp$description = tolower(dt_temp$description)
  dt_temp$description = removeWords(dt_temp$description, stopwords('en'))
  dt_temp$description = stemDocument(dt_temp$description)
  dt_temp$description = gsub('[[:digit:]]+', '', dt_temp$description)
  dt_temp$description = strsplit(dt_temp$description,"\\s+") #split words based on the several spaces
  return(dt_temp)
}
#function to return the top 25 of each agency
top_25 = function(files) {
  app_files = na.omit(files)
  mylist <- lapply(app_files, word_change) #lapply the function into the file
  mydata <- rbindlist( mylist )
  calcWeight =  function(to, desc){ # return the weight 
    temp <- to/length(desc)
    foo = rep(temp, length(desc))
    names(foo) = desc
    return (foo)}
  #mapply onto price and description 
  mydata$new_weight = mapply(calcWeight, mydata$total_obligation, mydata$description)
  library(tidyr)
  mydatanew = unnest(mydata) #unlist the data frame
  #make new data frame under 3 col keys, IDv, weights
  new_1 <- 
    data.frame(keys = mydatanew$description, ID = mydatanew$funding_agency_id,
               weights = mydatanew$new_weight, 
               stringsAsFactors = TRUE)
  #filter weights by keys and ID and sum the same weights by keys and ID
  new_2 <- aggregate( weights ~  keys + ID  , data = new_1 , FUN = sum )
  library(dplyr)
  #group by func to get top 25 
  my_weights <- new_2 %>% 
    arrange(desc(weights)) %>% 
    group_by(ID) %>% 
    slice(seq_len(25)) # length 25 
  return (my_weights)
}

#1
my_top25 = top_25(my_files)

#2
#filter the file size to get top 91 agencies 
files_over_1M = my_files[sapply(my_files, file.size) > 1048576 ]
files_size_in_range = files_over_1M[sapply(files_over_1M, file.size) < 50048576]
files_size_in_range = na.omit(files_size_in_range)

my_top25_91 = top_25(files_size_in_range)

#3#filter by ID
NSF = my_top25_91[my_top25_91$ID == '655',]
FBI = my_top25_91[my_top25_91$ID == '262',]
USCBP = my_top25_91[my_top25_91$ID == '778',]
FS = my_top25_91[my_top25_91$ID == '110',]

#5 using lapply, parLapply and clusterApplyLB to compare the time
library(parallel)
system.time(result_1 <- lapply(files_size_in_range,top_25))
mycluster = makeCluster(2L, type = "PSOCK")
clusterEvalQ(mycluster, library(tm))
clusterEvalQ(mycluster, library(data.table))
require(data.table)
system.time(result_2 <- parLapply(mycluster, files_size_in_range, word_change))
system.time(result_3 <- clusterApplyLB(mycluster, files_size_in_range, word_change))



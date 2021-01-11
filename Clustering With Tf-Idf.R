library(ldatuning)
library(factoextra)
library(topicmodels)
library(tm)
library(tidyverse)
library(slam)
library(topicmodels)
library(qdapDictionaries)
library(quanteda)
library(wordcloud)
library(tidytext)
library(ggplot2)
library(dplyr)
library(textstem)
library(data.table)
library(rstudioapi)
library(udpipe)
library(RWeka)
library(qdap)
library(roperators)
library(sjmisc)
library(Julia)
library(stringi)
library(forcats)
library(writexl)
library(readxl)
library(corpus)
library(fastTextR)
library(textmineR)
library(textTinyR)
library(ClusterR)
library(reticulate)
library(spacyr)
library(tidyr)
library(tidyverse)
library(widyr)
library(irlba)
library(broom)
library(text2vec)
library(magrittr)
library(tsne)
library(tidytext)
library(ggplot2)
library(textmineR)
library(Rcpp)
library(cluster)
library(factoextra)
library(NbClust)
library(spatialEco)
library(fpc)

options(header=F,stringsAsFactors = F)
#this code get the exact place where your current file is at
dirname(rstudioapi::getSourceEditorContext()$path)
path_data_processed <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)), "data", "processed")
path_data_raw <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)), "data", "raw")
path_data_report <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),'data', "reports")
path_data_prod <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),'data', "prod")

data<-read.csv(file.path(path_data_processed,'data_consolidated_20200629.csv'),na.strings = '')
sum(is.na(data$`Problem Statement`))

data20<-data%>%filter(year==20)
data19<-data%>%filter(year==19)

Digi20<-data20%>%filter(category_digital==T)
Man20<-data20%>%filter(category_manpower==T)
Other20<-data20%>%filter(category_manpower==F & category_digital==F)


### Function to split enquiry text into useful and non-useful parts
splitter<-function(data){
  separators<-readLines(file.path(path_data_raw,'separators.txt'),warn=F) #njs: change due to seperators.csv not being available
  newdata<-data
  newdata$split_or_not<-0
  separators<-stemDocument(separators)
  newdata$split_text<-''  
  newdata$sep_word<-'NIL'
  
  ### Carry out splitting of enquiry text
  ### Create variable that stores part of text which is actual enquiry ##
  ## If separator word is not found, just keep the whole text
  for (j in 1:nrow(newdata))
  {for (word in unlist(text_tokens(stripWhitespace(newdata$Problem.Statement[j])))){
    if (stemDocument(tolower(word)) %in% separators)
    {newdata$split_text[j]<-sub(paste('.*?',word,sep=''),word,tolower(stripWhitespace(newdata$Problem.Statement[j])))
    newdata$split_or_not[j]<-1
    newdata$sep_word[j]<-word
    break}}
    if (newdata$split_or_not[j]==0)
      newdata$split_text[j]<-newdata$Problem.Statement[j]}
  newdata$split_or_not<-factor(newdata$split_or_not)
  return (newdata)
}

## Function to clean text
cleaner<-function(corpus_data){
  cleanset<-tm_map(corpus_data,stripWhitespace)
  cleanset<-tm_map(cleanset,content_transformer(tolower))
  cleanset<-tm_map(cleanset,stripWhitespace)
  cleanset<-tm_map(cleanset,gsub,pattern='e-commerce',replacement='ecommerce')
  ## Remove Non-ASCII Characters
  cleanset<-tm_map(cleanset,gsub,pattern='[^ -~]+',replacement='')
  ## Replace some common terms with their abbreviations, such as psg
  refer<-read.table(file=file.path(path_data_raw,'ref1.txt'),sep=',',quote='',comment.char='')
  class(refer)
  for (i in 1:nrow(refer))
  {
    cleanset<-tm_map(cleanset,gsub,pattern=refer[i,2],replacement=refer[i,1])
    cleanset<-tm_map(cleanset,stripWhitespace)
  }
  cleanset<-tm_map(cleanset,gsub,pattern='\\W+',replacement=' ')
  cleanset<-tm_map(cleanset,stripWhitespace)
  ## Remove single letter words
  cleanset<-tm_map(cleanset,gsub,pattern='\\b[A-z]\\b{1}',replacement=' ')
  cleanset<-tm_map(cleanset,stripWhitespace)
  cleanset<-tm_map(cleanset,gsub,pattern='\\b\\d+\\b',replacement=' ')
  cleanset<-tm_map(cleanset,stripWhitespace)
  cleanset<-tm_map(cleanset,lemmatize_strings)
  cleanset<-tm_map(cleanset,stripWhitespace)
  ## Remove whitespace at front of document
  cleanset<-tm_map(cleanset,gsub,pattern='^[ \t]+',replacement='')
  ## Remove whitespace at end of document
  cleanset<-tm_map(cleanset,gsub,pattern='[ \t]+$',replacement='')
  return (cleanset)}

## Function to build document term matrix
buildmatrix<-function(cleanset,min,max){
  cleanset_new<-VCorpus(VectorSource(cleanset))
  min1<-min/100
  max1<-max/100
  ndocs <- length(cleanset_new)
  # ignore overly sparse terms 
  minDocFreq <- ndocs * min1
  # ignore overly common terms 
  maxDocFreq <- ndocs * max1
  
  tok <- function(x)
    NGramTokenizer(x, Weka_control(min=1, max=2))
  
  dtm <- DocumentTermMatrix(cleanset_new, control=list(tokenize=tok,
                                                   bounds = list(global = c(minDocFreq, maxDocFreq))))
  
  Terms(dtm)
  
  new_dtm<-tidy(dtm)
  
  new_dtm<-new_dtm%>%separate(term,c('word1','word2'),sep=' ')
  
  ## Lemmatize stopwords
  lemma_stopwords<-lemmatize_strings(stop_words$word)
  
  ## Remove stopwords
  new_dtm<-new_dtm%>%filter(!word1 %in% lemma_stopwords)%>%filter(!word2 %in% lemma_stopwords)
  
  ## Read in list of words to remove
  rem<-readLines(file.path(path_data_raw,'removewords.txt'),warn=F)
  new_dtm<-new_dtm%>%filter(!word1 %in% rem)%>%filter(!word2 %in% rem)
  
  
  new_dtm<-new_dtm%>%unite(word,word1,word2,sep=' ')%>%select(document,word)
  new_dtm<-new_dtm%>%count(document,word)
  
  new_dtm<-new_dtm%>%separate(word,c('word1','word2'),sep=' ')
  
  new_dtm<-new_dtm%>%mutate(word2=replace(word2,word2=='NA',''))
  new_dtm<-new_dtm%>%unite(word,word1,word2,sep=' ')%>%select(document,word)
  new_dtm<-new_dtm%>%mutate(word=gsub(word,pattern='[ \t]+$',replacement=''))
  new_dtm<-new_dtm%>%count(document,word)
  new_dtm<-new_dtm%>%mutate(document=as.integer(document))
  new_dtm
  
  new_dtm<-new_dtm%>%cast_dtm(document,word,n)
  return (new_dtm)
}


## Prepare document term matrix into format to be fed into model
prepmatrix<-function(dtm){
  rowtotals<-apply(dtm,1,sum)
  matrix_new<-dtm[rowtotals>0,]
  matrix_new
  matrix_new<-as.matrix(matrix_new)
  return (matrix_new)
}


## Function for Normalization of Tfidf values
norm_eucl<-function(built_matrix)
  {built_matrix/apply(built_matrix,1,function(x)
    sum(x^2)^0.5)}

## Function to print top words in each cluster
## Top words are those that have highest combined Tf-Idf values
## Across all documents
PrintTopClusterWords<-function(clusternum,dtm,results,n){
  for (i in 1:clusternum)
  {matrixsubset<-as.matrix(dtm[results$clustering==i,])
  sumup<-sort(colSums(matrixsubset),decreasing = T)
  topwords<-names(head(sumup,n))
  topwords<-paste(topwords,collapse=', ')
  cat('Cluster',i,":",topwords,'\n\n')}
}

## Function to create topics table
CreateTopicsTable<-function(dtm,results,n,CategoryID){
  TopicsTable<-data.frame(TopicID=numeric(),
                          TopicKeyWords=character(),DateTimeCreated=character(),Count=numeric())
  for (i in 1:length(unique(results$clustering)))
  {matrixsubset<-as.matrix(dtm[results$clustering==i,])
  sumup<-sort(colSums(matrixsubset),decreasing = T)
  topwords<-names(head(sumup,n))
  topwords<-paste(topwords,collapse=', ')
  DateTime<-timestamp()
  DateTime<-gsub('##-+\\s|\\s-+##','',DateTime)
  TopicsTable[nrow(TopicsTable)+1,]<-list(i,topwords,DateTime,nrow(matrixsubset))}
  TopicsTable<-TopicsTable%>%arrange(TopicID)
  TopicsTable$CategoryID<-CategoryID
  TopicsTable$TopicDescription<-'NA'
  TopicsTable<-TopicsTable[,c(5,1,2,6,3,4)]
  return (TopicsTable)
}

## Function to get samples of documents assigned to the topics
getSamples<-function(dataset,cluster_results,topic_n,n,category_name)
{ results<-cluster_results
  topics_assign<-data.frame(topicID=results$clustering)
  topics_assign$index<-row.names(topics_assign)
  data<-dataset
  data$index<-row.names(data)
  assigntable<-merge(data,topics_assign,by='index',all.x=T)
  assigntable$index<-as.integer(assigntable$index)
  assigntable<-assigntable%>%arrange(index)
  topics_subset<-assigntable%>%filter(topicID==topic_n)
  samples<-sample_n(topics_subset,n)
  samples<-samples[,c('Problem.Statement','topicID')]
  print(sprintf("There are %d lines for topic %s, and function returns %d samples.", nrow(topics_subset), topic_n, n))
  write.csv(samples, file.path(path_data_report, sprintf("Clustering_%sCategory_topic%s_sample.csv", category_name, topic_n)),row.names=F)
}

### Function to create a table showing the assignment of topics to each document (identified by Advisory Id)
getAssignmentTables<-function(df,cluster_results,categoryID)
{
  
  for (topic_no in 1:length(unique(cluster_results$clustering))) {
    assignment_indexes_docs<-lapply(cluster_results$clustering, function(x) which(topic_no %in% x))
    assignment_indexes<-names(unlist(assignment_indexes_docs))
    df_topics <- df[assignment_indexes,]
    df_topics$TopicIdAssigned<-topic_no
    if (topic_no==1)
      temptable<-df_topics
    else
      temptable<-rbind(temptable,df_topics)
  }
  temptable$CategoryIdAssigned<-categoryID
  temptable<-temptable[,c('Business.Advsiory.ID','CategoryIdAssigned',
                          'TopicIdAssigned')]
  temptable<-temptable%>%rename(AdvisoryId=Business.Advsiory.ID)
  DateTime<-timestamp()
  DateTime<-gsub('##-+\\s|\\s-+##','',DateTime)
  temptable$DateTimeAssigned<-DateTime
  return (temptable)}

## Function to combine 3 dataframes into one
combineTables<-function(df1,df2,df3,title,date)
{ combinedT<-rbind(df1,df2)
combinedT<-rbind(combinedT,df3)
write.csv(combinedT,file=file.path(path_data_prod,sprintf('%s_%s.csv',title,date)),
          row.names=F)
return (combinedT)
}

### Set the number of clusters
clusters_num<-5

### Do Others subset first

## Split text into useful and non-useful information
## and retain useful text in another variable 
data20_Others<-splitter(Other20)

## Use data from Jan and Feb (2 months) only
data20_Others<-data20_Others%>%filter(month=='Jan' | month=='Feb')

summary(data20_Others)

## Create corpus
corpus_Others<-Corpus(VectorSource(data20_Others$split_text))

## Clean corpus
cleanset_Others<-cleaner(corpus_Others)

## Build document term matrix (dtm)
dtm_Others<-buildmatrix(cleanset_Others,0.5,95)

## Apply tf-idf function on dtm cell values
dtmWeigted_Others<-weightTfIdf(dtm_Others)

## Prepare matrix into structure to be fed in K Medoids model
matrix_Others<-as.matrix(dtmWeigted_Others)

## Normalize Tfidf values
matrix_Others<-norm_eucl(matrix_Others)

## Build K medoids model
results_Others<-pam(matrix_Others, clusters_num, metric = "euclidean", stand = FALSE)

## Print Top Cluster Words
## Top words in each cluster are those that have the highest
## combined tfidf scores across all documents
PrintTopClusterWords(clusters_num,dtmWeigted_Others,results_Others,5)


### Create Topics Table for Others
Others_TopicTable<-CreateTopicsTable(dtmWeigted_Others,
                                     results_Others,5,'Others20')

## Create Assignment table
OthersAssignmentTable<-getAssignmentTables(data20_Others,results_Others,'Others20')

## Get samples of documents assigned to the topics
getSamples(data20_Others,results_Others,3,50,'Others')
getSamples(data20_Others,results_Others,1,50,'Others')
getSamples(data20_Others,results_Others,4,50,'Others')



### Do Digital subset now
data20_Digi<-splitter(Digi20)

## Use data from Jan and Feb (2 months) only
data20_Digi<-data20_Digi%>%filter(month=='Jan' | month=='Feb')

## Create corpus
corpusd<-Corpus(VectorSource(data20_Digi$split_text))

## Clean corpus
cleansetd<-cleaner(corpusd)


### Build document term matrix (dtm)
dtm_digi<-buildmatrix(cleansetd,0.5,95)

## Apply tf-idf function on dtm cell values
dtmWeigted_Digi<-weightTfIdf(dtm_digi)

## Prepare matrix into structure to be fed in K Medoids model
matrix_Digi<-as.matrix(dtmWeigted_Digi)

## Normalize Tfidf values
matrix_Digi<-norm_eucl(matrix_Digi)

## Build K medoids model
results_Digi<-pam(matrix_Digi, clusters_num, metric = "euclidean", stand = FALSE)

## Print Top Words for each cluster
## Top words in each cluster are those that have the highest
## combined tfidf scores across all documents
PrintTopClusterWords(clusters_num,dtmWeigted_Digi,results_Digi,5)

## Create Topics Table for Digital
Digital_TopicsTable<-CreateTopicsTable(dtmWeigted_Digi,results_Digi,5,'Digital20')

## Create Assignment table
DigitalAssignmentTable<-getAssignmentTables(data20_Digi,results_Digi,'Digital20')

## Get samples of documents assigned to the topics
getSamples(data20_Digi,results_Digi,2,50,'Digital')
getSamples(data20_Digi,results_Digi,1,50,'Digital')
getSamples(data20_Digi,results_Digi,5,50,'Digital')



### Do Manpower subset
data20_Man<-splitter(Man20)

## Use data from Jan and Feb (2 months) only
data20_Man<-data20_Man%>%filter(month=='Jan' | month=='Feb')

## Create corpus
corpusm<-Corpus(VectorSource(data20_Man$split_text))

## Clean corpus
cleansetm<-cleaner(corpusm)

### Build document term matrix (dtm)
Manpower20<-buildmatrix(cleansetm,0.5,95)

## Apply tf-idf function on dtm cell values
dtmWeigted_Man<-weightTfIdf(Manpower20)

## Prepare matrix into structure to be fed in K Medoids model
matrix_Man<-as.matrix(dtmWeigted_Man)

## Normalize tfidf values
matrix_Man<-norm_eucl(matrix_Man)

## Build K medoids model
results_Man<-pam(matrix_Man, clusters_num, metric = "euclidean", stand = FALSE)

## Print top words in each cluster for Manpower
## Top words in each cluster are those that have the highest
## combined tfidf scores across all documents
PrintTopClusterWords(clusters_num,dtmWeigted_Man,results_Man,5)

## Create Topics Table for Manpower
Man_TopicsTable<-CreateTopicsTable(dtmWeigted_Man,results_Man,5,'Manpower20')

## Create Assignment table
ManpowerAssignmentTable<-getAssignmentTables(data20_Man,results_Man,'Manpower20')

## Get samples of documents assigned to the topics
getSamples(data20_Man,results_Man,5,50,'Manpower')
getSamples(data20_Man,results_Man,3,50,'Manpower')
getSamples(data20_Man,results_Man,1,50,'Manpower')
getSamples(data20_Man,results_Man,2,50,'Manpower')

### Write combined topics table to csv
combinedTopicsTable<-combineTables(Others_TopicTable,Digital_TopicsTable,Man_TopicsTable,'TopicCreated','270720')

## Write combined assignment table to csv
combinedAssignmentTable<-combineTables(OthersAssignmentTable,DigitalAssignmentTable,ManpowerAssignmentTable,'TopicAssigned','270720')



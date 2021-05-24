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
library(topicdoc)

options(header=F,stringsAsFactors = F)
#this code get the exact place where your current file is at
dirname(rstudioapi::getSourceEditorContext()$path)
#display your current working directory
path_data_processed <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)), "data", "processed")
path_data_raw <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)), "data", "raw")
path_data_report <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),'data', "reports")
path_data_prod <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),'data', "prod")

data_consolidated <- read.csv(file.path(path_data_processed, 'data_consolidated_20200629.csv'),na.strings = '')


### Function to split enquiry text into useful and non-useful parts
## Separator words could be words like
## discuss, engage, enquire
splitter<-function(data){
  separators<-readLines(file.path(path_data_raw,'separators.txt'),warn=F) 
  newdata<-data
  newdata$split_or_not<-0
  separators<-stemDocument(separators)
  newdata$split_text<-''  
  newdata$sep_word<-'NIL'
  
  ### Carry out splitting of enquiry text based on separator word
  ### Create variable that stores part of text which is actual enquiry
  ## If separator word is not found, just keep the whole text
  ## Separator words are words like "Enquire"
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
  ## Remove unecessary whitespace
  cleanset<-tm_map(corpus_data,stripWhitespace)
  
  ## Convert to lowercase
  cleanset<-tm_map(cleanset,content_transformer(tolower))
  
  ## Remove Non-ASCII Characters
  cleanset<-tm_map(cleanset,gsub,pattern='[^ -~]+',replacement='')
  
  ## Replace some common terms with their abbreviations, such as from
  ## productivity solutions grant to psg
  ## to standardize common terms
  refer<-read.table(file=file.path(path_data_raw,'abbr.txt'),sep=',',quote='',comment.char='')
  class(refer)
  for (i in 1:nrow(refer))
  {
    cleanset<-tm_map(cleanset,gsub,pattern=refer[i,2],replacement=refer[i,1])
    cleanset<-tm_map(cleanset,stripWhitespace)
  }
  
  ## Remove non-word characters
  cleanset<-tm_map(cleanset,gsub,pattern='\\W+',replacement=' ')
  cleanset<-tm_map(cleanset,stripWhitespace)
  
  ## Remove single letter words
  cleanset<-tm_map(cleanset,gsub,pattern='\\b[A-z]\\b{1}',replacement=' ')
  cleanset<-tm_map(cleanset,stripWhitespace)
  
  ## Remove numbers
  cleanset<-tm_map(cleanset,gsub,pattern='\\b\\d+\\b',replacement=' ')
  cleanset<-tm_map(cleanset,stripWhitespace)
  
  ## Lemmatize words
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
  
  ## Create dtm with unigrams and bigrams
  dtm <- DocumentTermMatrix(cleanset_new, control=list(tokenize=tok,
                                                       bounds = list(global = c(minDocFreq, maxDocFreq))))
  
  Terms(dtm)
  
  new_dtm<-tidy(dtm)
  
  new_dtm<-new_dtm%>%separate(term,c('word1','word2'),sep=' ')
  
  ## Lemmatize stopwords
  lemma_stopwords<-lemmatize_strings(stop_words$word)
  
  ## Remove stopwords
  new_dtm<-new_dtm%>%filter(!word1 %in% lemma_stopwords)%>%filter(!word2 %in% lemma_stopwords)
  
  
  new_dtm<-new_dtm%>%mutate(word2=replace(word2,word2=='NA',''))
  new_dtm<-new_dtm%>%unite(word,word1,word2,sep=' ')%>%select(document,word)
  new_dtm<-new_dtm%>%mutate(word=gsub(word,pattern='[ \t]+$',replacement=''))
  new_dtm<-new_dtm%>%count(document,word)
  new_dtm<-new_dtm%>%mutate(document=as.integer(document))

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


## Function to build LDA model
genlda<-function(built_matrix,num_topics){
  lda<-LDA(built_matrix,k=num_topics,method='Gibbs',control=list(seed=1000))
  return (lda)
}

## Function to generate average topic coherence scores for LDA models 
## with different minimum term frequency and number of topics combinations
## Function will take quite long to run
## as it is generating many LDA models all at once
GenRecordTable<-function(cleanseto,minrange)
{
  record_table<-data.frame(minfreq=numeric(),numtopics=integer(),
                           coherence=numeric(),number_of_terms=integer())
  for (j in minrange)
  {
    dtm<-buildmatrix(cleanseto,j,95)
    
    numterms<-length(Terms(dtm))
    
    built_matrix<-prepmatrix(dtm)
    
    ## Range of topics to test is in the range of 5 to 9
    for (i in 5:9)
    {
      ldao20<-genlda(built_matrix,i)
      
      coh<-topic_coherence(ldao20,built_matrix,top_n_tokens=5)
      avecoh<-mean(coh)
      record_table[nrow(record_table)+1,]<-list(j,i,avecoh,numterms)
    }
  }
  
  record_table<-record_table%>%arrange(desc(coherence))
  return (record_table)
}

## Function to plot keywords in each topic for LDA model
plottopics<-function(lda,num_words){
  
  text_topics<-tidy(lda,matrix='beta')
  text_topics
  
  text_topterms<-text_topics%>%
    group_by(topic)%>%
    top_n(num_words,beta)%>%
    ungroup()%>%
    arrange(topic,-beta)
  
  text_topterms%>%
    mutate(term=reorder_within(term,beta,topic))%>%
    ggplot(aes(term,beta))+
    geom_col(show.legend = FALSE,fill='#606060')+
    facet_wrap(~topic,scales='free')+
    coord_flip()+
    scale_x_reordered()
}



## Function to list frequencies of all words in the document term matrix
freqlister<-function(built_matrix){
  frequency<-colSums(built_matrix)
  frequency
  frequency<-sort(frequency,decreasing=T)
  frequency}

## Function to plot top occuring words in the document term matrix
freqplotter<-function(built_matrix,n){
  matrix_transposed<-t(built_matrix)
  freqtable<-data.frame(term=rownames(matrix_transposed),
                        freq=rowSums(matrix_transposed),
                        row.names=NULL)
  
  
  freqtable<-freqtable[order(-freqtable$freq),][1:n,]
  freqtable
  
  freqplot<-ggplot(freqtable,aes(x=reorder(term,-freq),freq))+
    geom_bar(stat='identity',fill='#606060')+
    labs(x="Terms",y="Frequency",title="Most Frequent Terms")+
    geom_text(aes(label=freq),vjust=-0.5,size=3)+
    theme(axis.text.x=element_text(angle=45,hjust=1),
          plot.title=element_text(hjust=0.5))
  
  freqplot}

## Function to get samples of documents assigned to a particular topic
get_samples <- function(lda_object, df, n_samples, extra_threshold, topic_no_vec, category_name){
  
  for (topic_no in topic_no_vec) {
    if (!missing(extra_threshold)) {
      idx_to_extract <- as.numeric(slot(lda_object, "documents")[slot(lda_object, "gamma")[,topic_no]>=(1/length(unique(topics(lda_object))))+extra_threshold])  
    } else {
      idx_to_extract <- as.numeric(names(topics(lda_object)==topic_no))
    }
    
    df_topics <- df[idx_to_extract,]
    print(sprintf("There are %d lines for topic %s, and function returns %d samples.", dim(df_topics)[1], topic_no, n_samples))
    random_sample <- sample_n(data.frame(df_topics$Problem.Statement), n_samples)
    write.csv(random_sample, file.path(path_data_report, sprintf("LDA_%sCategory_topic%s_sample.csv", category_name, topic_no)),row.names=F)
  }
  
}




## Function to create a table for the topics showing keywords for each topic
getTopicsTables<-function(lda,n,df,extra_threshold,categoryID)
{
  text_topics<-tidy(lda,matrix='beta')
  text_topics
  
  text_topterms<-text_topics%>%
    group_by(topic)%>%
    top_n(n,beta)%>%
    ungroup()%>%
    arrange(topic,-beta)
  
  TopicsOthers  <-text_topterms
  
  topictable<-data.frame(CategoryID=character(),
                        TopicID=numeric(),TopicKeyWords=character(),
                        TopicDescription=character(),
                        DateTimeCreated=character())
  for (i in 1:length(unique(TopicsOthers$topic)))
  {temp<-TopicsOthers%>%filter(topic==i)
  keywords<-vector()
  for (j in 1:nrow(temp))
  {keywords<-append(keywords,temp$term[j])
  }
  keywords<-paste(keywords,collapse=', ')
  print(keywords)
  DateTime<-timestamp()
  DateTime<-gsub('##-+\\s|\\s-+##','',DateTime)
  topictable[nrow(topictable)+1,]<-list(categoryID,i,keywords,'NA',DateTime)
  }
  topictable$Count<-0
  for (topic_no in 1:length(unique(TopicsOthers$topic))) {
    idx_to_extract <- as.numeric(slot(lda, "documents")[slot(lda, "gamma")[,topic_no]>=(1/length(unique(TopicsOthers$topic)))+extra_threshold])  
    df_topics <- df[idx_to_extract,]
    topictable$Count[topic_no]<-nrow(df_topics)
  }
  return (topictable)}

### Function to create a table showing the assignment of topics to each document (identified by Advisory Id)
getAssignmentTables<-function(lda,df,extra_threshold,categoryID)
{
 
  for (topic_no in 1:length(unique(topics(lda)))) {
    idx_to_extract <- as.numeric(slot(lda, "documents")[slot(lda, "gamma")[,topic_no]>=(1/length(unique(topics(lda))))+extra_threshold])  
    df_topics <- df[idx_to_extract,]
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


minrange<-c(1:5)
minrangesmall<-vector()
for (i in 1:9)
{minrangesmall<-append(minrangesmall,i/10)}

## This is the range of minimum frequencies of terms in the document term matrix we will test to
## see which one gives the most optimal number of topics
## The range on min. frequencies we will test is from 0.1 to 0.9, and from 1 to 5
minrange<-prepend(minrange,minrangesmall)
sprintf('The range of minimum term frequencies we will test is %s',paste(minrange,collapse=', '))

### Do Others subset first

## Split text into useful and non-useful information
## and retain useful text in another variable 
data_others<-splitter(Other20)

## Use data from Jan and Feb (2 months) only
data_others<-data_others%>%filter(month=='Jan' | month=='Feb')

summary(data_others)

## Create corpus
corpus_others<-Corpus(VectorSource(data_others$split_text))

## Clean corpus
cleanseto<-cleaner(corpus_others)


### Look at average topic coherence scores for LDA models
### generated by different minimum term frequency and number of topics combinations

## Function will take quite long to run because
## it's generating many LDA models all at once
record_table_others<-GenRecordTable(cleanseto,minrange)

cat('The number of topics which give best coherence is',record_table_others[which.max(record_table_others$coherence),2],
    '\nThe min freq which give best coherence is',record_table_others[which.max(record_table_others$coherence),1])

## Assign min. freq and num. of topics which give highest average topic coherence score
minFreqO<-record_table_others[which.max(record_table_others$coherence),1]
numTopicsO<-record_table_others[which.max(record_table_others$coherence),2]

## Use minfreq 0.5 and number of topics 5 to build document term matrix (dtm)
dtm_others<-buildmatrix(cleanseto,minFreqO,95)

## View terms in dtm
Terms(dtm_others)

## Prepare matrix into structure to be fed in LDA model
dtm_others<-prepmatrix(dtm_others)

## Create LDA model
ldao20<-genlda(dtm_others,numTopicsO)

## View coherence scores for the different topics in the lda model
topic_coherence(ldao20,dtm_others,5)

## Create Topic table
OthersTopicTable<-getTopicsTables(ldao20,5,data_others,0.025,'Others20')

## Create Assignment table
OthersAssignmentTable<-getAssignmentTables(ldao20,data_others,0.025,'Others20')

## Plot top 5 keywords for each topic
plottopics(ldao20,5)

## Get samples of documents assigned to the different topics
get_samples(ldao20,data_others,30,0.025,2,'Others')
get_samples(ldao20,data_others,30,0.025,3,'Others')
get_samples(ldao20,data_others,30,0.025,1,'Others')
get_samples(ldao20,data_others,30,0.025,5,'Others')


### Do Digital subset now

## Split text into useful and non-useful information
## and retain useful text in another variable 
data_digital<-splitter(Digi20)

## Use data from Jan and Feb (2 months) only
data_digital<-data_digital%>%filter(month=='Jan'|month=='Feb')

## Create corpus
corpusd<-Corpus(VectorSource(data_digital$split_text))

## Clean corpus
cleansetd<-cleaner(corpusd)

### Look at average topic coherence scores for LDA models
### generated by different minimum term frequency and number of topics combinations

## Function will take quite long to run because
## it's generating many LDA models all at once
record_table_digital<-GenRecordTable(cleansetd,minrange)

cat('The number of topics which give best coherence is',record_table_digital[which.max(record_table_digital$coherence),2],
    '\nThe min freq which give best coherence is',record_table_digital[which.max(record_table_digital$coherence),1])

## Assign min. freq and num. of topics which give highest average topic coherence score
minFreqD<-record_table_digital[which.max(record_table_digital$coherence),1]
numTopicsD<-record_table_digital[which.max(record_table_digital$coherence),2]


## Use minfreq 2 and number of topics 8 to build document term matrix (dtm)
dtm_digital<-buildmatrix(cleansetd,minFreqD,95)

## View terms in dtm
Terms(dtm_digital)

## Prepare matrix into structure to be fed in LDA model
dtm_digital<-prepmatrix(dtm_digital)

## Create LDA model
ldad20<-genlda(dtm_digital,numTopicsD)

## View coherence scores for the different topics in the lda model
topic_coherence(ldad20,dtm_digital,5)

## Plot top 5 keywords for each topic
plottopics(ldad20,5)

## Create Topic table
DigitalTopicTable<-getTopicsTables(ldad20,5,data_digital,0.025,'Digital20')

## Create Assignment table
DigitalAssignmentTable<-getAssignmentTables(ldad20,data_digital,0.025,'Digital20')

## Get samples of documents assigned to the different topics
get_samples(ldad20,data_digital,30,0.025,1,'Digital')
get_samples(ldad20,data_digital,30,0.025,2,'Digital')
get_samples(ldad20,data_digital,30,0.025,4,'Digital')
get_samples(ldad20,data_digital,30,0.025,8,'Digital')
get_samples(ldad20,data_digital,30,0.025,5,'Digital')


### Do Manpower subset now

## Split text into useful and non-useful information
## and retain useful text in another variable 
data_man<-splitter(Man20)

## Use data from Jan and Feb (2 months) only
data_man<-data_man%>%filter(month=='Jan'|month=='Feb')

## Create corpus
corpusm<-Corpus(VectorSource(data_man$split_text))

## Clean corpus
cleansetm<-cleaner(corpusm)

### Look at average topic coherence scores for LDA models
### generated by different minimum term frequency and number of topics combinations

## Function will take quite long to run because
## it's generating many LDA models all at once
record_table_manpower<-GenRecordTable(cleansetm,minrange)

cat('The number of topics which give best coherence is',record_table_manpower[which.max(record_table_manpower$coherence),2],
    '\nThe min freq which give best coherence is',record_table_manpower[which.max(record_table_manpower$coherence),1])

## Assign min. freq and num. of topics which give highest average topic coherence score
minFreqM<-record_table_manpower[which.max(record_table_manpower$coherence),1]
numTopicsM<-record_table_manpower[which.max(record_table_manpower$coherence),2]


## Use minfreq 0.6 and number of topics 6 to build document term matrix (dtm)
dtm_man<-buildmatrix(cleansetm,minFreqM,95)

## View terms in dtm
Terms(dtm_man)

## Prepare matrix into structure to be fed in LDA model
dtm_man<-prepmatrix(dtm_man)

ldam20<-genlda(dtm_man,numTopicsM)

## Plot top 5 keywords for each topic
plottopics(ldam20,5)

## View coherence scores for the different topics in the lda model
topic_coherence(ldam20,dtm_man,5)

## Create Topic table
ManpowerTopicTable<-getTopicsTables(ldam20,5,data_man,0.025,'Manpower20')

## Create Assignment table
ManpowerAssignmentTable<-getAssignmentTables(ldam20,data_man,0.025,'Manpower20')

## Get samples of documents assigned to the different topics
get_samples(ldam20,data_man,30,0.025,3,'Manpower')
get_samples(ldam20,data_man,30,0.025,5,'Manpower')
get_samples(ldam20,data_man,30,0.025,4,'Manpower')
get_samples(ldam20,data_man,50,0.025,2,'Manpower')
get_samples(ldam20,data_man,50,0.025,1,'Manpower')


### Write combined topics table to csv
combinedTopicsTable<-combineTables(OthersTopicTable,DigitalTopicTable,ManpowerTopicTable,'TopicCreated','200720')

## Write combined assignment table to csv
combinedAssignmentTable<-combineTables(OthersAssignmentTable,DigitalAssignmentTable,ManpowerAssignmentTable,'TopicAssigned','200720')



#data20<-data_consolidated%>%filter(year==20)

#Other20<-data20%>%filter(category_digital==F&category_manpower==F)
#Digi20<-data20%>%filter(category_digital==T)
#Man20<-data20%>%filter(category_manpower==T)


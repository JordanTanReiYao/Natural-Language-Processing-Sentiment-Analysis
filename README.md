# Natural Language Processing - Sentiment Analysis
 Python and R scripts that made use of Latent Dirichlet Allocation, TF-IDF, and Word Vectors to uncover significant topics in text data.

 Extensive data cleaning is carried out before feeding the data into the models.

 ## In Clustering (Word Embeddings).ipynb:
 * The code here makes use of word vectors to group sentences that have similar word vector representations together. The grouping (clustering) is done using K-Means. We can then see what are the top words in each cluster to identify significant topics.

 ## In Clustering (TF-IDF).R:
 * The code makes use of TF-IDF to group sentences that have words with similar TF-IDF scores together. K-Medoids clustering is used here. The top words in each cluster are then identified.

  ## In LDA (Topic_Coherence).R:
 * The code makes use of Latent Dirichlet Allocation to identify significant topics from the text corpus. The ideal number of topics is determined by looking at the coherence scores for the topics. The top words for each topic are then identified.
 

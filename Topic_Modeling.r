# Topic Modeling with LDA
#Sourced from: https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/

#load topic models library
library(topicmodels)

# Load Document term Matrix
load(file="my_dtm_stem_mystopwords_sparce-Spoke_preprosals_20160112.RData")
dtm<-my_dtm_sparse
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE


#Number of topics
k <- 5
That done, we can now do the actual work – run the topic modelling algorithm on our corpus. Here is the code:
  
  #Run LDA using Gibbs sampling
  ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))


#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
#write.csv(ldaOut.topics,file=paste(“LDAGibbs”,k,”DocsToTopics.csv”))


#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
#write.csv(ldaOut.terms,file=paste(“LDAGibbs”,k,”TopicsToTerms.csv”))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
#write.csv(topicProbabilities,file=paste(“LDAGibbs”,k,”TopicProbabilities.csv”))


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
#write.csv(topic2ToTopic3,file=paste(“LDAGibbs”,k,”Topic2ToTopic3.csv”))

#Combined matrix
my_LDA<-cbind(ldaOut.topics,unlist(topic1ToTopic2),unlist(topic2ToTopic3),topicProbabilities)
colnames(my_LDA)[2:3]<-c("t1_2","t2_3")

#write to file
write.csv(my_LDA,file=paste("LDAGibbs",k,"Spoke_preproposals_20160112.csv"))

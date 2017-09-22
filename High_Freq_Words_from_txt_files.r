# Text Mining from Files
#Source: Mix of  
#https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
#AND
# https://github.com/gimoya/theBioBucket-Archives/blob/master/R/txtmining_pdf.R

# Tell R what folder contains your 1000s of txt files
dest <- "/Users/afiimani/Desktop/HUBS/BD Spokes 2 page pre-proposals/"


# read txt files into R
mytxtfiles <- list.files(path = dest, pattern = "txt",  full.names = TRUE)

library(tm)
library(SnowballC)
# So first convert to a data type called "Corpus". We use DirSource because we are directly pulling from the directory. Could be other things.
my_corpus <- Corpus(DirSource(dest, pattern = "txt"))
my_corpus_stem <- tm_map(my_corpus,stemDocument)
  #create the toSpace content transformer
  toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
  my_corpus_stem <- tm_map(my_corpus_stem, toSpace, "\f")
  #Fix problems with steming 
  my_corpus_stem <- tm_map(my_corpus_stem, content_transformer(gsub), pattern = "analyt", replacement = "analy")
  my_corpus_stem <- tm_map(my_corpus_stem, content_transformer(gsub), pattern = "analysi", replacement = "analy")
  my_corpus_stem <- tm_map(my_corpus_stem, content_transformer(gsub), pattern = "healthcar", replacement = "health")
# docs <- tm_map(docs, toSpace, “-“)
# docs <- tm_map(docs, toSpace, “’”)
# docs <- tm_map(docs, toSpace, “‘”)
# docs <- tm_map(docs, toSpace, “•”)
# docs <- tm_map(docs, toSpace, “””)
# docs <- tm_map(docs, toSpace, ““”)
# 
# docs <- tm_map(docs, toSpace, “\”)
# warnings may appear after you run the previous line, they
# can be ignored
# Inpect one document: writeLines(as.character(my_corpus$content[[30]]))

# Create a Document Term Matrix (rows = documents; columns= words) 
my_dtm <- DocumentTermMatrix(my_corpus, control = list(removePunctuation = TRUE, 
                                                       stripWhitespace = TRUE,
                                                       removeNumbers = TRUE, 
                                                       stopwords =  TRUE, 
                                                       tolower = TRUE, 
                                                       wordLengths=c(3,25)))
# This last line with 'wordLengths' overrides the default minimum
# word length of 3 characters to Inf. We want to limit words that are greater than 25 character because those are probably errors. See below:
inspect(my_dtm_stem[1:5,1:5])
my_dtm_stem <- DocumentTermMatrix(my_corpus_stem, control = list(removePunctuation = TRUE, 
                                                       stripWhitespace = TRUE,
                                                       removeNumbers = TRUE, 
                                                       stopwords =  TRUE, 
                                                       tolower = TRUE, 
                                                       wordLengths=c(3,25)))

dtm2 <- as.matrix(my_dtm_stem)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
frequency[1:50]
my_stopwords<-c("able","also","use","like","id","basic","better","will","can","data","big","spoke","project","hub","south","science","well","including","proposal","across","nsf")
my_dtm_stem_mystopwords <- my_dtm_stem[,!(Terms(my_dtm_stem)) %in% my_stopwords] # remove my stopwords from the columns in the term matrix 
dtm2 <- as.matrix(my_dtm_stem_mystopwords)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
frequency[1:100]

#Fix problems with steming 
my_corpus_stem <- tm_map(my_corpus_stem, content_transformer(gsub), pattern = "analyt", replacement = "analy")
my_corpus_stem <- tm_map(my_corpus_stem, content_transformer(gsub), pattern = "analysi", replacement = "analy")
my_corpus_stem <- tm_map(my_corpus_stem, content_transformer(gsub), pattern = "healthcar", replacement = "health")
# Rerun the dtm_stem code above

 
# Cleaning the Data Sparsity: Get rid of words that do not appear often in any document
my_dtm_sparse<-removeSparseTerms(my_dtm_stem_mystopwords, .98) # Create a matrix that has 98% of the sparsity of the orginal sparity of the matrix. So sparasity goes down. Check here
inspect(my_dtm_sparse[1:5,1:5])
colnames(dtm2)[1:50] # Look for funny words with symbols in front at the beginning
colnames(dtm2)[6000:6039] # And at the end

write.csv(as.matrix(my_dtm_sparse),file="my_dtm_stem_mystopwords_sparce-Spoke_preprosals_20160112")
save(my_dtm_sparse,file="my_dtm_stem_mystopwords_sparce-Spoke_preprosals_20160112.RData")

# Find most frequent words 
my_highfreqs <- findFreqTerms(my_dtm_sparse, lowfreq = 5) # create a vector of the high-frequency words
my_highfreqs_values <- colSums(as.matrix(my_dtm_sparse[, my_highfreqs])) # Do column sums to get freq
my_highfreqs_values_df <- data.frame(word = names(my_highfreqs_values),freq = my_highfreqs_values) # Create a dataframe of words and frequencies 

#Sort by frequency of word
library(dplyr)
my_highfreqs_values_df %>% arrange(freq) #Doing the same thing as above but in dplyr
#OR
#my_highfreqs_values_df <- my_highfreqs_values_df[with(my_highfreqs_values_df, order(-freq)),] 

# Create your own stop word list
my_highfreqs_values_df[1:100,1] #Look at this list and add words that do not discriminate
#stopwords(kind = "en") # View built-in English stopword
my_stopwords<-c("able","also","use","like","id","basic","better","will","can","data","big","spoke","project","hub","south","science","well","including","proposal","across","nsf")
my_dtm_sparse_mystopwords <- my_dtm_sparse[,!(Terms(my_dtm_sparse)) %in% my_stopwords] # remove my stopwords from the columns in the term matrix 

# Rank by frequency again
my_highfreqs <- findFreqTerms(my_dtm_sparse_mystopwords, lowfreq = 5) 
my_highfreqs_values <- colSums(as.matrix(my_dtm_sparse_mystopwords[, my_highfreqs])) 
my_highfreqs_values_df <- data.frame(word = names(my_highfreqs_values),freq = my_highfreqs_values) 
my_highfreqs_values_df <- my_highfreqs_values_df[with(my_highfreqs_values_df, order(-freq)), ] # Sort high frequency words 


#Visualize 
hist(my_highfreqs_values_df[,2], breaks=50, ylim=[0,200])

# Stem words and add a column

my_highfreqs_values_df$stem <- wordStem(row.names(my_highfreqs_values_df), language = "english")

library(ggplot2)
# Histogram plot of High frequency words (ggplot= grammar of graphic plot) (aes = asethic or columns to plot x,y ) geom_bar= geometry of the plot is bar stat= how high should the bar be (identity = use the raw values)
#reorder = ranks the words by frequency
#theme rotate the axis of the text

#Temp fix 
library(devtools)
devtools::install_github("hadley/plyr") 

ggplot(my_highfreqs_values_df, aes(reorder(word, -freq), freq)) +
  geom_bar(stat = "identity", fill="white",colour="darkgreen") +
  theme_minimal() +
  xlab("high frequency words") +
  ylab("freqeuncy") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.2))
  + ylim(100, 400)
#+ xlim(my_highfreqs_values_df[20,2], my_highfreqs_values_df[1,2])
#+coord_flip()


# find words associated with each of the high frequency words
my_highfreqs_assocs <- findAssocs(my_dtm_sparse_mystopwords, my_highfreqs, 0.25)
# have a look...
my_highfreqs_assocs 

library(wordcloud)
library(Rstem)
m <- as.matrix(tdm)
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

# Stem words
my_highfreqs_values_df$stem <- wordStem(row.names(my_highfreqs_values_df), language = "english")

# and put words to column, otherwise they would be lost when aggregating
d$word <- row.names(d)

# remove web address (very long string):
d <- d[nchar(row.names(d)) < 20, ]

# aggregate freqeuncy by word stem and
# keep first words..
agg_freq <- aggregate(freq ~ stem, data = d, sum)
agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])

d <- cbind(freq = agg_freq[, 2], agg_word)

# sort by frequency
d <- d[order(d$freq, decreasing = T), ]

# print wordcloud:
wordcloud(d$word, d$freq)













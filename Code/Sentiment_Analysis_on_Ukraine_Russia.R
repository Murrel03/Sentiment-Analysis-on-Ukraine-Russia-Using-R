df
set.seed(23)
rand_df <- df[sample(nrow(df), size=11000), ]
##summary
summary(df)

##packages
library("tm")  #text mining it remove is,was....
library("SnowballC")  #it treat fish,fishing,fished as same...
library("wordcloud")
library("RColorBrewer")
library("syuzhet") #sentiment scores and emotion classification
library(ggplot2)
library(tidyverse)
library(tidytext)
library(factoextra)
library(igraph)
library(textdata)


library('rJava')

data=rand_df$content
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(data)) #corpus is collection of txt document to load text minning or nlp
TextDoc

##cleaning the content column

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, toSpace, "#")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form()
TextDoc <- tm_map(TextDoc, stemDocument)
#removing url
url=function(x) gsub('http[[:alnum:]]*','',x)
TextDoc=tm_map(TextDoc,content_transformer(url))
#unwantedwords
TextDoc=tm_map(TextDoc,removeWords,c('https','tco','will','amp','say','can'))
as.character(TextDoc[[1]])

##document matrix

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc) #here in row it represent words and column document(sentence) while do row sum we can get the freq
dtm_m <- as.matrix(TextDoc_dtm)


# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE) #it gives the word and their count
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
dtm_d
# Display the top most frequent words
head(dtm_d, 10)

#ploting the top 10 most repeated words in graph
barplot(dtm_d[1:40,]$freq, las = 2, names.arg = dtm_d[1:40,]$word,
        col =rainbow(40), main ="Top 40 most frequent words",
        ylab = "Word frequencies")


##word cloud-->its an image composed of keyword found in the body of the txt and the size of each word indicate th freq in the body of txt
set.seed(222)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,max.words =200, random.order=FALSE, rot.per=0.50, 
          colors=brewer.pal(8, "Dark2"))


##word assosiation--> corelation 
findAssocs(TextDoc_dtm, terms = c("standwithukrain","russia","border"), corlimit = 0.15)			
#or
#findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 2000), corlimit = 0.25)


##emotion score NRC-->is a list of english words assosiated with 8 basic emotion
data1=tolower(data)
emotion=get_nrc_sentiment(data1)
emotion
emo_bar=colSums(emotion)
emo=data.frame(count=emo_bar,emotion=names(emo_bar))
#visualisation
ggplot(emo,aes(x=reorder(emotion,-count),y=count))+geom_bar(stat='identity')


#it gives the particular word and its sentiment and its frequency
text.df=tibble(text=str_to_lower(data))


bing_words_counts=text.df %>% unnest_tokens(output = word,input=text) %>%
  inner_join(get_sentiments('bing')) %>%
  count(word,sentiment,sort=TRUE)
bing_words_counts
##selecting top 10 words (in negative and positive)
bing_top_10_words_by_sentiment=bing_words_counts %>%
  group_by(sentiment) %>%
  slice_max(order_by = n,n=10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n))
bing_top_10_words_by_sentiment


#creating barplot from above
bing_top_10_words_by_sentiment %>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment,scales='free_y')+
  labs(y='Contribution to sentiment',x=NULL)+
  coord_flip()


##clustering
#hierarchical(dendogram)  it indicates orders and cluster where formed
head(dtm_m,100)
distance=dist(scale(head(dtm_m,50)),method='euclidean')
print(distance)

hc=hclust(distance,method='ward.D') #It minimizes the total within-cluster variance. At each step the pair of clusters with minimum between-cluster distance are merged.
plot(hc)

#for enhance visualization
require(factoextra)
fviz_dend(x=hc,cex=.8,lwd=.8,k=9,k_colors=rainbow(9))

#recatangle
fviz_dend(x=hc,cex=.8,lwd=.8,k=9,rect=TRUE,rect_border='gray',rect_fill=TRUE)

#differnt views
fviz_dend(x=hc,cex=.8,lwd=.8,k=9,rect=TRUE,rect_border='gray',rect_fill=TRUE,type='phylogenic')

#kmeans clustering
#k means clustering 
library(fpc)
library(cluster)
#install.packages('clusplot')
a
z=sort(dtm_m,decresing='True')
d <- dist(scale(head(dtm_m,5)), method="euclidian")

kfit <- kmeans (d, 4)

clusplot(as.matrix(d), kfit$cluster, color=T, shade =T, labels=2, lines=0,main='clustering 5 words')



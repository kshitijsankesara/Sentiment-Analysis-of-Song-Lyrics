library(ggplot2)
library(readr)
install.packages("data.table")
library(data.table)
install.packages("tidytext")
library(tidytext)
install.packages("tidyverse")
library(tidyverse)
install.packages("magrittr")
library(magrittr)
install.packages("DT")
library(DT)
install.packages("stringr")
library(stringr)
install.packages("wordcloud")
library(wordcloud)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("igraph")
library(igraph)
install.packages("ggraph")
library(ggraph)
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
install.packages("dplyr")
library(dplyr)
install.packages("tidytext")
library(tidytext) 
install.packages("tidyr")
library(tidyr) 
install.packages("widyr")
library(widyr)
install.packages("quanteda")
library(quanteda)
install.packages("ggplot2")
library(ggplot2) 
install.packages("ggrepel")
library(ggrepel) 
install.packages("gridExtra")
library(gridExtra) 
install.packages("knitr")
library(knitr) 
install.packages("kableExtra")
library(kableExtra) 
install.packages("formattable")
library(formattable) 
install.packages("yarrr")
library(yarrr)  
install.packages("radarchart")
library(radarchart)
install.packages("igraph")
library(igraph) 
install.packages("ggraph")
library(ggraph)
install.packages("reshape2")
library(reshape2)

songdata<-fread("songdata.csv",stringsAsFactors = F)
View(songdata)

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), 
        axis.ticks = aticks, 
        panel.grid.minor = pgminor, 
        legend.title = lt,
        legend.position = lp)
}

my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

songdata$len = str_count(songdata$text)
songdata %>%
  ggplot(aes(x = len)) +    
  geom_histogram(fill= "orange",bins = 40) +
  labs(x= 'Word Length of Lyrics',y = 'No. of words', title = paste()) +
  theme_bw()


songdata%>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(25) %>%
  
ggplot(aes(x = word,y = n)) +
geom_bar(stat='identity',colour="white", fill ="red") +
geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'red',
            fontface = 'bold') +
labs(x = 'Word', y = 'Count', 
       title = "Top 25 Words") +
coord_flip() + 
theme_bw()


dcorpus <- corpus(songdata$text)
dfm1 <- dfm(
  dcorpus, 
  ngrams = 1, 
  remove = c("rm", stopwords("english"),"undesirable_words"),
  remove_punct = TRUE,
  remove_numbers = TRUE,
  stem = TRUE)

topfeat <- topfeatures(dfm1, n = 25)
textplot_wordcloud(dfm1, min.freq = 3e4, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))


dfm2 <- dcorpus %>%
  corpus_sample(size = floor(ndoc(dcorpus) * 0.30)) %>%
  dfm(
    ngrams = 2,
    remove = c("rm", stopwords("english")),
    remove_punct = TRUE,
    remove_numbers = TRUE,
    concatenator = " "
  )


set.seed(100)
textplot_wordcloud(dfm2, min.freq = 3500, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))




topfeat2 <- topfeatures(dfm2, n = 25)

# convert to df and plot
data.frame(term = names(topfeat2), freq = unname(topfeat2)) %>%
  ggplot(aes(x = reorder(term, freq), y = freq/1000)) + 
  geom_bar(stat = 'identity', fill = 'black') + 
  labs(x = 'Bigrams', y = 'Count (000s)', title = '25 Most Common Bigrams') + 
  coord_flip()



dfm3 <- dcorpus %>%
  corpus_sample(size = floor(ndoc(dcorpus) * 0.50)) %>%
  dfm(
    ngrams = 3,
    ignoredFeatures = c("rm", stopwords("english")),
    remove_punct = TRUE,
    remove_numbers = TRUE,
    concatenator = " "
  )
tf <- topfeatures(dfm3, n = 25)

# convert to df and plot
data.frame(term = names(tf), freq = unname(tf)) %>%
  ggplot(aes(x = reorder(term, freq), y = freq/1000)) + 
  geom_bar(stat = 'identity', fill = 'red') + 
  labs(x = 'Trigrams', y = 'Count (000s)', title = '25 Most Common Trigrams') + 
  coord_flip() 


library(readr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(stringr)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(tm)
library(tidytext)
library(tidyr)
library(repr)
install.packages("Rmisc")
install.packages("repr")

lyrics <- read_csv("songdata.csv")
artist<- as.data.frame(table(as.data.frame(lyrics$artist)))
colnames(artist) <- c("artist", "Num_of_songs")
print(artist)
View(artist)
most_songs <- arrange(artist, desc(Num_of_songs))
head(most_songs,10)
View(most_songs)

options(repr.plot.width=5, repr.plot.height=4)
tilt_theme <- theme(axis.text.x=element_text(angle=30, hjust=1))
p1 <- ggplot(data = head(most_songs,10), aes(artist, Num_of_songs, fill = Num_of_songs)) +
  geom_bar(stat = "identity") +
  ggtitle("Artists with Most Songs") +
  tilt_theme
p1


least_songs <- tail(most_songs, 15)
p2 <- ggplot(data = least_songs, aes(artist, Num_of_songs, fill = Num_of_songs)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=Num_of_songs), vjust=1.6, color="white", size=3) +
  ggtitle("Artists with least number of songs") +
  tilt_theme
p2

p3 <- ggplot(artist, aes(x=Num_of_songs)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="orange")+
  geom_density(alpha=.1, fill="black")
p3

count_words <- function(vec){
  return (length(unlist((str_extract_all(tolower(vec), '\\w+')))))
}
lyrics$word_count <- sapply(lyrics$text, count_words)
head(lyrics$word_count)

p4 <- ggplot(lyrics, aes(x=word_count)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="red")
p4

lyrics$title_word_count <- sapply(lyrics$song, count_words)
head(lyrics$title_word_count)

longest_song <- arrange(lyrics, desc(word_count))
longest_song <- head(longest_song, 10)
shortest_song <- arrange(lyrics, word_count)
shortest_song <- head(shortest_song, 10)
longest_song$song
shortest_song$song

options(repr.plot.width=8, repr.plot.height=4)
p5 <- ggplot(data = longest_song, aes(song, word_count, fill = title_word_count)) +
  geom_bar(stat = "identity") +
  ggtitle("Longest Songs") +
  tilt_theme
p5

p6 <- ggplot(data = shortest_song, aes(song, word_count, fill = title_word_count)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Shortest Songs") +
  tilt_theme
p6


options(repr.plot.width=4, repr.plot.height=4)
p7 <- ggplot(lyrics, aes(x=title_word_count)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="red", binwidth = 1, bins = 1)
p7


options(repr.plot.width=10, repr.plot.height=10)
texts <- lyrics$song
#texts <- iconv(texts, to = "utf-8")
corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c("and", "this", "there")) 
corpus <- Corpus(VectorSource(corpus))
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
d <- d[-which(d$word %in% c("and","this","that")),]
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 4,
          max.words=75, random.order=TRUE, 
          colors=brewer.pal(1, "Dark2"))


longest_title <- subset(lyrics, lyrics$title_word_count > 13)
longest_title$song
shortest_title <- subset(lyrics, lyrics$title_word_count == 1)
shortest_title$song[1:10]


texts <- longest_title$song
corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- Corpus(VectorSource(corpus))
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,scale=c(2,0.5),
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

texts <- shortest_title$song
corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- Corpus(VectorSource(corpus))
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,scale=c(2,0.5),
          max.words=50, random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


options(repr.plot.width=6, repr.plot.height=4)
p8 <- ggplot(lyrics, aes(x=factor(title_word_count), y=word_count, fill = factor(title_word_count))) + 
  geom_boxplot() 
p8


cor(lyrics$title_word_count, lyrics$word_count)


fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
lyrics$text <- sapply(lyrics$text, fix.contractions)


# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
lyrics$text <- sapply(lyrics$text, removeSpecialChars)


# convert everything to lower case
lyrics$text <- sapply(lyrics$text, tolower)

str(lyrics[13, ]$text, nchar.max = 300)


nrc_sentiment <- get_sentiments("nrc")
unique(nrc_sentiment$sentiment)

lyrics_words <- select(lyrics, c("artist", "text"))
lyrics_words <- lyrics_words %>% unnest_tokens(word, text)
head(lyrics_words)
dim(lyrics_words)


joy <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "joy")) 
joy <- as.data.frame(sort(table(joy$word)))
columns_sentiment <- c("word", "Freq")
colnames(joy) <- columns_sentiment
tail(joy, 10)
View(joy)

trust <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "trust")) 
trust <- as.data.frame(sort(table(trust$word)))
colnames(trust) <- columns_sentiment
tail(trust, 10)
View(trust)

fear <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "fear")) 
fear <- as.data.frame(sort(table(fear$word)))
colnames(fear) <- columns_sentiment
View(fear)

sadness <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "sadness")) 
sadness <- as.data.frame(sort(table(sadness$word)))
colnames(sadness) <- columns_sentiment
View(sadness)

anger <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "anger")) 
anger <- as.data.frame(sort(table(anger$word)))
colnames(anger) <- columns_sentiment
View(anger)


options(repr.plot.width=13, repr.plot.height=8)

p9 <- ggplot(data = tail(joy, 10), aes(word, Freq, fill = word)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Joy") +
  guides(fill=FALSE) +
  tilt_theme
p9


p10 <- ggplot(data = tail(trust, 10), aes(word, Freq, fill = word)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Trust") +
  guides(fill=FALSE) +
  tilt_theme
p10



p11 <- ggplot(data = tail(fear, 10), aes(word, Freq, fill = word)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Fear") +
  guides(fill=FALSE) +
  tilt_theme
p11


p12 <- ggplot(data = tail(sadness, 10), aes(word, Freq, fill = word)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Sadness") +
  guides(fill=FALSE) +
  tilt_theme
p12


p13 <- ggplot(data = tail(anger, 10), aes(word, Freq, fill = word)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Anger") +
  guides(fill=FALSE) +
  tilt_theme
p13

pos <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "positive")) 
pos <- as.data.frame(sort(table(pos$word)))
colnames(pos) <- columns_sentiment

neg <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "negative")) 
neg <- as.data.frame(sort(table(neg$word)))
colnames(neg) <- columns_sentiment

options(repr.plot.width=8, repr.plot.height=5)
p17 <- ggplot(data = tail(pos, 20), aes(word, Freq, fill = word)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Positive") +
  guides(fill=FALSE) +
  tilt_theme
p17

p18 <- ggplot(data = tail(neg, 20), aes(word, Freq, fill = word)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Negative") +
  guides(fill=FALSE) +
  tilt_theme
p18

lyrics_words <- select(lyrics, c("artist", "song","text"))
lyrics_words <- lyrics_words %>% unnest_tokens(word, text)
head(lyrics_words)
dim(lyrics_words)

joy_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "joy")) 
joy_s <- as.data.frame(sort(table(joy_s$song)))
columns_sentiment <- c("Song", "Freq")
colnames(joy_s) <- columns_sentiment
#tail(joy_s, 10)

trust_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "trust")) 
trust_s <- as.data.frame(sort(table(trust_s$song)))
colnames(trust_s) <- columns_sentiment
#tail(trust_s, 10)

fear_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "fear")) 
fear_s <- as.data.frame(sort(table(fear_s$song)))
colnames(fear_s) <- columns_sentiment

sadness_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "sadness")) 
sadness_s <- as.data.frame(sort(table(sadness_s$song)))
colnames(sadness_s) <- columns_sentiment

anger_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "anger")) 
anger_s <- as.data.frame(sort(table(anger_s$song)))
colnames(anger_s) <- columns_sentiment

options(repr.plot.width=8, repr.plot.height=15)
par(mfrow=c(3,2))
#Create word cloud of positive words of NRC lexicon
wordcloud(words = pos$word, freq = pos$Freq, min.freq = 50,
          max.words=100, scale=c(1, 0.5), random.order = FALSE, random.color = FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
title("Positive words in NRC Lexicon", outer=FALSE)
#colors= c("indianred1","indianred2","indianred3","indianred"))
#Create word cloud of negative words of NRC lexicon
wordcloud(words = neg$word, freq = neg$Freq, min.freq = 50,
          max.words=100, scale=c(1, 0.5), random.order = FALSE, random.color = FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Accent"))
          
          
options(repr.plot.width=15, repr.plot.height=8)

p23 <- ggplot(data = tail(joy_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Joy Songs") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
p23

p24 <- ggplot(data = tail(trust_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  ggtitle("Trust") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
p25 <- ggplot(data = tail(fear_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  ggtitle("Fear") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
p26 <- ggplot(data = tail(sadness_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  ggtitle("Sadness") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
p27 <- ggplot(data = tail(anger_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  ggtitle("Anger") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme


joy_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "joy")) 
joy_s <- as.data.frame(sort(table(joy_s$song)))
columns_sentiment <- c("Song", "Freq")
colnames(joy_s) <- columns_sentiment
#tail(joy_s, 10)

trust_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "trust")) 
trust_s <- as.data.frame(sort(table(trust_s$song)))
colnames(trust_s) <- columns_sentiment
#tail(trust_s, 10)

fear_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "fear")) 
fear_s <- as.data.frame(sort(table(fear_s$song)))
colnames(fear_s) <- columns_sentiment

sadness_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "sadness")) 
sadness_s <- as.data.frame(sort(table(sadness_s$song)))
colnames(sadness_s) <- columns_sentiment

anger_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "anger")) 
anger_s <- as.data.frame(sort(table(anger_s$song)))
colnames(anger_s) <- columns_sentiment


options(repr.plot.width=15, repr.plot.height=8)
p23 <- ggplot(data = tail(joy_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Joy") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
p24 <- ggplot(data = tail(trust_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Trust") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
p25 <- ggplot(data = tail(fear_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Fear") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
p26 <- ggplot(data = tail(sadness_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Sadness") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
p27 <- ggplot(data = tail(anger_s, 10), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Anger") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
p23
p24
p25
p26
p27


pos_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "positive")) 
pos_s <- as.data.frame(sort(table(pos_s$song)))
colnames(pos_s) <- columns_sentiment

neg_s <- lyrics_words %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment == "negative")) 
neg_s <- as.data.frame(sort(table(neg_s$song)))
colnames(neg_s) <- columns_sentiment

options(repr.plot.width=12, repr.plot.height=4)
p31 <- ggplot(data = tail(pos_s, 20), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Positive") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme

p32 <- ggplot(data = tail(neg_s, 20), aes(Song, Freq, fill = Song)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Negative") +
  guides(fill=FALSE) + coord_flip() +
  tilt_theme
multiplot(p31, p32, cols=2)

p31
p32

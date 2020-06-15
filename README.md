# Sentiment-Analysis-of-Song-Lyrics

IST 736: Text Mining

Introduction:
For this project, I am analyzing song data of 57650 records. I’m using text mining approaches like Clustering words and predicting artists by song and emotion analysis. I have used the Kaggle dataset (https://www.kaggle.com/mousehead/songlyrics). I’m targeting the Lyrics variable for our text mining and sentiment analysis. 

**Data Preprocessing:**
1. Removed Stopwords
2. Converted everything to Lowercase
3. Perform stemming on text data
4. Removed unnecessary characters
5. Perform vectorization

**Text Exploration:**
1. Build Wordcloud
2. Build bar plots showing the top 25 words with frequency
3. Build graphs showing the top 25 bigrams and trigrams and its frequency
4. Build visualizations for the top 10 artists based on the number of songs
5. Analyzed the Longest and Shortest songs

I performed **K-Means clustering** on the dataset to classify the songs with similar lyrics. I also used NRC Sentiment Lexicon for performing **sentiment analysis** on the lyrics of songs. The major sentiments were Joy, Fear, Sadness, Trust, and Anger. I analyzed the top words for each sentiment of the song. I divided songs into positive and negative sentiment based on the words. 

**Programming: R**

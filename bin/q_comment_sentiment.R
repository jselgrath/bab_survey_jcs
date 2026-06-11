# Equity in Ocean Access (Benefits and Barriers (bab))
# Jennifer Selgrath 
# California Marine Sanctuary Foundation/ CINMS

# goal: subset comment data for analysis of + vs - comment scoring
# https://programminghistorian.org/en/lessons/sentiment-analysis-syuzhet

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(syuzhet)
library(tidytext)
library(wordcloud)
library(ggwordcloud)
library(RColorBrewer)
# install.packages("sentimentr")
library(sentimentr)

# check if reaL:
# Cero, I., Luo, J., & Falligant, J. (2023). Lexicon-based sentiment analysis in behavioral research. Center for Open Science. https://doi.org/10.31219/osf.io/gw97k
# 
# Cheng, X., Yan, X., Lan, Y., & Guo, J. (2026). Breast cancer screening knowledge and sentiments in Singaporean women. Journal of Medical Internet Research, 26(1). https://www.jmir.org/2026/1/e78439/PDF
# 
# Hogenboom, A., van Iterson, P., Heerschop, B., Frasincar, F., & Kaymak, U. (2011). Determining negation scope and strength in sentiment analysis. 2011 IEEE International Conference on Systems, Man, and Cybernetics, 2589-2594. https://doi.org/10.1109/icsmc.2011.6084066

# Klinkhammer, D. (2022). Sentiment analysis with R: Natural language processing for semi-automated assessments of qualitative data. arXiv. https://doi.org/10.48550/arxiv.2206.12649


# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("C:/Users/Jennifer.Selgrath/Documents/r_projects/bab_survey_jcs")

d1<- read_csv("./results/data_long9.csv") %>%
  dplyr::select(response_id,
         QDemographic_PrimaryZip:QDemographic_Swimming,Phase,Format,City,Primary_County,EJ_Score:Distance_Binned,
         q_demographic_race,q_demographic_gender,q_demographic_education_clean,
         q_mapping_county,
         file_origin:comment_notes,
         influencer_any_b)%>% #influencer_most_b,
  filter(!is.na(comment_clean))%>%
  mutate(rand=rnorm(1225,mean=0,sd=1))%>%
  dplyr::select(-QDemographic_Race_TEXT, -QDemographic_Asian,-QDemographic_Asian_TEXT,-QDemographic_Tribal_TEXT)%>%
  dplyr::select(response_id,comment_clean:comment_notes,QDemographic_PrimaryZip:influencer_any_b,comment,rand)%>%
  arrange(rand)%>%
  glimpse()

names(d1)

d2<-d1%>%
  dplyr::select(response_id,comment_clean)%>%
  # mutate(rand=rnorm(1225,mean=0,sd=1))%>%
  # filter(!is.na(comment_clean))%>%
  # arrange(rand)%>%
  glimpse()


# syuzhet version p- this does individual word analysis, but misses complex concepts (e.g., not clean) --------------------------

#Calculate raw sentiment scores using the 'bing' lexicon
# (Returns a positive or negative integer based on word counts)
d2$comment_clean[is.na(d2$comment_clean)] <- "" # fill in empty text (should be none)

d3 <- d2 %>%
  mutate(comment_clean = enc2utf8(comment_clean)) %>%
  mutate(comment_clean = iconv(comment_clean, from = "UTF-8", to = "ASCII", sub = "")) %>%
  mutate(comm_sentiment_raw = get_sentiment(comment_clean, method = "bing")) %>%
  glimpse()

range(d3$comm_sentiment_raw) # -9 to 10

# Map the raw scores into your custom -2 to 2 scale
d4<-d3%>%
  mutate(comm_sentiment_syuzhet = case_when(
    comm_sentiment_raw >= 3   ~ 2,   # Strong Positive (3 to 10)
    comm_sentiment_raw >= 1   ~ 1,   # Positive (1 to 2)
    comm_sentiment_raw == 0   ~ 0,   # Strictly Neutral (0)
    comm_sentiment_raw <= -1  ~ -1,  # Negative (-1 to -2)
    comm_sentiment_raw <= -3  ~ -2   # Strong Negative (-9 to -3)
  )) %>%
  glimpse()


# sentimentr version ------------------

# 1. Calculate element-level sentiment scores directly on the text
# (sentimentr automatically handles negations, amplifiers, and question marks)
sentiment_scores_lr <- d2 %>%
  mutate(comment_clean = enc2utf8(comment_clean)) %>%
  mutate(comment_clean = iconv(comment_clean, from = "UTF-8", to = "ASCII", sub = "")) %>%
  mutate(sentence_splits = get_sentences(comment_clean)) %>%
  
  with(sentiment_by(sentence_splits, by = response_id)) %>% # averages sentence scores if a comment has multiple sentences
  as_tibble() %>% # sentiment_by returns a data.table > tibble/df
  dplyr::select(response_id, ave_sentiment) %>%#mean sentiment scores. typically ranges between -1.0 and 1.0
  right_join(d2, by = "response_id") %>%
  mutate(ave_sentiment = replace_na(ave_sentiment, 0)) %>% # If a comment had no text or was entirely neutral, treat score as 0
  glimpse()


# 2. Join back to your original data and map to your custom scale
d6 <- d2 %>%
  left_join(sentiment_scores_lr, by = "response_id") %>%
  # If a comment had no text or was entirely neutral, treat score as 0
  mutate(ave_sentiment = replace_na(ave_sentiment, 0)) %>% 
  # Map continuous sentiment scores to your discrete -2 to 2 scale
  mutate(comm_sentiment_sentimentr = case_when(
    ave_sentiment >=  0.5   ~  2,  # Strong positive
    ave_sentiment >   0.0   ~  1,  # Mild positive
    ave_sentiment ==  0.0   ~  0,  # Neutral
    ave_sentiment >= -0.5   ~ -1,  # Mild negative
    ave_sentiment <  -0.5   ~ -2   # Strong negative
  )) %>%
  # Drop calculation column to match your original output
  # dplyr::select(-ave_sentiment) %>% 
  glimpse()


# tidytext version ------------------


# word bins (allows for complex sentiments e.g., not clean)--------------------------

# Define a vector of common negation words
negation_words <- c("not", "no", "never", "without", "barely")

# tokenize into bin grams
bigrams_filtered <-  d2 %>%
  unnest_tokens(bigram, comment_clean, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  glimpse()

bing_lexicon <- get_sentiments("bing")%>%
  glimpse()

sentiment_adjusted <- bigrams_filtered %>%
  # Get sentiment for the second word
  inner_join(bing_lexicon, by = c("word2" = "word")) %>%
  # If word1 is a negation, flip the sentiment
  mutate(adjusted_sentiment = case_when(
    word1 %in% negation_words & sentiment == "positive" ~ "negative",
    word1 %in% negation_words & sentiment == "negative" ~ "positive",
    TRUE ~ sentiment
  ))%>%
  glimpse()



# Tokenize comments into single words
d5 <- d2 %>%
  dplyr::select(response_id, comment_clean) %>%
  unnest_tokens(word, comment_clean)%>%
  glimpse()

# 3. Join with the 'bing' lexicon and calculate net score per response
sentiment_scores <- d5 %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(response_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_score = positive - negative)

# 4. Join back to original data and categorize into your -2 to 2 scale
d6 <- d2 %>%
  left_join(sentiment_scores, by = "response_id") %>%
  # If a comment had no sentiment words found, treat net_score as 0
  mutate(net_score = replace_na(net_score, 0)) %>% 
  mutate(comm_sentiment_tidytext = case_when(
    net_score >= 3  ~ 2,
    net_score >= 1  ~ 1,
    net_score == 0  ~ 0,
    net_score <= -1  ~ -1,
    net_score <= -3  ~ -2
  )) %>%
  dplyr::select(-positive, -negative, -net_score)%>% # Drop calculation columns
  glimpse() 


# join methods
d7<-d6%>%
  dplyr::select(-comment_clean)%>% #(-rand,-
  full_join(d4)%>%
  dplyr::select(response_id,comment_clean,comm_sentiment_tidytext,comm_sentiment_syuzhet,comm_sentiment_raw)%>%
  glimpse()

d8<-d7%>%
  dplyr::select(response_id,comm_sentiment_syuzhet)%>%
  full_join(d1)%>%
  glimpse()

d8

d9<-d7[1:30,]%>%
  dplyr::select(response_id,comment_clean)%>%
  glimpse()


# save -------------------
write_csv(d7,"./results/comments_sentiment.csv")
write_csv(d8,"./results/comments_sentiment_demographics.csv")
write_csv(d9,"./results/comments_sentiment_30.csv")


# WORD CLOUD -----------------------------
# remove filler words and count frequencies (and, the, of, etc.)
word_counts <- d5 %>%
  anti_join(stop_words, by = "word") %>%
  filter(word!="trump")%>% #remove names
  
  # (Optional) If you only want words that match the Bing sentiment lexicon
  inner_join(get_sentiments("bing"), by = "word") %>%
  
  # Count word frequencies, keeping the sentiment column for coloring
  count(word, sentiment, sort = TRUE) %>%
  
  # Optional: Grab just the top 100 words so the cloud isn't too cluttered
  slice_max(n, n = 100, with_ties = FALSE)%>%
  glimpse()





# wordcloud--------------

# Extract colorblind-friendly colors from RColorBrewer
# cb_colors <- brewer.pal(8, "Dark2")
# neg_color <- cb_colors[2] # orange
# pos_color <- cb_colors[3] # blue/teal

cb_colors <- brewer.pal(5, "RdBu")
neg_color <- cb_colors[1] # dark red/rust
pos_color <- cb_colors[5] # dark blue


w1<-
  ggplot(word_counts, aes(label = word, size = n, color = sentiment)) +
  geom_text_wordcloud_area(shape = "circle") + # Ensures area is proportional to frequency
  scale_size_area(max_size = 30) +             # Controls how big the largest words get
  scale_color_manual(values = c("negative" = neg_color, "positive" = pos_color)) +
  theme_minimal()  +                            # Removes background grid/axes
  theme(
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
w1


ggsave("./doc/q_comment_sentiment_wc.png",  w1,width = 8, height = 8,     # size in inches
       units = "in",              # "in", "cm", or "mm"
       dpi = 300,                 # resolution 
       bg = "transparent"               # background color (use "white" if needed)
)



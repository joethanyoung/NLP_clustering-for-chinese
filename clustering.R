# import required pkgs
library(jiebaR)
library(tm)
library(slam)
library(proxy)
library(cluster)
library(readxl)
library(dplyr)
library(text2vec)
library(Matrix)
library(ggplot2)
library(cluster)


# read file
df <- read_excel("L06103基础表投诉处理清单.xlsx", skip = 1) # 使用你自己的数据

# take a look on dataframe
dataframe <- df 
dataframe %>% glimpse

# 
worker <- worker(bylines = F) 
seg_words <- lapply(dataframe$投诉事由, function(x) segment(x, worker))

# 
it <- itoken(seg_words, progressbar = FALSE)
v <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(v)
dtm <- create_dtm(it, vectorizer)

# make code more productive
set.seed(1234) 
fit <- kmeans(dtm, centers = 5)


# fit cluster
dataframe$cluster <- fit$cluster

# present in table form
table(dataframe$cluster)

# 
terms <- v$term

# function
get_top_terms <- function(cluster_number, dtm, terms, n = 10) {
  term_counts <- colSums(dtm[dataframe$cluster == cluster_number, ])
  top_terms <- sort(term_counts, decreasing = TRUE)[1:n]
  names(top_terms)
}

# 
lapply(1:5, get_top_terms, dtm = dtm, terms = terms)

# evulate the results
gap_stat <- clusGap(dtm, FUN = kmeans, nstart = 10, K.max = 30, B = 5, verbose = TRUE)

# 
print(gap_stat, method = 'firstmax')
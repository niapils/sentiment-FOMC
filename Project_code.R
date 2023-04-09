require(pdftools); require(ggplot2); require(tm); require(quanteda); require(RWeka); require(NLP) 
require(SentimentAnalysis); require(COSINE); require(lsa); require(ca); require(topicmodels); require(wordcloud)


setwd("B:/NHH/SEM 3/DATA ANALYSIS/Applied Textual Analysis/Project")


###########################################################################################
#                      TASK1 : DOWNLOAD FOMC TRANSCRIPTS                                  #
###########################################################################################

# download the source code and extract href for yearly reports
sc<-readLines("https://www.federalreserve.gov/monetarypolicy/fomc_historical_year.htm")[1177]

# Extract the href from 2005 to 2008
sc <- unlist(strsplit(sc, split = "<li><a"))
sc <- gsub(".*=\"|\".*","",sc[3:6])


# Download source code for all years and extract href for meeting transcripts 
file.href <- vector("character")
for(i in sc){
  print(paste("https://www.federalreserve.gov",i, sep=""))
  ss <- readLines(paste("https://www.federalreserve.gov",i, sep = ""))
  ss <- subset(ss, grepl("meeting.pdf",ss )==T)
  ss <- unlist(strsplit(ss, split = "href="))
  ss<- subset(ss, grepl("meeting.pdf", ss))
  ss<- gsub(".*monetarypolicy|\">.*","",ss)
  file.href <- append(file.href, ss, after = length(file.href))
}

period <- gsub(".*FOMC|meeting.*","",file.href)
file.path <- paste0("FOMC/", period, ".pdf")

# Takes the hrefs for meetings and download the transcripts of all meetings

# Download all transcript files
for(i in 1:length(file.href)){
  # create url
  web.url <- paste("https://www.federalreserve.gov/monetarypolicy",file.href[i], sep = "")
  # Download transcripts
  download.file(web.url, mode = "wb", destfile = file.path[i])
}

# Read raw text into R and combine them 
file.list <- list.files("FOMC/")
FOMC.raw.trancript <- character(0)

for(i in 1:length(file.list)){
  print(file.list[i])
  transcript <- pdf_text(paste("FOMC/",file.list[i], sep = ""))
  transcript <- iconv(transcript, to="utf-8")
  transcript <- paste(transcript, collapse = " ")
  FOMC.raw.trancript <- append(FOMC.raw.trancript, transcript, 
                               after = length(FOMC.raw.trancript))
  }
names(FOMC.raw.trancript) <- file.list

# Keep only the 27 transcripts we need
FOMC.raw.trancript <- FOMC.raw.trancript[3:29]

# Remove redundant files 
rm(i, period, sc, ss, transcript, web.url, file.path, file.list, file.href)




###########################################################################################
#                     TASK 2: STRUCTURE THE DOWNLOADED TRANSCRIPTS                        #
###########################################################################################

# load provided data 
load("raw.transcripts.Rdata")


#Transcript starts page 3 after "Transcript of the Federal Open Market Committee Meeting of January 30-31,2007"
#Transcript ends before "END OF MEETING" page 215

# cleaning data
# Keeps only transcripts  
clean.transcripts <- character()

for (i in 1:length(raw.transcripts)) {
  if (i == 2 | i== 3){
    clean <- gsub(".*Transcript of Federal Open Market Committee Meeting|END OF MEETING.*","",raw.transcripts[i])
  } else 
    clean <- gsub(".*Transcript of the Federal Open Market Committee Meeting|END OF MEETING.*","",raw.transcripts[i])
  clean.transcripts <- append(clean.transcripts, clean, 
                              after = length(clean.transcripts))
  }
# removes text enclosed in square brackets because they are descriptions of an action not spoken words
clean.transcripts <- gsub('\\[.*?\\]', '',clean.transcripts)
# removes text enclosed in round brackets because they are mostly either appendix, numbers or full names of an abbreviated text
clean.transcripts <- gsub('\\(.*?\\)', '', clean.transcripts)
# Split into senntences 
clean.transcripts <- gsub("[\r\n]", " ", clean.transcripts)
# Removes digits they do not hold any interesting information for our analysis
clean.transcripts <- gsub("[[:digit:]]", " ", clean.transcripts)
# Removes puntuations // We will use this for topic model later 
clean.transcripts.topic <- gsub("[[:punct:]]", " ", clean.transcripts)
# collapsing extra white spaces 
clean.transcripts <- gsub("\\s+", " ", clean.transcripts.topic)


###########################################################################################
#                               TASK 3: KEYWORD EXTRACTION                                #
###########################################################################################

# Seperating into control, crisis and pre-crisis corpus 
control.corpus <- clean.transcripts[1:9]
pre_crisis.corpus <- clean.transcripts[10:18]
crisis.corpus <- clean.transcripts[19:27]


# BIGRAMS EXTRACTION: PACKAGE USED: "quanteda"

# Control Corpus Bigrams 
mydfm <- dfm(control.corpus, ngrams=2, remove_punct=T, remove_numbers=T)
feat <- featnames(mydfm)
feat_split <- stringi::stri_split_fixed(feat, mydfm@concatenator)
feat_stop <- feat[sapply(feat_split, function(x) any(x %in% stopwords()))]
control.ngrams <- dfm_remove(mydfm, feat_stop)  
control.ngrams <- as.data.frame(t(control.ngrams))
control.ngrams$freq.control <- rowSums(control.ngrams)
control.ngrams$bigrams <- rownames(control.ngrams)

# Crisis Corpus Bigrams
mydfm <- dfm(crisis.corpus, ngrams=2, remove_punct=T, remove_numbers=T)
feat <- featnames(mydfm)
feat_split <- stringi::stri_split_fixed(feat, mydfm@concatenator)
feat_stop <- feat[sapply(feat_split, function(x) any(x %in% stopwords()))]
crisis.ngrams <- dfm_remove(mydfm, feat_stop)  
crisis.ngrams <- as.data.frame(t(crisis.ngrams))
crisis.ngrams$freq.crisis <- rowSums(crisis.ngrams)
crisis.ngrams$bigrams <- rownames(crisis.ngrams)

# Take out the needed variables 
crisis.ngrams <- crisis.ngrams[,c(10,11)]
control.ngrams <- control.ngrams[,c(10,11)]

# Merge control and crisis bigrams with frequencies. Bigrams that appear in only one corpus
# would have NA so removomg NAs will remove those bigrams. 
control.crisis.ngrams <- merge(control.ngrams,  crisis.ngrams, 
                              by = "bigrams", NA.RM=T)

# Relative frequencies 
control.crisis.ngrams$control.rel.freq <- 100*(control.crisis.ngrams$freq.control/
                                                (sum(control.crisis.ngrams$freq.control)))

control.crisis.ngrams$crisis.rel.freq <- 100*(control.crisis.ngrams$freq.crisis/
                                               (sum(control.crisis.ngrams$freq.crisis)))

# Ranking bigrams
control.crisis.ngrams$rank <- control.crisis.ngrams$crisis.rel.freq/control.crisis.ngrams$control.rel.freq
control.crisis.ngrams <- control.crisis.ngrams[order(control.crisis.ngrams$rank, decreasing = T),]

# keywords 
keywords <- control.crisis.ngrams$bigrams[1:50]
keywords


# Pre_crisis corpus Bigrams  
mydfm <- dfm(pre_crisis.corpus, ngrams=2, remove_punct=T, remove_numbers=T)
feat <- featnames(mydfm)
feat_split <- stringi::stri_split_fixed(feat, mydfm@concatenator)
feat_stop <- feat[sapply(feat_split, function(x) any(x %in% stopwords()))]
pre_crisis.ngrams <- dfm_remove(mydfm, feat_stop)  
pre_crisis.ngrams <- as.data.frame(pre_crisis.ngrams)

# getting length of all bigrams
pre_crisis.ngrams$doc.length = rowSums(pre_crisis.ngrams)
use.later = pre_crisis.ngrams[55713]
################

pre_crisis.ngrams = as.data.frame(t(pre_crisis.ngrams))
pre_crisis.ngrams$bigrams = rownames(pre_crisis.ngrams)

# compare with keywords
pre_crisis.ngrams = subset(pre_crisis.ngrams, (pre_crisis.ngrams$bigrams  %in% keywords)==T)
pre_crisis.ngrams = as.data.frame(t(pre_crisis.ngrams[-10]))
pre_crisis.ngrams$freq.pre_crisis = rowSums(pre_crisis.ngrams)
pre_crisis.ngrams$date = rownames(pre_crisis.ngrams)

# adding the length of all bigrams 
pre_crisis.ngrams = cbind(pre_crisis.ngrams[41:42], use.later)
pre_crisis.ngrams$Norm.Freq = 100*(pre_crisis.ngrams$freq.pre_crisis/pre_crisis.ngrams$doc.length)

# time series plot 
freq <- pre_crisis.ngrams$Norm.Freq
names(freq) <- rownames(pre_crisis.ngrams)

barplot(freq, ylab = "Frequency", xlab = "Date")

# remove redundant data
rm(mydfm, feat, feat_split, feat_stop, use.later, freq, keywords)



###########################################################################################
#                                      TASK 4: SENTIMENT                                  #
###########################################################################################

# Create a dtm for sentiment analysis
corpus <- Corpus(VectorSource(clean.transcripts))
dtm <- DocumentTermMatrix(corpus, control = list(stemming = T, stopwords=T,
                                    removePunctuation=T, removeNumbers=T, stripWhitespace=T))
dtm.freq <- as.matrix(dtm)
dtm.freq <- sort(rowSums(dtm.freq),decreasing=TRUE)
dtm.freq <- data.frame(date  = names(dtm.freq),dtm.freq)

dtm.freq <- as.matrix(dtm)
dtm.freq <- (rowSums(dtm.freq))
dtm.freq <- data.frame(date  = names(dtm.freq),dtm.freq)


sentiment.transcripts <- analyzeSentiment(dtm)
sentiment.transcripts <- sentiment.transcripts[,c(1,2,8)] # Keep only GI and LM

# Time Series Graph of Sentiment Analysis
sent <- sentiment.transcripts$SentimentGI
names(sent) <- dtm.freq$date

barplot(sent, ylab = "Sentiment Score", xlab = "Date", ylim = c(0,-0.06))

# Remove redundant data
# rm(sent, corpus, text.source)


###########################################################################################
#                                 TASK 5:COSINE SIMILARITY                                #
###########################################################################################

# Collapsing each corpuses into one vector 
control.collapse <- paste(control.corpus, collapse = " ")
crisis.collapse <- paste(crisis.corpus, collapse = " ")

# collapsed Vector
corpus.collapse <- rbind(control.collapse, crisis.collapse)
corpus.collapse <- append(corpus.collapse, pre_crisis.corpus, after = length(corpus.collapse))
names(corpus.collapse)[1:2] <- c("control", "crisis")

corpus <- Corpus(VectorSource(corpus.collapse))
dtm <- TermDocumentMatrix(corpus, control = list(stemming = T, stopwords=T,
                           removePunctuation=T, removeNumbers=T, stripWhitespace=T,
                           bounds=list(global = c(1,8)), wordLengths = c(3, 20) ))

# Cosine Similarity 
CosineSimilarity <- function(A, B) {
  sum(A * B) / sqrt(sum(A^2)*sum(B^2)) }

cos.sim.control <- numeric(length = 11)
cos.sim.crisis <- numeric(length = 11)

# name the vector
names(cos.sim.control) <- dtm$dimnames$Docs
names(cos.sim.crisis) <- dtm$dimnames$Docs

# compute cosine similarity
for(i in c(1,3:11)){
  cos.sim.control[i] <- CosineSimilarity(dtm[ , i], dtm[ , 1])
}
cos.sim.control

for(i in c(2:11)){
  cos.sim.crisis[i] <- CosineSimilarity(dtm[ , i], dtm[ , 2])
}
cos.sim.crisis

barplot(cos.sim.control[-c(1,2)], ylab = "Cosine Similarity: Control and Pre-Crisis", xlab = "Date",
        ylim = c(0,0.5))

barplot(cos.sim.crisis[-c(1,2)], ylab = "Cosine Similarity: Crisis and Pre-Crisis", xlab = "Date",
        ylim = c(0,0.5))

###########################################################################################
#                           Task 6: CORRESPONDENCE ANALYSIS                               #
###########################################################################################

# Split the pre-crisis corpus
pre_crisis1 <- paste(pre_crisis.corpus[1:3], collapse = " ")
pre_crisis2 <- paste(pre_crisis.corpus[4:6], collapse = " ")
pre_crisis3 <- paste(pre_crisis.corpus[7:9], collapse = " ")
pre_crisis.split <- rbind(pre_crisis1, pre_crisis2, pre_crisis3)

corres.corpus <- rbind(control.collapse, crisis.collapse, pre_crisis.split)
names(corres.corpus) <- c("control", "crisis", "pre_crisis1", "pre_crisis2", "pre_crisis3")

rm(pre_crisis1,pre_crisis2,pre_crisis3, pre_crisis.split )


corpus <- Corpus(VectorSource(corres.corpus))

dtm <- TermDocumentMatrix(
  corpus,
  control = list( removePunctuation = T,
                  stopwords = T,
                  removeNumbers = T,
                  wordLengths = c(3, 20),
                  bounds = list( global = c(2, 4))
  )
)

dtm.matrix <- as.matrix(dtm)

# Row profile shows how the terms are used in the text 
plot(ca(dtm.matrix), map = "rowprincipal",
     main = "rowprincipal map",
     what = c("all","none"),
     labels = c(0,0)
)

plot(ca(dtm.matrix), map = "rowprincipal",
     main = "rowprincipal map",
     what = c("all","all"),
     labels = c(2,2)
)

# Column profils shows how close the documents are to each other 
plot(ca(dtm.matrix), map = "colprincipal",
     main = "colprincipal map",
     what = c("none","all"),
     labels = c(2,2)
)

summary(ca(dtm.matrix))

# we make a 3d plot 
plot3d.ca(ca(dtm.matrix), map="colprincipal",
          what = c("none","all"),
          labels = c(0,2)
)


###########################################################################################
#                     Task 7: ADDITIONAL ANALYSIS - TOPIC MODEL                           #
###########################################################################################

# In this task, we use the topic model to look at the most common terms in the financial crisis. 
# We then used the topic distribution over document to see the weight of financial crisis as a topic 
# over time. We conpared the weights to see if there were significant changes of the topic weights over time.
# We do this to see if the financial crisis were discussed often in the pre-crisis period as compared to the control corpus. 

# structures the data. Splits into paragraphs and make it into a dataframe
FOMC.meeting <- gsub("\\s{7,}", "//", clean.transcripts.topic)
FOMC.meeting <- gsub("\\s+", " ", FOMC.meeting)
FOMC.meeting <- strsplit(FOMC.meeting, split = "//")
FOMC.meeting <- as.data.frame(t(rbind(FOMC.meeting)))
colnames(FOMC.meeting) <- "disclosure"
FOMC.meeting$date <- names(clean.transcripts)


# DTM for Topic model - Control 
corpus <- Corpus(VectorSource(FOMC.meeting$disclosure))

# document term matrix
dtm <- DocumentTermMatrix(corpus, control = list( removePunctuation = T,
                  stopwords = T,
                  removeNumbers = T,
                  wordLengths = c(3, 20),
                  bounds = list( global = c(1,27))))

# Generating topics
topic <- LDA(dtm,
             k = 10,
             method = "Gibbs",
             control = list(
               seed = 2000,
               burnin = 0, # how often sampled before estimation recorded
               thin = 200, # recording of samplings
               iter = 200, # number of iterations
               best = T, # take the best of the recorded ones
               keep = 1, # save some data for each estimation
               verbose = 10, # progress in console
               save = F
             ))

# setting up the term distribution 
term.distribution <- exp(topic@beta)

# Checking the most commom terms in each topic
# Looking at the term distribustion we think topic 10 closely matches financial crisis
terms.topics <- apply(term.distribution, 1, function(t.dist){
  paste(topic@terms[order(t.dist, decreasing = T)][1:10], collapse = ", ")
})

terms.topics

# Checking topic distribution per document 
topic.distribution <- topic@gamma
top <- as.data.frame(topic.distribution)
top$date <- names(clean.transcripts)

# A time series plot of the financial crisis topic 
plot(top$V10, type="l", main = "Time Series of Weight on Topic 10 Over Time", 
     xlab = "Period", ylab = "Weight", col="blue", xaxt="n")

axis(side=1, at=seq(1, 27, by=2))  


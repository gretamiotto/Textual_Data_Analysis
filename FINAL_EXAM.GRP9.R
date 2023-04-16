

setwd("C:/Users/toivo/OneDrive/Desktop/NHH_Courses/BAN432/Final Exam") 



library(rvest) 
library(data.table)
library(tidyverse)
library(tidymodels)
library(tidytext)
library(tm)
library(SentimentAnalysis)
library(tokenizers)
library(doParallel)
require(tibble)
library(gridExtra)
library(dplyr)
library(stargazer)



# Cleaning and Saving the Data

WSJdata.tk <- list.files("WSJdata") 
WSJ.tibble<-tibble()


# reads in the WSJ articles and takes the necessary information, the date, the headline, the text, and the file name


for (i in WSJdata.tk) {
  folder<-list.files(paste0("WSJdata/",i))
  for (j in folder) {
    temp<-html_table(read_html(paste0("WSJdata/",i,"/",j))) %>%
    rbindlist(idcol = T) %>%
    filter(X1 == "PD" | X1 == "LP" | X1 == "TD") %>%
    pivot_wider(names_from = "X1", values_from = "X2") %>%
      mutate(ticker=i)
    WSJ.tibble<-rbind(WSJ.tibble,temp)
  }}

#changing the format of the date, and creating a column containing the complete text

WSJ.tibble<- WSJ.tibble %>%
  transmute(date=as.Date(WSJ.tibble$PD, "%d %B %Y"), 
            text = paste(WSJ.tibble$LP,WSJ.tibble$TD),
            lp=WSJ.tibble$LP,
            ticker=ticker)

#reading in the returns csv file

returns<-read_csv("dailyReturn.csv") 

#merging the returns CSV file with the text tibble

WSJ.return <- left_join(returns, WSJ.tibble, by = c("date","ticker"))

#loading in the transcripts data

load("earningsCallsTranscripts.Rdata")

#merging the transcripts data frame and the text tibble

transcripts<-transcripts %>%
  rename(date="publishedOn")
WSJ.return<-left_join(WSJ.return, transcripts, by = c("date","permno"))

WSJ.return$call.merge<-paste(WSJ.return$intro,WSJ.return$qa)

#creating lagged return, volume variables, and article dummy in the data frame to be used in the analysis later

WSJ.return <- WSJ.return %>%
  arrange(ticker, date) %>% 
  mutate (article = ifelse(is.na(text),0,1),
          vol.scaled = (vol-min(vol))/(max(vol)-min(vol)), 
          call = ifelse(is.na(id),0,1)) %>%
  mutate(ret.lag1= ifelse(lag(ticker,n=1L)== .$ticker,lag(.$ret,n=1L),0),
         ret.lag2= ifelse(lag(ticker,n=2L)== .$ticker,lag(.$ret,n=2L),0),
         ret.lag3= ifelse(lag(ticker,n=3L)== .$ticker,lag(.$ret,n=3L),0),
         vol.lag1= ifelse(lag(ticker,n=1L)== .$ticker,lag(.$vol.scaled,n=1L),0),
         vol.lag2= ifelse(lag(ticker,n=2L)== .$ticker,lag(.$vol.scaled,n=2L),0),
         vol.lag3= ifelse(lag(ticker,n=3L)== .$ticker,lag(.$vol.scaled,n=3L),0),
         ret.lead1= ifelse(lead(ticker,n=1L)== .$ticker,lead(.$ret,n=1L),0),
         ret.lead2= ifelse(lead(ticker,n=2L)== .$ticker,lead(.$ret,n=2L),0),
         ret.lead3= ifelse(lead(ticker,n=3L)== .$ticker,lead(.$ret,n=3L),0)
         
         )

# saving the data into the working directory for a shortcut and to avoid re running this stuff
save(WSJ.return,file = "WSJ.return.Rdata")








load("WSJ.return.Rdata")







## Task 1 Exploratory Analysis


#conducting a logistic regression to determine which factors increase the propesity of an article being written

log.reg1<-glm(article~abs(ret)+abs(ret.lag1)+abs(ret.lag2)+abs(ret.lag3)+vol.scaled+vol.lag1+vol.lag2+vol.lag3, 
              family = "binomial", data=WSJ.return)

log.reg<-glm(article~abs(ret.lag1)+vol.lag1, family="binomial", data=WSJ.return)

summary(log.reg1)

#key finding is that what happens in the market the day before the article is the most significant in increasing the probability of an article being written.






#plotting returns with article frequencies 

WSJ.tk<-WSJ.return %>%
  split(.$ticker)

plot1<-WSJ.tk[[4]] %>% 
  mutate(dummy=ifelse(article==0,NA,article-1)) %>%
  ggplot(aes(x=date,y=ret))+
  geom_line(colour="#7180AC") + 
  geom_point(aes(y=dummy),alpha=0.2,colour="#EE964B") + theme_classic()

plot1




## Task 2 Constructing a Sentiment Dictionary


#This creates 4 vectors of firms; 2 random groups of 50% of the firms and 1 with the highest frequency of articles in group B and one with the lowest frequency of articles in group B

all.firms <- WSJ.return$ticker %>% unique() 


set.seed(6939)
firmsA <- sample(all.firms, (length(all.firms)+1)/2) 
firmsB <- setdiff(all.firms, firmsA) 

firmsBHigh<-WSJ.return %>%   
  filter(ticker %in% firmsB) %>%
  filter(text != is_null(text))%>% 
  group_by(ticker) %>%
  summarise(count=n()) %>%
  filter(count>quantile(.$count,0.5)) %>%
  .$ticker %>% as.vector()

firmsBLow<-WSJ.return %>%   
  filter(ticker %in% firmsB) %>%
  filter(text != is_null(text))%>% 
  group_by(ticker) %>%
  summarise(count=n()) %>%
  filter(count<quantile(.$count,0.5)) %>%
  .$ticker %>% as.vector()




# using only LP to create the sentiment dictionaries we create a binary dictionary classifying the words as either positive or negative and a weighted dictionary with higher rates to the most positive or most negative words and lower weight to more neutral words

#keeping not and n't as stopwords becuase they are important in sentiment analysis

stopword<-grep(pattern="not|n't", x=stopwords(), value = TRUE) %>%
  setdiff(stopwords("en"),.) 

# cleaning the text in the lp
WSJ.return$lp<- WSJ.return$lp%>%  
  tolower() %>% 
  gsub('\\n', " ",.) %>%
  gsub('\\r', " ",.) %>%
  removePunctuation() %>%
  removeWords(stopword) %>% 
  removeNumbers()

#cleaning the text

WSJ.return$text<- WSJ.return$text%>%  
  tolower() %>% 
  gsub('\\n', " ",.) %>%
  gsub('\\r', " ",.) %>%
  removePunctuation() %>%
  removeWords(stopword) %>% 
  removeNumbers()




# creates a data frame with all of the documents of firms in group A that have are following days of high returns 

pos.docs <- WSJ.return %>%  
    filter(ticker %in% firmsA) %>% 
    filter(lp != is_null(lp)) %>% 
    filter(ret.lag1> quantile(.$ret.lag1, 0.55))

#creating a frequency table of all of the words in positive documents that occur more than X times
pos.freq.table<-unnest_tokens(tbl=pos.docs, input=lp,output = words) %>% 
  .$words %>% 
  table() %>% 
  as.data.frame() %>%
  filter(Freq>50) %>% # potential parameter to change
  rename(terms=".")

# creates a data frame with all of the documents of firms in group A that have are following days of low returns 

neg.docs <- WSJ.return %>%   
    filter(ticker %in% firmsA) %>% 
    filter(lp != is_null(lp)) %>% 
    filter(ret.lag1< quantile(.$ret.lag1, 0.45))

#creating a frequency table of all of the words in negative documents that occur more than X times
    
neg.freq.table<-unnest_tokens(tbl=neg.docs, input=lp,output = words) %>% 
  .$words %>% 
  table() %>% 
  as.data.frame() %>%
  filter(Freq>50) %>%
  rename(terms=".") # potential parameter to change
  
#creates a table with all of the words in the dictionary with a binary classification and a weighted

ranking<-full_join(pos.freq.table,neg.freq.table,by="terms") %>%
    mutate(Freq.x=ifelse(is.na(Freq.x),0,Freq.x),
         Freq.y=ifelse(is.na(Freq.y),0,Freq.y)) %>%
    mutate(rank = Freq.x - Freq.y) %>%
    mutate(weighting = rank/max(abs(rank))) %>% 
    mutate(sentiment=ifelse(rank>0,"Positive","Negative"))

#creating a binary dictionary
  
dictionary.binary.lp<-SentimentDictionaryBinary(positiveWords = ranking %>% 
                                        filter(sentiment=="Positive") %>% 
                                        pull(terms) %>% as.vector(),
                                      negativeWords = ranking %>% 
                                        filter(sentiment=="Negative") %>% 
                                        pull(terms) %>%
                                        as.vector())
#creating the weighted dictionary

dictionary.weighted.lp<-SentimentDictionaryWeighted(as.vector(ranking$terms),ranking$weighting)





# creates a data frame with all of the documents of firms in group A that have are following days of high returns 

pos.docs.txt <- WSJ.return %>%  
    filter(ticker %in% firmsA) %>% 
    filter(text != is_null(text)) %>% 
    filter(ret.lag1> quantile(.$ret.lag1, 0.55)) #parameter to change

#creating a frequency table of all of the words in positive documents that occur more than X times
pos.freq.table.txt<-unnest_tokens(tbl=pos.docs.txt, input=text,output = words) %>% 
  .$words %>% 
  table() %>% 
  as.data.frame() %>%
  filter(Freq>50) %>%
  rename(terms=".")

# creates a data frame with all of the documents of firms in group A that have are following days of low returns 


neg.docs.txt <- WSJ.return %>%   
    filter(ticker %in% firmsA) %>% 
    filter(text != is_null(text)) %>% 
    filter(ret.lag1< quantile(.$ret.lag1, 0.45))

#creating a frequency table of all of the words in negative documents that occur more than X times
    
neg.freq.table.txt<-unnest_tokens(tbl=neg.docs.txt, input=text,output = words) %>% 
  .$words %>% 
  table() %>% 
  as.data.frame() %>%
  filter(Freq>50) %>%
  rename(terms=".")

#creates a table with all of the words in the dictionary with a binary classification and a weighted
  
ranking.txt<-full_join(pos.freq.table.txt,neg.freq.table.txt,by="terms") %>%
    mutate(Freq.x=ifelse(is.na(Freq.x),0,Freq.x),
         Freq.y=ifelse(is.na(Freq.y),0,Freq.y)) %>%
    mutate(rank = Freq.x - Freq.y) %>%
    mutate(weighting = rank/max(abs(rank))) %>% 
    mutate(sentiment=ifelse(rank>0,"Positive","Negative"))

#creating a weighted dictionary with the full text

dictionary.weighted.txt<-SentimentDictionaryWeighted(as.vector(ranking.txt$terms),ranking.txt$weighting)




## Task 3 Internal Validity

#creating a function that will give a sentiment score with each of the dictionaries created 

sentiment.prediction<-function(firm.vector,text) {
  df<-WSJ.return %>%
                 filter(ticker %in% firm.vector) %>%
                 filter(text != is_null(text))
  
  corpus <- Corpus(VectorSource(df$text))
  
  dtm <- DocumentTermMatrix(corpus, 
                          control = list(
                            wordLengths = c(4,20),
                            global=c(10,3000)
                            ))
  df$bin.dic.sentiment <- ruleSentimentPolarity(dtm,dictionary.binary.lp)
  weighted.lp.prediction<-ruleLinearModel(dtm,dictionary.weighted.lp)
  weighted.txt.prediction<-ruleLinearModel(dtm,dictionary.weighted.txt)
  
  df<-cbind(df,weighted.lp.prediction,weighted.txt.prediction)
  list<-list(dtm,df)
  return(list) }







# creating 4 data frames with each subset of firms and their sentiment score

A<-sentiment.prediction(firmsA,text)
B<-sentiment.prediction(firmsB,text)
B.High<-sentiment.prediction(firmsBHigh,text)
B.Low<-sentiment.prediction(firmsBLow,text)








# functions created for plotting and doing linear regression
plot.lm.bin.lp<-function(df) {
  lm_plot<-list()
  lm<-lm(bin.dic.sentiment~ret.lag1, data=df[[2]])
  plot<-df[[2]] %>%
  ggplot(aes(ret.lag1, bin.dic.sentiment)) +
  geom_point(colour="#7180AC") + 
  geom_smooth(method="lm", colour="#EE964B") + 
  ylab("Sentiment Score") +
  xlab("1 Day Lag Return") +
  ggtitle(paste("Returns and Sentiment", deparse(substitute(df)))) +
    theme_classic()+
    theme(plot.title=element_text(size=12))

  lm_plot<-list(lm,plot)
}

plot.lm.weight.lp<-function(df) {
  lm_plot<-list()
  lm<-lm(weighted.lp.prediction~ret.lag1, data=df[[2]])
  plot<-df[[2]] %>%
  ggplot(aes(ret.lag1, weighted.lp.prediction)) +
  geom_point(colour="#7180AC") + 
  geom_smooth(method="lm", colour="#EE964B") + 
  ylab("Sentiment Score") +
  xlab("1 Day Lag Return") +
  ggtitle(paste("Returns and Sentiment", deparse(substitute(df)))) +
    theme_classic()+
    theme(plot.title=element_text(size=12))

  lm_plot<-list(lm,plot)
}

plot.lm<-function(df) {
  lm_plot<-list()
  lm<-lm(weighted.txt.prediction~ret.lag1, data=df[[2]])
  plot<-df[[2]] %>%
  ggplot(aes(ret.lag1, weighted.txt.prediction)) +
  geom_point(colour="#7180AC") + 
  geom_smooth(method="lm", colour="#EE964B") + 
  ylab("Sentiment Score") +
  xlab("1 Day Lag Return") +
  ggtitle(paste("Returns and Sentiment", deparse(substitute(df)))) +
    theme_classic()+
    theme(plot.title=element_text(size=12))
  lm_plot<-list(lm,plot)
}




#doing a regression with one day lag returns and the sentiment scores for the binary lp dictionary

lm.WSJA.lp.bin<-plot.lm.bin.lp(A)[[1]]
lm.WSJB.lp.bin<-plot.lm.bin.lp(B)[[1]]
lm.WSJBhigh.lp.bin<-plot.lm.bin.lp(B.High)[[1]]
lm.WSJBlow.lp.bin<-plot.lm.bin.lp(B.Low)[[1]]



#plotting the sentiment scores for a lp binary dictionary 

plot.WSJA.lp.bin<-plot.lm.bin.lp(A)[[2]]
plot.WSJB.lp.bin<-plot.lm.bin.lp(B)[[2]]
plot.WSJBhigh.lp.bin<-plot.lm.bin.lp(B.High)[[2]]
plot.WSJBlow.lp.bin<-plot.lm.bin.lp(B.Low)[[2]]

grid.arrange(plot.WSJA.lp.bin, plot.WSJB.lp.bin, plot.WSJBhigh.lp.bin, plot.WSJBlow.lp.bin, nrow=2)






#doing a regression with one day lag returns and the sentiment scores for the binary lp dictionary

lm.WSJA.weight.lp<-plot.lm.weight.lp(A)[[1]]
lm.WSJB.weight.lp<-plot.lm.weight.lp(B)[[1]]
lm.WSJBhigh..weight.lp<-plot.lm.weight.lp(B.High)[[1]]
lm.WSJBlow..weight.lp<-plot.lm.weight.lp(B.Low)[[1]]

#plotting the sentiment scores for a lp weighted dictionary 


plot.WSJA.weight.lp<-plot.lm.weight.lp(A)[[2]]
plot.WSJB.weight.lp<-plot.lm.weight.lp(B)[[2]]
plot.WSJBhigh.weight.lp<-plot.lm.weight.lp(B.High)[[2]]
plot.WSJBlow.weight.lp<-plot.lm.weight.lp(B.Low)[[2]]

grid.arrange(plot.WSJA.weight.lp, plot.WSJB.weight.lp, plot.WSJBhigh.weight.lp, plot.WSJBlow.weight.lp, nrow=2)





#doing a regression with one day lag returns and the sentiment scores for the weighted full text dictionary

lm.WSJA<-plot.lm(A)[[1]]
lm.WSJB<-plot.lm(B)[[1]]
lm.WSJBhigh<-plot.lm(B.High)[[1]]
lm.WSJBlow<-plot.lm(B.Low)[[1]]

#plotting the sentiment scores for a  weighted text dictionary 



plot.WSJA<-plot.lm(A)[[2]]
plot.WSJB<-plot.lm(B)[[2]]
plot.WSJBhigh<-plot.lm(B.High)[[2]]
plot.WSJBlow<-plot.lm(B.Low)[[2]]

grid.arrange(plot.WSJA, plot.WSJB, plot.WSJBhigh, plot.WSJBlow, nrow=2)






# creating a table that compares the sentment score to the response

sentimentA <- matrix(A[[2]]$weighted.txt.prediction, 
                    dimnames=list(A[[2]]$lp, c("Sentiment")))
A.comp.resp<-as.data.frame(compareToResponse(sentimentA, A[[2]]$ret.lag1))

sentimentB <- matrix(B[[2]]$weighted.txt.prediction, 
                    dimnames=list(B[[2]]$lp, c("Sentiment")))
B.comp.resp<-as.data.frame(compareToResponse(sentimentB, B[[2]]$ret.lag1))

sentimentBhigh <- matrix(B.High[[2]]$weighted.txt.prediction, 
                    dimnames=list(B.High[[2]]$lp, c("Sentiment")))
Bhigh.comp.resp<-as.data.frame(compareToResponse(sentimentBhigh, B.High[[2]]$ret.lag1))

sentimentBlow <- matrix(B.Low[[2]]$weighted.txt.prediction, 
                    dimnames=list(B.Low[[2]]$lp, c("Sentiment")))
Blow.comp.resp<-as.data.frame(compareToResponse(sentimentBlow, B.Low[[2]]$ret.lag1))

ComptoResponse<-(cbind(A.comp.resp,B.comp.resp$Sentiment,Bhigh.comp.resp$Sentiment,Blow.comp.resp$Sentiment)) %>%
  rename(A="Sentiment", B='B.comp.resp$Sentiment', Bhigh="Bhigh.comp.resp$Sentiment", Blow="Blow.comp.resp$Sentiment")

View(ComptoResponse)



Task 4 External Validity

#cleaning the transcript text

WSJ.return$call.merge<- WSJ.return$call.merge%>%  # cleaning the text in the article heading 
  tolower() %>% 
  gsub('\\n', " ",.) %>%
  gsub('\\r', " ",.) %>%
  removePunctuation() %>%
  removeWords(stopword) %>% 
  removeNumbers()

#function that calculates the sentiment score of each earnings call

sentiment.prediction.transcript<-function(firm.vector) {
  df<-WSJ.return %>%
                 filter(ticker %in% firm.vector) %>%
                 filter(id != is_null(id))
  
  corpus <- Corpus(VectorSource(df$call.merge))
  
  dtm <- DocumentTermMatrix(corpus, 
                          control = list(
                            wordLengths = c(4,20),
                            global=c(10,3000)
                            ))
  df$bin.dic.sentiment <- ruleSentimentPolarity(dtm,dictionary.binary.lp)
  weighted.lp.prediction<-ruleLinearModel(dtm,dictionary.weighted.lp)
  weighted.txt.prediction<-ruleLinearModel(dtm,dictionary.weighted.txt)
  
  df<-cbind(df,weighted.lp.prediction,weighted.txt.prediction)
  list<-list(dtm,df)
  return(list) }

#creating data frames with each subset of firms

A.Transcript<-sentiment.prediction.transcript(firmsA)
B.Transcript<-sentiment.prediction.transcript(firmsB)
B.High.Transcript<-sentiment.prediction.transcript(firmsBHigh)
B.Low.Transcript<-sentiment.prediction.transcript(firmsBLow)





#creating a plot function for transcripts

plot.lmt<-function(df) {
  lm_plot<-list()
  lm<-lm(weighted.txt.prediction~ret.lead1, data=df[[2]])
  plot<-df[[2]] %>%
  ggplot(aes(ret.lead1, weighted.txt.prediction)) +
  geom_point(colour="#7180AC") + 
  geom_smooth(method="lm", colour="#EE964B") + 
  ylab("Sentiment Score") +
  xlab("1 Day lead Return") +
  ggtitle(paste("Returns and Sentiment", deparse(substitute(df)))) +
    theme_classic()+
    theme(plot.title=element_text(size=12))
  lm_plot<-list(lm,plot)
}

#linear regress of sentment with one day lead returns

lm.WSJA.transcript<-plot.lmt(A.Transcript)[[1]]
lm.WSJB.transcript<-plot.lmt(B.Transcript)[[1]]
lm.WSJBhigh.transcript<-plot.lmt(B.High.Transcript)[[1]]
lm.WSJBlow.transcript<-plot.lmt(B.Low)[[1]]

#plotting the results

plot.WSJA.transcript<-plot.lmt(A.Transcript)[[2]]
plot.WSJB.transcript<-plot.lmt(B.Transcript)[[2]]
plot.WSJBhigh.transcript<-plot.lmt(B.High.Transcript)[[2]]
plot.WSJBlow.transcript<-plot.lmt(B.Low.Transcript)[[2]]

grid.arrange(plot.WSJA.transcript, plot.WSJB.transcript, plot.WSJBhigh.transcript, plot.WSJBlow.transcript, nrow=2)






#comparing sentiment score to response.

sentimentA.Transcript <- matrix(A.Transcript[[2]]$weighted.txt.prediction, 
                    dimnames=list(A.Transcript[[2]]$lp, c("Sentiment")))
A.comp.resp.Transcript<-as.data.frame(compareToResponse(sentimentA.Transcript, A.Transcript[[2]]$ret.lead1))

sentimentB.Transcript <- matrix(B.Transcript[[2]]$weighted.txt.prediction, 
                    dimnames=list(B.Transcript[[2]]$lp, c("Sentiment")))
B.comp.resp.Transcript<-as.data.frame(compareToResponse(sentimentB.Transcript, B.Transcript[[2]]$ret.lead1))

sentimentBhigh.Transcript <- matrix(B.High.Transcript[[2]]$weighted.txt.prediction, 
                    dimnames=list(B.High.Transcript[[2]]$lp, c("Sentiment")))
Bhigh.comp.resp.Transcript<-as.data.frame(compareToResponse(sentimentBhigh.Transcript, B.High.Transcript[[2]]$ret.lead1))

sentimentBlow.Transcript <- matrix(B.Low.Transcript[[2]]$weighted.txt.prediction, 
                    dimnames=list(B.Low.Transcript[[2]]$lp, c("Sentiment")))
Blow.comp.resp.Transcript<-as.data.frame(compareToResponse(sentimentBlow.Transcript, B.Low.Transcript[[2]]$ret.lead1))

ComptoResponse<-(cbind(A.comp.resp.Transcript,B.comp.resp.Transcript$Sentiment,Bhigh.comp.resp.Transcript$Sentiment,Blow.comp.resp.Transcript$Sentiment)) %>%
  rename(A="Sentiment", B='B.comp.resp.Transcript$Sentiment', Bhigh="Bhigh.comp.resp.Transcript$Sentiment", Blow="Blow.comp.resp.Transcript$Sentiment")

View(ComptoResponse)




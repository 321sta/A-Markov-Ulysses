#Practical 1: A Markov Ulysses
#basic model: pth-order Markov model 
#advantage1: search for in common words(m) instead of all words(n)
#advantage2: not m**p array if it's p-nd order, but (n-p)*(p+1) array
#advantage3: store tokens (indices in m) instead of actual words



#3 prepare for data
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73,
          fileEncoding="UTF-8")
a <- gsub("_(","",a,fixed=TRUE)                   ## remove "_("

#4 treat punctuation just like words
split_punct <- function(text1,punc) {     
  pp<-grep(punc,text1,fixed=TRUE)                 ##get index of the word with punctuation
  if (length(pp)==0){                             ##judge whether the index vector is null
    return(text1)
  }
  text2<-character(length(text1)+length(pp))      ##create a new vector to hold the new text
  pps<-pp+1:length(pp)                            ##compute the index number of the place which punctuation should be at
  text2[pps]<-punc                                ##put the punctuation to the new place 
  dd<-gsub(punc,'',text1,fixed=TRUE)              ##replace the original punctuation with ""
  text2[-pps]<-dd                                 ##Fill the remainder of the new text vector
  return(text2)
}
t<-c('i','am','human.','am','i?')                 ##an example to test
w<-split_punct(t,'?')


#5 treated text
s<-a                                              ##use loop and the split_punct to separate "," and "." and ";" and "!" and ":" and "?" 
for (i in c(',','.',';','!',':','?')){
  s<-split_punct(s,i)
}
text<-s


#6 acquire common-word set
ttext<-tolower(text)                              ##Convert all uppercase letters to lowercase 
# (a)
uu <-unique(ttext)                                ##create a unique vector where all words appear only once
# (b)
ii<-match (ttext,uu)                              ##create an vector to hold the index of original words matched to the unique vector
# (c)
tt<-tabulate(ii)                                  ##Count the number of times each index occurs
dd<-data.frame(word=uu,count=tt)                  ##create a dataframe to hold the word and its occurrence times
# (d)
dd_sort<-sort(dd$count,decreasing = TRUE)         ##Rank the dataframe in descending order of word occurrence
threshold<-dd_sort[1000]                          ##set the 1000th highest word's occurrence as the threshold
m<- length(which(dd$count>=threshold))            ##There are 1005 words in total whose number of occurrence are higher than threshold,so the length of M is set as 1005
# (e)
common_words_indices<-which(dd$count>=threshold)  ##create a vector to hold the index of most common words matched to the unique vector
b <- dd$word[common_words_indices]                ##output the most common words





## !!!!!! Note that some functions in questions 7-9 require 
## !!!!!! Running the function from question 10 first




#7 Prepare matrix for model
#(a)
new_b<-capitalizing(text,b)                       ##Capitalize the first letter of each character.
iii<-match(text,new_b)                            ##Find the index of elements in 'text' within 'new_b'
#(b) 
mlag <- 4                                         #start from 4
n <- length(text)

##Matrix M of common word token sequences
M<- matrix(NA,nrow=n-mlag,ncol=mlag+1)
for (i in 1:(mlag+1)){
  M[,i]<-iii[i:(n-mlag-1+i)] 
}


#8 Simulation by small language model
nw <-50
model<-function(mlag,b,nw,M){
  set.seed(1)
  first_word <- sample(b, 1)                                         ##Generate the first token
  firstword_index<-match(first_word,b)                               ##The first word's index in 'b'
  ss1<-array(NA,nw)                                                  ##Creat an empty array
  ss1[1]<-firstword_index
  for (i in 2:nw) { 
    for (j in mlag:1) if (i>j) {                                     ##Skip lags too long for current i
      context<-ss1[(i-j):(i-1)]
      if (j==1){                                                     ##When j = 1, match the first word of each row
        rows<- which(M[,j]==context)
      }
      else{                                                          ##Otherwise, match the j words of each row
        rows <- apply(M[, 1:j], 1, function(x) all(x == context))
      }
      if (length(rows)!=0){                                          ##If the number of matching rows is not zero, select a non-NA from them as the predicted value
        all_tokens <- M[rows, j + 1]
        treated_tokens <- all_tokens[!is.na(all_tokens)]             ##Select elements that are not NA
        if (length(treated_tokens) > 0) {
          ss1[i]<- sample(treated_tokens, 1)
          break  
        }
        else {
          ss1[i]<- sample(1:length(b), 1)                            ##If there are only NAs, randomly select a word
        }
      }
      else{
        ss1[i]<- sample(1:length(b), 1)                              ##If the number of matching rows is zero, randomly select a word
      }
  }
  }
  return(ss1)
}

section1 <- paste(b[model(mlag, new_b, nw, M)], collapse = ' ')      ##Simulation by small language model
formatted_section1 <- format_sentence(section1)                      ##Delete space before punctuation
cat(formatted_section1)                                              ##Print out the corresponding text 


#9 control group simulation
i2<-match (text,new_b)                                               ##create a vector to hold the index of modified b matched to the punct_split text
count2<-tabulate(i2)                                                 ##Count the number of occurrences of modified most common words
freq2<-count2/sum(count2)                                            ##calculate the occurrence frequency of modified most common words
section2 <- paste(sample(b,size=50,replace=TRUE,prob=freq2), collapse = ' ')  ##Select most common words by frequency
formatted_section2 <- format_sentence(section2)                      ##use the function written in question 10 to remove " " before punctuation
cat(formatted_section2)                                              ##output the text


#10 capitalize and format for punctuation
capitalizing <- function(a, b) {
  new_b <- character(length(b))                                      ##Creat an new empty character vector with same length as b
  for (i in seq_along(b)) {
    word <- b[i]
    lower_word <- tolower(word)                                      ##Convert the word to lowercase.
    ##Count the number of times a word in vector a starts with an uppercase
    upper_count <- sum(grepl(paste0("^", toupper(substring(lower_word, 1, 1)), substring(lower_word, 2)), a))
    ##Count the number of times a word in vector a starts with the lowercase
    lower_count <- sum(grepl(paste0("^", lower_word), a))
    
    #compare upper_count and lower_count
    if (upper_count > lower_count) {
          ##If upper_count > lower_count, the first letter of the word will be capitalized
      new_b[i] <- paste0(toupper(substring(lower_word, 1, 1)), substring(lower_word, 2))
    } else {
      new_b[i] <- lower_word##Otherwise, the first letter of the word will be lowercase
    }
  }
  return(new_b)  
}
format_sentence <- function(sentence) {                              ##no space before punctuation
  sentence <- gsub(" +([,.:;!?])", "\\1", sentence)
  return(sentence)
}


# s2626102 Jingwen Jiang  40%      Write the main content of the code.
# s2646482 Chengpeng Dai  30%      Discuss, modify, and refine the code. Write comments.
# s2752993 Ziyi Ye        30%      Discuss, modify, and refine the code. Write comments.


#Practical 1: A Markov Ulysses
#basic model: pth-order Markov model 
#advantage1: search for in common words(m) instead of all words(n)
#advantage2: not m**p array if it's p-nd order, but (n-p)*(p+1) array
#advantage3: store tokens (indices in m) instead of actual words

setwd("/Users/jingwenjiang/Documents/Practical-1-A-Markov-Ulysses") 

# 3 prepare for data
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73,
          fileEncoding="UTF-8")
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("


# 4 treat punctuation just like words
split_punct <- function(text1,punc) {
  pp<-grep(punc,text1,fixed=TRUE)
  if (length(pp)==0){
    return(text1)
  }
  text2<-character(length(text1)+length(pp))
  pps<-pp+1:length(pp)
  text2[pps]<-punc
  dd<-gsub(punc,'',text1,fixed=TRUE)
  text2[-pps]<-dd
  return(text2)
}
t<-c('i','am','human.','am','i?') ##example
w<-split_punct(t,'?')


# 5 treated text
s<-a
for (i in c(',','.',';','!',':','?')){
  s<-split_punct(s,i)
}
text<-s


# 6 acquire common-word set
ttext<-tolower(text)
# (a)
uu <-unique(ttext) #32794
# (b)
ii<-match (ttext,uu) #309129
# (c)
tt<-tabulate(ii)
dd<-data.frame(word=uu,count=tt)
# (d)
dd_sort<-sort(dd$count,decreasing = TRUE)
threshold<-dd_sort[1000]
m<- length(which(dd$count>=threshold)) #1005
# (e)
common_words_indices<-which(dd$count>=threshold)
b <- dd$word[common_words_indices]



# 7 create small language model 
#(a)
iii<-match(ttext,b)
#(b) 
mlag <- 4 #start from 4
n <- length(ttext)
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)
for (i in 1:(mlag+1)){
  M[,i]<-iii[i:(n-mlag-1+i)]
}

  
# 8 simulation by small language model
nw <-50
first_word <- sample(b, 1)
for (i in 2:nw) {
  for (j in mlag:1) if (i>j) { 
    ss<-
    
  }
}
section1<-cat(,sep=' ')



# 9 control group simulation
i2<-match (ttext,b) 
count2<-tabulate(i2)
freq2<-count2/sum(count2)
sections2<-cat(sample(b,size=50,replace=TRUE,prob=freq2),sep=' ')





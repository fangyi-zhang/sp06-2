##3
setwd("C:/Users/86132/Desktop/SP")  
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

##4
split_punct <- function(book,string){
  m <- grep(string, book, fixed=TRUE)
  book[m] <- paste(book[m], " %", sep="")
  n <- paste(book, collapse=" ")
  p <- strsplit(n," ")[[1]]
  t <- gsub(string, "", p, ignore.case = FALSE, perl = FALSE,
            fixed = TRUE, useBytes = FALSE)
  t1 <- gsub("%", string, t, ignore.case = FALSE, perl = FALSE,
             fixed = TRUE, useBytes = FALSE)
  return (t1)
}

##5
del_1 <- split_punct(a, ',')
del_2 <- split_punct(del_1, '.')
del_3 <- split_punct(del_2, ';')
del_4 <- split_punct(del_3, '!')
del_5 <- split_punct(del_4, ':')
del_6 <- split_punct(del_5, "?")

del_6

##6
library(mgcv)
text <- tolower(del_6)
k <- unique(text, ordered = FALSE)
k1 <- match(text, k)
k2 <- tabulate(k1)

x <- 0
for (i in 1:length(k2)) {
  if (k2[i] > 89)
    x = x + 1
}

(x - 1000) < 5 #true

g <- numeric(0)
x <- 0
for (i in 1:length(k2)) {
  if (k2[i] >= 90) {
    x <- x + 1
    g[x] <- i
  } else
  {
    x <- x + 0
  }
  
}

b <- numeric(0)
for (i in 1:length(g)) {
  b[i] <- k[g[i]]
}
b

##7
q <- match(text, b)   # Match the word in b with the original text. If the word in the original text is not in b, it is NA
q1 <- cbind(q[1:length(q)-1], q[2:length(q)])   # q is mismatched to obtain a two column matrix, and each row represents a word pair
q2 <- rowSums(is.na(q1))   # Calculate the phrase containing Na, Na = 1 (the word pair does not appear)

l <- numeric(0)   # Define the index vector l to record the index of word pairs without NA
x <- 0
for (i in 1:length(q2)) {
  # Loop record the index of word pairs without NA and assign it to l
  if (q2[i] == 0) { 
    x <- x + 1
    l[x] <- i
  } else
  {
    x <- x + 0
  }
  
}

q11 <- matrix(nrow = length(l), ncol = 2)   # Define matrix q11
for (i in 1:length(l)) {
  # Loop record possible word pairs and assign them to q11
  q11[i, ] <- q1[l[i], ]
}

A0 <- matrix(0, length(b), length(b))   # Define matrix A
for (i in 1:length(q11) / 2) {
  # Loop and count the number of occurrences of each word pair
  A0[q11[i, 1], q11[i, 2]] <- A0[q11[i, 1], q11[i, 2]] + 1
}


A <- matrix(0, length(b), length(b))   # Define matrix A storage conditional probability
for (i in 1:length(b)) {
  # Loop and count the conditional probability
  for (j in 1:length(b)) {
    A[i, j] <- A0[i, j] / sum(A0[i, ])
  }
}
A   # The conditional probability matrix A is obtained
sum(A[1,])==1   # Judge whether the sum of the probabilities of each line is 1

##8
index <- numeric(0)   # Define index vector named index
index[1] <- sample(1:length(b), 1)   # A random index is taken as the sequence number of the first extracted word
for (i in 2:50) {
  # Loop out the current word according to the probability of taking out the previous word
  index[i] <- sample(1:length(b), 1, prob = A[index[i - 1], ])
}
index   # Get the index of 50 words

s <- numeric(0)   # Initializes a vector that records 50 words
for (i in 1:50) {
  # Loop retrieve the words according to the index
  s[i] <- b[index[i]]   
}
cat(s)   # Print 50 words


##9
m_total<-match(text,b) 
n_total<-tabulate(m_total)
m_lower<-match(del_6,b) 
n_lower<-tabulate(m_lower) 

P<-n_lower/n_total   

ggg<-numeric(0)
x=0
for (i in 1:length(P)) {
  if (P[i]<0.5){
    x=x+1;ggg[x]<-i}else 
    {x=x+0}
}

#install.packages("Hmisc")
library(Hmisc)
B<-b
for (i in 1:length(ggg)) {
  B[ggg[i]]<-capitalize(b[ggg[i]])
} 

s_new<-numeric(0)
for (i in 1:50) {
  s_new[i]<-B[index[i]]
}
cat(s_new)

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

##7
q <- match(text, b)
q1 <- cbind(q[1:1305893], q[2:1305894])
q2 <- rowSums(is.na(q1))

l <- numeric(0)
x <- 0
for (i in 1:length(q2)) {
  if (q2[i] == 0) {
    x <- x + 1
    l[x] <- i
  } else
  {
    x <- x + 0
  }
  
}

q11 <- matrix(nrow = 964810, ncol = 2)
for (i in 1:length(l)) {
  q11[i, ] <- q1[l[i], ]
}

A0 <- matrix(0, length(b), length(b))
for (i in 1:length(q11) / 2) {
  A0[q11[i, 1], q11[i, 2]] <- A0[q11[i, 1], q11[i, 2]] + 1
}


A <- matrix(0, length(b), length(b))
for (i in 1:length(b)) {
  for (j in 1:length(b)) {
    A[i, j] <- A0[i, j] / sum(A0[i, ])
  }
}


##8
index <- numeric(0)
index[1] <- sample(1:length(b), 1)
for (i in 2:50) {
  index[i] <- sample(1:length(b), 1, prob = A[index[i - 1], ])
}
index

s <- numeric(0)
for (i in 1:50) {
  s[i] <- b[index[i]]
}
cat(s)


##9
m_total<-match(text,b) 
n_total<-tabulate(w0)
m_lower<-match(del_6,b) 
n_lower<-tabulate(w1) 

P<-n_lower/n_total   

ggg<-numeric(0)
x=0
for (i in 1:length(P)) {
  if (P[i]<0.5){
    x=x+1;ggg[x]<-i}else 
    {x=x+0}
}

install.packages("Hmisc")
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

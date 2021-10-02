setwd("C:/Users/86132/Desktop/SP100%")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

#remove ","……
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


del_1<-split_punct(a,',')
del_2<-split_punct(del_1,'.')
del_3<-split_punct(del_2,';')
del_4<-split_punct(del_3,'!')
del_5<-split_punct(del_4,':')
del_6<-split_punct(del_5,"?")

library(mgcv)
text <- tolower(del_6)
k <- unique(text, ordered = FALSE)
k1 <- match(text,k)
k2 <- tabulate(k1)

x=0
for (i in 1:length(k2)) {
  if (k2[i]>89) 
    x=x+1
}


(x-1000)<5

g<-numeric(0)
x=0
for (i in 1:length(k2)) {
  if (k2[i]>=90){
    x=x+1;g[x]<-i}else 
    {x=x+0}
  
}

b<-numeric(0)
for (i in 1:length(g)) {
  b[i]<-k[g[i]]
} 

##7
q<-match(text,b)
q



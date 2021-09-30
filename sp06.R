setwd("C:/Users/86132/Desktop/SP100%")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

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
k <- uniquecombs(text, ordered = FALSE)
k1<-attr(k, "index")


# X <- matrix(c(1,2,3,1,2,3,4,5,6,1,3,2,4,5,6,1,1,1),6,3,byrow=TRUE)
# print(X)
# Xu <- uniquecombs(X);Xu
# ind <- attr(Xu,"index")
# ## find the value for row 3 of the original from Xu
# Xu[ind[3],];X[3,]

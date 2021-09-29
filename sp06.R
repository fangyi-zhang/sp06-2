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


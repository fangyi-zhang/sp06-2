setwd("/Users/fyz/Desktop/statistical programming")
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

##6
b<-tolower(del_6)
c<-unique(b)
d<-match(b,c)
h<-tabulate(d)

x=0
for (i in 1:length(h)) {
  if (h[i]>=90) 
    x=x+1
}
x

g<-numeric(0)
x=0
for (i in 1:length(h)) {
  if (h[i]>=90){
    x=x+1;g[x]<-i}else 
      {x=x+0}
  
}

l<-numeric(0)
for (i in 1:length(g)) {
  l[i]<-c[g[i]]
} 


##7





setwd("C:/Users/86132/Desktop/SP100%")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

m<-grep(",",a,fixed=TRUE)
a[m]<-paste(a[m]," A",sep="")
k<-paste(a,collapse=" ")
pow1 <- strsplit(k," ")[[1]]
t<-gsub(",", "", pow1, ignore.case = FALSE, perl = FALSE,
        fixed = TRUE, useBytes = FALSE)
t1<-gsub("A", ",", t, ignore.case = FALSE, perl = FALSE,
         fixed = TRUE, useBytes = FALSE)

m<-grep(".",t1,fixed=TRUE)
t1[m]<-paste(t1[m]," A",sep="")
k<-paste(t1,collapse=" ")
pow1 <- strsplit(k," ")[[1]]
t<-gsub(".", "", pow1, ignore.case = FALSE, perl = FALSE,
        fixed = TRUE, useBytes = FALSE)
t2<-gsub("A", ".", t, ignore.case = FALSE, perl = FALSE,
         fixed = TRUE, useBytes = FALSE)


m<-grep(":",t2,fixed=TRUE)

t2[m]<-paste(t2[m]," A",sep="")
k<-paste(t2,collapse=" ")
a1 <- strsplit(k," ")[[1]]
t<-gsub(":", "", a1, ignore.case = FALSE, perl = FALSE,
        fixed = TRUE, useBytes = FALSE)
t3<-gsub("A", ":", t, ignore.case = FALSE, perl = FALSE,
         fixed = TRUE, useBytes = FALSE)
t3

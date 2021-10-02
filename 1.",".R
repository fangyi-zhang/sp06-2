setwd("/Users/fyz/Desktop/statistical programming")
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

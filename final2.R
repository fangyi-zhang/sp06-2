##3
setwd("C:/Users/86132/Desktop/SP")   ## Set file path
a <- scan("1581-0.txt",what="character",skip=156)   ## read file
n <- length(a)   ## Find the number of words
a <- a[-((n-2909):n)] ## strip license

##4
split_punct <- function(book,string){   
  ## Search for each word containing the punctuation mark, remove it from the word, and add the mark as a new entry in the vector of words, after the word it came from 
  l <- grep(string, book, fixed=TRUE)   ## Use "grep" function to find the index containing the "string" punctuation mark in the "book"
  book[l] <- paste(book[l], " %", sep="")   ## Use "paste" function to add “ %” after each found "string" 
  n <- paste(book, collapse=" ")   ## Put the words in “book” into a single string with “ ” between two words
  p <- strsplit(n," ")[[1]]   ## Use "strsplit" to split n into individual words by “ ”
  t <- gsub(string, "", p, ignore.case = FALSE, perl = FALSE,
            fixed = TRUE, useBytes = FALSE)   ## Find “string” in p and replace with “”
  t1 <- gsub("%", string, t, ignore.case = FALSE, perl = FALSE,
             fixed = TRUE, useBytes = FALSE)   ## Find “%” in t and replace with “string”
  return (t1)   ## return the output
}


##5
## From words they are attached to in the bible text
## Use split_punct function to separate the punctuation marks, ",", ".", ";", "!", ":" and "?"
del_1 <- split_punct(a, ',')   
del_2 <- split_punct(del_1, '.')   
del_3 <- split_punct(del_2, ';')   
del_4 <- split_punct(del_3, '!')   
del_5 <- split_punct(del_4, ':')   
del_6 <- split_punct(del_5, "?")

del_6   ## The final text after separating symbols to make them independent

##6
library(mgcv)
text <- tolower(del_6)   ## Use "tolower" to replace the capital letters in words with lower case letters
k <- unique(text, ordered = FALSE)   ## Use "unique" to find the vector of unique words
k1 <- match(text, k)  ## Use "match" to find the vector of indicies indicating which element in the unique word vector eachelement in the (lower case) bible text corresponds to
k2 <- tabulate(k1)  ## Use "tabulate" to count up how many times each unique word occurs in the text

m <- 0
for (i in 1:length(k2)) {
  ## Decide 90 as the threshold
  ## Count the number of words that appear more than or equal to 90 
  if (k2[i] >=90)
    m = m + 1
}
m  ## Find that there are 1004 words with the number of occurrences greater than or equal to 90

g <- numeric(0)
x <- 0
for (i in 1:length(k2)) {
  ## Find the index of the most frequent 1004 words in the separated symbol text
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
  ## Create vector b, put the most frequent 1004 words into it through index
  b[i] <- k[g[i]]
}
b  ## Get the most frequent 1004 words in bible text

##7
q <- match(text, b)   ## Match the word in b with the original text. If the word in the original text is not in b, it is NA
q1 <- cbind(q[1:length(q)-1], q[2:length(q)])   ## q is mismatched to obtain a two column matrix, and each row represents a word pair
q2 <- rowSums(is.na(q1))   ## Use rowsums to calculate the phrase containing Na, Na = 1 (the word pair does not appear)

l <- numeric(0)   ## Define the index vector l to record the index of word pairs without NA
x <- 0
for (i in 1:length(q2)) {
  ## Loop record the index of word pairs without NA and assign it to l
  if (q2[i] == 0) { 
    x <- x + 1
    l[x] <- i
  } else
  {
    x <- x + 0
  }
  
}

q11 <- matrix(nrow = length(l), ncol = 2)   ## Define matrix q11
for (i in 1:length(l)) {
  ## Loop record possible word pairs and assign them to q11
  q11[i, ] <- q1[l[i], ]
}

A0 <- matrix(0, length(b), length(b))   ## Define matrix A
for (i in 1:length(q11) / 2) {
  ## Loop and count the number of occurrences of each word pair
  A0[q11[i, 1], q11[i, 2]] <- A0[q11[i, 1], q11[i, 2]] + 1
}


A <- matrix(0, length(b), length(b))   ## Define matrix A storage conditional probability
for (i in 1:length(b)) {
  ## Loop and count the conditional probability
  for (j in 1:length(b)) {
    A[i, j] <- A0[i, j] / sum(A0[i, ])
  }
}
A   ## The conditional probability matrix A is obtained
sum(A[1,])==1   ## Judge whether the sum of the probabilities of each line is 1

##8
index <- numeric(0)   ## Define index vector named index
index[1] <- sample(1:length(b), 1)   ## A random index is taken as the sequence number of the first extracted word
for (i in 2:50) {
  ## Use sample to loop out the next word according to the probability of the previous word
  index[i] <- sample(1:length(b), 1, prob = A[index[i - 1], ])
}
index   ## Get the index of 50 words

s <- numeric(0)   ## Initializes a vector that records 50 words
for (i in 1:50) {
  ## Loop retrieve the words according to the index
  s[i] <- b[index[i]]   
}
cat(s)   ## Print 50 words


##9
a_total <- match(text, b)   ## use match function to find the vector of indicies indicating which element in the unique word vector each element in the bible text corresponds to
n_total <- tabulate(a_total)   ## Count the total number of times unique words appear in biblical text in uppercase and lowercase
a_lower <- match(del_6, b)    ## Find the identification vector to indicate the word corresponding to the Bible text (not lowercase) in the unique word vector
n_lower <- tabulate(a_lower)  ## Count the number of times unique words appear in the Bible text in lowercase form

P <- n_lower / n_total   ## Calculate the probability that unique words appear in lowercase

ggg <- numeric(0)   ## Create a variable length vector ggg to record the index of the words appearing uppercase more
x = 0
for (i in 1:length(P)) {
  ## loop record the index of words whose first letter is capitalized more than the first letter is lowercase
  if (P[i] < 0.5) {
    x = x + 1
    ggg[x] <- i
  } else
  {
    x = x + 0
  }
}   

# install.packages("Hmisc")
library(Hmisc)
B <- b   ## Initialize B with b
for (i in 1:length(ggg)) {
  ## Loop to capitalize the specified word in the unique word vector b
  B[ggg[i]] <- capitalize(b[ggg[i]])   
}

s_new <- numeric(0)  ## Create a variable length vector s_new to record the new 50 words
for (i in 1:50) {
  ## Loop through 50 words in the new unique word B vector
  s_new[i] <- B[index[i]]   
}
cat(s_new)   ## print new 50 words

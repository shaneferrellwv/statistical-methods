######################################################
#                                                    #
# 605.631 – Statistical Methods for Computer Science #
# Module 1                                           #
#                                                    #
######################################################

source('stattools Module1.R')

#Assigning values to a variable
a<-3
a

b<-5
b

d<-a + b
d

#Basic operations
a + b
a - b
a * b
a / b
a^b
exp(a)
log(a)
log(exp(a))
log10(a)
log10(exp(a))

#Working in R
getwd()
setwd()  #or use the file navigation in R Studio
ls()
rm=list(ls()) #clear the workspace

#Create a vector
x<- c(2,5,1,3)
y<- 1:4 #cool shortcut
length(x)
z<-rep(2, times=10)
z
zz<-rep(1:2, times=10)
zz
zzz<-rep(1:2, length.out=8)
zzz
xx<-seq(from=1, to=10, by=2)
xx
xxx<-seq(1,10,length.out = 20)
xxx

#Data table
d<-data.table(c(2,1,3,5), c(5,6,2,1), c(2,1,5,3))
d
names(d)<-c("tall", "grande", "venti")
d

#Descriptive statistics
mean(d$tall)
median(d$grande)
sd(d$venti)
cor(d$tall,d$venti)
min(d)
max(d)
sum(d)

#Functions
perm <- function(n, x) {
  factorial(n) / factorial(n-x)
}

comb <- function(n, x) {
  factorial(n) / (factorial(n-x) * factorial(x))
}

distrib <- function(n, r) {
  comb((n+r-1),(r-1))
}

#Permutations and Combinations
factorial(10)
perm(10,10)
comb(10,10)
comb(10,3)
distrib(14,3)

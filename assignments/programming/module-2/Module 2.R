######################################################
#                                                    #
# 605.631 "Statistical Methods for Computer Science" #
# Module 2: Probability                              #
#                                                    #
######################################################

source("stattools Module2.R")

rm(list=ls()) #clear global environment

#Assigning sample space
S <- data.frame(lands = c("down", "up", "side"));S

# roll a fair die
die <- sample(6, size = 30, replace = TRUE)

# flip a coin
coin <- sample(c('H','T'), size = 30, replace = TRUE)

#Sample space tossing coin
T1 <- tosscoin(1);T1

T3 <- tosscoin(3);T3

#Sample space rolling dice
R1 <- rolldie(1);R1

R2 <- rolldie(2);R2

#Sample space drawing cards
head(cards())

#Sample space sampling from urns with replacement
U3wr <- urnsamples(1:3, size = 2, replace = TRUE, ordered = TRUE);U3wr

#Sample space sampling from urns without replacement
U3wor <- urnsamples(1:3, size = 2, replace = FALSE, ordered = TRUE);U3wor

#Now lets talk about Events. use "makespace"
rm(list=ls()) #clear global environment
S <- tosscoin(2, makespace = TRUE);S

#We want the event that the first toss is tails so we choose those rows
S[c(2,4), ]

#Let's use the sample space of a deck of cards
rm(list=ls()) #clear global environment


S <- cards(jokers = FALSE)


#Set Union, Intersection, and Difference
A <- subset(S, suit == "Heart");A
B <- subset(S, rank %in% 5:8);B

#Union of sets A and B
union(A, B)

#Inersection of sets A and B
intersect(A, B)

#Set difference of sets A and B
setdiff(A, B)
setdiff(B, A)

#Complement with setdiff
A_c <- setdiff(S,A);A_c

#Create prabability space
rm(list=ls()) #clear global environment


S <- rolldie(1);S

p <- rep(1/6, times = 6);p

# Probspace forms a probability space from a set of outcomes and (optional)
# vector of probabilities.
probspace(S, probs = p)

probspace(1:6, p)

probspace(1:6)

probspace(1:7)

rolldie(1, makespace = TRUE)

# A word on uneuqally likely outcomes
probspace(tosscoin(1), probs = c(0.70, 0.30))

iidspace(c("H","T"), ntrials = 3, probs = c(0.5, 0.5))

iidspace(c("H","T"), ntrials = 3, probs = c(0.7, 0.3))


#*********************************************************
# Example 1: If 2 dice are rolled, what is the probability
#   that the sum of the upturned faces will equal 7?
#*********************************************************
rm(list=ls()) #clear global environment

S <- rolldie(2, makespace = TRUE);S
Ex1 <- subset(S, X1+X2 == 7);Ex1
sum(Ex1$probs)
nrow(Ex1)/nrow(S)

#*********************************************************
# Example 1a: If 8 dice are rolled, what is the probability
#   that the sum of the upturned faces will equal 26?
#*********************************************************
rm(list=ls()) #clear global environment

S <- rolldie(8, makespace = TRUE);nrow(S)
Ex1a <- subset(S, X1+X2+X3+X4+X5+X6+X7+X8 == 26);nrow(Ex1a)
sum(Ex1a$probs)
nrow(Ex1a)/nrow(S)
Prob(S, X1+X2+X3+X4+X5+X6+X7+X8 == 26)

#*********************************************************
# Example 2: If 3 balls are "randomly drawn" from a bowl
#   containing 6 white and 5 black balls, what is the 
#   probability tht one of the balls is white and the 
#   other two are black?
#*********************************************************
rm(list=ls()) #clear global environment

# Build your bowl
bowl <- rep(c('white','black'),times=c(6,5));bowl

# The urnsamples(x, ...) function creates a sample space associated with the
# experiment of sampling distinguishable objects from an urn. Order matters!
S <- urnsamples(bowl, size = 3, replace = FALSE, ordered = TRUE);nrow(S)

# Use probspace to form a probability space from S
P <- probspace(S)

# Use Prob to find this is (120+120+120)/990 = 4/11
Prob(P, isin(P,c('white','black','black')))

#*********************************************************
# Example 3: There are 11 artists who each submit a portfolio 
#   containing 7 paintings for competition in an art exhibition. 
#   Unfortunately, the gallery director only has space in the 
#   winners? section to accomodate 12 paintings in a row equally 
#   spread over three consecutive walls. The director decides to
#   give the first, second, and third place winners each a wall 
#   to display the work of their choice. The walls boast 31 
#   separate lighting options apiece. How many displays are 
#   possible?
#   Answer: The judges will pick 3 (ranked) winners out of 11 
#   (with rep=FALSE, ord=TRUE). Each artist will select 4 of 
#   his/her paintings from 7 for display in a row 
#   (rep=FALSE, ord=TRUE), and lastly, each of the 3 walls 
#   has 31 lighting possibilities (rep=TRUE, ord=TRUE). 
#   These three numbers can be calculated quickly with...
#*********************************************************
rm(list=ls()) #clear global environment

# Set the vectors
n <- c(11, 7, 31)
k <- c(3, 4, 3)
r <- c(FALSE,FALSE,TRUE)

x <- nsamp(n, k, rep = r, ord = TRUE)

#  By the Multiplication Principle, the number of ways
#   to complete the experiment is the product of the 
#   entries of x. We used the prod function
prod(x)

# Compare this with the some standard ways to compute
(11*10*9)*(7*6*5*4)*31^3

#*********************************************************
# Example 4: Two cards are randomly selected from an ordinary
#   playing deck. What is the probability that they form a 
#   blackjack? That is, what is the probability that one of 
#   the cards is an ace and the other one is either a ten, a 
#   jack, a queen, or a king?
#*********************************************************
deck <- cards(jokers = FALSE)

S <- urnsamples(deck, size = 2, replace = FALSE, ordered = TRUE);nrow(S)
P <- probspace(S)

event <- subset(P, all(suit=="Heart"))
Prob(event)

event <- subset(P, any(rank=='A') & any(rank==10))
Prob(event)*4


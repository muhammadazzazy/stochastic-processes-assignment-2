install.packages("markovchain")
install.packages("diagram")
library(markovchain)
library(diagram)

###### Simulate discrete-time Markov chain ########################
# Simulates n steps of a Markov chain 
# markov(init,mat,n,states)
# Generates X0, ..., Xn for a Markov chain with initiial
#  distribution init and transition matrix mat
# Labels can be a character vector of states; default is 1, .... k

markov <- function(init,mat,n,labels) { 
  if (missing(labels)) labels <- 1:length(init)
  simlist <- numeric(n+1) #creating a vector of zeros
  
  states <- 1:length(init)
  simlist[1] <- sample(states,1,prob=init)
  for (i in 2:(n+1)) 
  { simlist[i] <- sample(states,1,prob=mat[simlist[i-1],]) }
  labels[simlist]
}
# define a transition matrix
tmA <- matrix(c(0.25,0.65,0.1,.25,0.25,.5,.35,.25,0.4),nrow = 3, byrow = TRUE)
# create the DTMC
dtmcA <- new("markovchain",transitionMatrix=tmA, states=c("No Rain","Light Rain","Heavy Rain"), name="MarkovChain A") 
dtmcA
plot(dtmcA)
stateNames <- c("No Rain","Light Rain","Heavy Rain")
row.names(tmA) <- stateNames; colnames(tmA) <- stateNames
plotmat(tmA,pos = c(1,2),box.type = "circle", main = "Markov Chain",box.col = "green")
# It is possible to simulate states distribution after n-steps
init<-c(0,1,0)
steps<-2
finalState<-init*dtmcA^steps #using power operator
finalState

install.packages("markovchain")
install.packages("diagram")
library(markovchain)
library(diagram)

###### Simulate discrete-time Markov chain ########################
# Simulates n steps of a Markov chain 
# markov(init,mat,n,states)
# Generates X0, ..., Xn for a Markov chain with initial
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
tmQ1 <- matrix(c(0.2,0.8,0,0.5,0,0.5,0.25,0.25,0.5),nrow = 3, byrow = TRUE)
# create the DTMC
dtmcQ1 <- new("markovchain",transitionMatrix=tmQ1, states=c("1","2","3"), 
              name="MarkovChain") 
dtmcQ1
plot(dtmcQ1)
stateNames <- c("1","2","3")
row.names(tmQ1) <- stateNames; colnames(tmQ1) <- stateNames
plotmat(t(tmQ1), pos = c(1, 2), box.type = "circle", main = "Markov Chain", 
        box.col = "green", box.size=0.047, cex.txt=0.75, dtext=0.75)
# It is possible to simulate states distribution after n-steps
init<-c(1,0,0)
steps<-4
finalState<-init*dtmcQ1^steps #using power operator
finalState
finalState[2]

library("markovchain")
#Q1)
states1 <- c("0","1")
p1 <- matrix(c(0.6,0.4,
               0.1,0.9), nrow = 2, byrow = TRUE, dimnames = (list(states1, states1)))
tpm1 <- new("markovchain", states = states1,
            transitionMatrix = p1, name = "Q1")
tpm1
summary(tpm1)
period(tpm1)
stat1 <- steadyStates(tpm1) 
stat1

#Q2)
states2 <- c("ambulatory", "Bedridden", "discharged")
p2 <- matrix(c(0.4,0.3,0.3,
               0.5,0.2,0.3,
               0, 0, 1), nrow = 3, byrow = TRUE, dimnames = (list(states2,states2)))
p2
tpm2 <-  new("markovchain", states = states2,
             transitionMatrix = p2, name = "Q2")
tpm2
summary(tpm2)
period(tpm2)
stat2 <- steadyStates(tpm2) 
stat2
#OR
id2 <- c(0.7,0.3,0)
stat2i <- id2*tpm2^1000
stat2i

#Q3)

states3 <- c("m1", "m2", "m3")
p3 <- matrix(c(0.65,0.25,0.1,
               0.5,0.35,0.15,
               0.7, 0.2, 0.1), nrow = 3, byrow = TRUE, dimnames = (list(states3,states3)))
p3
tpm3 <-  new("markovchain", states = states3,
             transitionMatrix = p3, name = "Q3")
tpm3
summary(tpm3)
period(tpm3)
stat3 <- steadyStates(tpm3) 
stat3


#Q7)
states7 <- c("0", "50", "100", "150", "200")
p7 <- matrix(c(1,0,0,0,0,
               qi1,0,pi1,0,0,
               0,qi1,0,pi1,0,
               0,0,qi1,0,pi1,
               0,0,0,0,1), nrow = 5, byrow = TRUE, dimnames = (list(states7,states7)))
p7
tpm7 <-  new("markovchain", states = states7,
             transitionMatrix = p7, name = "Q7")
tpm7
summary(tpm7)
period(tpm7)
gamb.ruin.prob = function(p,i,k){
  q <- 1 - p
  if (p == q){
    ruin.prob <- 1 - (i/k)
  }else{
    r <- q/p
    ruin.prob <- ((r^i) - (r^k))/(1 - (r^k))
  }
}
gruin7i1 <- gamb.ruin.prob(0.3,0:4,4)
gruin7i2 <- gamb.ruin.prob(0.8,0:4,4)
gruin7i1
gruin7i2

#Q4)

states4 <- c("0","1")
p4 <- matrix(c(0.88,0.12,
               0.15,0.85), nrow = 2, byrow = TRUE, dimnames = (list(states4, states4)))
tpm4 <- new("markovchain", states = states4,
            transitionMatrix = p4, name = "Q4")
tpm4
summary(tpm4)
period(tpm4)
q4id <- c(0.6,0.4)
q4i1 <- q4id*tpm4^5
q4i1
q4i2 <- q4id*tpm4^10
q4i2

#Q5)

states5 <- c("0","1","2","3")
q5id <- c(1/4, 1/4, 1/4, 1/4)
p5 <- matrix(c(1,0,0,0,
               0.4,0,0.6,0,
               0.2,0,0.1,0.7,
               0,0,0,1), nrow = 4, byrow = TRUE, dimnames = (list(states5, states5)))
tpm5 <- new("markovchain", states = states5,
            transitionMatrix = p5, name = "Q5")
tpm5
summary(tpm5)
period(tpm5)
q5 <- q5id * tpm5^1000 
q5[4]
q5[1]


#Q6)
states6 <- c("0", "25", "50", "75")
p6 <- matrix(c(1,0,0,0,
               0.5,0,0.5,0,
               0,0.5,0,0.5,
               0,0,0,1), nrow = 4, byrow = TRUE, dimnames = (list(states6,states6)))
p6
tpm6 <-  new("markovchain", states = states6,
             transitionMatrix = p6, name = "Q6")
tpm6
summary(tpm6)
period(tpm6)
gamb.ruin.prob = function(p,i,k){
  q <- 1 - p
  if (p == q){
    ruin.prob <- 1 - (i/k)
  }else{
    r <- q/p
    ruin.prob <- ((r^i) - (r^k))/(1 - (r^k))
  }
}
q6i11 <- tpm6^3
q6i1 <- q6i11[3,1]
q6i1
gruin6 <- gamb.ruin.prob(0.5,3,10000)
gruin6
# As k tends to infinity,at p = 0.5, ruin probablity tends to 1 

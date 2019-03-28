library(nleqslv)

## The following section calculates the distance matrix for the grid cells
t <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3)
dist <- c()

for (i in 1:length(t)){
  ref <- which(t==i, arr.ind=TRUE)
  for (j in 1:length(t)){
    dist_tmp <- sqrt(sum((ref - which(t==j, arr.ind=TRUE))^2)) 
    dist <- c(dist, dist_tmp)
  }
}

dist_matrix <- matrix(dist, nrow=9, ncol=9)

## Initializing parameters
regions <- 10
sigma <- 5
alpha <- 0.75
Fc <- 0.4
A <- rlnorm(regions, log(3), log(1.5))
H <- rlnorm(regions, log(3), log(1.5))
L_=1
d11 <- 1
d12 <- 1
d21 <- 1
d22 <- 1

sigma_ <- (sigma-1)/(2*sigma-1)
gamma1 <- sigma*(1-alpha)/alpha
gamma2 <- 1+(sigma/(sigma-1))-(sigma-1)*(1-alpha)/alpha

#solve for L^sigma_/gamma1

## Equilibrium conditions
eq <- function(L) {

  n <- length(L)
  t <- c(1,1)
  
  s1 <- sum((1/(sigma*Fc)) * ((sigma/(sigma-1)*d11)^(1-sigma)) * L^(gamma2/gamma1) * A^(sigma*(sigma-1)/(2*sigma-1)) * H^((sigma-1)*(sigma-1)*(1-alpha)/(alpha*(2*alpha-1))))
  
  W <- ((L[n] * A[n]^((-sigma*(sigma-1)*(sigma-1))/(2*sigma-1)) * H[n]^(-sigma*(sigma-1)*(1-alpha)/(alpha*(2*sigma-1))))/s1)^(1/(1-sigma))
  
  L_s <- L[-length(L)] 
  A_s <- A[-length(A)] 
  H_s <- H[-length(H)] 
  
  cond1 <- L_s * A_s^((-sigma*(sigma-1)*(sigma-1))/(2*sigma-1)) * H_s^(-sigma*(sigma-1)*(1-alpha)/(alpha*(2*sigma-1))) -
    W^(1-sigma) * sum((1/(sigma*Fc)) * ((sigma/(sigma-1)*d11)^(1-sigma)) * L^(gamma2/gamma1) * A^(sigma*(sigma-1)/(2*sigma-1)) * H^((sigma-1)*(sigma-1)*(1-alpha)/(alpha*(2*alpha-1))))
  
  cond2 <- (1 - sum(L))

  return(c(cond1,cond2))
}

init <- rep(1/regions, regions)
nleqslv(init, eq)
 

























G <- matrix(c(1,1,1,1), ncol = 1, nrow = 4)




## Equilibrium conditions
eq <- function(L) {
  #L <- var[1:2]
  #w <- var[3:4]
  
  #cond1 <- sum(L)-L_
  cond1 <- 0.5 - L[1]+L[2]
  cond2 <- 1-L[1]-L[2]
  
  return(c(cond1,cond2))
}

nleqslv(c(1,2), eq)




# scalar cannot solve a vector equation...

## Condition for existence and uniqueness 
unique <- (1+sigma/(sigma-1)-((sigma-1)*(1-alpha)/alpha))/sigma*(1-alpha)/alpha
  
  
var <- c(1,2,3,4,5,5)

L <- var[1:2]
w <- var[3:4]
V <- var[5:6]
pi <- (L * (d * w/A)^(1-sigma))/(sum(L * (d*w/A)^(1-sigma)))

cond1 <- sum(L)-L_
cond2 <- L - ((a1 * pi^(-alpha*(sigma-1)))^gamma1)   /   (sum(a1 * pi^(-alpha/(sigma-1)))^gamma1)
cond3 <- V - (a1 * pi^(-alpha/(sigma-1)) * L^gamma2)/gamma3

return(c(cond1, cond2, cond3))


pi <- (L * (d * w/A)^(1-sigma))/(sum(L * (d*w/A)^(1-sigma)))

cond1 <- sum(L)-L_
cond2 <- L - ((a1 * pi^(-alpha*(sigma-1)))^gamma1)   /   (sum(a1 * pi^(-alpha/(sigma-1)))^gamma1)
cond3 <- V - (a1 * pi^(-alpha/(sigma-1)) * L^gamma2)/gamma3








## Initializing parameters
sigma <- 5
alpha <- 0.75
Fc <- 0.4
#A <- rlnorm(2, log(10), log(2.5))
A <- c(4,4)
H <- c(1, 2)
L_=1
d11 <- 1
d12 <- 1
d21 <- 1
d22 <- 1

sigma_ <- (sigma-1)/(2*sigma-1)
gamma1 <- sigma*(1-alpha)/alpha
gamma2 <- 1+(sigma/(sigma-1))-(sigma-1)*(1-alpha)/alpha

#solve for L^sigma_/gamma1

## Equilibrium conditions
eq <- function(L) {
  #L <- var[1:2]
  #w <- var[3:4]
  
  W <- ((L[2] * A[2]^((-sigma*(sigma-1)*(sigma-1))/(2*sigma-1)) * H[2]^(-sigma*(sigma-1)*(1-alpha)/(alpha*(2*sigma-1))))/((1/(sigma*Fc)) * ((sigma/(sigma-1)*d11)^(1-sigma)) * L[2]^(gamma2/gamma1) * A[2]^(sigma*(sigma-1)/(2*sigma-1)) * H[2]^((sigma-1)*(sigma-1)*(1-alpha)/(alpha*(2*alpha-1))) +
                                                                                                                            (1/(sigma*Fc)) * ((sigma/(sigma-1)*d12)^(1-sigma)) * L[1]^(gamma2/gamma1) * A[1]^(sigma*(sigma-1)/(2*sigma-1)) * H[1]^((sigma-1)*(sigma-1)*(1-alpha)/(alpha*(2*alpha-1)))))^(1/(1-sigma))
  
  cond1 <- L[1] * A[1]^((-sigma*(sigma-1)*(sigma-1))/(2*sigma-1)) * H[1]^(-sigma*(sigma-1)*(1-alpha)/(alpha*(2*sigma-1))) -
    W^(1-sigma) * ((1/(sigma*Fc)) * ((sigma/(sigma-1)*d11)^(1-sigma)) * L[1]^(gamma2/gamma1) * A[1]^(sigma*(sigma-1)/(2*sigma-1)) * H[1]^((sigma-1)*(sigma-1)*(1-alpha)/(alpha*(2*alpha-1))) +
                     (1/(sigma*Fc)) * ((sigma/(sigma-1)*d12)^(1-sigma)) * L[2]^(gamma2/gamma1) * A[2]^(sigma*(sigma-1)/(2*sigma-1)) * H[2]^((sigma-1)*(sigma-1)*(1-alpha)/(alpha*(2*alpha-1))))
  
  
  cond2 <- 1 - L[1]-L[2] 
  
  return(c(cond1,cond2))
}

nleqslv(c(1,0), eq)









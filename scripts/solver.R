library(nleqslv)

regions <- 900
## The following section calculates the distance matrix for the grid cells

# Creating a spatial point dataset to calculate distances
t <- matrix(seq(regions), 
            nrow=sqrt(regions), 
            ncol=sqrt(regions),
            byrow=TRUE)

x <- c()
y <- c()
for (i in 1:length(t)){
  x <- c(x, which(t==i, arr.ind=TRUE)[1,1])
  y <- c(y, which(t==i, arr.ind=TRUE)[1,2])
}
coords <- cbind(x,y)
cells <-  SpatialPoints(coords)
dist <- c()
for (i in 1:length(cells)){
  for (j in 1:length(cells)){
  dist <- c(dist, gDistance(cells[i], cells[j]))
  }
}

# Distance matrix based on eucliadian distance
dist_matrix_full <- (matrix(dist, 
                       nrow=(regions), 
                       ncol=(regions)))

d11 <- matrix(rep(1,regions), 
              nrow=regions/2, 
              ncol=regions/2)
d12 <- matrix(rep(2.5,regions), 
              nrow=regions/2, 
              ncol=regions/2)
d21 <- matrix(rep(2.5,regions), 
              nrow=regions/2, 
              ncol=regions/2)
d22 <- matrix(rep(1,regions), 
              nrow=regions/2, 
              ncol=regions/2)
d1 <- cbind(d11,d12)
d2 <- cbind(d21,d22)
dist_matrix <- rbind(d1,d2)
dist_matrix <- dist_matrix + dist_matrix_full
diag(dist_matrix) <- 1

# Port with low transportation cost to the other country
dist_matrix[450,] <- 1

## Initializing parameters
sigma <- 5
alpha <- 0.75
Fc <- 4
A <- rlnorm(regions, log(5), log(1.1))
#A[((regions/2):regions)] <- 6
#H <- rlnorm(regions, log(5), log(1.1))
#A <- c(rep(3,regions))
H <- rep(10,regions)
L_=1
sigma_ <- (sigma-1)/(2*sigma-1)
gamma1 <- sigma*(1-alpha)/alpha
gamma2 <- 1+(sigma/(sigma-1))-(sigma-1)*(1-alpha)/alpha
a <- sigma_*gamma1
init <- rep(1/regions, regions)

## Solve for population density
eq <- function(L) {

  n <- length(L)

  s1 <- sum((1/(sigma*Fc))  *  
              ((sigma/(sigma-1)*dist_matrix[regions,])^(1-sigma))  *  
              L^(a*gamma2/gamma1)  *  
              A^(sigma*(sigma-1)/(2*sigma-1))  *  
              H^((sigma-1)*(sigma-1)*(1-alpha)/(alpha*(2*sigma-1))))
  
  dist_matrix_adjusted <- ((1/(sigma*Fc))  * 
            (((sigma/(sigma-1))*dist_matrix)^(1-sigma))) 
           
  L_adjusted <- L^(a*(gamma2/gamma1)) * 
            A^(sigma*(sigma-1)/(2*sigma-1))  *  
            H^((sigma-1)*(sigma-1)*(1-alpha)/(alpha*(2*sigma-1)))
  
  s2 <- dist_matrix_adjusted %*% L_adjusted
  
  W <- ((L[n]^a * 
           A[n]^((-sigma*(sigma-1)*(sigma-1))/(2*sigma-1)) * 
           H[n]^(-sigma*(sigma-1)*(1-alpha)/(alpha*(2*sigma-1))))/s1)

  L_s <- L[-length(L)] 
  A_s <- A[-length(A)] 
  H_s <- H[-length(H)] 
  s2 <- s2[-length(s2)]
  
  cond1 <- L_s^a *
    A_s^((-sigma*(sigma-1)*(sigma-1))/(2*sigma-1)) * 
    H_s^(-sigma*(sigma-1)*(1-alpha)/(alpha*(2*sigma-1)))  -   W*s2 
  
  cond2 <- (1 - sum(L))
  
  return(c(cond1,cond2))
}
L <- unlist(nleqslv(init, eq)[1])

## Solve for wages
w <-  (1/((A^(sigma-1) * 
         L^((sigma-1)*(1-alpha)/alpha) *
         H^(-(sigma-1)*(1-alpha)/alpha))))^(1/(1-2*sigma))

## Solve for prices
L_adj <- dist_matrix%*%L
P <- (1/(sigma*Fc))  *  
     (sigma/(sigma-1))  * 
     ((L_adj*w/A)^(1-sigma))^(1/(1-sigma))


## Put solutions in matrix form
P_solution <- matrix(P, 
                     nrow = sqrt(regions),
                     ncol = sqrt(regions), 
                     byrow=TRUE)

L_solution <- matrix(L, 
                     nrow = sqrt(regions),
                     ncol = sqrt(regions), 
                     byrow=TRUE)

A_solution <- matrix(A, 
                     nrow = sqrt(regions),
                     ncol = sqrt(regions),
                     byrow=TRUE)

w_solution <- matrix(w, 
                     nrow = sqrt(regions),
                     ncol = sqrt(regions),
                     byrow=TRUE)

## Plotting the solutions
library(raster)
library(tmap)
library(viridis)
L_dist <- raster((L_solution))
A_dist <- raster((A_solution))
P_dist <- raster((P_solution))
w_dist <- raster((w_solution))
trade_costs <- raster((dist_matrix_full))

p1 <- tm_shape((L_dist)) +
  tm_raster(palette=plasma(256),n=40) +
  tm_layout(frame=FALSE, legend.show=FALSE,bg.color="white", 
            main.title="Population")

p2 <- tm_shape(A_dist) +
  tm_raster(palette=plasma(256),n=40) +
tm_layout(frame=FALSE, 
          legend.show=FALSE,
          bg.color="white",
          main.title="Productivity")

p3 <- tm_shape(P_dist) +
  tm_raster(palette=plasma(256),n=10) +
  tm_layout(frame=FALSE, 
            legend.show=FALSE,
            bg.color="white",
            main.title="Price level")

p4 <- tm_shape(w_dist) +
  tm_raster(palette=plasma(256),n=10) +
  tm_layout(frame=FALSE, 
            legend.show=FALSE,
            bg.color="white",
            main.title="Wages")

tmap_arrange(p1,p2,p3,p4)



## Solutions for the closed economy
P_solution_closed <- matrix(P, 
                     nrow = sqrt(regions),
                     ncol = sqrt(regions), 
                     byrow=TRUE)

L_solution_closed <- matrix(L, 
                     nrow = sqrt(regions),
                     ncol = sqrt(regions), 
                     byrow=TRUE)

A_solution_closed <- matrix(A, 
                     nrow = sqrt(regions),
                     ncol = sqrt(regions),
                     byrow=TRUE)

w_solution_closed <- matrix(w, 
                     nrow = sqrt(regions),
                     ncol = sqrt(regions),
                     byrow=TRUE)

P_solution_d <- P_solution/P_solution_closed
L_solution_d <- L_solution/L_solution_closed
w_solution_d <- w_solution/w_solution_closed

L_dist <- raster((L_solution_d))
A_dist <- raster((A_solution))
P_dist <- raster((P_solution_d))
w_dist <- raster((w_solution_d))






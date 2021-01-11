
##  ##
# 01 # Importance of being uncertain
##  ##

set.seed(444)
#http://stackoverflow.com/questions/22728422/how-do-i-stop-set-seed-after-a-specific-line-of-code

#generate RANDOM NUMBERS
#http://www.cookbook-r.com/Numbers/Generating_random_numbers/
#a <- floor(runif(10000, min=0, max=30))
#a <- sample(1:30,10000, replace = T)
#hist(a)




# (1) Population parameters estimated by sampling ------------------------------------------------------------------------------------

#grDevices::png("results/CLT-1.png",width = 960,height = 480)

par(mfrow=c(1,2))#, oma=c(0,0,0,0), mar=c(3,3,3,3) # http://research.stowers.org/mcm/efg/R/Graphics/Basics/mar-oma/index.htm

b <- rnorm(5000,0,3)
b <- c(b[which(b>0)],
  rnorm(1500,10,1),
  rnorm(4500,20,2),
  rnorm(1500,25,1))

h <- hist(b, xlim = c(0,30), breaks = 100,   # Frequency histogram of values
          main = "population\ndistribution", # in population b
          xlab = "random numbers")

m <- mean(b)                                 # popultion mean (mu)
sd <- sd(b)                                  # popultion standard deviation (sigma)
segments(m,0, m, max(h$counts), lwd = 2, col = "red")
l1 <- m-sd
l2 <- m+sd
coordx <- c(l1,seq(l1,l2,0.01),l2)
coordy <- c(0,rep(max(h$counts),length(coordx)-2),0)
polygon(coordx,coordy, lty = 0,
        col = rgb(0.9, 0.5, 0.1, 0.3))
text(m, max(h$counts)+10, expression(mu), cex=1.5) # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
text(m-(sd/2), -10, expression(sigma), cex=1.5)
arrows(m,-10,(m-(sd/2))+1,-10, code = 1,
       length = 0.05, col = "red", lwd=2)
arrows(m-sd,-10,(m-(sd/2))-1,-10, code = 1,
       length = 0.05, col = "red", lwd=2)

#c <- sample(b, 5); mean(c)
c <- NULL
for(i in seq(1,10000,1)){                    # Means of 10,000 random samples of size n=5
  c <- c(c,mean(sample(b, 5)))               # taken from the values in population b
}                                            # each value is bar(X)[i]

h <- hist(c, xlim = c(0,30), breaks = 100,   # Frequency histogram of means
          main = "sampling distribution of\nsample means", # of all posible samples
          xlab = "random numbers",           # of size n=5 taken from population b
          ylim = c(0,max(h$counts)))         # (which is impossible, but we take 10,000 random samples)

m <- mean(c)                                 # (all possible) Sample mean (mu[bar(X)])
sd <- sd(c)                                  # (all possible) Sample standard deviation (sigma[bar(X)])
segments(m,0, m, max(h$counts), lwd = 2, col = "red")
l1 <- m-sd
l2 <- m+sd
coordx <- c(l1,seq(l1,l2,0.01),l2)
coordy <- c(0,rep(max(h$counts),length(coordx)-2),0)
polygon(coordx,coordy, lty = 0,
        col = rgb(0.9, 0.5, 0.1, 0.3))
text(m, max(h$counts)+10, expression(mu[bar(X)]), cex=1.5)
text(m-(sd/2), -10, expression(sigma[bar(X)]), cex=1.5)
arrows(m,-10,(m-(sd/2))+1,-10, code = 1,
       length = 0.05, col = "red", lwd=2)
arrows(m-sd,-10,(m-(sd/2))-1,-10, code = 1,
       length = 0.05, col = "red", lwd=2)

#dev.off()




# (2) Sampling distribution w/ Sample Size increase ------------------------------------------------------------------------------------



distplot <- function(b,t) {
  # 1. plot a frequency histogram of values in population "b"     # Population distribution
  # 2. visualize its distribution parameters
  h <- hist(b, breaks = 100, xlim = c(0,30),
            main = t,
            xlab = "", ylab = "", axes = F)
  m <- mean(b)
  sd <- sd(b)
  segments(m,0, m, max(h$counts), lwd = 2, col = "red")
  l1 <- m-sd
  l2 <- m+sd
  coordx <- c(l1,seq(l1,l2,0.01),l2)
  coordy <- c(0,rep(max(h$counts),length(coordx)-2),0)
  polygon(coordx,coordy, lty = 0,
          col = rgb(0.9, 0.5, 0.1, 0.3))
  abline(h=0)
}



# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Distributions.html

par(mfrow=c(1,4))#, oma=c(0,0,0,0), mar=c(3,3,3,3)

b <- rnorm(5000,0,3)
b <- c(b[which(b>0)],rnorm(1500,10,1),
       rnorm(4500,20,2),rnorm(1500,25,1))
t <- "Irregular"
distplot(b,t)

b <- rnorm(length(b),m,sd)
t <- "Normal"
distplot(b,t)

b <- rnbinom(length(b),m,.5)#hist(rnbinom(10000,10,.5)) # http://stackoverflow.com/questions/20254084/plot-normal-left-and-right-skewed-distribution-in-r
#b <- rbeta(length(b),2,8)#hist(rbeta(10000,2,8))
t <- "Skewed"
distplot(b,t)

b <- runif(length(b),0,30)
t <- "Uniform"
distplot(b,t)



clt <- function(b,n,t) {
  # 1. generate a vector of sample means of size "n" 
  #    from values in population "b"
  # 2. plot a frequency histogram of sample means       # Sampling distribution of sample mean
  # 3. visualize its distribution parameters
  c <- NULL
  for(i in seq(1,10000,1)){
    c <- c(c,mean(sample(b, n)))
  }
  h <- hist(c, breaks = 100, xlim = c(0,30),
            main = paste0("n=",n),
            xlab = "", ylab = "", axes = F)
  m <- mean(c)
  sd <- sd(c)
  segments(m,0, m, max(h$counts), lwd = 2, col = "red")
  l1 <- m-sd
  l2 <- m+sd
  coordx <- c(l1,seq(l1,l2,0.01),l2)
  coordy <- c(0,rep(max(h$counts),length(coordx)-2),0)
  polygon(coordx,coordy, lty = 0,
          col = rgb(0.9, 0.5, 0.1, 0.3))
  abline(h=0)
}



grDevices::png("results/CLT-2.png",width = 960,height = 960)

par(mfcol=c(5,4))

b <- rnorm(5000,0,3)
b <- c(b[which(b>0)],rnorm(1500,10,1),
       rnorm(4500,20,2),rnorm(1500,25,1))

m <- mean(b)                                 # popultion mean (mu)
sd <- sd(b)                                  # popultion standard deviation (sigma)

t <- "Irregular"
distplot(b,t)
clt(b,3,t)
clt(b,5,t)
clt(b,10,t)
clt(b,20,t)

b <- rnorm(length(b),m,sd)
t <- "Normal"
distplot(b,t)
clt(b,3,t)
clt(b,5,t)
clt(b,10,t)
clt(b,20,t)

b <- rnbinom(length(b),m,.5)#hist(rnbinom(10000,10,.5))
t <- "Skewed"
distplot(b,t)
clt(b,3,t)
clt(b,5,t)
clt(b,10,t)
clt(b,20,t)

b <- runif(length(b),0,30)
t <- "Uniform"
distplot(b,t)
clt(b,3,t)
clt(b,5,t)
clt(b,10,t)
clt(b,20,t)

dev.off()




# (3) mean, sd, sem as size increases ------------------------------------------------------------------------------------

#grDevices::png("results/CLT-3.png",width = 960,height = 480)

N <- 100                                    # the maximum sample size evaluated

b <- rnorm(5000,0,3)
b <- c(b[which(b>0)], rnorm(1500,10,1),     # from the Irregular distribution
       rnorm(4500,20,2),rnorm(1500,25,1))

# POPULATION PARAMETERS
m <- mean(b)                                # population mean (mu)
sd <- sd(b)                                 # population uncertainty (sigma)
sem <- sd(b)/(sqrt(length(b)))              # population mean uncertainty (sigma[bar(X)])
                                            # sample size dependency
k <- NULL
for(l in 2:N){                              # population mean uncertainty (sigma[bar(X)])
  s <- data.frame(size=l,                   # for each sample size
                  sem=sd(b)/(sqrt(l)))      # in order to plot it as a line
  k <- rbind(k,s)                           # using points(type="l")
}

# EMPTY PLOT
# http://stackoverflow.com/questions/4785657/r-how-to-draw-an-empty-plot
plot(1,type = "n",
     ylim = c(0,20),xlim = c(2,N+10), 
     xlab = "sample size (n)", ylab = "",
     pch=20, bty="n")

# SAMPLE ESTIMATES
q <- NULL
for(r in 2:N){                             # evaluate sample sizes from n=2 to n=100 (r)
  for(i in 1:3){                           # 3 random samples per sample size (i)
    p <- data.frame(size=r,                # + sample estimates
                    mean=mean(sample(b, r)),      # sample mean (bar(X))
                    std=sd(sample(b, r)),         # sample uncertainty (s[X])
                    sem=sd(sample(b, r))/sqrt(r)) # sample mean uncertainty (s[bar(X)])
    points(p$size,p$mean, pch=19)          # plot each estimate with
    points(p$size,p$std, pch=1)            # its respective sample size
    points(p$size,p$sem, pch=20)
    q <- rbind(q,p)
  }
}

# ADD POPULATION PARAMETERS as text and segments
text(N+5, m, expression(mu), cex=1.5, col = "red")
segments(0,m,N+1, m, lwd = 2, col = "red")
text(N+5, sd, expression(sigma), cex=1.5, col = "red")
segments(0,sd,N+1,sd, lwd = 2, col = "red")
text(N+5, sem+.5, expression(paste(sigma[bar(X)],"=",frac(sigma,sqrt(n)))), cex=1.5, col = "red") # http://vis.supstat.com/2013/04/mathematical-annotation-in-r/
points(k$size, k$sem, type = "l", lwd=2, col="red")
#segments(0,sem,N+1,sem, lwd = 2, col = "red")

# ADD SAMPLE ESTIMATES as text
text(N/2, m+4, expression(paste("sample mean (", bar(X),")")), cex=1)
text(N/2, sd+2, expression(paste("sample standard deviation (", s[X],")")), cex=1)
text(N/2, sem+3, expression(paste("Standard error of the mean (s.e.m) (", s[bar(X)],")")), cex=1)

#dev.off()

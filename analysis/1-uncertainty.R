
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
par(mfrow=c(1,2))#, oma=c(0,0,0,0), mar=c(3,3,3,3)
b <- rnorm(5000,0,3)
b <- c(b[which(b>0)],
  rnorm(1500,10,1),
  rnorm(4500,20,2),
  rnorm(1500,25,1))
h <- hist(b, xlim = c(0,30), breaks = 100,
          main = "population\ndistribution",
          xlab = "random numbers")
m <- mean(b)
sd <- sd(b)
segments(m,0, m, max(h$counts), lwd = 2, col = "red")
l1 <- m-sd
l2 <- m+sd
coordx <- c(l1,seq(l1,l2,0.01),l2)
coordy <- c(0,rep(max(h$counts),length(coordx)-2),0)
polygon(coordx,coordy, lty = 0,
        col = rgb(0.9, 0.5, 0.1, 0.3))
text(m, max(h$counts)+10, expression(mu), cex=1.5)
text(m-(sd/2), -10, expression(sigma), cex=1.5)
arrows(m,-10,(m-(sd/2))+1,-10, code = 1,
       length = 0.05, col = "red", lwd=2)
arrows(m-sd,-10,(m-(sd/2))-1,-10, code = 1,
       length = 0.05, col = "red", lwd=2)

#c <- sample(b, 5); mean(c)
c <- NULL
for(i in seq(1,10000,1)){
  c <- c(c,mean(sample(b, 5)))
}
h <- hist(c, xlim = c(0,30), breaks = 100,
          main = "sampling distribution of\nsample means",
          xlab = "random numbers",
          ylim = c(0,max(h$counts)))
m <- mean(c)
sd <- sd(c)
segments(m,0, m, max(h$counts), lwd = 2, col = "red")
l1 <- m-sd
l2 <- m+sd
coordx <- c(l1,seq(l1,l2,0.01),l2)
coordy <- c(0,rep(max(h$counts),length(coordx)-2),0)
polygon(coordx,coordy, lty = 0,
        col = rgb(0.9, 0.5, 0.1, 0.3))
text(m, max(h$counts)+10, expression(mu[bar(x)]), cex=1.5)
text(m-(sd/2), -10, expression(sigma[bar(x)]), cex=1.5)
arrows(m,-10,(m-(sd/2))+1,-10, code = 1,
       length = 0.05, col = "red", lwd=2)
arrows(m-sd,-10,(m-(sd/2))-1,-10, code = 1,
       length = 0.05, col = "red", lwd=2)
#dev.off()




# (2) Sampling distribution w/ Sample Size increase ------------------------------------------------------------------------------------



distplot <- function(b,t) {
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



#grDevices::png("results/CLT-2.png",width = 960,height = 960)

par(mfcol=c(5,4))

b <- rnorm(5000,0,3)
b <- c(b[which(b>0)],rnorm(1500,10,1),
       rnorm(4500,20,2),rnorm(1500,25,1))
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

#dev.off()




# (3) mean, sd, sem as size increases ------------------------------------------------------------------------------------

#grDevices::png("results/CLT-3.png",width = 960,height = 480)

N <- 100

b <- rnorm(5000,0,3)
b <- c(b[which(b>0)],
       rnorm(1500,10,1),
       rnorm(4500,20,2),
       rnorm(1500,25,1))

# POPULATION MEASUREMENT
m <- mean(b)
sd <- sd(b)
sem <- sd(b)/(sqrt(length(b)))

k <- NULL
for(l in 2:N){
  s <- data.frame(size=l,
                  sem=sd(b)/(sqrt(l)))
  k <- rbind(k,s)
}

# SAMPLE ESTIMATION

# http://stackoverflow.com/questions/4785657/r-how-to-draw-an-empty-plot
plot(1,type = "n",
     ylim = c(0,20),xlim = c(2,N+10), 
     xlab = "sample size (n)", ylab = "",
     pch=20, bty="n")

q <- NULL
for(r in 2:N){
  for(i in 1:3){
    p <- data.frame(size=r,
                    mean=mean(sample(b, r)),
                    std=sd(sample(b, r)),
                    sem=sd(sample(b, r))/sqrt(r))
    points(p$size,p$mean, pch=19)
    points(p$size,p$std, pch=1)
    points(p$size,p$sem, pch=20)
    q <- rbind(q,p)
  }
}

text(N+5, m, expression(mu), cex=1.5, col = "red")
segments(0,m,N+1, m, lwd = 2, col = "red")
text(N+5, sd, expression(sigma), cex=1.5, col = "red")
segments(0,sd,N+1,sd, lwd = 2, col = "red")
text(N+5, sem+.5, expression(paste(sigma[bar(x)],"=",frac(sigma,sqrt(n)))), cex=1.5, col = "red")
points(k$size, k$sem, type = "l", lwd=2, col="red")
#segments(0,sem,N+1,sem, lwd = 2, col = "red")

text(N/2, m+4, expression(paste("sample mean (", bar(x),")")), cex=1)
text(N/2, sd+2, expression(paste("sample standard deviation (", s[x],")")), cex=1)
text(N/2, sem+3, expression(paste("Standard error of the mean (s.e.m) (", s[bar(x)],")")), cex=1)

#dev.off()
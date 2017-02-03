#set.seed(444)
#http://stackoverflow.com/questions/22728422/how-do-i-stop-set-seed-after-a-specific-line-of-code

#generate RANDOM NUMBERS
#http://www.cookbook-r.com/Numbers/Generating_random_numbers/
#a <- floor(runif(10000, min=0, max=30))
#a <- sample(1:30,10000, replace = T)
#hist(a)

grDevices::png("results/CLT-1.png",width = 960,height = 480)

par(mfrow=c(1,2))
b <- rnorm(5000,0,3)
b <- c(b[which(b>0)],
  rnorm(1500,10,1),
  rnorm(4500,20,2),
  rnorm(1500,25,1))
hist(b, xlim = c(0,30), breaks = 100,
     main = "population distribution",
     xlab = "random numbers")
m <- mean(b)
sd <- sd(b)
abline(v=m, col="red", lwd=2)
l1 <- m-sd
l2 <- m+sd
coordx <- c(l1,seq(l1,l2,0.01),l2)
coordy <- c(0,rep(500,length(coordx)-2),0)
polygon(coordx,coordy, lty = 0,
        col = rgb(0.9, 0.5, 0.9, 0.2))
c <- sample(b, 5)
mean(c)
c <- NULL
for(i in seq(1,10000,1)){
  #set.seed(system("date +%s%N", intern = TRUE))
  c <- c(c,mean(sample(b, 5)))
}
hist(c, xlim = c(0,30), breaks = 100,
     main = "sampling distribution of \n sample means",
     xlab = "random numbers")
m <- mean(c)
sd <- sd(c)
abline(v=m, col="red", lwd=2)
l1 <- m-sd
l2 <- m+sd
coordx <- c(l1,seq(l1,l2,0.01),l2)
coordy <- c(0,rep(500,length(coordx)-2),0)
polygon(coordx,coordy, lty = 0,
        col = rgb(0.9, 0.5, 0.9, 0.2))

dev.off()
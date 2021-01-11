
##  ##
# 02 # Bayes theorem and Bayes statistics
##  ##

# Bayes Theorem --------------------------------------------------------------------------------------




# Bayes Statistics --------------------------------------------------------------------------------------

x <- seq(0,1,0.01)
a <- data.frame(x,y=x**3)
plot(a,type="l")
flux::auc(a$x, a$y)#AUC= 0.25

b <- data.frame(x,y=4*(x**3))
plot(b,type="l")
flux::auc(b$x, b$y)#AUC= ~1.00


curve(dbeta(x,4,2), ylim=c(0,3), col="black")
curve(dbeta(x,4,2), ylim=c(0,3), col="red", add=T)
curve(dbeta(x,7,3), ylim=c(0,3), col="blue", add=T)

betashape <- function(a_prior, b_prior, a_lik, b_lik, y_lim, main) {
  curve(dbeta(x,a_prior,b_prior), ylim=c(0,y_lim), col="black", main=main)
  curve(dbeta(x,a_lik,b_lik), ylim=c(0,y_lim), col="red", add=T)
  curve(dbeta(x,(a_prior-1)+(a_lik-1)+1,(b_prior-1)+(b_lik-1)+1), ylim=c(0,y_lim), col="blue", add=T)
}

#par(mfrow=c(1,5))
betashape(4,2,4,2,y_lim=3, main= "effect of prior in posterior\nweak prior")
betashape(16,6,4,2,y_lim=5, main= "effect of prior in posterior\nstrong prior")
betashape(4,2,2,4,y_lim=3, main= "diminishing effect of prior\n4 tosses")
betashape(4,2,6,16,y_lim=5, main= "diminishing effect of prior\n20 tosses")
betashape(4,2,26,76,y_lim=10, main= "diminishing effect of prior\n100 tosses")
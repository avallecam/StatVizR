par(mfrow = c(3,2)) ## some random par change
#par("mfrow")

### IMPUT DATA
####DEFINE PROBLEM####
prot <- read.csv("data-raw/concentra.csv")
prot$Conc.1
conc.1<-prot$Conc.1
conc.2<-prot$Conc.2

####################################################
##
################### FOR CONC.1
##
####################################################


## PLOT density ##
## then add CURVE with NORMAL DISTRIBUTION (dnorm) ##

z <- conc.1
m<-mean(z)
md<-median(z)
sd<-sd(z)

plot(density(z), lwd=3, main="Densidad de conc.1")   # PLOT requires: x-y coordinates

abline(v=m,col = "red", lwd=3)
abline(v=m-2*sd, lty=2, lwd=2)
abline(v=m+2*sd, lty=2, lwd=2)
abline(v=md, col="blue", lwd=3)

curve(dnorm(x,m, sd), col = "red", add = TRUE, 
      lwd = 3, lty=2)                          # CURVE requires: x as a function?

## PLOT a HISTROGRAM, ##
## then add a LINE with density ##
## + CURVE with dnorm ##
hist(z, freq = F, 
     main = "Densidad de conc.1 proteica", 
     xlab = "Intervalos por Sturges", 
     ylab = "Densidad", 
     col = "gray90", xlim = c(22,38))  # HIST require x as a vector of values

lines(density(z), lwd=3)               # LINES requires: x-y coordinates

abline(v=m,col = "red", lwd=3)
abline(v=m-2*sd, lty=2, lwd=2)
abline(v=m+2*sd, lty=2, lwd=2)
abline(v=md, col="blue", lwd=3)

curve(dnorm(x,m, sd), col = "red", add = TRUE, 
      lwd = 3, lty=2)                           # CURVE requires: x as a function?
 


################################################
####------------- other graph ----------------##
################################################

# MAIN title: 68% of data #
plot(density(z), lwd=2, lty=2, col="grey70",
     xlim = c(m-3*sd,m+3*sd),
     ylim=c(0,max(density(z)$y)),
     main = "~68% de los valores de conc.1 / plot",
     xlab = "concentracion de proteina",
     ylab = "densidad f(x)",
     cex= 2) #cex NOT working
curve(dnorm(x,m, sd), col = "red",
      lwd = 2, add = T)

#########------------LINES------------###########
### work LINES over PLOT & HIST
### a line have no limits (different from a segment)

## horizontal line y=0
abline(h=0)

#########-----------SEGMENTS-----------###########
## SEGMENTO hasta el valor maximo de la NORMAL! ##
segments(m,0, m, dnorm(m, m, sd), lty = 2, col = "blue")


#########------------SOMBREAR-------------########
## objetivo 01: CURVA NORMAL / conc.1

## DEFINIR limites de ZONA a sombrear##
#68%
l1<-m+1*sd
l2<-m-1*sd
#95%
#n1<-m+2*sd
#n2<-m-2*sd

##coordenadas de zona a sombrear CONC.1 ##
#68%
cord.x <- c(l2, seq(l2,l1, 0.01), l1)
cord.y <- c(0,dnorm(seq(l2,l1, 0.01), m, sd),0)
#95%
#cord2.x <- c(n2, seq(n2,n1, 0.01), n1)
#cord2.y <- c(0,dnorm(seq(n2,n1, 0.01), m, sd),0)



## poligono (----------SOMBREADOR----------)
#68%
polygon(cord.x, cord.y, col = rgb(0.5, 0.5, 0.9, 0.2),lty=2)
#95%
#polygon(cord2.x, cord2.y, col = rgb(0.5, 0.5, 0.9, 0.2),lty=2)



##################################################
##########-----------TEXTOS-----------############
##################################################

## INGRESAR TEXTO sobre LIMITES del sombreo##
#68%
text(l2, dnorm(l2, m, sd), "media - 1*sd", pos=2, cex = 0.8)
text(l1, dnorm(l1, m, sd), "media + 1*sd", pos=4, cex = 0.8)
#95%
#text(n2, dnorm(n2, m, sd), "media - 2*sd", pos=2, cex = 0.8)
#text(n1, dnorm(n1, m, sd), "media + 2*sd", pos=4, cex = 0.8)

##------------ TEXTO ESTADISTICO II -------------## 
# ?COMO? se realiza el cálculo

# 68% 
#de los valores del gr?fico (m-sd;m+sd) #
porcentaje <- round(100*(1-2*(1-pnorm(1))),1)
li <- max(density(z)$y)/2
text(m, li, paste(porcentaje, "%", sep = ""), 
     cex = 2, col = "gray50")
# 95% 
#de los valores del gr?fico (m-sd;m+sd) #
#porcentaje2 <- round(100*(1-2*(1-pnorm(2))),1)
#le <- max(density(z)$y)/2
#text(m, le, paste(porcentaje2, "%", sep = ""), 
#     cex = 2, col = "gray50")


##################

# MAIN title: 95% of data #
plot(density(z), lwd=2, lty=2, col="grey70",
     xlim = c(m-3*sd,m+3*sd),
     ylim=c(0,max(density(z)$y)),
     main = "~95% de los valores de conc.1 / plot",
     xlab = "concentracion de proteina",
     ylab = "densidad f(x)",
     cex= 2) #cex NOT working
curve(dnorm(x,m, sd), col = "red",
      lwd = 2, add = T)

#########------------LINES------------###########
### work LINES over PLOT & HIST
### a line have no limits (different from a segment)

## horizontal line y=0
abline(h=0)

#########-----------SEGMENTS-----------###########
## SEGMENTO hasta el valor maximo de la NORMAL! ##
segments(m,0, m, dnorm(m, m, sd), lty = 2, col = "blue")


#########------------SOMBREAR-------------########
## objetivo 01: CURVA NORMAL / conc.1

## DEFINIR limites de ZONA a sombrear##
#68%
#l1<-m+1*sd
#l2<-m-1*sd
#95%
n1<-m+2*sd
n2<-m-2*sd

##coordenadas de zona a sombrear CONC.1 ##
#68%
#cord.x <- c(l2, seq(l2,l1, 0.01), l1)
#cord.y <- c(0,dnorm(seq(l2,l1, 0.01), m, sd),0)
#95%
cord2.x <- c(n2, seq(n2,n1, 0.01), n1)
cord2.y <- c(0,dnorm(seq(n2,n1, 0.01), m, sd),0)



## poligono (----------SOMBREADOR----------)
#68%
#polygon(cord.x, cord.y, col = rgb(0.5, 0.5, 0.9, 0.2),lty=2)
#95%
polygon(cord2.x, cord2.y, col = rgb(0.5, 0.5, 0.9, 0.2),lty=2)



##################################################
##########-----------TEXTOS-----------############
##################################################

## INGRESAR TEXTO sobre LIMITES del sombreo##
#68%
#text(l2, dnorm(l2, m, sd), "media - 1*sd", pos=2, cex = 0.8)
#text(l1, dnorm(l1, m, sd), "media + 1*sd", pos=4, cex = 0.8)
#95%
text(n2, dnorm(n2, m, sd), "media - 2*sd", pos=2, cex = 0.8)
text(n1, dnorm(n1, m, sd), "media + 2*sd", pos=4, cex = 0.8)

##------------ TEXTO ESTADISTICO II -------------## 
# ?COMO? se realiza el cálculo

# 68% 
#de los valores del gr?fico (m-sd;m+sd) #
#porcentaje <- round(100*(1-2*(1-pnorm(1))),1)
#li <- max(density(z)$y)/2
#text(m, li, paste(porcentaje, "%", sep = ""), 
#     cex = 2, col = "gray50")
# 95% 
#de los valores del gr?fico (m-sd;m+sd) #
porcentaje2 <- round(100*(1-2*(1-pnorm(2))),1)
le <- max(density(z)$y)/2
text(m, le, paste(porcentaje2, "%", sep = ""), 
     cex = 2, col = "gray50")



####################################################
##
######-------- FOR CONC.1 + CONC.2 ---------########
##
####################################################

## PLOT density, ##
## then add LINES with conc.2 ##
## readapt the limits ##

# define conc.2

y<- conc.2
m_y<-mean(y)       # red
md_y<-median(y)    # blue
sd_y<-sd(y)

# start ploting both conc.1 --> MAIN PLOT 95% of data?
# plot conc.1
plot(density(z), lwd=2,
     xlim = c(m-3*sd,m_y+3*sd),
     ylim=c(0,max(density(z)$y)),
     main = "~95% de los valores de conc.1 y conc.2",
     xlab = "[proteina]",
     ylab = "densidad f(x)")
# add conc.2
lines(density(y), lwd=2, col="red")

# DO ggplot could work? #
# ggplot2::ggplot(data = data.frame(x=z,t=density(z)$y), aes(t,x)) + geom_point()

# DELINEATE normal curves
curve(dnorm(x,m_y, sd_y), col = "red", add = TRUE, 
      lwd = 1, lty=2)
curve(dnorm(x,m, sd), lwd = 1, add = T, lty=2)


#########-----------LINES-----------###########
# add some lines
abline(v=m,col = "red", lwd=2)
abline(v=m-2*sd_y, lty=2, lwd=1)
abline(v=m+2*sd_y, lty=2, lwd=1)
abline(v=md, col="blue", lwd=2)

abline(v=m_y,col = "red", lwd=2)
abline(v=m_y-2*sd_y, lty=2, lwd=1, col="red")
abline(v=m_y+2*sd_y, lty=2, lwd=1, col="red")
abline(v=md_y, col="blue", lwd=2)

abline(h=0)

#####-----------SOMBREAR-------------#####
## objetivo 01: CURVA NORMAL / conc.1
## ARRIBA! ##

##
## objetivo 02: CURVA DE DENSIDAD / conc.2

## DEFINIR limites de ZONA a sombrear ##
#65%
k1<-m_y+1*sd_y
k2<-m_y-1*sd_y
#95%
m1<-m_y+2*sd_y
m2<-m_y-2*sd_y

###########################################
## coordenadas de zona a sombrear CONC.2 ##
###########################################

## STRATEGY for colour under the curve:
# 1) make a matrix/data.frame
# 2) subset density_x acording to 68% or 95%
#    to also  subset their density_y

c2x<-density(y)$x
c2y<-density(y)$y

## create a DATA.FRAME FOR THE COORDIANTES ## 
df_c2<-data.frame(c2x,c2y)

########## use SUBSET
#68%
df_c2_r<-subset(df_c2, c2x>k2 & c2x<k1)

########## now SUBSET
#95%
df_c2_r2<-subset(df_c2, c2x>m2 & c2x<m1)

#### COORDENATES #####
#68%
cord.x_y <- c(k2, df_c2_r$c2x, k1)
cord.y_y <- c(0,df_c2_r$c2y,0)
#95%
cord2.x_y <- c(m2, df_c2_r2$c2x, m1)
cord2.y_y <- c(0,df_c2_r2$c2y,0)


###### POLIGONOS -somberadores- para conc.2 #####
#68%
polygon(cord.x_y, cord.y_y, col = rgb(1.0, 0.7, 0.2, 0.2),lty=2)
#95%
polygon(cord2.x_y, cord2.y_y, col = rgb(1.0, 0.7, 0.2, 0.2),lty=2)

###### end of task ######

###########################################
## coordenadas de zona a sombrear CONC.1 ##
###########################################

c1x<-density(z)$x
c1y<-density(z)$y

## create a DATA.FRAME FOR THE COORDIANTES ## 
df_c1<-data.frame(c1x,c1y)

########## use SUBSET
#68%
df_c1_r<-subset(df_c1, c1x>l2 & c1x<l1)

########## now SUBSET
#95%
df_c1_r2<-subset(df_c1, c1x>n2 & c1x<n1)

#### COORDENATES #####
#68%
cord.x_x <- c(l2, df_c1_r$c1x, l1)
cord.y_x <- c(0,df_c1_r$c1y,0)
#95%
cord2.x_x <- c(n2, df_c1_r2$c1x, n1)
cord2.y_x <- c(0,df_c1_r2$c1y,0)


###### POLIGONOS -somberadores- para conc.1 #####
#68%
polygon(cord.x_x, cord.y_x, col = rgb(0.5, 0.5, 0.9, 0.2),lty=2)
#95%
polygon(cord2.x_x, cord2.y_x, col = rgb(0.5, 0.5, 0.9, 0.2),lty=2)

###### end ######



#############################################################
##
##################### SUMMARY TABLE #########################
##
#############################################################

# note: FIND THE APPLICATION OF: 
# apply, sapply, lapply ##

# export that table on LATEX
#install.packages("stargazer")
library(stargazer)
stargazer(prot, type = "text", title="Descriptive statistics", digits=1, out="results/table1.txt")

getwd()

# OTHER
library(fBasics)
basicStats(prot)

# OTHER

tmp <- do.call(df, 
               list(mean = apply(df, 2, mean),
                    sd = apply(df, 2, sd),
                    median = apply(df, 2, median),
                    min = apply(df, 2, min),
                    max = apply(df, 2, max),
                    n = apply(df, 2, length)))
tmp

## OTHER
# not rounded
my.summary <- function(prot){
  c(mean=mean(prot),
    sd=sd(prot),
    median=median(prot),
    min=min(prot),
    max=max(prot), 
    n=length(prot))
}
# rounded
my.summary <- function(prot){
  c(mean=round(mean(prot),2),
    sd=round(sd(prot),2),
    median=round(median(prot),2),
    min=round(min(prot),2),
    max=round(max(prot),2), 
    n=round(length(prot),2))
}

# all these calls should give you the same results.
apply(prot, 2, my.summary)
sapply(prot, my.summary)
t(sapply(prot, my.summary))
do.call(cbind,lapply(prot, my.summary))

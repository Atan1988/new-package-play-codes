library(blink)
data(RLdata500)
head(RLdata500)

# X.c contains the categorical variables
# X.s contains the string variables 
# p.c is the number of categorical variables
# p.s contains the number of string variables
X.c <- RLdata500[c("by","bm","bd")]
X.c <- as.matrix(RLdata500[,"bd"],ncol=1)
p.c <- ncol(X.c)
X.s <- as.matrix(RLdata500[-c(2,4,7)])
p.s <- ncol(X.s)

file.num <- rep(c(1,2,3),c(200,150,150))

# Subjective choices for distortion probability prior
a <-1
b <- 999

d <- function(string1,string2){adist(string1,string2)}

c <- 1

lam.gs <- rl.gibbs(file.num=file.num,X.s=X.s,X.c=X.c,num.gs=2,a=a,b=b,c=c,d=d, M=500)

estLink <- lam.gs
estPopSize <- apply(estLink , 1, function(x) {length(unique(x))})
plot(density(estPopSize),xlim=c(300,500),main="",lty=1, "Observed Population Size", ylim= c(0,1))
abline(v=450,col="red")
abline(v=mean(estPopSize),col="black",lty=2)
mean(estPopSize)

sd(estPopSize)
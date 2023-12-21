#Sathwik's R Script
library(ggplot2)
data(trees)
head(trees)

mean(trees$Girth)


hist(trees$Height)

ggplot(trees, aes(x = Height)) + 
  stat_bin(breaks = seq(60, 90, by = 5),aes(y=after_stat(density)), 
           colour = "black",fill = "lightgreen",closed = "left") + 
  scale_x_continuous(breaks = seq(60, 90, by = 5)) + 
  geom_density(aes(color="Kernel Density"),linewidth=1) + # Add kernel density
  geom_function(fun = dnorm, args = list(mean = mean(trees$Height), 
                                         sd = sd(trees$Height)),aes(color="Normal"),linetype="dashed",linewidth=1) + 
  scale_color_manual(name="",values=c("blue","red"))# Add normal density 

trees$Height

help(rnorm)
hist(rnorm(10000000,22,1))

hist(rnorm(10000000,22,1))

help(rbinom)
hist(rbinom(1000,100,0.2))
rbinom(1000,100,0.2)

dnorm()


x=seq(-2,2,1)
y=dbinom(x,0,1)

ggplot()+geom_line(aes(x,y))

y=pnorm(x,0,1)
ggplot()+geom_line(aes(x,y))


x=seq(0,1,0.1)
y=pnorm(x,100,)
ggplot()+geom_line(aes(x,y))

x<-seqx



library(ggplot2)
# two-sided
rejection2 <- function(n,d=10) {
  results <- data.frame(
    x=seq(0,n),
    y=dbinom(seq(0,n),n,.5),
    keep=sapply(seq(0,n),
                function(x) 
                  (x<qbinom(.025,n,.5)) | (x>qbinom(.975,n,.5))))
  ggplot(
    data=results,aes(x=x,y=y,fill=keep))+ 
    geom_bar(stat="identity")+
    scale_x_continuous(breaks=seq(0,n,d))+
    ggtitle("Two sided rejection region at alpha=.05")
  
}

rejection1 <-function(n,d=10) {
  results <- data.frame(
    x=seq(0,n),
    y=dbinom(seq(0,n),n,.5),
    keep=sapply(seq(0,n),function(x) (x>qbinom(.95,n,.5))))
  ggplot(data=results,aes(x=x,y=y,fill=keep))+
    geom_bar(stat="identity")+
    scale_x_continuous(breaks=seq(0,n,d))+
    ggtitle("One sided rejection region at alpha=.05")
}

rejection2(100)


results <- data.frame(
  x=seq(0,100),
  y=dbinom(seq(0,100),100,.5),
  keep=sapply(seq(0,100),
              function(x) 
                (x<qbinom(.025,100,.5)) | (x>qbinom(.975,100,.5))))
ggplot(
  data=results,aes(x=x,y=y,fill=keep))+ 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=seq(0,100,d))


results



x=seq(0,100)
x

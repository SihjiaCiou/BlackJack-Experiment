bj_data = read.csv("~/變異數分析/blackjack_data.csv")

attach(bj_data)
factor(strategy)
factor(deck.number)
bj_data$bet.ratio = as.factor(bet.ratio)

#EDA
library(ggplot2)
require(scales) # for percent scale

ggplot(bj_data, aes(x = bj_data$bet.ratio, y = remaining.stakes, fill = factor(deck.number))) +
  geom_boxplot(width = 0.8)+ 
  facet_grid(. ~ strategy ,space = "free") + # spilt windows by strategy
  labs(fill = "number of deck") + #change legend title
  scale_y_continuous(name = "remaining cash") +
  scale_x_discrete(name = "bet ratio"  )+
  theme_bw()+ #adjust the backround
  theme(legend.justification=c(0.005,0), # legend position
        legend.position=c(0.005,0))

#part(a) ANOVA table

fit.bj = aov(remaining.stakes ~ strategy*bet.ratio*deck.number, data=bj_data)
summary(fit.bj)


#fit linear and quadratic orthogonal polynomial regression 
R_lin = poly(rep(c(0.01,0.02,0.03), each = 10, times = 8),degree=2)[,1]
R_qua = poly(rep(c(0.01,0.02,0.03), each = 10, times = 8),degree=2)[,2]


fit.orth.bj = lm(remaining.stakes ~ strategy+ R_lin + R_qua+ deck.number + 
                          strategy:R_lin + strategy:R_qua + deck.number:R_lin + deck.number:R_qua +strategy:deck.number +
                            strategy:deck.number:R_lin + strategy:deck.number:R_qua , data=bj_data)

aovtable2 = summary.aov(fit.orth.bj)
capture.output(aovtable2,file="aov2.txt")
bj_mse = 1199346

#part(b) compute mean and sd of factors

# Strategy 
xtabs(remaining.stakes~strategy, bj_data)/xtabs(~strategy,bj_data) #main factor mean _strategy 
round(sqrt(bj_mse/(3*2*10)),2)  #stand error of strategy mean

#bet ratio
xtabs(remaining.stakes~bet.ratio, bj_data)/xtabs(~bet.ratio,bj_data) #main factor mean_bet ratio
round(sqrt(bj_mse/(4*2*10)),2) #stand error of bet ratio mean

#number of deck
xtabs(remaining.stakes~deck.number, bj_data)/xtabs(~deck.number,bj_data) #main factor mean_deck number
round(sqrt(bj_mse/(3*3*10)),2)

# Strategy * bet ratio
cellmean = xtabs(remaining.stakes~strategy + bet.ratio, bj_data)/xtabs(~strategy + bet.ratio,bj_data) #two factor marginal mean _strategy* bet ratio
round(sqrt(bj_mse/(2*10)),2)
capture.output(cellmean,file="cellmean.txt")

#part(c) plot of interaction effect 


## method1
#interaction.plot(bet.ratio,strategy,remaining.stakes) 

## method2
q = xtabs(remaining.stakes~strategy + bet.ratio, bj_data)/xtabs(~strategy + bet.ratio,bj_data)
as.table(q)
capture.output(q,file="interaction.txt")

#plot interaction of strategy*bet ratio 
plot(c(0.01,0.02,0.03),q[1,],type = "l",ylim = c(3000,6000), xlab = "bet ratio", ylab = "remaining stakes")
points(c(0.01,0.02,0.03),q[1,],pch = 1)
lines(c(0.01,0.02,0.03), q[2,] ,col = 2)
points(c(0.01,0.02,0.03), q[2,], pch = 1)
lines(c(0.01,0.02,0.03), q[3,] ,col = 4)
points(c(0.01,0.02,0.03), q[3,] , pch = 1)
lines(c(0.01,0.02,0.03), q[4,] ,col = 11)
points(c(0.01,0.02,0.03), q[4,] , pch = 1)
legend("bottomleft", c("basic strategy", "dealer strategy", "NN strategy" ,"random strategy"),col = c(1,2,4,11),
       text.col = c(1,2,4,11),lty = 1,pch = NA)


# Strategy * number of deck
xtabs(remaining.stakes~strategy + deck.number, bj_data)/xtabs(~strategy + deck.number,bj_data) #two factor marginal mean _strategy* # of deck 
round(sqrt(bj_mse/(3*10)),2)

#plot interaction of strategy*deck number 
s = xtabs(remaining.stakes~strategy + deck.number, bj_data)/xtabs(~strategy + deck.number,bj_data)
as.table(s)

plot(c(1,6),s[1,],type = "l",ylim = c(3000,6000), xlab = "number of deck", ylab = "remaining stakes")
points(c(1,6),s[1,],pch = 1)
lines(c(1,6), s[2,] ,col = 2)
points(c(1,6), s[2,], pch = 1)
lines(c(1,6), s[3,] ,col = 4)
points(c(1,6), s[3,] , pch = 1)
lines(c(1,6), s[4,] ,col = 11)
points(c(1,6), s[4,] , pch = 1)
legend("bottomleft", c("basic strategy", "dealer strategy", "NN strategy" , "random strategy"),col = c(1,2,4,11),
       text.col = c(1,2,4,11),lty = 1,pch = NA)

# bet ratio* number of deck
xtabs(remaining.stakes~bet.ratio + deck.number, bj_data)/xtabs(~ bet.ratio + deck.number,bj_data) #two factor marginal mean _strategy* deck number
round(sqrt(bj_mse/(3*10)),2)

#plot interaction of bet ratio *deck number 
k = xtabs(remaining.stakes~bet.ratio + deck.number, bj_data)/xtabs(~bet.ratio + deck.number,bj_data)
plot(c(1,6),k[1,],type = "l",ylim = c(3000,6000), xlab = "number of deck", ylab = "remaining stakes")
points(c(1,6),k[1,],pch = 1)
lines(c(1,6), k[2,] ,col = 2)
points(c(1,6), k[2,], pch = 1)
lines(c(1,6), k[3,] ,col = 4)
points(c(1,6), k[3,] , pch = 1)
legend("bottomleft", c("bet ratio =0.01", "bet ratio =0.02", "bet ratio =0.03"),col = c(1,2,4),
       text.col = c(1,2,4),lty = 1,pch = NA)


#part(d) pairwise contrast_ by TukeyHSD method

install.packages("agricolae")
library(agricolae)
library("base")

TukeyHSD(fit.bj,"strategy")
plot(TukeyHSD(fit.bj,"strategy"))

#pard(e) residual analysis 
#test for normality and variances homogeneity assumption of anova model

aov_residuals <- residuals(object =fit.orth.bj )
par(mfrow = c(1,2))
par(mar = c(5,4,3,2))
plot(aov_residuals,xlab = "remaining stakes", ylab = "residual", main = "residual plot" , pch = 20)
abline(0, 0)  
qqnorm(aov_residuals)



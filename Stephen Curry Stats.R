h <- read.csv("Stephen Curry Stats.csv")

#HYPOTHESIS TESTING
# H0: uW – uL <= 0
# HA: uW – uL > 0
qW <- subset(h,select = c(PTS),subset = Result=="W")
qL <- subset(h,select = c(PTS),subset = Result=="L")
list=as.list(qW)
list2=as.list(qL)
vec=unlist(list)
vec1=unlist(list2)
W <- sample(vec,200,replace = F)
L <- sample(vec1,200,replace = F)
t_test0 <- t.test(W,L,var.equal = T,alternative = "two.sided")
t_test0
###  3D PIE CHARTS
install.packages("plotrix")
library("plotrix")
sum(h$Result=="L")
sum(h$Result=="W")
a <- c(555,323)
lbl <- c("Win(63.21%)","Lose(36.79%)")
pie3D(a,labels = lbl,explode = 0.1, main="Win - Lose")

sum(h$Total.FT) # 3153
sum(h$Successful.FT) #2839
d <- c(2839,314)
lbl4 <- c("Successful Free Throw(90,05%)","Unsuccessful Throw Points(9.95%)")
pie3D(d,labels = lbl4,explode = 0.3,main="Total percentage of Free Throw")

sum(h$Successful.Shots)
sum(h$Total.Shots)
x <- c(6790,6151)
lbl5 <- c("Unsuccessful Shots (52.46%)","Successful Shots(47.54%)")
pie3D(x,labels = lbl5,explode = 0.1, main="Successful Shots - Unsuccessful Shots")

### CORRELATION GRAPH

cor1 <- cor(h$AST,h$PTS)
hh <- lm(h$PTS~h$AST,data = h)
plot(h$AST,h$PTS,
     xlim = c(min(h$AST), max(h$AST)),
     ylim = c(min(h$PTS), max(h$PTS)),
     main = "Assists and Points In Data", xlab = "Assists", ylab = "Points")
abline(hh,lwd=4,lty=1,col = "Red")
legend("topright",legend=cor1,title="Correlation Coefficient")

### BARPLOT

A <- subset(h,select = c(Opponent,PTS,Score.GS),subset = Minutes>=35)
B <- subset(A,select=c(PTS),subset=Opponent=="CLE")
E <- subset(A,select=c(Score.GS),subset=Opponent=="CLE")
BB <- sum(B)/length(unlist(B))
CC <- sum(E)/length(unlist(E))
barplot(unlist(B),ylab = "Total points scored by Curry",xlab = "Total points",names.arg = unlist(E),col = "Blue",
        main="Contribution of Curry to the matches more than 35 minutes against Cleveland",density = 20)
legend("topleft",legend=c(BB,CC),pch=c(2,2),col=c("Blue","Blue"))

### BOXPLOT

vvv <- subset(h,select = c(Minutes,AST))
cor(vvv)
boxplot(Minutes~AST,data = vvv,col="blue",main="Assists And Minutes In The Matches")
mean(h$Minutes)
cor(h$Minutes,h$AST)

### MULTIPLE LINEAR REGRESSION

MLR1 <- lm(PTS~ Total.3.Points+ Total.FT,data=h)
summary(MLR1)
MLR2 <- lm(TO ~ Minutes + PF,data=h)
summary(MLR2)

### CROSSTAB

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
aaa <- subset(h,select = c(PTS,AST,Result),subset =Opponent=="UTAH" )
bbb <- subset(h,select = c(PTS,AST,Result),subset =Opponent=="CLE" )
crosstab(aaa,row.vars = "PTS",col.vars = "Result",type = "f")
crosstab(bbb,row.vars = "PTS",col.vars = "Result",type = "f")


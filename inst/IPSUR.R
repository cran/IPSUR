###################################################
### chunk number 1: 
###################################################
#line 218 "IPSUR.Rnw"
###  IPSUR.R - Introduction to Probability and Statistics Using R
###  Copyright (C) 2011  G. Jay Kerns, <gkerns@ysu.edu>
###  This program is free software: you can redistribute it and/or modify
###  it under the terms of the GNU General Public License as published by
###  the Free Software Foundation, either version 3 of the License, or
###  (at your option) any later version.
###  This program is distributed in the hope that it will be useful,
###  but WITHOUT ANY WARRANTY; without even the implied warranty of
###  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###  GNU General Public License for more details.
###  You should have received a copy of the GNU General Public License
###  along with this program.  If not, see <http://www.gnu.org/licenses/>
###################################################


###################################################
### chunk number 2: 
###################################################
#line 234 "IPSUR.Rnw"
seed <- 42
set.seed(seed)
options(width = 75)
#library(random)
#i_seed <- randomNumbers(n = 624, col = 1, min = -1e+09, max = 1e+09)
#.Random.seed[2:626] <- as.integer(c(1, i_seed))
#save.image(file = "seed.RData")


###################################################
### chunk number 3: 
###################################################
#line 244 "IPSUR.Rnw"
options(useFancyQuotes = FALSE)
#library(prob)
library(RcmdrPlugin.IPSUR)
# Generate RcmdrTestDrive
n <- 168
# generate order 
order <- 1:n
# generate race 
race <- sample(c("White","AfAmer","Asian","Other"), size=n, prob=c(76,13,5,6), replace = TRUE)
race <- factor(race)
# generate gender and smoke 
tmp <- sample(4, size=n, prob=c(12,38,9,41), replace = TRUE) 
gender <- factor(ifelse(tmp < 3,"Male", "Female")) 
smoke <- factor(ifelse(tmp %in% c(1,3), "Yes", "No"))
# generate parking
parking <- rgeom(n, prob = 0.4) + 1
# generate salary 
m <- 17 + (as.numeric(gender)-1) 
s <- 1 + (2 - as.numeric(gender)) 
salary <- rnorm(n, mean = m, sd = s)
# simulate reduction 
x <- arima.sim(list(order=c(1,0,0), ar=.9), n=n) 
reduction <- as.numeric((20*x + order)/n + 5)
# simulate before and after
before <- rlogis(n, location = 68, scale = 3) 
m <- (as.numeric(smoke)-1)*2.5 
after <- before - rnorm(n, mean = m, sd=0.1)
RcmdrTestDrive <- data.frame(order = order, race = race, smoke = smoke, gender = gender, salary = salary, reduction = reduction, before = before, after = after, parking = parking)
# clean up
remove(list = names(RcmdrTestDrive))
remove(x, n, m, s, tmp)


###################################################
### chunk number 4: 
###################################################
#line 278 "IPSUR.Rnw"
plot.htest <- function (x, hypoth.or.conf = 'Hypoth',...) { 
require(HH) 
if (x$method == "1-sample proportions test with continuity correction" || x$method == "1-sample proportions test without continuity correction"){
mu <- x$null.value
obs.mean <- x$estimate
n <- NA
std.dev <- abs(obs.mean - mu)/sqrt(x$statistic)
deg.freedom <- NA
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
}
} else if (x$method == "One Sample z-test"){
mu <- x$null.value
obs.mean <- x$estimate
n <- x$parameter[1]
std.dev <- x$parameter[2]
deg.freedom <- NA
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
} 
} else if (x$method == "One Sample t-test" || x$method == "Paired t-test"){
mu <- x$null.value
obs.mean <- x$estimate
n <- x$parameter + 1
std.dev <- x$estimate/x$statistic*sqrt(n)
deg.freedom <- x$parameter
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
}
} else if (x$method == "Welch Two Sample t-test"){
mu <- x$null.value
obs.mean <- -diff(x$estimate)
n <- x$parameter + 2
std.dev <- obs.mean/x$statistic*sqrt(n)
deg.freedom <- x$parameter
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
} 
} else if (x$method == " Two Sample t-test"){
mu <- x$null.value
obs.mean <- -diff(x$estimate)
n <- x$parameter + 2
std.dev <- obs.mean/x$statistic*sqrt(n)
deg.freedom <- x$parameter
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
}
}
return(normal.and.t.dist(mu.H0 = mu, obs.mean = obs.mean, std.dev = std.dev, n = n, deg.freedom = deg.freedom, alpha.right = alpha.right, Use.obs.mean = TRUE, Use.alpha.left = Use.alpha.left, Use.alpha.right = Use.alpha.right, hypoth.or.conf = hypoth.or.conf))
}


###################################################
### chunk number 5:  eval=FALSE
###################################################
## #line 646 "IPSUR.Rnw"
## install.packages("IPSUR")
## library(IPSUR)
## read(IPSUR)


###################################################
### chunk number 6: 
###################################################
#line 881 "IPSUR.Rnw"
getOption("defaultPackages")


###################################################
### chunk number 7: 
###################################################
#line 1071 "IPSUR.Rnw"
2 + 3       # add
4 * 5 / 6   # multiply and divide
7^8         # 7 to the 8th power


###################################################
### chunk number 8: 
###################################################
#line 1083 "IPSUR.Rnw"
options(digits = 16)
10/3                 # see more digits
sqrt(2)              # square root
exp(1)               # Euler's constant, e
pi       
options(digits = 7)  # back to default


###################################################
### chunk number 9: 
###################################################
#line 1112 "IPSUR.Rnw"
x <- 7*41/pi   # don't see the calculated value
x              # take a look


###################################################
### chunk number 10: five
###################################################
#line 1150 "IPSUR.Rnw"
sqrt(-1)              # isn't defined
sqrt(-1+0i)           # is defined
sqrt(as.complex(-1))  # same thing
(0 + 1i)^2            # should be -1
typeof((0 + 1i)^2)


###################################################
### chunk number 11: 
###################################################
#line 1177 "IPSUR.Rnw"
x <- c(74, 31, 95, 61, 76, 34, 23, 54, 96)
x


###################################################
### chunk number 12: 
###################################################
#line 1208 "IPSUR.Rnw"
seq(from = 1, to = 5)
seq(from = 2, by = -0.1, length.out = 4)


###################################################
### chunk number 13: 
###################################################
#line 1216 "IPSUR.Rnw"
1:5


###################################################
### chunk number 14: 
###################################################
#line 1233 "IPSUR.Rnw"
x[1]
x[2:4]
x[c(1,3,4,8)]
x[-c(1,3,4,8)]


###################################################
### chunk number 15: 
###################################################
#line 1243 "IPSUR.Rnw"
LETTERS[1:5]
letters[-(6:24)]



###################################################
### chunk number 16: 
###################################################
#line 1256 "IPSUR.Rnw"
x <- 1:5
sum(x)
length(x)
min(x)
mean(x)      # sample mean
sd(x)        # sample standard deviation


###################################################
### chunk number 17: 
###################################################
#line 1278 "IPSUR.Rnw"
intersect


###################################################
### chunk number 18: 
###################################################
#line 1289 "IPSUR.Rnw"
rev


###################################################
### chunk number 19: 
###################################################
#line 1297 "IPSUR.Rnw"
methods(rev)


###################################################
### chunk number 20: 
###################################################
#line 1310 "IPSUR.Rnw"
rev.default


###################################################
### chunk number 21: 
###################################################
#line 1320 "IPSUR.Rnw"
wilcox.test
methods(wilcox.test)


###################################################
### chunk number 22: 
###################################################
#line 1341 "IPSUR.Rnw"
exp


###################################################
### chunk number 23: 
###################################################
#line 1626 "IPSUR.Rnw"
str(precip)
precip[1:4]


###################################################
### chunk number 24: 
###################################################
#line 1647 "IPSUR.Rnw"
str(rivers)


###################################################
### chunk number 25: 
###################################################
#line 1666 "IPSUR.Rnw"
str(discoveries)
discoveries[1:4]


###################################################
### chunk number 26:  eval=FALSE
###################################################
## #line 1707 "IPSUR.Rnw"
## stripchart(precip, xlab="rainfall")
## stripchart(rivers, method="jitter", xlab="length")
## stripchart(discoveries, method="stack", xlab="number")


###################################################
### chunk number 27: 
###################################################
#line 1728 "IPSUR.Rnw"
par(mfrow = c(1,3)) # 3 plots: 1 row, 3 columns
stripchart(precip, xlab="rainfall")
stripchart(rivers, method="jitter", xlab="length")
stripchart(discoveries, method="stack", xlab="number")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 28:  eval=FALSE
###################################################
## #line 1777 "IPSUR.Rnw"
## hist(precip, main = "")
## hist(precip, freq = FALSE, main = "")


###################################################
### chunk number 29: 
###################################################
#line 1791 "IPSUR.Rnw"
par(mfrow = c(1,2)) # 2 plots: 1 row, 2 columns
hist(precip, main = "")
hist(precip, freq = FALSE, main = "")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 30:  eval=FALSE
###################################################
## #line 1824 "IPSUR.Rnw"
## hist(precip, breaks = 10, main = "")
## hist(precip, breaks = 200, main = "")


###################################################
### chunk number 31: 
###################################################
#line 1832 "IPSUR.Rnw"
par(mfrow = c(1,2)) # 2 plots: 1 row, 2 columns
hist(precip, breaks = 10, main = "")
hist(precip, breaks = 200, main = "")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 32: 
###################################################
#line 1879 "IPSUR.Rnw"
library(aplpack)
stem.leaf(UKDriverDeaths, depth = FALSE)


###################################################
### chunk number 33: 
###################################################
#line 1918 "IPSUR.Rnw"
plot(LakeHuron, type = "h")
plot(LakeHuron, type = "p")


###################################################
### chunk number 34: 
###################################################
#line 1929 "IPSUR.Rnw"
par(mfrow = c(2,1)) # 2 plots: 1 row, 2 columns
plot(LakeHuron, type = "h")
plot(LakeHuron, type = "p")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 35: 
###################################################
#line 1995 "IPSUR.Rnw"
str(state.abb)


###################################################
### chunk number 36: 
###################################################
#line 2012 "IPSUR.Rnw"
str(state.region)
state.region[1:5]


###################################################
### chunk number 37: 
###################################################
#line 2042 "IPSUR.Rnw"
Tbl <- table(state.division)
Tbl               # frequencies
Tbl/sum(Tbl)      # relative frequencies
prop.table(Tbl)   # same thing


###################################################
### chunk number 38:  eval=FALSE
###################################################
## #line 2066 "IPSUR.Rnw"
## barplot(table(state.region), cex.names = 0.50)
## barplot(prop.table(table(state.region)), cex.names = 0.50)


###################################################
### chunk number 39: 
###################################################
#line 2087 "IPSUR.Rnw"
par(mfrow = c(1,2)) # 2 plots: 1 row, 2 columns
barplot(table(state.region), cex.names = 0.50)
barplot(prop.table(table(state.region)), cex.names = 0.50)
par(mfrow = c(1,1))


###################################################
### chunk number 40:  eval=FALSE
###################################################
## #line 2125 "IPSUR.Rnw"
## library(qcc)
## pareto.chart(table(state.division), ylab="Frequency")


###################################################
### chunk number 41: 
###################################################
#line 2133 "IPSUR.Rnw"
library(qcc)
pareto.chart(table(state.division), ylab="Frequency")


###################################################
### chunk number 42:  eval=FALSE
###################################################
## #line 2157 "IPSUR.Rnw"
## x <- table(state.region)
## dotchart(as.vector(x), labels = names(x))


###################################################
### chunk number 43: 
###################################################
#line 2165 "IPSUR.Rnw"
x <- table(state.region)
dotchart(as.vector(x), labels = names(x))


###################################################
### chunk number 44: 
###################################################
#line 2203 "IPSUR.Rnw"
x <- 5:9
y <- (x < 7.3)
y


###################################################
### chunk number 45: 
###################################################
#line 2221 "IPSUR.Rnw"
!y


###################################################
### chunk number 46: 
###################################################
#line 2239 "IPSUR.Rnw"
x <- c(3, 7, NA, 4, 7)
y <- c(5, NA, 1, 2, 2)
x + y


###################################################
### chunk number 47: 
###################################################
#line 2254 "IPSUR.Rnw"
sum(x)
sum(x, na.rm = TRUE)


###################################################
### chunk number 48: 
###################################################
#line 2267 "IPSUR.Rnw"
is.na(x)
z <- x[!is.na(x)]
sum(z)


###################################################
### chunk number 49: 
###################################################
#line 2367 "IPSUR.Rnw"
library(aplpack)
stem.leaf(faithful$eruptions)


###################################################
### chunk number 50: 
###################################################
#line 2735 "IPSUR.Rnw"
library(e1071)
skewness(discoveries)
2*sqrt(6/length(discoveries))


###################################################
### chunk number 51: 
###################################################
#line 2745 "IPSUR.Rnw"
kurtosis(UKDriverDeaths)
4*sqrt(6/length(UKDriverDeaths))


###################################################
### chunk number 52: 
###################################################
#line 2829 "IPSUR.Rnw"
stem.leaf(rivers)


###################################################
### chunk number 53: 
###################################################
#line 2854 "IPSUR.Rnw"
stem.leaf(precip)


###################################################
### chunk number 54: 
###################################################
#line 2967 "IPSUR.Rnw"
boxplot.stats(rivers)$out


###################################################
### chunk number 55: 
###################################################
#line 2974 "IPSUR.Rnw"
boxplot.stats(rivers, coef = 3)$out


###################################################
### chunk number 56: 
###################################################
#line 3026 "IPSUR.Rnw"
x <- 5:8
y <- letters[3:6]
A <- data.frame(v1 = x, v2 = y)


###################################################
### chunk number 57: 
###################################################
#line 3049 "IPSUR.Rnw"
A[3,]
A[1, ]
A[ ,2]


###################################################
### chunk number 58: 
###################################################
#line 3067 "IPSUR.Rnw"
names(A)
A$v1


###################################################
### chunk number 59:  eval=FALSE
###################################################
## #line 3220 "IPSUR.Rnw"
## library(lattice)
## xyplot()


###################################################
### chunk number 60:  eval=FALSE
###################################################
## #line 3302 "IPSUR.Rnw"
## library(lattice)
## bwplot(~weight | feed, data = chickwts)


###################################################
### chunk number 61: 
###################################################
#line 3310 "IPSUR.Rnw"
library(lattice)
print(bwplot(~ weight | feed, data = chickwts))


###################################################
### chunk number 62:  eval=FALSE
###################################################
## #line 3327 "IPSUR.Rnw"
## histogram(~age | education, data = infert)


###################################################
### chunk number 63: 
###################################################
#line 3334 "IPSUR.Rnw"
library(lattice)
print(histogram(~age | education, data = infert))


###################################################
### chunk number 64:  eval=FALSE
###################################################
## #line 3349 "IPSUR.Rnw"
## xyplot(Petal.Length ~ Petal.Width | Species, data = iris)


###################################################
### chunk number 65: 
###################################################
#line 3356 "IPSUR.Rnw"
library(lattice)
print(xyplot(Petal.Length ~ Petal.Width | Species, data = iris))


###################################################
### chunk number 66:  eval=FALSE
###################################################
## #line 3371 "IPSUR.Rnw"
## coplot(conc ~ uptake | Type * Treatment, data = CO2)


###################################################
### chunk number 67: 
###################################################
#line 3378 "IPSUR.Rnw"
library(lattice)
print(coplot(conc ~ uptake | Type * Treatment, data = CO2))


###################################################
### chunk number 68: 
###################################################
#line 3411 "IPSUR.Rnw"
attach(RcmdrTestDrive)
names(RcmdrTestDrive)


###################################################
### chunk number 69: "Find summary statistics"
###################################################
#line 3438 "IPSUR.Rnw"
summary(RcmdrTestDrive)


###################################################
### chunk number 70: 
###################################################
#line 3460 "IPSUR.Rnw"
table(race)


###################################################
### chunk number 71: 
###################################################
#line 3471 "IPSUR.Rnw"
barplot(table(RcmdrTestDrive$race), main="", xlab="race", ylab="Frequency", legend.text=FALSE, col=NULL) 


###################################################
### chunk number 72: 
###################################################
#line 3508 "IPSUR.Rnw"
x <- tapply(salary, list(gender = gender), mean)
x


###################################################
### chunk number 73: 
###################################################
#line 3516 "IPSUR.Rnw"
by(salary, gender, mean, na.rm = TRUE)


###################################################
### chunk number 74: 
###################################################
#line 3537 "IPSUR.Rnw"
x[which(x==max(x))]


###################################################
### chunk number 75: 
###################################################
#line 3546 "IPSUR.Rnw"
y <- tapply(salary, list(gender = gender), sd)
y


###################################################
### chunk number 76: 
###################################################
#line 3562 "IPSUR.Rnw"
boxplot(salary~gender, xlab="salary", ylab="gender", main="", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 77: 
###################################################
#line 3599 "IPSUR.Rnw"
x = sort(reduction)


###################################################
### chunk number 78: 
###################################################
#line 3603 "IPSUR.Rnw"
x[137]
IQR(x)
fivenum(x)
fivenum(x)[4] - fivenum(x)[2]


###################################################
### chunk number 79: 
###################################################
#line 3619 "IPSUR.Rnw"
boxplot(reduction, xlab="reduction", main="", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 80: 
###################################################
#line 3624 "IPSUR.Rnw"
temp <- fivenum(x)
inF <- 1.5 * (temp[4] - temp[2]) + temp[4]
outF <- 3 * (temp[4] - temp[2]) + temp[4]
which(x > inF)
which(x > outF)


###################################################
### chunk number 81: 
###################################################
#line 3670 "IPSUR.Rnw"
c(mean(before), median(before))
c(mean(after), median(after))


###################################################
### chunk number 82: 
###################################################
#line 3690 "IPSUR.Rnw"
boxplot(before, xlab="before", main="", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 83: 
###################################################
#line 3706 "IPSUR.Rnw"
boxplot(after, xlab="after", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 84: 
###################################################
#line 3728 "IPSUR.Rnw"
sd(before)
mad(after)
IQR(after)/1.349


###################################################
### chunk number 85: 
###################################################
#line 3746 "IPSUR.Rnw"
library(e1071)
skewness(before)
kurtosis(before)


###################################################
### chunk number 86: 
###################################################
#line 3768 "IPSUR.Rnw"
skewness(after)
kurtosis(after)


###################################################
### chunk number 87: 
###################################################
#line 3785 "IPSUR.Rnw"
hist(before, xlab="before", data=RcmdrTestDrive) 


###################################################
### chunk number 88: 
###################################################
#line 3791 "IPSUR.Rnw"
hist(after, xlab="after", data=RcmdrTestDrive) 


###################################################
### chunk number 89: 
###################################################
#line 3846 "IPSUR.Rnw"
require(diagram)
par(mex = 0.2, cex = 0.5)
openplotmat(frame.plot=TRUE)
straightarrow(from = c(0.46,0.74), to = c(0.53,0.71), arr.pos = 1)
straightarrow(from = c(0.3,0.65), to = c(0.3,0.51), arr.pos = 1)
textellipse(mid = c(0.74,0.55), box.col = grey(0.95), radx = 0.24, rady = 0.22, lab = c(expression(bold(underline(DETERMINISTIC))), expression(2*H[2]+O[2] %->% H[2]*O), "3 + 4 = 7"), cex = 2 )
textrect(mid = c(0.3, 0.75), radx = 0.15, rady = 0.1, lab = c("Experiments"), cex = 2 )
textellipse(mid = c(0.29,0.25), box.col = grey(0.95), radx = 0.27, rady = 0.22, lab = c(expression(bold(underline(RANDOM))), "toss coin, roll die", "count ants on sidewalk", "measure rainfall" ), cex = 2 )


###################################################
### chunk number 90: 
###################################################
#line 3894 "IPSUR.Rnw"
S <- data.frame(lands = c("down","up","side"))
S


###################################################
### chunk number 91: 
###################################################
#line 3926 "IPSUR.Rnw"
library(prob)
tosscoin(1) 


###################################################
### chunk number 92: 
###################################################
#line 3935 "IPSUR.Rnw"
tosscoin(3) 


###################################################
### chunk number 93: 
###################################################
#line 3941 "IPSUR.Rnw"
rolldie(1) 


###################################################
### chunk number 94: 
###################################################
#line 3954 "IPSUR.Rnw"
head(cards()) 


###################################################
### chunk number 95: 
###################################################
#line 4026 "IPSUR.Rnw"
urnsamples(1:3, size = 2, replace = TRUE, ordered = TRUE)


###################################################
### chunk number 96: 
###################################################
#line 4044 "IPSUR.Rnw"
urnsamples(1:3, size = 2, replace = FALSE, ordered = TRUE)


###################################################
### chunk number 97: 
###################################################
#line 4063 "IPSUR.Rnw"
urnsamples(1:3, size = 2, replace = FALSE, ordered = FALSE) 


###################################################
### chunk number 98: 
###################################################
#line 4078 "IPSUR.Rnw"
urnsamples(1:3, size = 2, replace = TRUE, ordered = FALSE) 


###################################################
### chunk number 99: 
###################################################
#line 4141 "IPSUR.Rnw"
S <- tosscoin(2, makespace = TRUE) 
S[1:3, ] 
S[c(2,4), ] 


###################################################
### chunk number 100: 
###################################################
#line 4151 "IPSUR.Rnw"
S <- cards() 


###################################################
### chunk number 101: 
###################################################
#line 4155 "IPSUR.Rnw"
subset(S, suit == "Heart") 
subset(S, rank %in% 7:9)


###################################################
### chunk number 102: 
###################################################
#line 4163 "IPSUR.Rnw"
subset(rolldie(3), X1+X2+X3 > 16) 


###################################################
### chunk number 103: 
###################################################
#line 4184 "IPSUR.Rnw"
x <- 1:10 
y <- 8:12 
y %in% x


###################################################
### chunk number 104: 
###################################################
#line 4204 "IPSUR.Rnw"
isin(x,y) 


###################################################
### chunk number 105: 
###################################################
#line 4213 "IPSUR.Rnw"
x <- 1:10 
y <- c(3,3,7) 


###################################################
### chunk number 106: 
###################################################
#line 4218 "IPSUR.Rnw"
all(y %in% x)
isin(x,y) 


###################################################
### chunk number 107: 
###################################################
#line 4233 "IPSUR.Rnw"
isin(x, c(3,4,5), ordered = TRUE) 
isin(x, c(3,5,4), ordered = TRUE) 


###################################################
### chunk number 108: 
###################################################
#line 4244 "IPSUR.Rnw"
S <- rolldie(4) 
subset(S, isin(S, c(2,2,6), ordered = TRUE)) 


###################################################
### chunk number 109: 
###################################################
#line 4278 "IPSUR.Rnw"
S = cards() 
A = subset(S, suit == "Heart") 
B = subset(S, rank %in% 7:9)


###################################################
### chunk number 110: 
###################################################
#line 4286 "IPSUR.Rnw"
union(A,B) 
intersect(A,B) 
setdiff(A,B) 
setdiff(B,A) 


###################################################
### chunk number 111: 
###################################################
#line 4556 "IPSUR.Rnw"
outcomes <- rolldie(1) 
p <- rep(1/6, times = 6) 
probspace(outcomes, probs = p) 


###################################################
### chunk number 112: 
###################################################
#line 4568 "IPSUR.Rnw"
probspace(1:6, probs = p) 


###################################################
### chunk number 113: 
###################################################
#line 4577 "IPSUR.Rnw"
probspace(1:6) 


###################################################
### chunk number 114: 
###################################################
#line 4587 "IPSUR.Rnw"
rolldie(1, makespace = TRUE)


###################################################
### chunk number 115: 
###################################################
#line 4617 "IPSUR.Rnw"
probspace(tosscoin(1), probs = c(0.70, 0.30)) 


###################################################
### chunk number 116: 
###################################################
#line 4863 "IPSUR.Rnw"
S <- cards(makespace = TRUE) 
A <- subset(S, suit == "Heart") 
B <- subset(S, rank %in% 7:9)


###################################################
### chunk number 117: 
###################################################
#line 4871 "IPSUR.Rnw"
prob(A) 


###################################################
### chunk number 118: 
###################################################
#line 4877 "IPSUR.Rnw"
prob(S, suit == "Heart") 


###################################################
### chunk number 119: 
###################################################
#line 5077 "IPSUR.Rnw"
nsamp(n=3, k=2, replace = TRUE, ordered = TRUE) 
nsamp(n=3, k=2, replace = FALSE, ordered = TRUE) 
nsamp(n=3, k=2, replace = FALSE, ordered = FALSE) 
nsamp(n=3, k=2, replace = TRUE, ordered = FALSE) 


###################################################
### chunk number 120: 
###################################################
#line 5116 "IPSUR.Rnw"
n <- c(11,7,31) 
k <- c(3,4,3) 
r <- c(FALSE,FALSE,TRUE) 


###################################################
### chunk number 121: 
###################################################
#line 5122 "IPSUR.Rnw"
x <- nsamp(n, k, rep = r, ord = TRUE) 


###################################################
### chunk number 122: 
###################################################
#line 5135 "IPSUR.Rnw"
prod(x) 


###################################################
### chunk number 123: 
###################################################
#line 5141 "IPSUR.Rnw"
(11*10*9)*(7*6*5*4)*313 


###################################################
### chunk number 124: 
###################################################
#line 5147 "IPSUR.Rnw"
prod(9:11)*prod(4:7)*313 


###################################################
### chunk number 125: 
###################################################
#line 5153 "IPSUR.Rnw"
prod(factorial(c(11,7))/factorial(c(8,3)))*313 


###################################################
### chunk number 126: 
###################################################
#line 5229 "IPSUR.Rnw"
g <- Vectorize(pbirthday.ipsur)
plot(1:50, g(1:50), xlab = "Number of people in room", ylab = "Prob(at least one match)")
abline(h = 0.5)
abline(v = 23, lty = 2)
remove(g)


###################################################
### chunk number 127: 
###################################################
#line 5367 "IPSUR.Rnw"
library(prob)
S <- rolldie(2, makespace = TRUE)  # assumes ELM
head(S)                            #  first few rows


###################################################
### chunk number 128: 
###################################################
#line 5375 "IPSUR.Rnw"
A <- subset(S, X1 == X2)
B <- subset(S, X1 + X2 >= 8)


###################################################
### chunk number 129: 
###################################################
#line 5385 "IPSUR.Rnw"
prob(A, given = B)
prob(B, given = A)


###################################################
### chunk number 130: 
###################################################
#line 5395 "IPSUR.Rnw"
prob(S, X1==X2, given = (X1 + X2 >= 8) )
prob(S, X1+X2 >= 8, given = (X1==X2) )


###################################################
### chunk number 131: 
###################################################
#line 5481 "IPSUR.Rnw"
library(prob)
L <- cards()
M <- urnsamples(L, size = 2)
N <- probspace(M)


###################################################
### chunk number 132: 
###################################################
#line 5497 "IPSUR.Rnw"
prob(N, all(rank == "A"))


###################################################
### chunk number 133: 
###################################################
#line 5533 "IPSUR.Rnw"
library(prob)
L <- rep(c("red","green"), times = c(7,3))
M <- urnsamples(L, size = 3, replace = FALSE, ordered = TRUE)
N <- probspace(M)


###################################################
### chunk number 134: 
###################################################
#line 5553 "IPSUR.Rnw"
prob(N, isrep(N, "red", 3))


###################################################
### chunk number 135: 
###################################################
#line 5561 "IPSUR.Rnw"
prob(N, isrep(N, "red", 2))


###################################################
### chunk number 136: 
###################################################
#line 5571 "IPSUR.Rnw"
prob(N, isin(N, c("red","green","red"), ordered = TRUE))


###################################################
### chunk number 137: 
###################################################
#line 5581 "IPSUR.Rnw"
prob(N, isin(N, c("red","green","red")))


###################################################
### chunk number 138: 
###################################################
#line 5613 "IPSUR.Rnw"
.Table <- xtabs(~smoke+gender, data=RcmdrTestDrive)
addmargins(.Table) # Table with Marginal Distributions
remove(.Table)


###################################################
### chunk number 139: 
###################################################
#line 5734 "IPSUR.Rnw"
S <- tosscoin(10, makespace = TRUE)
A <- subset(S, isrep(S, vals = "T", nrep = 10))
1 - prob(A)


###################################################
### chunk number 140: 
###################################################
#line 5775 "IPSUR.Rnw"
iidspace(c("H","T"), ntrials = 3, probs = c(0.7, 0.3)) 


###################################################
### chunk number 141: 
###################################################
#line 5983 "IPSUR.Rnw"
prior <- c(0.6, 0.3, 0.1)
like <- c(0.003, 0.007, 0.010)
post <- prior * like
post / sum(post)


###################################################
### chunk number 142: 
###################################################
#line 6007 "IPSUR.Rnw"
newprior <- post
post <- newprior * like^7
post / sum(post)


###################################################
### chunk number 143: 
###################################################
#line 6031 "IPSUR.Rnw"
fastpost <- prior * like^8
fastpost / sum(fastpost)


###################################################
### chunk number 144: 
###################################################
#line 6127 "IPSUR.Rnw"
S <- rolldie(3, nsides = 4, makespace = TRUE) 
S <- addrv(S, U = X1-X2+X3) 


###################################################
### chunk number 145: 
###################################################
#line 6136 "IPSUR.Rnw"
head(S)


###################################################
### chunk number 146: 
###################################################
#line 6143 "IPSUR.Rnw"
prob(S, U > 6) 


###################################################
### chunk number 147: 
###################################################
#line 6162 "IPSUR.Rnw"
S <- addrv(S, FUN = max, invars = c("X1","X2","X3"), name = "V") 
S <- addrv(S, FUN = sum, invars = c("X1","X2","X3"), name = "W") 
head(S) 


###################################################
### chunk number 148: 
###################################################
#line 6193 "IPSUR.Rnw"
marginal(S, vars = "V") 


###################################################
### chunk number 149: 
###################################################
#line 6203 "IPSUR.Rnw"
marginal(S, vars = c("V", "W")) 


###################################################
### chunk number 150: 
###################################################
#line 6220 "IPSUR.Rnw"
rnorm(1)


###################################################
### chunk number 151: 
###################################################
#line 6385 "IPSUR.Rnw"
x <- c(0,1,2,3)
f <- c(1/8, 3/8, 3/8, 1/8)


###################################################
### chunk number 152: 
###################################################
#line 6396 "IPSUR.Rnw"
mu <- sum(x * f)
mu


###################################################
### chunk number 153: 
###################################################
#line 6407 "IPSUR.Rnw"
sigma2 <- sum((x-mu)^2 * f)
sigma2
sigma <- sqrt(sigma2)
sigma


###################################################
### chunk number 154: 
###################################################
#line 6418 "IPSUR.Rnw"
F = cumsum(f)
F


###################################################
### chunk number 155: 
###################################################
#line 6430 "IPSUR.Rnw"
library(distrEx)
X <- DiscreteDistribution(supp = 0:3, prob = c(1,3,3,1)/8)
E(X); var(X); sd(X)


###################################################
### chunk number 156: 
###################################################
#line 6596 "IPSUR.Rnw"
A <- data.frame(Pr=dbinom(0:4, size = 4, prob = 0.5))
rownames(A) <- 0:4 
A


###################################################
### chunk number 157: 
###################################################
#line 6623 "IPSUR.Rnw"
pbinom(9, size=12, prob=1/6) - pbinom(6, size=12, prob=1/6)
diff(pbinom(c(6,9), size = 12, prob = 1/6))  # same thing


###################################################
### chunk number 158: 
###################################################
#line 6672 "IPSUR.Rnw"
plot(0, xlim = c(-1.2, 4.2), ylim = c(-0.04, 1.04), type = "n", xlab = "number of successes", ylab = "cumulative probability")
abline(h = c(0,1), lty = 2, col = "grey")
lines(stepfun(0:3, pbinom(-1:3, size = 3, prob = 0.5)), verticals = FALSE, do.p = FALSE)
points(0:3, pbinom(0:3, size = 3, prob = 0.5), pch = 16, cex = 1.2)
points(0:3, pbinom(-1:2, size = 3, prob = 0.5), pch = 1, cex = 1.2)


###################################################
### chunk number 159: 
###################################################
#line 6699 "IPSUR.Rnw"
library(distr)
X <- Binom(size = 3, prob = 1/2)
X


###################################################
### chunk number 160: 
###################################################
#line 6712 "IPSUR.Rnw"
d(X)(1)   # pmf of X evaluated at x = 1
p(X)(2)   # cdf of X evaluated at x = 2


###################################################
### chunk number 161: 
###################################################
#line 6726 "IPSUR.Rnw"
plot(X, cex = 0.2)


###################################################
### chunk number 162: 
###################################################
#line 6936 "IPSUR.Rnw"
X <- Binom(size = 3, prob = 0.45)
library(distrEx)
E(X)
E(3*X + 4)


###################################################
### chunk number 163: 
###################################################
#line 6952 "IPSUR.Rnw"
var(X)
sd(X)


###################################################
### chunk number 164: 
###################################################
#line 7003 "IPSUR.Rnw"
x <- c(4, 7, 9, 11, 12)
ecdf(x)


###################################################
### chunk number 165:  eval=FALSE
###################################################
## #line 7018 "IPSUR.Rnw"
## plot(ecdf(x))


###################################################
### chunk number 166: 
###################################################
#line 7025 "IPSUR.Rnw"
plot(ecdf(x))


###################################################
### chunk number 167: 
###################################################
#line 7044 "IPSUR.Rnw"
epdf <- function(x) function(t){sum(x %in% t)/length(x)}
x <- c(0,0,1)
epdf(x)(0)       # should be 2/3


###################################################
### chunk number 168: 
###################################################
#line 7055 "IPSUR.Rnw"
x <- c(0,0,1)
sample(x, size = 7, replace = TRUE)


###################################################
### chunk number 169: 
###################################################
#line 7132 "IPSUR.Rnw"
dhyper(3, m = 17, n = 233, k = 5)


###################################################
### chunk number 170: 
###################################################
#line 7143 "IPSUR.Rnw"
A <- data.frame(Pr=dhyper(0:4, m = 17, n = 233, k = 5))
rownames(A) <- 0:4 
A


###################################################
### chunk number 171: 
###################################################
#line 7160 "IPSUR.Rnw"
dhyper(5, m = 17, n = 233, k = 5)


###################################################
### chunk number 172: 
###################################################
#line 7181 "IPSUR.Rnw"
phyper(2, m = 17, n = 233, k = 5)


###################################################
### chunk number 173: 
###################################################
#line 7199 "IPSUR.Rnw"
phyper(1, m = 17, n = 233, k = 5, lower.tail = FALSE)


###################################################
### chunk number 174: 
###################################################
#line 7254 "IPSUR.Rnw"
rhyper(10, m = 17, n = 233, k = 5)


###################################################
### chunk number 175: 
###################################################
#line 7330 "IPSUR.Rnw"
pgeom(4, prob = 0.812, lower.tail = FALSE)


###################################################
### chunk number 176: 
###################################################
#line 7382 "IPSUR.Rnw"
dnbinom(5, size = 7, prob = 0.5)


###################################################
### chunk number 177: 
###################################################
#line 7493 "IPSUR.Rnw"
diff(ppois(c(47, 50), lambda = 50))


###################################################
### chunk number 178: 
###################################################
#line 7608 "IPSUR.Rnw"
xmin <- qbinom(.0005, size=31 , prob=0.447) 
xmax <- qbinom(.9995, size=31 , prob=0.447) 
.x <- xmin:xmax 
plot(.x, dbinom(.x, size=31, prob=0.447), xlab="Number of Successes", ylab="Probability Mass",    main="Binomial Dist'n: Trials = 31, Prob of success = 0.447", type="h") 
points(.x, dbinom(.x, size=31, prob=0.447), pch=16) 
abline( h = 0, lty = 2, col = "grey" ) 
remove(.x, xmin, xmax)


###################################################
### chunk number 179: 
###################################################
#line 7623 "IPSUR.Rnw"
xmin <- qbinom(.0005, size=31 , prob=0.447) 
xmax <- qbinom(.9995, size=31 , prob=0.447) 
.x <- xmin:xmax 
plot( stepfun(.x, pbinom((xmin-1):xmax, size=31, prob=0.447)), verticals=FALSE, do.p=FALSE, xlab="Number of Successes", ylab="Cumulative Probability", main="Binomial Dist'n: Trials = 31, Prob of success = 0.447") 
points( .x, pbinom(xmin:xmax, size=31, prob=0.447), pch = 16, cex=1.2 ) 
points( .x, pbinom((xmin-1):(xmax-1), size=31, prob=0.447), pch = 1,    cex=1.2 ) 
abline( h = 1, lty = 2, col = "grey" ) 
abline( h = 0, lty = 2, col = "grey" ) 
remove(.x, xmin, xmax) 


###################################################
### chunk number 180: 
###################################################
#line 7639 "IPSUR.Rnw"
dbinom(17, size = 31, prob = 0.447)


###################################################
### chunk number 181: 
###################################################
#line 7646 "IPSUR.Rnw"
pbinom(13, size = 31, prob = 0.447)


###################################################
### chunk number 182: 
###################################################
#line 7653 "IPSUR.Rnw"
pbinom(11, size = 31, prob = 0.447, lower.tail = FALSE)


###################################################
### chunk number 183: 
###################################################
#line 7660 "IPSUR.Rnw"
pbinom(14, size = 31, prob = 0.447, lower.tail = FALSE)


###################################################
### chunk number 184: 
###################################################
#line 7667 "IPSUR.Rnw"
sum(dbinom(16:19, size = 31, prob = 0.447))
diff(pbinom(c(19,15), size = 31, prob = 0.447, lower.tail = FALSE))


###################################################
### chunk number 185: 
###################################################
#line 7675 "IPSUR.Rnw"
library(distrEx)
X = Binom(size = 31, prob = 0.447)
E(X)


###################################################
### chunk number 186: 
###################################################
#line 7684 "IPSUR.Rnw"
var(X)


###################################################
### chunk number 187: 
###################################################
#line 7691 "IPSUR.Rnw"
sd(X)


###################################################
### chunk number 188: 
###################################################
#line 7698 "IPSUR.Rnw"
E(4*X + 51.324)


###################################################
### chunk number 189: 
###################################################
#line 7704 "IPSUR.Rnw"
rnorm(1)


###################################################
### chunk number 190: 
###################################################
#line 7956 "IPSUR.Rnw"
f <- function(x) 3*x^2
integrate(f, lower = 0.14, upper = 0.71)


###################################################
### chunk number 191: 
###################################################
#line 7973 "IPSUR.Rnw"
g <- function(x) 3/x^3
integrate(g, lower = 1, upper = Inf)


###################################################
### chunk number 192: 
###################################################
#line 7990 "IPSUR.Rnw"
library(distr)
f <- function(x) 3*x^2
X <- AbscontDistribution(d = f, low1 = 0, up1 = 1)
p(X)(0.71) - p(X)(0.14)


###################################################
### chunk number 193: 
###################################################
#line 8001 "IPSUR.Rnw"
library(distrEx)
E(X)
var(X)
3/80


###################################################
### chunk number 194: 
###################################################
#line 8120 "IPSUR.Rnw"
pnorm(1:3)-pnorm(-(1:3))


###################################################
### chunk number 195: 
###################################################
#line 8160 "IPSUR.Rnw"
g <- function(x) pnorm(x, mean = 100, sd = 15) - 0.99
uniroot(g, interval = c(130, 145))


###################################################
### chunk number 196: 
###################################################
#line 8164 "IPSUR.Rnw"
temp <- round(uniroot(g, interval = c(130, 145))$root, 4)


###################################################
### chunk number 197: 
###################################################
#line 8235 "IPSUR.Rnw"
qnorm(0.99, mean = 100, sd = 15)


###################################################
### chunk number 198: 
###################################################
#line 8246 "IPSUR.Rnw"
qnorm(c(0.025, 0.01, 0.005), lower.tail = FALSE)


###################################################
### chunk number 199: 
###################################################
#line 8419 "IPSUR.Rnw"
library(distr)
X <- Norm(mean = 0, sd = 1)
Y <- 4 - 3*X
Y


###################################################
### chunk number 200: 
###################################################
#line 8435 "IPSUR.Rnw"
Y <- exp(X)
Y


###################################################
### chunk number 201: 
###################################################
#line 8462 "IPSUR.Rnw"
W <- sin(exp(X) + 27)
W


###################################################
### chunk number 202: 
###################################################
#line 8475 "IPSUR.Rnw"
p(W)(0.5)
W <- sin(exp(X) + 27)
p(W)(0.5)


###################################################
### chunk number 203:  eval=FALSE
###################################################
## #line 8600 "IPSUR.Rnw"
## curve(dchisq(x, df = 3), from = 0, to = 20, ylab = "y")
## ind <- c(4, 5, 10, 15)
## for (i in ind) curve(dchisq(x, df = i), 0, 20, add = TRUE)


###################################################
### chunk number 204: 
###################################################
#line 8609 "IPSUR.Rnw"
curve(dchisq(x, df = 3), from = 0, to = 20, ylab = "y")
ind <- c(4, 5, 10, 15)
for (i in ind) curve(dchisq(x, df = i), 0, 20, add = TRUE)


###################################################
### chunk number 205: 
###################################################
#line 8772 "IPSUR.Rnw"
library(actuar)
mgamma(1:4, shape = 13, rate = 1)


###################################################
### chunk number 206: 
###################################################
#line 8779 "IPSUR.Rnw"
plot(function(x){mgfgamma(x, shape = 13, rate = 1)}, from=-0.1, to=0.1, ylab = "gamma mgf")


###################################################
### chunk number 207: 
###################################################
#line 8786 "IPSUR.Rnw"
plot(function(x){mgfgamma(x, shape = 13, rate = 1)}, from=-0.1, to=0.1, ylab = "gamma mgf")


###################################################
### chunk number 208: 
###################################################
#line 8818 "IPSUR.Rnw"
rnorm(1)


###################################################
### chunk number 209: 
###################################################
#line 8845 "IPSUR.Rnw"
pnorm(2.64, lower.tail = FALSE)


###################################################
### chunk number 210: 
###################################################
#line 8852 "IPSUR.Rnw"
pnorm(0.87) - 1/2


###################################################
### chunk number 211: 
###################################################
#line 8859 "IPSUR.Rnw"
2 * pnorm(-1.39)


###################################################
### chunk number 212: 
###################################################
#line 9174 "IPSUR.Rnw"
S <- rolldie(2, makespace = TRUE)
S <- addrv(S, FUN = max, invars = c("X1","X2"), name = "U")
S <- addrv(S, FUN = sum, invars = c("X1","X2"), name = "V")
head(S)


###################################################
### chunk number 213: 
###################################################
#line 9192 "IPSUR.Rnw"
UV <- marginal(S, vars = c("U", "V"))
head(UV)


###################################################
### chunk number 214: 
###################################################
#line 9202 "IPSUR.Rnw"
xtabs(round(probs,3) ~ U + V, data = UV)


###################################################
### chunk number 215: 
###################################################
#line 9210 "IPSUR.Rnw"
marginal(UV, vars = "U")
head(marginal(UV, vars = "V"))


###################################################
### chunk number 216: 
###################################################
#line 9220 "IPSUR.Rnw"
temp <- xtabs(probs ~ U + V, data = UV)
rowSums(temp)
colSums(temp)


###################################################
### chunk number 217: 
###################################################
#line 9314 "IPSUR.Rnw"
Eu <- sum(S$U*S$probs)
Ev <- sum(S$V*S$probs)
Euv <- sum(S$U*S$V*S$probs)
Euv - Eu * Ev


###################################################
### chunk number 218:  eval=FALSE
###################################################
## #line 9720 "IPSUR.Rnw"
## library(mvtnorm)
## x <- y <- seq(from = -3, to = 3, length.out = 30)
## f <- function(x,y) dmvnorm(cbind(x,y), mean = c(0,0), sigma = diag(2))
## z <- outer(x, y, FUN = f)
## persp(x, y, z, theta = -30, phi = 30, ticktype = "detailed")


###################################################
### chunk number 219: 
###################################################
#line 9734 "IPSUR.Rnw"
library(mvtnorm)
x <- y <- seq(from = -3, to = 3, length.out = 30)
f <- function(x,y) dmvnorm(cbind(x,y), mean = c(0,0), sigma = diag(2))
z <- outer(x, y, FUN = f)
persp(x, y, z, theta = -30, phi = 30, ticktype = "detailed")


###################################################
### chunk number 220: 
###################################################
#line 10055 "IPSUR.Rnw"
library(combinat)
tmp <- t(xsimplex(3, 6))
p <- apply(tmp, MARGIN = 1, FUN = dmultinom, prob = c(36,27,37))
library(prob)
S <- probspace(tmp, probs = p)
ProbTable <- xtabs(probs ~ X1 + X2, data = S)
round(ProbTable, 3)


###################################################
### chunk number 221: 
###################################################
#line 10095 "IPSUR.Rnw"
library(lattice)
print(cloud(probs ~ X1 + X2, data = S, type = c("p","h"), lwd = 2, pch = 16, cex = 1.5), screen = list(z = 15, x = -70))


###################################################
### chunk number 222:  eval=FALSE
###################################################
## #line 10347 "IPSUR.Rnw"
## curve(dt(x, df = 30), from = -3, to = 3, lwd = 3, ylab = "y")
## ind <- c(1, 2, 3, 5, 10)
## for (i in ind) curve(dt(x, df = i), -3, 3, add = TRUE)


###################################################
### chunk number 223: 
###################################################
#line 10356 "IPSUR.Rnw"
curve(dt(x, df = 30), from = -3, to = 3, lwd = 3, ylab = "y")
ind <- c(1, 2, 3, 5, 10)
for (i in ind) curve(dt(x, df = i), -3, 3, add = TRUE)


###################################################
### chunk number 224: 
###################################################
#line 10374 "IPSUR.Rnw"
qt(0.01, df = 23, lower.tail = FALSE)


###################################################
### chunk number 225:  eval=FALSE
###################################################
## #line 10456 "IPSUR.Rnw"
## library(TeachingDemos)
## example(clt.examp)


###################################################
### chunk number 226:  eval=FALSE
###################################################
## #line 10463 "IPSUR.Rnw"
## library(distrTeach)
## example(illustrateCLT)


###################################################
### chunk number 227: 
###################################################
#line 10644 "IPSUR.Rnw"
iqrs <- replicate(100, IQR(rnorm(100)))


###################################################
### chunk number 228: 
###################################################
#line 10650 "IPSUR.Rnw"
mean(iqrs)    # close to 1


###################################################
### chunk number 229: 
###################################################
#line 10656 "IPSUR.Rnw"
sd(iqrs)


###################################################
### chunk number 230: 
###################################################
#line 10665 "IPSUR.Rnw"
hist(iqrs, breaks = 20)


###################################################
### chunk number 231: 
###################################################
#line 10678 "IPSUR.Rnw"
mads <- replicate(100, mad(rnorm(100)))


###################################################
### chunk number 232: 
###################################################
#line 10684 "IPSUR.Rnw"
mean(mads)    # close to 1.349


###################################################
### chunk number 233: 
###################################################
#line 10690 "IPSUR.Rnw"
sd(mads)


###################################################
### chunk number 234: 
###################################################
#line 10699 "IPSUR.Rnw"
hist(mads, breaks = 20)


###################################################
### chunk number 235: 
###################################################
#line 10717 "IPSUR.Rnw"
k = 1
n = sample(10:30, size=10, replace = TRUE)
mu = round(rnorm(10, mean = 20))


###################################################
### chunk number 236: 
###################################################
#line 10831 "IPSUR.Rnw"
pnorm(43.1, mean = 37, sd = 9, lower.tail = FALSE)


###################################################
### chunk number 237: 
###################################################
#line 10936 "IPSUR.Rnw"
heights = rep(0, 16)
for (j in 7:15) heights[j] <- dhyper(3, m = 7, n = j - 7, k = 4)
plot(6:15, heights[6:15], pch = 16, cex = 1.5, xlab = "number of fish in pond", ylab = "Likelihood")
abline(h = 0)
lines(6:15, heights[6:15], type = "h", lwd = 2, lty = 3)
text(9, heights[9]/6, bquote(hat(F)==.(9)), cex = 2, pos = 4)
lines(9, heights[9], type = "h", lwd = 2)
points(9, 0, pch = 4, lwd = 3, cex = 2) 


###################################################
### chunk number 238:  eval=FALSE
###################################################
## #line 10995 "IPSUR.Rnw"
## curve(x^5*(1-x)^2, from = 0, to = 1, xlab = "p", ylab = "L(p)")
## curve(x^4*(1-x)^3, from = 0, to = 1, add = TRUE)
## curve(x^3*(1-x)^4, 0, 1, add = TRUE)


###################################################
### chunk number 239: 
###################################################
#line 11004 "IPSUR.Rnw"
curve(x^5*(1-x)^2, 0, 1, xlab = "p", ylab = "L(p)")
curve(x^4*(1-x)^3, 0, 1, add = TRUE)
curve(x^3*(1-x)^4, 0, 1, add = TRUE)


###################################################
### chunk number 240: 
###################################################
#line 11056 "IPSUR.Rnw"
dat <- rbinom(27, size = 1, prob = 0.3)
like <- function(x){
r <- 1
for (k in 1:27){ r <- r*dbinom(dat[k], size = 1, prob = x)}
return(r)
}
curve(like, from = 0, to = 1, xlab = "parameter space", ylab = "Likelihood", lwd = 3, col = "blue")
abline(h = 0, lwd = 1, lty = 3, col = "grey")
mle <- mean(dat)
mleobj <- like(mle)
lines(mle, mleobj, type = "h", lwd = 2, lty = 3, col = "red")
points(mle, 0, pch = 4, lwd = 2, cex = 2, col = "red")
text(mle, mleobj/6, substitute(hat(theta)==a, list(a=round(mle, 4))), cex = 2, pos = 4)


###################################################
### chunk number 241: 
###################################################
#line 11190 "IPSUR.Rnw"
x <- mtcars$am
L <- function(p,x) prod(dbinom(x, size = 1, prob = p))
optimize(L, interval = c(0,1), x = x, maximum = TRUE)


###################################################
### chunk number 242: 
###################################################
#line 11196 "IPSUR.Rnw"
A <- optimize(L, interval = c(0,1), x = x, maximum = TRUE)


###################################################
### chunk number 243: 
###################################################
#line 11216 "IPSUR.Rnw"
minuslogL <- function(p,x) -sum(dbinom(x, size = 1, prob = p, log = TRUE))
optimize(minuslogL, interval = c(0,1), x = x)


###################################################
### chunk number 244: 
###################################################
#line 11241 "IPSUR.Rnw"
minuslogL <- function(mu, sigma2){
  -sum(dnorm(x, mean = mu, sd = sqrt(sigma2), log = TRUE))
}


###################################################
### chunk number 245: 
###################################################
#line 11255 "IPSUR.Rnw"
x <- PlantGrowth$weight
library(stats4)
MaxLikeEst <- mle(minuslogL, start = list(mu = 5, sigma2 = 0.5))
summary(MaxLikeEst)


###################################################
### chunk number 246: 
###################################################
#line 11269 "IPSUR.Rnw"
mean(x)
var(x)*29/30
sd(x)/sqrt(30)


###################################################
### chunk number 247: 
###################################################
#line 11350 "IPSUR.Rnw"
set.seed(seed + 1)
library(TeachingDemos)
ci.examp()


###################################################
### chunk number 248: 
###################################################
#line 11409 "IPSUR.Rnw"
library(aplpack)
with(PlantGrowth, stem.leaf(weight))


###################################################
### chunk number 249: 
###################################################
#line 11423 "IPSUR.Rnw"
dim(PlantGrowth)   # sample size is first entry
with(PlantGrowth, mean(weight))
qnorm(0.975)


###################################################
### chunk number 250: 
###################################################
#line 11441 "IPSUR.Rnw"
library(TeachingDemos)
plot(z.test(PlantGrowth$weight, stdev = 0.70), "Conf")


###################################################
### chunk number 251: 
###################################################
#line 11548 "IPSUR.Rnw"
library(TeachingDemos)
temp <- with(PlantGrowth, z.test(weight, stdev = 0.7))
temp


###################################################
### chunk number 252:  eval=FALSE
###################################################
## #line 11559 "IPSUR.Rnw"
## library(IPSUR)
## plot(temp, "Conf")


###################################################
### chunk number 253: 
###################################################
#line 11716 "IPSUR.Rnw"
library(Hmisc)
binconf(x = 7, n = 25, method = "asymptotic")
binconf(x = 7, n = 25, method = "wilson")


###################################################
### chunk number 254: 
###################################################
#line 11727 "IPSUR.Rnw"
tab <- xtabs(~gender, data = RcmdrTestDrive)
prop.test(rbind(tab), conf.level = 0.95, correct = FALSE)


###################################################
### chunk number 255: 
###################################################
#line 11732 "IPSUR.Rnw"
A <- as.data.frame(Titanic)
library(reshape)
B <- with(A, untable(A, Freq))


###################################################
### chunk number 256: 
###################################################
#line 11941 "IPSUR.Rnw"
dhyper(0, m = 26, n = 26, k = 5)


###################################################
### chunk number 257: 
###################################################
#line 12068 "IPSUR.Rnw"
- qnorm(0.99)


###################################################
### chunk number 258: 
###################################################
#line 12077 "IPSUR.Rnw"
A <- as.data.frame(UCBAdmissions)
head(A)
xtabs(Freq ~ Admit, data = A)


###################################################
### chunk number 259: 
###################################################
#line 12085 "IPSUR.Rnw"
phat <- 1755/(1755 + 2771)
(phat - 0.4)/sqrt(0.4 * 0.6/(1755 + 2771)) 


###################################################
### chunk number 260: 
###################################################
#line 12109 "IPSUR.Rnw"
-qnorm(0.95)


###################################################
### chunk number 261: 
###################################################
#line 12160 "IPSUR.Rnw"
pnorm(-1.680919)


###################################################
### chunk number 262: 
###################################################
#line 12193 "IPSUR.Rnw"
prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less", conf.level = 0.99, correct = FALSE)


###################################################
### chunk number 263:  eval=FALSE
###################################################
## #line 12199 "IPSUR.Rnw"
## library(IPSUR)
## library(HH)
## temp <- prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less", conf.level = 0.99, correct = FALSE)
## plot(temp, 'Hypoth')


###################################################
### chunk number 264: 
###################################################
#line 12209 "IPSUR.Rnw"
library(HH)
plot(prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less", conf.level = 0.99, correct = FALSE), 'Hypoth')


###################################################
### chunk number 265: 
###################################################
#line 12311 "IPSUR.Rnw"
x <- rnorm(37, mean = 2, sd = 3)
library(TeachingDemos)
z.test(x, mu = 1, sd = 3, conf.level = 0.90)


###################################################
### chunk number 266: 
###################################################
#line 12320 "IPSUR.Rnw"
library(HH)
plot(prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less", conf.level = 0.99, correct = FALSE), 'Hypoth')


###################################################
### chunk number 267: 
###################################################
#line 12340 "IPSUR.Rnw"
x <- rnorm(13, mean = 2, sd = 3)
t.test(x, mu = 0, conf.level = 0.90, alternative = "greater")


###################################################
### chunk number 268: 
###################################################
#line 12375 "IPSUR.Rnw"
library(TeachingDemos)
sigma.test(women$height, sigma = 8)


###################################################
### chunk number 269: 
###################################################
#line 12451 "IPSUR.Rnw"
t.test(extra ~ group, data = sleep, paired = TRUE)


###################################################
### chunk number 270: 
###################################################
#line 12464 "IPSUR.Rnw"
ks.test(randu$x, "punif")


###################################################
### chunk number 271: 
###################################################
#line 12474 "IPSUR.Rnw"
shapiro.test(women$height)


###################################################
### chunk number 272: 
###################################################
#line 12486 "IPSUR.Rnw"
with(chickwts, by(weight, feed, shapiro.test))


###################################################
### chunk number 273: 
###################################################
#line 12492 "IPSUR.Rnw"
temp <- lm(weight ~ feed, data = chickwts)


###################################################
### chunk number 274: 
###################################################
#line 12498 "IPSUR.Rnw"
anova(temp)


###################################################
### chunk number 275: 
###################################################
#line 12507 "IPSUR.Rnw"
y1 <- rnorm(300, mean = c(2,8,22))
plot(y1, xlim = c(-1,25), ylim = c(0,0.45) , type = "n")
f <- function(x){dnorm(x, mean = 2)}
curve(f, from = -1, to = 5, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 8)}
curve(f, from = 5, to = 11, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 22)}
curve(f, from = 19, to = 25, add = TRUE, lwd = 2)
rug(y1)


###################################################
### chunk number 276: 
###################################################
#line 12532 "IPSUR.Rnw"
y2 <- rnorm(300, mean = c(4,4.1,4.3))
hist(y2, 30, prob = TRUE)
f <- function(x){dnorm(x, mean = 4)/3}
curve(f, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 4.1)/3}
curve(f, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 4.3)/3}
curve(f, add = TRUE, lwd = 2)


###################################################
### chunk number 277: 
###################################################
#line 12550 "IPSUR.Rnw"
library(HH)
old.omd <- par(omd = c(.05,.88, .05,1))
F.setup(df1 = 5, df2 = 30)
F.curve(df1 = 5, df2 = 30, col='blue')
F.observed(3, df1 = 5, df2 = 30)
par(old.omd)


###################################################
### chunk number 278: 
###################################################
#line 12597 "IPSUR.Rnw"
library(TeachingDemos)
power.examp()


###################################################
### chunk number 279: 
###################################################
#line 12754 "IPSUR.Rnw"
 # open window
plot(c(0,5), c(0,6.5), type = "n", xlab="x", ylab="y")
## the x- and y-axes
abline(h=0, v=0, col = "gray60")
# regression line
abline(a = 2.5, b = 0.5, lwd = 2)
# normal curves
x <- 600:3000/600
y <- dnorm(x, mean = 3, sd = 0.5)
lines(y + 1.0, x)
lines(y + 2.5, x + 0.75)
lines(y + 4.0, x + 1.5)
# pretty it up
abline(v = c(1, 2.5, 4), lty = 2, col = "grey")
segments(1,3, 1+dnorm(0,0,0.5),3, lty = 2, col = "gray")
segments(2.5, 3.75, 2.5+dnorm(0,0,0.5), 3.75, lty = 2, col = "gray")
segments(4,4.5, 4+dnorm(0,0,0.5),4.5, lty = 2, col = "gray")


###################################################
### chunk number 280: 
###################################################
#line 12787 "IPSUR.Rnw"
head(cars)


###################################################
### chunk number 281: 
###################################################
#line 12800 "IPSUR.Rnw"
plot(dist ~ speed, data = cars)


###################################################
### chunk number 282:  eval=FALSE
###################################################
## #line 12815 "IPSUR.Rnw"
## plot(dist ~ speed, data = cars)


###################################################
### chunk number 283: 
###################################################
#line 12895 "IPSUR.Rnw"
cars.lm <- lm(dist ~ speed, data = cars)


###################################################
### chunk number 284: 
###################################################
#line 12914 "IPSUR.Rnw"
coef(cars.lm)


###################################################
### chunk number 285: 
###################################################
#line 12931 "IPSUR.Rnw"
plot(dist ~ speed, data = cars, pch = 16)
abline(coef(cars.lm))


###################################################
### chunk number 286:  eval=FALSE
###################################################
## #line 12942 "IPSUR.Rnw"
## plot(dist ~ speed, data = cars, pch = 16)
## abline(coef(cars))


###################################################
### chunk number 287: 
###################################################
#line 13016 "IPSUR.Rnw"
cars[5, ]


###################################################
### chunk number 288: 
###################################################
#line 13049 "IPSUR.Rnw"
fitted(cars.lm)[1:5]


###################################################
### chunk number 289: 
###################################################
#line 13063 "IPSUR.Rnw"
predict(cars.lm, newdata = data.frame(speed = c(6, 8, 21)))


###################################################
### chunk number 290: 
###################################################
#line 13099 "IPSUR.Rnw"
residuals(cars.lm)[1:5]


###################################################
### chunk number 291: 
###################################################
#line 13121 "IPSUR.Rnw"
carsumry <- summary(cars.lm)
carsumry$sigma


###################################################
### chunk number 292: 
###################################################
#line 13180 "IPSUR.Rnw"
summary(cars.lm)


###################################################
### chunk number 293: 
###################################################
#line 13183 "IPSUR.Rnw"
A <- round(summary(cars.lm)$coef, 3)
B <- round(confint(cars.lm), 3)


###################################################
### chunk number 294: 
###################################################
#line 13199 "IPSUR.Rnw"
confint(cars.lm)


###################################################
### chunk number 295: 
###################################################
#line 13271 "IPSUR.Rnw"
new <- data.frame(speed = c(5, 6, 21))


###################################################
### chunk number 296: 
###################################################
#line 13278 "IPSUR.Rnw"
predict(cars.lm, newdata = new, interval = "confidence")


###################################################
### chunk number 297: 
###################################################
#line 13282 "IPSUR.Rnw"
carsCI <- round(predict(cars.lm, newdata = new, interval = "confidence"), 2)


###################################################
### chunk number 298: 
###################################################
#line 13288 "IPSUR.Rnw"
predict(cars.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 299: 
###################################################
#line 13292 "IPSUR.Rnw"
carsPI <- round(predict(cars.lm, newdata = new, interval = "prediction"), 2)


###################################################
### chunk number 300: 
###################################################
#line 13346 "IPSUR.Rnw"
library(HH)
print(ci.plot(cars.lm))


###################################################
### chunk number 301:  eval=FALSE
###################################################
## #line 13358 "IPSUR.Rnw"
## library(HH)
## ci.plot(cars.lm)


###################################################
### chunk number 302: 
###################################################
#line 13404 "IPSUR.Rnw"
summary(cars.lm)


###################################################
### chunk number 303: 
###################################################
#line 13407 "IPSUR.Rnw"
A <- round(summary(cars.lm)$coef, 3)
B <- round(confint(cars.lm), 3)


###################################################
### chunk number 304: 
###################################################
#line 13494 "IPSUR.Rnw"
anova(cars.lm)


###################################################
### chunk number 305: 
###################################################
#line 13508 "IPSUR.Rnw"
carsumry$r.squared


###################################################
### chunk number 306: 
###################################################
#line 13521 "IPSUR.Rnw"
sqrt(carsumry$r.squared)


###################################################
### chunk number 307: 
###################################################
#line 13563 "IPSUR.Rnw"
anova(cars.lm)


###################################################
### chunk number 308: 
###################################################
#line 13620 "IPSUR.Rnw"
plot(cars.lm, which = 2)


###################################################
### chunk number 309: 
###################################################
#line 13657 "IPSUR.Rnw"
shapiro.test(residuals(cars.lm))


###################################################
### chunk number 310: 
###################################################
#line 13708 "IPSUR.Rnw"
plot(cars.lm, which = 3)


###################################################
### chunk number 311: 
###################################################
#line 13743 "IPSUR.Rnw"
library(lmtest)
bptest(cars.lm)


###################################################
### chunk number 312: 
###################################################
#line 13777 "IPSUR.Rnw"
plot(cars.lm, which = 1)


###################################################
### chunk number 313: 
###################################################
#line 13807 "IPSUR.Rnw"
library(lmtest)
dwtest(cars.lm, alternative = "two.sided")


###################################################
### chunk number 314: 
###################################################
#line 13951 "IPSUR.Rnw"
sres <- rstandard(cars.lm)
sres[1:5]


###################################################
### chunk number 315: 
###################################################
#line 13959 "IPSUR.Rnw"
sres[which(abs(sres) > 2)]


###################################################
### chunk number 316: 
###################################################
#line 13968 "IPSUR.Rnw"
sdelres <- rstudent(cars.lm)
sdelres[1:5]


###################################################
### chunk number 317: 
###################################################
#line 13977 "IPSUR.Rnw"
t0.005 <- qt(0.005, df = 47, lower.tail = FALSE)
sdelres[which(abs(sdelres) > t0.005)]


###################################################
### chunk number 318: 
###################################################
#line 13986 "IPSUR.Rnw"
leverage <- hatvalues(cars.lm)
leverage[1:5]
leverage[which(leverage > 4/50)]


###################################################
### chunk number 319: 
###################################################
#line 14030 "IPSUR.Rnw"
dfb <- dfbetas(cars.lm)
head(dfb)


###################################################
### chunk number 320: 
###################################################
#line 14052 "IPSUR.Rnw"
dff <- dffits(cars.lm)
dff[1:5]


###################################################
### chunk number 321: 
###################################################
#line 14090 "IPSUR.Rnw"
cooksD <- cooks.distance(cars.lm)
cooksD[1:5]


###################################################
### chunk number 322: 
###################################################
#line 14100 "IPSUR.Rnw"
plot(cars.lm, which = 4)


###################################################
### chunk number 323: 
###################################################
#line 14118 "IPSUR.Rnw"
F0.50 <- qf(0.5, df1 = 2, df2 = 48)
cooksD[which(cooksD > F0.50)]


###################################################
### chunk number 324:  eval=FALSE
###################################################
## #line 14133 "IPSUR.Rnw"
## influence.measures(cars.lm)


###################################################
### chunk number 325:  eval=FALSE
###################################################
## #line 14144 "IPSUR.Rnw"
## par(mfrow = c(2,2))
## plot(cars.lm)
## par(mfrow = c(1,1))


###################################################
### chunk number 326: 
###################################################
#line 14158 "IPSUR.Rnw"
par(mfrow = c(2,2))
plot(cars.lm)
par(mfrow = c(1,1))


###################################################
### chunk number 327:  eval=FALSE
###################################################
## #line 14183 "IPSUR.Rnw"
## plot(cars.lm, which = 5)   # std'd resids vs lev plot
## identify(leverage, sres, n = 4)   # identify 4 points


###################################################
### chunk number 328: 
###################################################
#line 14307 "IPSUR.Rnw"
head(trees)


###################################################
### chunk number 329: 
###################################################
#line 14321 "IPSUR.Rnw"
library(lattice)
print(splom(trees))


###################################################
### chunk number 330:  eval=FALSE
###################################################
## #line 14332 "IPSUR.Rnw"
## library(lattice)
## splom(trees)


###################################################
### chunk number 331:  eval=FALSE
###################################################
## #line 14384 "IPSUR.Rnw"
## library(scatterplot3d)
## s3d <- with(trees, scatterplot3d(Girth, Height, Volume, pch = 16, highlight.3d = TRUE, angle = 60))
## fit <- lm(Volume ~ Girth + Height, data = trees)
## s3d$plane3d(fit)


###################################################
### chunk number 332: 
###################################################
#line 14394 "IPSUR.Rnw"
library(scatterplot3d)
s3d <- with(trees, scatterplot3d(Girth, Height, Volume, pch = 16, highlight.3d = TRUE, angle = 60))
fit <- lm(Volume ~ Girth + Height, data = trees)
s3d$plane3d(fit)


###################################################
### chunk number 333: 
###################################################
#line 14471 "IPSUR.Rnw"
trees.lm <- lm(Volume ~ Girth + Height, data = trees)
trees.lm


###################################################
### chunk number 334: 
###################################################
#line 14486 "IPSUR.Rnw"
head(model.matrix(trees.lm))


###################################################
### chunk number 335: 
###################################################
#line 14556 "IPSUR.Rnw"
fitted(trees.lm)[1:5]


###################################################
### chunk number 336: 
###################################################
#line 14573 "IPSUR.Rnw"
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))


###################################################
### chunk number 337: 
###################################################
#line 14579 "IPSUR.Rnw"
new


###################################################
### chunk number 338: 
###################################################
#line 14585 "IPSUR.Rnw"
predict(trees.lm, newdata = new)


###################################################
### chunk number 339: 
###################################################
#line 14589 "IPSUR.Rnw"
treesFIT <- round(predict(trees.lm, newdata = new), 1)


###################################################
### chunk number 340: 
###################################################
#line 14646 "IPSUR.Rnw"
residuals(trees.lm)[1:5]


###################################################
### chunk number 341: 
###################################################
#line 14656 "IPSUR.Rnw"
treesumry <- summary(trees.lm)
treesumry$sigma


###################################################
### chunk number 342: 
###################################################
#line 14714 "IPSUR.Rnw"
confint(trees.lm)


###################################################
### chunk number 343: 
###################################################
#line 14718 "IPSUR.Rnw"
treesPAR <- round(confint(trees.lm), 1)


###################################################
### chunk number 344: 
###################################################
#line 14765 "IPSUR.Rnw"
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))


###################################################
### chunk number 345: 
###################################################
#line 14771 "IPSUR.Rnw"
predict(trees.lm, newdata = new, interval = "confidence")


###################################################
### chunk number 346: 
###################################################
#line 14775 "IPSUR.Rnw"
treesCI <- round(predict(trees.lm, newdata = new, interval = "confidence"), 1)


###################################################
### chunk number 347: 
###################################################
#line 14781 "IPSUR.Rnw"
predict(trees.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 348: 
###################################################
#line 14785 "IPSUR.Rnw"
treesPI <- round(predict(trees.lm, newdata = new, interval = "prediction"), 1)


###################################################
### chunk number 349: 
###################################################
#line 14881 "IPSUR.Rnw"
treesumry$r.squared
treesumry$adj.r.squared


###################################################
### chunk number 350: 
###################################################
#line 14915 "IPSUR.Rnw"
treesumry$fstatistic


###################################################
### chunk number 351: 
###################################################
#line 14966 "IPSUR.Rnw"
treesumry


###################################################
### chunk number 352: 
###################################################
#line 14996 "IPSUR.Rnw"
plot(Volume ~ Girth, data = trees)


###################################################
### chunk number 353: 
###################################################
#line 15083 "IPSUR.Rnw"
treesquad.lm <- lm(Volume ~ scale(Girth) + I(scale(Girth)^2), data = trees)
summary(treesquad.lm)


###################################################
### chunk number 354:  eval=FALSE
###################################################
## #line 15099 "IPSUR.Rnw"
## plot(Volume ~ scale(Girth), data = trees)
## lines(fitted(treesquad.lm) ~ scale(Girth), data = trees)


###################################################
### chunk number 355: 
###################################################
#line 15113 "IPSUR.Rnw"
plot(Volume ~ scale(Girth), data = trees)
lines(fitted(treesquad.lm) ~ scale(Girth), data = trees)


###################################################
### chunk number 356: 
###################################################
#line 15136 "IPSUR.Rnw"
new <- data.frame(Girth = c(9.1, 11.6, 12.5))
predict(treesquad.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 357: 
###################################################
#line 15150 "IPSUR.Rnw"
summary(lm(Volume ~ Girth + I(Girth^2), data = trees))


###################################################
### chunk number 358: 
###################################################
#line 15232 "IPSUR.Rnw"
treesint.lm <- lm(Volume ~ Girth + Height + Girth:Height, data = trees)
summary(treesint.lm)


###################################################
### chunk number 359: 
###################################################
#line 15246 "IPSUR.Rnw"
confint(treesint.lm)
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))
predict(treesint.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 360: 
###################################################
#line 15297 "IPSUR.Rnw"
trees$Tall <- cut(trees$Height, breaks = c(-Inf, 76, Inf), labels = c("no","yes"))
trees$Tall[1:5]


###################################################
### chunk number 361: 
###################################################
#line 15339 "IPSUR.Rnw"
class(trees$Tall)


###################################################
### chunk number 362: 
###################################################
#line 15348 "IPSUR.Rnw"
treesdummy.lm <- lm(Volume ~ Girth + Tall, data = trees)
summary(treesdummy.lm)


###################################################
### chunk number 363:  eval=FALSE
###################################################
## #line 15398 "IPSUR.Rnw"
## treesTall <- split(trees, trees$Tall)
## treesTall[["yes"]]$Fit <- predict(treesdummy.lm, treesTall[["yes"]])
## treesTall[["no"]]$Fit <- predict(treesdummy.lm, treesTall[["no"]])
## plot(Volume ~ Girth, data = trees, type = "n")
## points(Volume ~ Girth, data = treesTall[["yes"]], pch = 1)
## points(Volume ~ Girth, data = treesTall[["no"]], pch = 2)
## lines(Fit ~ Girth, data = treesTall[["yes"]])
## lines(Fit ~ Girth, data = treesTall[["no"]])


###################################################
### chunk number 364: 
###################################################
#line 15412 "IPSUR.Rnw"
treesTall <- split(trees, trees$Tall)
treesTall[["yes"]]$Fit <- predict(treesdummy.lm, treesTall[["yes"]])
treesTall[["no"]]$Fit <- predict(treesdummy.lm, treesTall[["no"]])
plot(Volume ~ Girth, data = trees, type = "n")
points(Volume ~ Girth, data = treesTall[["yes"]], pch = 1)
points(Volume ~ Girth, data = treesTall[["no"]], pch = 2)
lines(Fit ~ Girth, data = treesTall[["yes"]])
lines(Fit ~ Girth, data = treesTall[["no"]])


###################################################
### chunk number 365: 
###################################################
#line 15513 "IPSUR.Rnw"
treesfull.lm <- lm(Volume ~ Girth + I(Girth^2) + Height + I(Height^2), data = trees)
summary(treesfull.lm)


###################################################
### chunk number 366: 
###################################################
#line 15530 "IPSUR.Rnw"
treesreduced.lm <- lm(Volume ~ -1 + Girth + I(Girth^2), data = trees)


###################################################
### chunk number 367: 
###################################################
#line 15538 "IPSUR.Rnw"
anova(treesreduced.lm, treesfull.lm)


###################################################
### chunk number 368: 
###################################################
#line 15549 "IPSUR.Rnw"
treesreduced2.lm <- lm(Volume ~ Girth + I(Girth^2) + Height, data = trees)
anova(treesreduced2.lm, treesfull.lm)


###################################################
### chunk number 369: 
###################################################
#line 15646 "IPSUR.Rnw"
treesNonlin.lm <- lm(log(Volume) ~ log(Girth) + log(Height), data = trees)
summary(treesNonlin.lm)


###################################################
### chunk number 370: 
###################################################
#line 15660 "IPSUR.Rnw"
exp(confint(treesNonlin.lm))


###################################################
### chunk number 371: 
###################################################
#line 15669 "IPSUR.Rnw"
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))
exp(predict(treesNonlin.lm, newdata = new, interval = "confidence"))


###################################################
### chunk number 372: 
###################################################
#line 15710 "IPSUR.Rnw"
# fake data 
set.seed(1) 
x <- seq(from = 0, to = 1000, length.out = 200) 
y <- 1 + 2*(sin((2*pi*x/360) - 3))^2 + rnorm(200, sd = 2)
plot(x, y)
acc.nls <- nls(y ~ a + b*(sin((2*pi*x/360) - c))^2, start = list(a = 0.9, b = 2.3, c = 2.9))
summary(acc.nls)
#plot(x, fitted(acc.nls))


###################################################
### chunk number 373: 
###################################################
#line 15919 "IPSUR.Rnw"
srs <- rnorm(25, mean = 3)
resamps <- replicate(1000, sample(srs, 25, TRUE), simplify = FALSE)
xbarstar <- sapply(resamps, mean, simplify = TRUE)


###################################################
### chunk number 374: 
###################################################
#line 15928 "IPSUR.Rnw"
hist(xbarstar, breaks = 40, prob = TRUE)
curve(dnorm(x, 3, 0.2), add = TRUE)


###################################################
### chunk number 375:  eval=FALSE
###################################################
## #line 15953 "IPSUR.Rnw"
## hist(xbarstar, breaks = 40, prob = TRUE)
## curve(dnorm(x, 3, 0.2), add = TRUE)  # overlay true normal density


###################################################
### chunk number 376: 
###################################################
#line 15969 "IPSUR.Rnw"
mean(xbarstar)
mean(srs)
mean(xbarstar) - mean(srs)


###################################################
### chunk number 377: 
###################################################
#line 15991 "IPSUR.Rnw"
sd(xbarstar)


###################################################
### chunk number 378: 
###################################################
#line 16022 "IPSUR.Rnw"
resamps <- replicate(1000, sample(rivers, 141, TRUE), simplify = FALSE)
medstar <- sapply(resamps, median, simplify = TRUE)
sd(medstar)


###################################################
### chunk number 379: 
###################################################
#line 16031 "IPSUR.Rnw"
hist(medstar, breaks = 40, prob = TRUE)


###################################################
### chunk number 380:  eval=FALSE
###################################################
## #line 16045 "IPSUR.Rnw"
## hist(medstar, breaks = 40, prob = TRUE)


###################################################
### chunk number 381: 
###################################################
#line 16049 "IPSUR.Rnw"
median(rivers)
mean(medstar)
mean(medstar) - median(rivers)


###################################################
### chunk number 382: 
###################################################
#line 16077 "IPSUR.Rnw"
library(boot)
mean_fun <- function(x, indices) mean(x[indices])
boot(data = srs, statistic = mean_fun, R = 1000)


###################################################
### chunk number 383: 
###################################################
#line 16085 "IPSUR.Rnw"
median_fun <- function(x, indices) median(x[indices])
boot(data = rivers, statistic = median_fun, R = 1000)


###################################################
### chunk number 384: 
###################################################
#line 16154 "IPSUR.Rnw"
btsamps <- replicate(2000, sample(stack.loss, 21, TRUE), simplify = FALSE)
thetast <- sapply(btsamps, median, simplify = TRUE)
mean(thetast)
median(stack.loss)
quantile(thetast, c(0.025, 0.975))


###################################################
### chunk number 385: 
###################################################
#line 16169 "IPSUR.Rnw"
library(boot)
med_fun <- function(x, ind) median(x[ind])
med_boot <- boot(stack.loss, med_fun, R = 2000)
boot.ci(med_boot, type = c("perc", "norm", "bca"))


###################################################
### chunk number 386: 
###################################################
#line 16322 "IPSUR.Rnw"
library(coin)
oneway_test(len ~ supp, data = ToothGrowth)


###################################################
### chunk number 387: 
###################################################
#line 16336 "IPSUR.Rnw"
t.test(len ~ supp, data = ToothGrowth, alt = "greater", var.equal = TRUE)


###################################################
### chunk number 388: 
###################################################
#line 16340 "IPSUR.Rnw"
A <- show(oneway_test(len ~ supp, data = ToothGrowth))
B <- t.test(len ~ supp, data = ToothGrowth, alt = "greater", var.equal = TRUE)


###################################################
### chunk number 389:  eval=FALSE
###################################################
## #line 16415 "IPSUR.Rnw"
## install.packages("IPSUR", repos="http://R-Forge.R-project.org")
## library(IPSUR)
## read(IPSUR)


###################################################
### chunk number 390:  eval=FALSE
###################################################
## #line 16429 "IPSUR.Rnw"
## install.packages("IPSUR", repos="http://R-Forge.R-project.org")
## library(IPSUR)
## read(IPSUR)


###################################################
### chunk number 391:  eval=FALSE
###################################################
## #line 16443 "IPSUR.Rnw"
## install.packages("IPSUR", repos="http://R-Forge.R-project.org")
## library(IPSUR)
## read(IPSUR)


###################################################
### chunk number 392: 
###################################################
#line 16458 "IPSUR.Rnw"
sessionInfo()


###################################################
### chunk number 393: 
###################################################
#line 16977 "IPSUR.Rnw"
x <- c(3, 5, 9)


###################################################
### chunk number 394: 
###################################################
#line 16985 "IPSUR.Rnw"
y <- c(3, "5", TRUE)


###################################################
### chunk number 395: 
###################################################
#line 17005 "IPSUR.Rnw"
matrix(letters[1:6], nrow = 2, ncol = 3)


###################################################
### chunk number 396: 
###################################################
#line 17013 "IPSUR.Rnw"
matrix(letters[1:6], nrow = 2, ncol = 3, byrow = TRUE)


###################################################
### chunk number 397: 
###################################################
#line 17022 "IPSUR.Rnw"
matrix(c(1,"2",NA, FALSE), nrow = 2, ncol = 3)


###################################################
### chunk number 398: 
###################################################
#line 17033 "IPSUR.Rnw"
A <- matrix(1:6, 2, 3)
B <- matrix(2:7, 2, 3)
A + B
A * B


###################################################
### chunk number 399: 
###################################################
#line 17048 "IPSUR.Rnw"
try(A * B)     # an error
A %*% t(B)     # this is alright


###################################################
### chunk number 400: 
###################################################
#line 17056 "IPSUR.Rnw"
solve(A %*% t(B))     # input matrix must be square


###################################################
### chunk number 401: 
###################################################
#line 17064 "IPSUR.Rnw"
array(LETTERS[1:24], dim = c(3,4,2))


###################################################
### chunk number 402: 
###################################################
#line 17080 "IPSUR.Rnw"
x <- c(1.3, 5.2, 6)
y <- letters[1:3]
z <- c(TRUE, FALSE, TRUE)
A <- data.frame(x, y, z)
A


###################################################
### chunk number 403: 
###################################################
#line 17093 "IPSUR.Rnw"
names(A) <- c("Fred","Mary","Sue")
A


###################################################
### chunk number 404: 
###################################################
#line 17122 "IPSUR.Rnw"
A <- as.data.frame(Titanic)
head(A)


###################################################
### chunk number 405: 
###################################################
#line 17142 "IPSUR.Rnw"
library(reshape)
B <- with(A, untable(A, Freq))
head(B)


###################################################
### chunk number 406: 
###################################################
#line 17165 "IPSUR.Rnw"
C <- B[, -5]
rownames(C) <- 1:dim(C)[1]
head(C)


###################################################
### chunk number 407: 
###################################################
#line 17181 "IPSUR.Rnw"
tab <- matrix(1:6, nrow = 2, ncol = 3)
rownames(tab) <- c('first', 'second')
colnames(tab) <- c('A', 'B', 'C')
tab  # Counts


###################################################
### chunk number 408: 
###################################################
#line 17196 "IPSUR.Rnw"
p <- c("milk","tea")
g <- c("milk","tea")
catgs <- expand.grid(poured = p, guessed = g)
cnts <- c(3, 1, 1, 3)
D <- cbind(catgs, count = cnts)
xtabs(count ~ poured + guessed, data = D)


###################################################
### chunk number 409:  eval=FALSE
###################################################
## #line 17286 "IPSUR.Rnw"
## library(foreign)
## read.spss("foo.sav")


###################################################
### chunk number 410: 
###################################################
#line 17336 "IPSUR.Rnw"
Tmp <- Puromycin[order(Puromycin$conc), ]
head(Tmp)


###################################################
### chunk number 411:  eval=FALSE
###################################################
## #line 17343 "IPSUR.Rnw"
## with(Puromycin, Puromycin[order(conc), ])


###################################################
### chunk number 412:  eval=FALSE
###################################################
## #line 17351 "IPSUR.Rnw"
## with(Puromycin, Puromycin[order(state, conc), ])


###################################################
### chunk number 413: 
###################################################
#line 17358 "IPSUR.Rnw"
Tmp <- with(Puromycin, Puromycin[order(-conc), ])
head(Tmp)


###################################################
### chunk number 414: 
###################################################
#line 17368 "IPSUR.Rnw"
Tmp <- with(Puromycin, Puromycin[order(-xtfrm(state)), ])
head(Tmp)


###################################################
### chunk number 415:  eval=FALSE
###################################################
## #line 18182 "IPSUR.Rnw"
## library(odfWeave)
## odfWeave(file = "infile.odt", dest = "outfile.odt")


###################################################
### chunk number 416: 
###################################################
#line 18247 "IPSUR.Rnw"
library(Hmisc)
summary(cbind(Sepal.Length, Sepal.Width) ~ Species, data = iris)


###################################################
### chunk number 417: 
###################################################
#line 18270 "IPSUR.Rnw"
set.seed(095259)


###################################################
### chunk number 418: 
###################################################
#line 18320 "IPSUR.Rnw"
options(digits = 16)
runif(1)


###################################################
### chunk number 419: 
###################################################
#line 18772 "IPSUR.Rnw"
rm(.Random.seed)
try(dir.create("../../data"), silent = TRUE)
save.image(file = "../../data/IPSUR.RData", compress = "xz")
Stangle(file="IPSUR.Rnw", output="../IPSUR.R", annotate=TRUE)



###################################################
### chunk number 1: 
###################################################
###  IPSUR.R - Introduction to Probability and Statistics Using R
###  Copyright (C) 2010  G. Jay Kerns, <gkerns@ysu.edu>
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
## install.packages("IPSUR")
## library(IPSUR)
## read(IPSUR)


###################################################
### chunk number 6: 
###################################################
getOption("defaultPackages")


###################################################
### chunk number 7: 
###################################################
2 + 3       # add
4 * 5 / 6   # multiply and divide
7^8         # 7 to the 8th power


###################################################
### chunk number 8: 
###################################################
options(digits = 16)
10/3                 # see more digits
sqrt(2)              # square root
exp(1)               # Euler's constant, e
pi       
options(digits = 7)  # back to default


###################################################
### chunk number 9: 
###################################################
x <- 7*41/pi   # don't see the calculated value
x              # take a look


###################################################
### chunk number 10: five
###################################################
sqrt(-1)              # isn't defined
sqrt(-1+0i)           # is defined
sqrt(as.complex(-1))  # same thing
(0 + 1i)^2            # should be -1
typeof((0 + 1i)^2)


###################################################
### chunk number 11: 
###################################################
x <- c(74, 31, 95, 61, 76, 34, 23, 54, 96)
x


###################################################
### chunk number 12: 
###################################################
seq(from = 1, to = 5)
seq(from = 2, by = -0.1, length.out = 4)


###################################################
### chunk number 13: 
###################################################
1:5


###################################################
### chunk number 14: 
###################################################
x[1]
x[2:4]
x[c(1,3,4,8)]
x[-c(1,3,4,8)]


###################################################
### chunk number 15: 
###################################################
LETTERS[1:5]
letters[-(6:24)]



###################################################
### chunk number 16: 
###################################################
x <- 1:5
sum(x)
length(x)
min(x)
mean(x)      # sample mean
sd(x)        # sample standard deviation


###################################################
### chunk number 17: 
###################################################
intersect


###################################################
### chunk number 18: 
###################################################
rev


###################################################
### chunk number 19: 
###################################################
methods(rev)


###################################################
### chunk number 20: 
###################################################
rev.default


###################################################
### chunk number 21: 
###################################################
wilcox.test
methods(wilcox.test)


###################################################
### chunk number 22: 
###################################################
exp


###################################################
### chunk number 23: 
###################################################
str(precip)
precip[1:4]


###################################################
### chunk number 24: 
###################################################
str(rivers)


###################################################
### chunk number 25: 
###################################################
str(discoveries)
discoveries[1:4]


###################################################
### chunk number 26:  eval=FALSE
###################################################
## stripchart(precip, xlab="rainfall")
## stripchart(rivers, method="jitter", xlab="length")
## stripchart(discoveries, method="stack", xlab="number")


###################################################
### chunk number 27: 
###################################################
par(mfrow = c(1,3)) # 3 plots: 1 row, 3 columns
stripchart(precip, xlab="rainfall")
stripchart(rivers, method="jitter", xlab="length")
stripchart(discoveries, method="stack", xlab="number")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 28:  eval=FALSE
###################################################
## hist(precip, main = "")
## hist(precip, freq = FALSE, main = "")


###################################################
### chunk number 29: 
###################################################
par(mfrow = c(1,2)) # 2 plots: 1 row, 2 columns
hist(precip, main = "")
hist(precip, freq = FALSE, main = "")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 30:  eval=FALSE
###################################################
## hist(precip, breaks = 10, main = "")
## hist(precip, breaks = 200, main = "")


###################################################
### chunk number 31: 
###################################################
par(mfrow = c(1,2)) # 2 plots: 1 row, 2 columns
hist(precip, breaks = 10, main = "")
hist(precip, breaks = 200, main = "")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 32: 
###################################################
library(aplpack)
stem.leaf(UKDriverDeaths, depth = FALSE)


###################################################
### chunk number 33: 
###################################################
plot(LakeHuron, type = "h")
plot(LakeHuron, type = "p")


###################################################
### chunk number 34: 
###################################################
par(mfrow = c(2,1)) # 2 plots: 1 row, 2 columns
plot(LakeHuron, type = "h")
plot(LakeHuron, type = "p")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 35: 
###################################################
str(state.abb)


###################################################
### chunk number 36: 
###################################################
str(state.region)
state.region[1:5]


###################################################
### chunk number 37: 
###################################################
Tbl <- table(state.division)
Tbl               # frequencies
Tbl/sum(Tbl)      # relative frequencies
prop.table(Tbl)   # same thing


###################################################
### chunk number 38:  eval=FALSE
###################################################
## barplot(table(state.region), cex.names = 0.50)
## barplot(prop.table(table(state.region)), cex.names = 0.50)


###################################################
### chunk number 39: 
###################################################
par(mfrow = c(1,2)) # 2 plots: 1 row, 2 columns
barplot(table(state.region), cex.names = 0.50)
barplot(prop.table(table(state.region)), cex.names = 0.50)
par(mfrow = c(1,1))


###################################################
### chunk number 40:  eval=FALSE
###################################################
## library(qcc)
## pareto.chart(table(state.division), ylab="Frequency")


###################################################
### chunk number 41: 
###################################################
library(qcc)
pareto.chart(table(state.division), ylab="Frequency")


###################################################
### chunk number 42:  eval=FALSE
###################################################
## x <- table(state.region)
## dotchart(as.vector(x), labels = names(x))


###################################################
### chunk number 43: 
###################################################
x <- table(state.region)
dotchart(as.vector(x), labels = names(x))


###################################################
### chunk number 44: 
###################################################
x <- 5:9
y <- (x < 7.3)
y


###################################################
### chunk number 45: 
###################################################
!y


###################################################
### chunk number 46: 
###################################################
x <- c(3, 7, NA, 4, 7)
y <- c(5, NA, 1, 2, 2)
x + y


###################################################
### chunk number 47: 
###################################################
sum(x)
sum(x, na.rm = TRUE)


###################################################
### chunk number 48: 
###################################################
is.na(x)
z <- x[!is.na(x)]
sum(z)


###################################################
### chunk number 49: 
###################################################
library(aplpack)
stem.leaf(faithful$eruptions)


###################################################
### chunk number 50: 
###################################################
library(e1071)
skewness(discoveries)
2*sqrt(6/length(discoveries))


###################################################
### chunk number 51: 
###################################################
kurtosis(UKDriverDeaths)
4*sqrt(6/length(UKDriverDeaths))


###################################################
### chunk number 52: 
###################################################
stem.leaf(rivers)


###################################################
### chunk number 53: 
###################################################
stem.leaf(precip)


###################################################
### chunk number 54: 
###################################################
boxplot.stats(rivers)$out


###################################################
### chunk number 55: 
###################################################
boxplot.stats(rivers, coef = 3)$out


###################################################
### chunk number 56: 
###################################################
x <- 5:8
y <- letters[3:6]
A <- data.frame(v1 = x, v2 = y)


###################################################
### chunk number 57: 
###################################################
A[3,]
A[1, ]
A[ ,2]


###################################################
### chunk number 58: 
###################################################
names(A)
A$v1


###################################################
### chunk number 59:  eval=FALSE
###################################################
## library(lattice)
## xyplot()


###################################################
### chunk number 60:  eval=FALSE
###################################################
## library(lattice)
## bwplot(~weight | feed, data = chickwts)


###################################################
### chunk number 61: 
###################################################
library(lattice)
print(bwplot(~ weight | feed, data = chickwts))


###################################################
### chunk number 62:  eval=FALSE
###################################################
## histogram(~age | education, data = infert)


###################################################
### chunk number 63: 
###################################################
library(lattice)
print(histogram(~age | education, data = infert))


###################################################
### chunk number 64:  eval=FALSE
###################################################
## xyplot(Petal.Length ~ Petal.Width | Species, data = iris)


###################################################
### chunk number 65: 
###################################################
library(lattice)
print(xyplot(Petal.Length ~ Petal.Width | Species, data = iris))


###################################################
### chunk number 66:  eval=FALSE
###################################################
## coplot(conc ~ uptake | Type * Treatment, data = CO2)


###################################################
### chunk number 67: 
###################################################
library(lattice)
print(coplot(conc ~ uptake | Type * Treatment, data = CO2))


###################################################
### chunk number 68: 
###################################################
attach(RcmdrTestDrive)
names(RcmdrTestDrive)


###################################################
### chunk number 69: "Find summary statistics"
###################################################
summary(RcmdrTestDrive)


###################################################
### chunk number 70: 
###################################################
table(race)


###################################################
### chunk number 71: 
###################################################
barplot(table(RcmdrTestDrive$race), main="", xlab="race", ylab="Frequency", legend.text=FALSE, col=NULL) 


###################################################
### chunk number 72: 
###################################################
x <- tapply(salary, list(gender = gender), mean)
x


###################################################
### chunk number 73: 
###################################################
by(salary, gender, mean, na.rm = TRUE)


###################################################
### chunk number 74: 
###################################################
x[which(x==max(x))]


###################################################
### chunk number 75: 
###################################################
y <- tapply(salary, list(gender = gender), sd)
y


###################################################
### chunk number 76: 
###################################################
boxplot(salary~gender, xlab="salary", ylab="gender", main="", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 77: 
###################################################
x = sort(reduction)


###################################################
### chunk number 78: 
###################################################
x[137]
IQR(x)
fivenum(x)
fivenum(x)[4] - fivenum(x)[2]


###################################################
### chunk number 79: 
###################################################
boxplot(reduction, xlab="reduction", main="", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 80: 
###################################################
temp <- fivenum(x)
inF <- 1.5 * (temp[4] - temp[2]) + temp[4]
outF <- 3 * (temp[4] - temp[2]) + temp[4]
which(x > inF)
which(x > outF)


###################################################
### chunk number 81: 
###################################################
c(mean(before), median(before))
c(mean(after), median(after))


###################################################
### chunk number 82: 
###################################################
boxplot(before, xlab="before", main="", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 83: 
###################################################
boxplot(after, xlab="after", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 84: 
###################################################
sd(before)
mad(after)
IQR(after)/1.349


###################################################
### chunk number 85: 
###################################################
library(e1071)
skewness(before)
kurtosis(before)


###################################################
### chunk number 86: 
###################################################
skewness(after)
kurtosis(after)


###################################################
### chunk number 87: 
###################################################
hist(before, xlab="before", data=RcmdrTestDrive) 


###################################################
### chunk number 88: 
###################################################
hist(after, xlab="after", data=RcmdrTestDrive) 


###################################################
### chunk number 89: 
###################################################
S <- data.frame(lands = c("down","up","side"))
S


###################################################
### chunk number 90: 
###################################################
library(prob)
tosscoin(1) 


###################################################
### chunk number 91: 
###################################################
tosscoin(3) 


###################################################
### chunk number 92: 
###################################################
rolldie(1) 


###################################################
### chunk number 93: 
###################################################
head(cards()) 


###################################################
### chunk number 94: 
###################################################
urnsamples(1:3, size = 2, replace = TRUE, ordered = TRUE)


###################################################
### chunk number 95: 
###################################################
urnsamples(1:3, size = 2, replace = FALSE, ordered = TRUE)


###################################################
### chunk number 96: 
###################################################
urnsamples(1:3, size = 2, replace = FALSE, ordered = FALSE) 


###################################################
### chunk number 97: 
###################################################
urnsamples(1:3, size = 2, replace = TRUE, ordered = FALSE) 


###################################################
### chunk number 98: 
###################################################
S <- tosscoin(2, makespace = TRUE) 
S[1:3, ] 
S[c(2,4), ] 


###################################################
### chunk number 99: 
###################################################
S <- cards() 


###################################################
### chunk number 100: 
###################################################
subset(S, suit == "Heart") 
subset(S, rank %in% 7:9)


###################################################
### chunk number 101: 
###################################################
subset(rolldie(3), X1+X2+X3 > 16) 


###################################################
### chunk number 102: 
###################################################
x <- 1:10 
y <- 8:12 
y %in% x


###################################################
### chunk number 103: 
###################################################
isin(x,y) 


###################################################
### chunk number 104: 
###################################################
x <- 1:10 
y <- c(3,3,7) 


###################################################
### chunk number 105: 
###################################################
all(y %in% x)
isin(x,y) 


###################################################
### chunk number 106: 
###################################################
isin(x, c(3,4,5), ordered = TRUE) 
isin(x, c(3,5,4), ordered = TRUE) 


###################################################
### chunk number 107: 
###################################################
S <- rolldie(4) 
subset(S, isin(S, c(2,2,6), ordered = TRUE)) 


###################################################
### chunk number 108: 
###################################################
S = cards() 
A = subset(S, suit == "Heart") 
B = subset(S, rank %in% 7:9)


###################################################
### chunk number 109: 
###################################################
union(A,B) 
intersect(A,B) 
setdiff(A,B) 
setdiff(B,A) 


###################################################
### chunk number 110: 
###################################################
outcomes <- rolldie(1) 
p <- rep(1/6, times = 6) 
probspace(outcomes, probs = p) 


###################################################
### chunk number 111: 
###################################################
probspace(1:6, probs = p) 


###################################################
### chunk number 112: 
###################################################
probspace(1:6) 


###################################################
### chunk number 113: 
###################################################
rolldie(1, makespace = TRUE)


###################################################
### chunk number 114: 
###################################################
probspace(tosscoin(1), probs = c(0.70, 0.30)) 


###################################################
### chunk number 115: 
###################################################
S <- cards(makespace = TRUE) 
A <- subset(S, suit == "Heart") 
B <- subset(S, rank %in% 7:9)


###################################################
### chunk number 116: 
###################################################
prob(A) 


###################################################
### chunk number 117: 
###################################################
prob(S, suit == "Heart") 


###################################################
### chunk number 118: 
###################################################
nsamp(n=3, k=2, replace = TRUE, ordered = TRUE) 
nsamp(n=3, k=2, replace = FALSE, ordered = TRUE) 
nsamp(n=3, k=2, replace = FALSE, ordered = FALSE) 
nsamp(n=3, k=2, replace = TRUE, ordered = FALSE) 


###################################################
### chunk number 119: 
###################################################
n <- c(11,7,31) 
k <- c(3,4,3) 
r <- c(FALSE,FALSE,TRUE) 


###################################################
### chunk number 120: 
###################################################
x <- nsamp(n, k, rep = r, ord = TRUE) 


###################################################
### chunk number 121: 
###################################################
prod(x) 


###################################################
### chunk number 122: 
###################################################
(11*10*9)*(7*6*5*4)*313 


###################################################
### chunk number 123: 
###################################################
prod(9:11)*prod(4:7)*313 


###################################################
### chunk number 124: 
###################################################
prod(factorial(c(11,7))/factorial(c(8,3)))*313 


###################################################
### chunk number 125: 
###################################################
g <- Vectorize(pbirthday.ipsur)
plot(1:50, g(1:50), xlab = "Number of people in room", ylab = "Prob(at least one match)")
abline(h = 0.5)
abline(v = 23, lty = 2)
remove(g)


###################################################
### chunk number 126: 
###################################################
library(prob)
S <- rolldie(2, makespace = TRUE)  # assumes ELM
head(S)                            #  first few rows


###################################################
### chunk number 127: 
###################################################
A <- subset(S, X1 == X2)
B <- subset(S, X1 + X2 >= 8)


###################################################
### chunk number 128: 
###################################################
prob(A, given = B)
prob(B, given = A)


###################################################
### chunk number 129: 
###################################################
prob(S, X1==X2, given = (X1 + X2 >= 8) )
prob(S, X1+X2 >= 8, given = (X1==X2) )


###################################################
### chunk number 130: 
###################################################
library(prob)
L <- cards()
M <- urnsamples(L, size = 2)
N <- probspace(M)


###################################################
### chunk number 131: 
###################################################
prob(N, all(rank == "A"))


###################################################
### chunk number 132: 
###################################################
library(prob)
L <- rep(c("red","green"), times = c(7,3))
M <- urnsamples(L, size = 3, replace = FALSE, ordered = TRUE)
N <- probspace(M)


###################################################
### chunk number 133: 
###################################################
prob(N, isrep(N, "red", 3))


###################################################
### chunk number 134: 
###################################################
prob(N, isrep(N, "red", 2))


###################################################
### chunk number 135: 
###################################################
prob(N, isin(N, c("red","green","red"), ordered = TRUE))


###################################################
### chunk number 136: 
###################################################
prob(N, isin(N, c("red","green","red")))


###################################################
### chunk number 137: 
###################################################
.Table <- xtabs(~smoke+gender, data=RcmdrTestDrive)
addmargins(.Table) # Table with Marginal Distributions
remove(.Table)


###################################################
### chunk number 138: 
###################################################
S <- tosscoin(10, makespace = TRUE)
A <- subset(S, isrep(S, vals = "T", nrep = 10))
1 - prob(A)


###################################################
### chunk number 139: 
###################################################
iidspace(c("H","T"), ntrials = 3, probs = c(0.7, 0.3)) 


###################################################
### chunk number 140: 
###################################################
prior <- c(0.6, 0.3, 0.1)
like <- c(0.003, 0.007, 0.010)
post <- prior * like
post / sum(post)


###################################################
### chunk number 141: 
###################################################
newprior <- post
post <- newprior * like^7
post / sum(post)


###################################################
### chunk number 142: 
###################################################
fastpost <- prior * like^8
fastpost / sum(fastpost)


###################################################
### chunk number 143: 
###################################################
S <- rolldie(3, nsides = 4, makespace = TRUE) 
S <- addrv(S, U = X1-X2+X3) 


###################################################
### chunk number 144: 
###################################################
head(S)


###################################################
### chunk number 145: 
###################################################
prob(S, U > 6) 


###################################################
### chunk number 146: 
###################################################
S <- addrv(S, FUN = max, invars = c("X1","X2","X3"), name = "V") 
S <- addrv(S, FUN = sum, invars = c("X1","X2","X3"), name = "W") 
head(S) 


###################################################
### chunk number 147: 
###################################################
marginal(S, vars = "V") 


###################################################
### chunk number 148: 
###################################################
marginal(S, vars = c("V", "W")) 


###################################################
### chunk number 149: 
###################################################
rnorm(1)


###################################################
### chunk number 150: 
###################################################
x <- c(0,1,2,3)
f <- c(1/8, 3/8, 3/8, 1/8)


###################################################
### chunk number 151: 
###################################################
mu <- sum(x * f)
mu


###################################################
### chunk number 152: 
###################################################
sigma2 <- sum((x-mu)^2 * f)
sigma2
sigma <- sqrt(sigma2)
sigma


###################################################
### chunk number 153: 
###################################################
F = cumsum(f)
F


###################################################
### chunk number 154: 
###################################################
library(distrEx)
X <- DiscreteDistribution(supp = 0:3, prob = c(1,3,3,1)/8)
E(X); var(X); sd(X)


###################################################
### chunk number 155: 
###################################################
A <- data.frame(Pr=dbinom(0:4, size = 4, prob = 0.5))
rownames(A) <- 0:4 
A


###################################################
### chunk number 156: 
###################################################
pbinom(9, size=12, prob=1/6) - pbinom(6, size=12, prob=1/6)
diff(pbinom(c(6,9), size = 12, prob = 1/6))  # same thing


###################################################
### chunk number 157: 
###################################################
plot(0, xlim = c(-1.2, 4.2), ylim = c(-0.04, 1.04), type = "n", xlab = "number of successes", ylab = "cumulative probability")
abline(h = c(0,1), lty = 2, col = "grey")
lines(stepfun(0:3, pbinom(-1:3, size = 3, prob = 0.5)), verticals = FALSE, do.p = FALSE)
points(0:3, pbinom(0:3, size = 3, prob = 0.5), pch = 16, cex = 1.2)
points(0:3, pbinom(-1:2, size = 3, prob = 0.5), pch = 1, cex = 1.2)


###################################################
### chunk number 158: 
###################################################
library(distr)
X <- Binom(size = 3, prob = 1/2)
X


###################################################
### chunk number 159: 
###################################################
d(X)(1)   # pmf of X evaluated at x = 1
p(X)(2)   # cdf of X evaluated at x = 2


###################################################
### chunk number 160: 
###################################################
plot(X, cex = 0.2)


###################################################
### chunk number 161: 
###################################################
X <- Binom(size = 3, prob = 0.45)
library(distrEx)
E(X)
E(3*X + 4)


###################################################
### chunk number 162: 
###################################################
var(X)
sd(X)


###################################################
### chunk number 163: 
###################################################
x <- c(4, 7, 9, 11, 12)
ecdf(x)


###################################################
### chunk number 164:  eval=FALSE
###################################################
## plot(ecdf(x))


###################################################
### chunk number 165: 
###################################################
plot(ecdf(x))


###################################################
### chunk number 166: 
###################################################
epdf <- function(x) function(t){sum(x %in% t)/length(x)}
x <- c(0,0,1)
epdf(x)(0)       # should be 2/3


###################################################
### chunk number 167: 
###################################################
x <- c(0,0,1)
sample(x, size = 7, replace = TRUE)


###################################################
### chunk number 168: 
###################################################
dhyper(3, m = 17, n = 233, k = 5)


###################################################
### chunk number 169: 
###################################################
A <- data.frame(Pr=dhyper(0:4, m = 17, n = 233, k = 5))
rownames(A) <- 0:4 
A


###################################################
### chunk number 170: 
###################################################
dhyper(5, m = 17, n = 233, k = 5)


###################################################
### chunk number 171: 
###################################################
phyper(2, m = 17, n = 233, k = 5)


###################################################
### chunk number 172: 
###################################################
phyper(1, m = 17, n = 233, k = 5, lower.tail = FALSE)


###################################################
### chunk number 173: 
###################################################
rhyper(10, m = 17, n = 233, k = 5)


###################################################
### chunk number 174: 
###################################################
pgeom(4, prob = 0.812, lower.tail = FALSE)


###################################################
### chunk number 175: 
###################################################
dnbinom(5, size = 7, prob = 0.5)


###################################################
### chunk number 176: 
###################################################
diff(ppois(c(47, 50), lambda = 50))


###################################################
### chunk number 177: 
###################################################
xmin <- qbinom(.0005, size=31 , prob=0.447) 
xmax <- qbinom(.9995, size=31 , prob=0.447) 
.x <- xmin:xmax 
plot(.x, dbinom(.x, size=31, prob=0.447), xlab="Number of Successes", ylab="Probability Mass",    main="Binomial Dist'n: Trials = 31, Prob of success = 0.447", type="h") 
points(.x, dbinom(.x, size=31, prob=0.447), pch=16) 
abline( h = 0, lty = 2, col = "grey" ) 
remove(.x, xmin, xmax)


###################################################
### chunk number 178: 
###################################################
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
### chunk number 179: 
###################################################
dbinom(17, size = 31, prob = 0.447)


###################################################
### chunk number 180: 
###################################################
pbinom(13, size = 31, prob = 0.447)


###################################################
### chunk number 181: 
###################################################
pbinom(11, size = 31, prob = 0.447, lower.tail = FALSE)


###################################################
### chunk number 182: 
###################################################
pbinom(14, size = 31, prob = 0.447, lower.tail = FALSE)


###################################################
### chunk number 183: 
###################################################
sum(dbinom(16:19, size = 31, prob = 0.447))
diff(pbinom(c(19,15), size = 31, prob = 0.447, lower.tail = FALSE))


###################################################
### chunk number 184: 
###################################################
library(distrEx)
X = Binom(size = 31, prob = 0.447)
E(X)


###################################################
### chunk number 185: 
###################################################
var(X)


###################################################
### chunk number 186: 
###################################################
sd(X)


###################################################
### chunk number 187: 
###################################################
E(4*X + 51.324)


###################################################
### chunk number 188: 
###################################################
rnorm(1)


###################################################
### chunk number 189: 
###################################################
f <- function(x) 3*x^2
integrate(f, lower = 0.14, upper = 0.71)


###################################################
### chunk number 190: 
###################################################
g <- function(x) 3/x^3
integrate(g, lower = 1, upper = Inf)


###################################################
### chunk number 191: 
###################################################
library(distr)
f <- function(x) 3*x^2
X <- AbscontDistribution(d = f, low1 = 0, up1 = 1)
p(X)(0.71) - p(X)(0.14)


###################################################
### chunk number 192: 
###################################################
library(distrEx)
E(X)
var(X)
3/80


###################################################
### chunk number 193: 
###################################################
pnorm(1:3)-pnorm(-(1:3))


###################################################
### chunk number 194: 
###################################################
g <- function(x) pnorm(x, mean = 100, sd = 15) - 0.99
uniroot(g, interval = c(130, 145))


###################################################
### chunk number 195: 
###################################################
temp <- round(uniroot(g, interval = c(130, 145))$root, 4)


###################################################
### chunk number 196: 
###################################################
qnorm(0.99, mean = 100, sd = 15)


###################################################
### chunk number 197: 
###################################################
qnorm(c(0.025, 0.01, 0.005), lower.tail = FALSE)


###################################################
### chunk number 198: 
###################################################
library(distr)
X <- Norm(mean = 0, sd = 1)
Y <- 4 - 3*X
Y


###################################################
### chunk number 199: 
###################################################
Y <- exp(X)
Y


###################################################
### chunk number 200: 
###################################################
W <- sin(exp(X) + 27)
W


###################################################
### chunk number 201: 
###################################################
p(W)(0.5)
W <- sin(exp(X) + 27)
p(W)(0.5)


###################################################
### chunk number 202:  eval=FALSE
###################################################
## curve(dchisq(x, df = 3), from = 0, to = 20, ylab = "y")
## ind <- c(4, 5, 10, 15)
## for (i in ind) curve(dchisq(x, df = i), 0, 20, add = TRUE)


###################################################
### chunk number 203: 
###################################################
curve(dchisq(x, df = 3), from = 0, to = 20, ylab = "y")
ind <- c(4, 5, 10, 15)
for (i in ind) curve(dchisq(x, df = i), 0, 20, add = TRUE)


###################################################
### chunk number 204: 
###################################################
library(actuar)
mgamma(1:4, shape = 13, rate = 1)


###################################################
### chunk number 205: 
###################################################
plot(function(x){mgfgamma(x, shape = 13, rate = 1)}, from=-0.1, to=0.1, ylab = "gamma mgf")


###################################################
### chunk number 206: 
###################################################
plot(function(x){mgfgamma(x, shape = 13, rate = 1)}, from=-0.1, to=0.1, ylab = "gamma mgf")


###################################################
### chunk number 207: 
###################################################
rnorm(1)


###################################################
### chunk number 208: 
###################################################
pnorm(2.64, lower.tail = FALSE)


###################################################
### chunk number 209: 
###################################################
pnorm(0.87) - 1/2


###################################################
### chunk number 210: 
###################################################
2 * pnorm(-1.39)


###################################################
### chunk number 211: 
###################################################
S <- rolldie(2, makespace = TRUE)
S <- addrv(S, FUN = max, invars = c("X1","X2"), name = "U")
S <- addrv(S, FUN = sum, invars = c("X1","X2"), name = "V")
head(S)


###################################################
### chunk number 212: 
###################################################
UV <- marginal(S, vars = c("U", "V"))
head(UV)


###################################################
### chunk number 213: 
###################################################
xtabs(round(probs,3) ~ U + V, data = UV)


###################################################
### chunk number 214: 
###################################################
marginal(UV, vars = "U")
head(marginal(UV, vars = "V"))


###################################################
### chunk number 215: 
###################################################
temp <- xtabs(probs ~ U + V, data = UV)
rowSums(temp)
colSums(temp)


###################################################
### chunk number 216: 
###################################################
Eu <- sum(S$U*S$probs)
Ev <- sum(S$V*S$probs)
Euv <- sum(S$U*S$V*S$probs)
Euv - Eu * Ev


###################################################
### chunk number 217:  eval=FALSE
###################################################
## library(mvtnorm)
## x <- y <- seq(from = -3, to = 3, length.out = 30)
## f <- function(x,y) dmvnorm(cbind(x,y), mean = c(0,0), sigma = diag(2))
## z <- outer(x, y, FUN = f)
## persp(x, y, z, theta = -30, phi = 30, ticktype = "detailed")


###################################################
### chunk number 218: 
###################################################
library(mvtnorm)
x <- y <- seq(from = -3, to = 3, length.out = 30)
f <- function(x,y) dmvnorm(cbind(x,y), mean = c(0,0), sigma = diag(2))
z <- outer(x, y, FUN = f)
persp(x, y, z, theta = -30, phi = 30, ticktype = "detailed")


###################################################
### chunk number 219: 
###################################################
library(combinat)
tmp <- t(xsimplex(3, 6))
p <- apply(tmp, MARGIN = 1, FUN = dmultinom, prob = c(36,27,37))
library(prob)
S <- probspace(tmp, probs = p)
ProbTable <- xtabs(probs ~ X1 + X2, data = S)
round(ProbTable, 3)


###################################################
### chunk number 220: 
###################################################
library(lattice)
print(cloud(probs ~ X1 + X2, data = S, type = c("p","h"), lwd = 2, pch = 16, cex = 1.5), screen = list(z = 15, x = -70))


###################################################
### chunk number 221:  eval=FALSE
###################################################
## curve(dt(x, df = 30), from = -3, to = 3, lwd = 3, ylab = "y")
## ind <- c(1, 2, 3, 5, 10)
## for (i in ind) curve(dt(x, df = i), -3, 3, add = TRUE)


###################################################
### chunk number 222: 
###################################################
curve(dt(x, df = 30), from = -3, to = 3, lwd = 3, ylab = "y")
ind <- c(1, 2, 3, 5, 10)
for (i in ind) curve(dt(x, df = i), -3, 3, add = TRUE)


###################################################
### chunk number 223: 
###################################################
qt(0.01, df = 23, lower.tail = FALSE)


###################################################
### chunk number 224:  eval=FALSE
###################################################
## library(TeachingDemos)
## example(clt.examp)


###################################################
### chunk number 225:  eval=FALSE
###################################################
## library(distrTeach)
## example(illustrateCLT)


###################################################
### chunk number 226: 
###################################################
iqrs <- replicate(100, IQR(rnorm(100)))


###################################################
### chunk number 227: 
###################################################
mean(iqrs)    # close to 1


###################################################
### chunk number 228: 
###################################################
sd(iqrs)


###################################################
### chunk number 229: 
###################################################
hist(iqrs, breaks = 20)


###################################################
### chunk number 230: 
###################################################
mads <- replicate(100, mad(rnorm(100)))


###################################################
### chunk number 231: 
###################################################
mean(mads)    # close to 1.349


###################################################
### chunk number 232: 
###################################################
sd(mads)


###################################################
### chunk number 233: 
###################################################
hist(mads, breaks = 20)


###################################################
### chunk number 234: 
###################################################
k = 1
n = sample(10:30, size=10, replace = TRUE)
mu = round(rnorm(10, mean = 20))


###################################################
### chunk number 235: 
###################################################
pnorm(43.1, mean = 37, sd = 9, lower.tail = FALSE)


###################################################
### chunk number 236: 
###################################################
heights = rep(0, 16)
for (j in 7:15) heights[j] <- dhyper(3, m = 7, n = j - 7, k = 4)
plot(6:15, heights[6:15], pch = 16, cex = 1.5, xlab = "number of fish in pond", ylab = "Likelihood")
abline(h = 0)
lines(6:15, heights[6:15], type = "h", lwd = 2, lty = 3)
text(9, heights[9]/6, bquote(hat(F)==.(9)), cex = 2, pos = 4)
lines(9, heights[9], type = "h", lwd = 2)
points(9, 0, pch = 4, lwd = 3, cex = 2) 


###################################################
### chunk number 237:  eval=FALSE
###################################################
## curve(x^5*(1-x)^2, from = 0, to = 1, xlab = "p", ylab = "L(p)")
## curve(x^4*(1-x)^3, from = 0, to = 1, add = TRUE)
## curve(x^3*(1-x)^4, 0, 1, add = TRUE)


###################################################
### chunk number 238: 
###################################################
curve(x^5*(1-x)^2, 0, 1, xlab = "p", ylab = "L(p)")
curve(x^4*(1-x)^3, 0, 1, add = TRUE)
curve(x^3*(1-x)^4, 0, 1, add = TRUE)


###################################################
### chunk number 239: 
###################################################
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
### chunk number 240: 
###################################################
x <- mtcars$am
L <- function(p,x) prod(dbinom(x, size = 1, prob = p))
optimize(L, interval = c(0,1), x = x, maximum = TRUE)


###################################################
### chunk number 241: 
###################################################
A <- optimize(L, interval = c(0,1), x = x, maximum = TRUE)


###################################################
### chunk number 242: 
###################################################
minuslogL <- function(p,x) -sum(dbinom(x, size = 1, prob = p, log = TRUE))
optimize(minuslogL, interval = c(0,1), x = x)


###################################################
### chunk number 243: 
###################################################
minuslogL <- function(mu, sigma2){
  -sum(dnorm(x, mean = mu, sd = sqrt(sigma2), log = TRUE))
}


###################################################
### chunk number 244: 
###################################################
x <- PlantGrowth$weight
library(stats4)
MaxLikeEst <- mle(minuslogL, start = list(mu = 5, sigma2 = 0.5))
summary(MaxLikeEst)


###################################################
### chunk number 245: 
###################################################
mean(x)
var(x)*29/30
sd(x)/sqrt(30)


###################################################
### chunk number 246: 
###################################################
set.seed(seed + 1)
library(TeachingDemos)
ci.examp()


###################################################
### chunk number 247: 
###################################################
library(aplpack)
with(PlantGrowth, stem.leaf(weight))


###################################################
### chunk number 248: 
###################################################
dim(PlantGrowth)   # sample size is first entry
with(PlantGrowth, mean(weight))
qnorm(0.975)


###################################################
### chunk number 249: 
###################################################
library(TeachingDemos)
plot(z.test(PlantGrowth$weight, stdev = 0.70), "Conf")


###################################################
### chunk number 250: 
###################################################
library(TeachingDemos)
temp <- with(PlantGrowth, z.test(weight, stdev = 0.7))
temp


###################################################
### chunk number 251:  eval=FALSE
###################################################
## library(IPSUR)
## plot(temp, "Conf")


###################################################
### chunk number 252: 
###################################################
library(Hmisc)
binconf(x = 7, n = 25, method = "asymptotic")
binconf(x = 7, n = 25, method = "wilson")


###################################################
### chunk number 253: 
###################################################
tab <- xtabs(~gender, data = RcmdrTestDrive)
prop.test(rbind(tab), conf.level = 0.95, correct = FALSE)


###################################################
### chunk number 254: 
###################################################
A <- as.data.frame(Titanic)
library(reshape)
B <- with(A, untable(A, Freq))


###################################################
### chunk number 255: 
###################################################
dhyper(0, m = 26, n = 26, k = 5)


###################################################
### chunk number 256: 
###################################################
- qnorm(0.99)


###################################################
### chunk number 257: 
###################################################
A <- as.data.frame(UCBAdmissions)
head(A)
xtabs(Freq ~ Admit, data = A)


###################################################
### chunk number 258: 
###################################################
phat <- 1755/(1755 + 2771)
(phat - 0.4)/sqrt(0.4 * 0.6/(1755 + 2771)) 


###################################################
### chunk number 259: 
###################################################
-qnorm(0.95)


###################################################
### chunk number 260: 
###################################################
pnorm(-1.680919)


###################################################
### chunk number 261: 
###################################################
prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less", conf.level = 0.99, correct = FALSE)


###################################################
### chunk number 262:  eval=FALSE
###################################################
## library(IPSUR)
## library(HH)
## temp <- prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less", conf.level = 0.99, correct = FALSE)
## plot(temp, 'Hypoth')


###################################################
### chunk number 263: 
###################################################
library(HH)
plot(prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less", conf.level = 0.99, correct = FALSE), 'Hypoth')


###################################################
### chunk number 264: 
###################################################
x <- rnorm(37, mean = 2, sd = 3)
library(TeachingDemos)
z.test(x, mu = 1, sd = 3, conf.level = 0.90)


###################################################
### chunk number 265: 
###################################################
library(HH)
plot(prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less", conf.level = 0.99, correct = FALSE), 'Hypoth')


###################################################
### chunk number 266: 
###################################################
x <- rnorm(13, mean = 2, sd = 3)
t.test(x, mu = 0, conf.level = 0.90, alternative = "greater")


###################################################
### chunk number 267: 
###################################################
library(TeachingDemos)
sigma.test(women$height, sigma = 8)


###################################################
### chunk number 268: 
###################################################
t.test(extra ~ group, data = sleep, paired = TRUE)


###################################################
### chunk number 269: 
###################################################
ks.test(randu$x, "punif")


###################################################
### chunk number 270: 
###################################################
shapiro.test(women$height)


###################################################
### chunk number 271: 
###################################################
with(chickwts, by(weight, feed, shapiro.test))


###################################################
### chunk number 272: 
###################################################
temp <- lm(weight ~ feed, data = chickwts)


###################################################
### chunk number 273: 
###################################################
anova(temp)


###################################################
### chunk number 274: 
###################################################
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
### chunk number 275: 
###################################################
y2 <- rnorm(300, mean = c(4,4.1,4.3))
hist(y2, 30, prob = TRUE)
f <- function(x){dnorm(x, mean = 4)/3}
curve(f, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 4.1)/3}
curve(f, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 4.3)/3}
curve(f, add = TRUE, lwd = 2)


###################################################
### chunk number 276: 
###################################################
library(HH)
old.omd <- par(omd = c(.05,.88, .05,1))
F.setup(df1 = 5, df2 = 30)
F.curve(df1 = 5, df2 = 30, col='blue')
F.observed(3, df1 = 5, df2 = 30)
par(old.omd)


###################################################
### chunk number 277: 
###################################################
library(TeachingDemos)
power.examp()


###################################################
### chunk number 278: 
###################################################
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
### chunk number 279: 
###################################################
head(cars)


###################################################
### chunk number 280: 
###################################################
plot(dist ~ speed, data = cars)


###################################################
### chunk number 281:  eval=FALSE
###################################################
## plot(dist ~ speed, data = cars)


###################################################
### chunk number 282: 
###################################################
cars.lm <- lm(dist ~ speed, data = cars)


###################################################
### chunk number 283: 
###################################################
coef(cars.lm)


###################################################
### chunk number 284: 
###################################################
plot(dist ~ speed, data = cars, pch = 16)
abline(coef(cars.lm))


###################################################
### chunk number 285:  eval=FALSE
###################################################
## plot(dist ~ speed, data = cars, pch = 16)
## abline(coef(cars))


###################################################
### chunk number 286: 
###################################################
cars[5, ]


###################################################
### chunk number 287: 
###################################################
fitted(cars.lm)[1:5]


###################################################
### chunk number 288: 
###################################################
predict(cars.lm, newdata = data.frame(speed = c(6, 8, 21)))


###################################################
### chunk number 289: 
###################################################
residuals(cars.lm)[1:5]


###################################################
### chunk number 290: 
###################################################
carsumry <- summary(cars.lm)
carsumry$sigma


###################################################
### chunk number 291: 
###################################################
summary(cars.lm)


###################################################
### chunk number 292: 
###################################################
A <- round(summary(cars.lm)$coef, 3)
B <- round(confint(cars.lm), 3)


###################################################
### chunk number 293: 
###################################################
confint(cars.lm)


###################################################
### chunk number 294: 
###################################################
new <- data.frame(speed = c(5, 6, 21))


###################################################
### chunk number 295: 
###################################################
predict(cars.lm, newdata = new, interval = "confidence")


###################################################
### chunk number 296: 
###################################################
carsCI <- round(predict(cars.lm, newdata = new, interval = "confidence"), 2)


###################################################
### chunk number 297: 
###################################################
predict(cars.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 298: 
###################################################
carsPI <- round(predict(cars.lm, newdata = new, interval = "prediction"), 2)


###################################################
### chunk number 299: 
###################################################
library(HH)
print(ci.plot(cars.lm))


###################################################
### chunk number 300:  eval=FALSE
###################################################
## library(HH)
## ci.plot(cars.lm)


###################################################
### chunk number 301: 
###################################################
summary(cars.lm)


###################################################
### chunk number 302: 
###################################################
A <- round(summary(cars.lm)$coef, 3)
B <- round(confint(cars.lm), 3)


###################################################
### chunk number 303: 
###################################################
anova(cars.lm)


###################################################
### chunk number 304: 
###################################################
carsumry$r.squared


###################################################
### chunk number 305: 
###################################################
sqrt(carsumry$r.squared)


###################################################
### chunk number 306: 
###################################################
anova(cars.lm)


###################################################
### chunk number 307: 
###################################################
plot(cars.lm, which = 2)


###################################################
### chunk number 308: 
###################################################
shapiro.test(residuals(cars.lm))


###################################################
### chunk number 309: 
###################################################
plot(cars.lm, which = 3)


###################################################
### chunk number 310: 
###################################################
library(lmtest)
bptest(cars.lm)


###################################################
### chunk number 311: 
###################################################
plot(cars.lm, which = 1)


###################################################
### chunk number 312: 
###################################################
library(lmtest)
dwtest(cars.lm, alternative = "two.sided")


###################################################
### chunk number 313: 
###################################################
sres <- rstandard(cars.lm)
sres[1:5]


###################################################
### chunk number 314: 
###################################################
sres[which(abs(sres) > 2)]


###################################################
### chunk number 315: 
###################################################
sdelres <- rstudent(cars.lm)
sdelres[1:5]


###################################################
### chunk number 316: 
###################################################
t0.005 <- qt(0.005, df = 47, lower.tail = FALSE)
sdelres[which(abs(sdelres) > t0.005)]


###################################################
### chunk number 317: 
###################################################
leverage <- hatvalues(cars.lm)
leverage[1:5]
leverage[which(leverage > 4/50)]


###################################################
### chunk number 318: 
###################################################
dfb <- dfbetas(cars.lm)
head(dfb)


###################################################
### chunk number 319: 
###################################################
dff <- dffits(cars.lm)
dff[1:5]


###################################################
### chunk number 320: 
###################################################
cooksD <- cooks.distance(cars.lm)
cooksD[1:5]


###################################################
### chunk number 321: 
###################################################
plot(cars.lm, which = 4)


###################################################
### chunk number 322: 
###################################################
F0.50 <- qf(0.5, df1 = 2, df2 = 48)
cooksD[which(cooksD > F0.50)]


###################################################
### chunk number 323:  eval=FALSE
###################################################
## influence.measures(cars.lm)


###################################################
### chunk number 324:  eval=FALSE
###################################################
## par(mfrow = c(2,2))
## plot(cars.lm)
## par(mfrow = c(1,1))


###################################################
### chunk number 325: 
###################################################
par(mfrow = c(2,2))
plot(cars.lm)
par(mfrow = c(1,1))


###################################################
### chunk number 326:  eval=FALSE
###################################################
## plot(cars.lm, which = 5)   # std'd resids vs lev plot
## identify(leverage, sres, n = 4)   # identify 4 points


###################################################
### chunk number 327: 
###################################################
head(trees)


###################################################
### chunk number 328: 
###################################################
library(lattice)
print(splom(trees))


###################################################
### chunk number 329:  eval=FALSE
###################################################
## library(lattice)
## splom(trees)


###################################################
### chunk number 330:  eval=FALSE
###################################################
## library(scatterplot3d)
## s3d <- with(trees, scatterplot3d(Girth, Height, Volume, pch = 16, highlight.3d = TRUE, angle = 60))
## fit <- lm(Volume ~ Girth + Height, data = trees)
## s3d$plane3d(fit)


###################################################
### chunk number 331: 
###################################################
library(scatterplot3d)
s3d <- with(trees, scatterplot3d(Girth, Height, Volume, pch = 16, highlight.3d = TRUE, angle = 60))
fit <- lm(Volume ~ Girth + Height, data = trees)
s3d$plane3d(fit)


###################################################
### chunk number 332: 
###################################################
trees.lm <- lm(Volume ~ Girth + Height, data = trees)
trees.lm


###################################################
### chunk number 333: 
###################################################
head(model.matrix(trees.lm))


###################################################
### chunk number 334: 
###################################################
fitted(trees.lm)[1:5]


###################################################
### chunk number 335: 
###################################################
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))


###################################################
### chunk number 336: 
###################################################
new


###################################################
### chunk number 337: 
###################################################
predict(trees.lm, newdata = new)


###################################################
### chunk number 338: 
###################################################
treesFIT <- round(predict(trees.lm, newdata = new), 1)


###################################################
### chunk number 339: 
###################################################
residuals(trees.lm)[1:5]


###################################################
### chunk number 340: 
###################################################
treesumry <- summary(trees.lm)
treesumry$sigma


###################################################
### chunk number 341: 
###################################################
confint(trees.lm)


###################################################
### chunk number 342: 
###################################################
treesPAR <- round(confint(trees.lm), 1)


###################################################
### chunk number 343: 
###################################################
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))


###################################################
### chunk number 344: 
###################################################
predict(trees.lm, newdata = new, interval = "confidence")


###################################################
### chunk number 345: 
###################################################
treesCI <- round(predict(trees.lm, newdata = new, interval = "confidence"), 1)


###################################################
### chunk number 346: 
###################################################
predict(trees.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 347: 
###################################################
treesPI <- round(predict(trees.lm, newdata = new, interval = "prediction"), 1)


###################################################
### chunk number 348: 
###################################################
treesumry$r.squared
treesumry$adj.r.squared


###################################################
### chunk number 349: 
###################################################
treesumry$fstatistic


###################################################
### chunk number 350: 
###################################################
treesumry


###################################################
### chunk number 351: 
###################################################
plot(Volume ~ Girth, data = trees)


###################################################
### chunk number 352: 
###################################################
treesquad.lm <- lm(Volume ~ scale(Girth) + I(scale(Girth)^2), data = trees)
summary(treesquad.lm)


###################################################
### chunk number 353:  eval=FALSE
###################################################
## plot(Volume ~ scale(Girth), data = trees)
## lines(fitted(treesquad.lm) ~ scale(Girth), data = trees)


###################################################
### chunk number 354: 
###################################################
plot(Volume ~ scale(Girth), data = trees)
lines(fitted(treesquad.lm) ~ scale(Girth), data = trees)


###################################################
### chunk number 355: 
###################################################
new <- data.frame(Girth = c(9.1, 11.6, 12.5))
predict(treesquad.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 356: 
###################################################
summary(lm(Volume ~ Girth + I(Girth^2), data = trees))


###################################################
### chunk number 357: 
###################################################
treesint.lm <- lm(Volume ~ Girth + Height + Girth:Height, data = trees)
summary(treesint.lm)


###################################################
### chunk number 358: 
###################################################
confint(treesint.lm)
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))
predict(treesint.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 359: 
###################################################
trees$Tall <- cut(trees$Height, breaks = c(-Inf, 76, Inf), labels = c("no","yes"))
trees$Tall[1:5]


###################################################
### chunk number 360: 
###################################################
class(trees$Tall)


###################################################
### chunk number 361: 
###################################################
treesdummy.lm <- lm(Volume ~ Girth + Tall, data = trees)
summary(treesdummy.lm)


###################################################
### chunk number 362:  eval=FALSE
###################################################
## treesTall <- split(trees, trees$Tall)
## treesTall[["yes"]]$Fit <- predict(treesdummy.lm, treesTall[["yes"]])
## treesTall[["no"]]$Fit <- predict(treesdummy.lm, treesTall[["no"]])
## plot(Volume ~ Girth, data = trees, type = "n")
## points(Volume ~ Girth, data = treesTall[["yes"]], pch = 1)
## points(Volume ~ Girth, data = treesTall[["no"]], pch = 2)
## lines(Fit ~ Girth, data = treesTall[["yes"]])
## lines(Fit ~ Girth, data = treesTall[["no"]])


###################################################
### chunk number 363: 
###################################################
treesTall <- split(trees, trees$Tall)
treesTall[["yes"]]$Fit <- predict(treesdummy.lm, treesTall[["yes"]])
treesTall[["no"]]$Fit <- predict(treesdummy.lm, treesTall[["no"]])
plot(Volume ~ Girth, data = trees, type = "n")
points(Volume ~ Girth, data = treesTall[["yes"]], pch = 1)
points(Volume ~ Girth, data = treesTall[["no"]], pch = 2)
lines(Fit ~ Girth, data = treesTall[["yes"]])
lines(Fit ~ Girth, data = treesTall[["no"]])


###################################################
### chunk number 364: 
###################################################
treesfull.lm <- lm(Volume ~ Girth + I(Girth^2) + Height + I(Height^2), data = trees)
summary(treesfull.lm)


###################################################
### chunk number 365: 
###################################################
treesreduced.lm <- lm(Volume ~ -1 + Girth + I(Girth^2), data = trees)


###################################################
### chunk number 366: 
###################################################
anova(treesreduced.lm, treesfull.lm)


###################################################
### chunk number 367: 
###################################################
treesreduced2.lm <- lm(Volume ~ Girth + I(Girth^2) + Height, data = trees)
anova(treesreduced2.lm, treesfull.lm)


###################################################
### chunk number 368: 
###################################################
treesNonlin.lm <- lm(log(Volume) ~ log(Girth) + log(Height), data = trees)
summary(treesNonlin.lm)


###################################################
### chunk number 369: 
###################################################
exp(confint(treesNonlin.lm))


###################################################
### chunk number 370: 
###################################################
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))
exp(predict(treesNonlin.lm, newdata = new, interval = "confidence"))


###################################################
### chunk number 371: 
###################################################
srs <- rnorm(25, mean = 3)
resamps <- replicate(1000, sample(srs, 25, TRUE), simplify = FALSE)
xbarstar <- sapply(resamps, mean, simplify = TRUE)


###################################################
### chunk number 372: 
###################################################
hist(xbarstar, breaks = 40, prob = TRUE)
curve(dnorm(x, 3, 0.2), add = TRUE)


###################################################
### chunk number 373:  eval=FALSE
###################################################
## hist(xbarstar, breaks = 40, prob = TRUE)
## curve(dnorm(x, 3, 0.2), add = TRUE)  # overlay true normal density


###################################################
### chunk number 374: 
###################################################
mean(xbarstar)
mean(srs)
mean(xbarstar) - mean(srs)


###################################################
### chunk number 375: 
###################################################
sd(xbarstar)


###################################################
### chunk number 376: 
###################################################
resamps <- replicate(1000, sample(rivers, 141, TRUE), simplify = FALSE)
medstar <- sapply(resamps, median, simplify = TRUE)
sd(medstar)


###################################################
### chunk number 377: 
###################################################
hist(medstar, breaks = 40, prob = TRUE)


###################################################
### chunk number 378:  eval=FALSE
###################################################
## hist(medstar, breaks = 40, prob = TRUE)


###################################################
### chunk number 379: 
###################################################
median(rivers)
mean(medstar)
mean(medstar) - median(rivers)


###################################################
### chunk number 380: 
###################################################
library(boot)
mean_fun <- function(x, indices) mean(x[indices])
boot(data = srs, statistic = mean_fun, R = 1000)


###################################################
### chunk number 381: 
###################################################
median_fun <- function(x, indices) median(x[indices])
boot(data = rivers, statistic = median_fun, R = 1000)


###################################################
### chunk number 382: 
###################################################
btsamps <- replicate(2000, sample(stack.loss, 21, TRUE), simplify = FALSE)
thetast <- sapply(btsamps, median, simplify = TRUE)
mean(thetast)
median(stack.loss)
quantile(thetast, c(0.025, 0.975))


###################################################
### chunk number 383: 
###################################################
library(boot)
med_fun <- function(x, ind) median(x[ind])
med_boot <- boot(stack.loss, med_fun, R = 2000)
boot.ci(med_boot, type = c("perc", "norm", "bca"))


###################################################
### chunk number 384: 
###################################################
library(coin)
oneway_test(len ~ supp, data = ToothGrowth)


###################################################
### chunk number 385: 
###################################################
t.test(len ~ supp, data = ToothGrowth, alt = "greater", var.equal = TRUE)


###################################################
### chunk number 386: 
###################################################
A <- show(oneway_test(len ~ supp, data = ToothGrowth))
B <- t.test(len ~ supp, data = ToothGrowth, alt = "greater", var.equal = TRUE)


###################################################
### chunk number 387:  eval=FALSE
###################################################
## install.packages("IPSUR", repos="http://R-Forge.R-project.org")
## library(IPSUR)
## read(IPSUR)


###################################################
### chunk number 388:  eval=FALSE
###################################################
## install.packages("IPSUR", repos="http://R-Forge.R-project.org")
## library(IPSUR)
## read(IPSUR)


###################################################
### chunk number 389:  eval=FALSE
###################################################
## install.packages("IPSUR", repos="http://R-Forge.R-project.org")
## library(IPSUR)
## read(IPSUR)


###################################################
### chunk number 390: 
###################################################
sessionInfo()


###################################################
### chunk number 391: 
###################################################
x <- c(3, 5, 9)


###################################################
### chunk number 392: 
###################################################
y <- c(3, "5", TRUE)


###################################################
### chunk number 393: 
###################################################
matrix(letters[1:6], nrow = 2, ncol = 3)


###################################################
### chunk number 394: 
###################################################
matrix(letters[1:6], nrow = 2, ncol = 3, byrow = TRUE)


###################################################
### chunk number 395: 
###################################################
matrix(c(1,"2",NA, FALSE), nrow = 2, ncol = 3)


###################################################
### chunk number 396: 
###################################################
A <- matrix(1:6, 2, 3)
B <- matrix(2:7, 2, 3)
A + B
A * B


###################################################
### chunk number 397: 
###################################################
try(A * B)     # an error
A %*% t(B)     # this is alright


###################################################
### chunk number 398: 
###################################################
solve(A %*% t(B))     # input matrix must be square


###################################################
### chunk number 399: 
###################################################
array(LETTERS[1:24], dim = c(3,4,2))


###################################################
### chunk number 400: 
###################################################
x <- c(1.3, 5.2, 6)
y <- letters[1:3]
z <- c(TRUE, FALSE, TRUE)
A <- data.frame(x, y, z)
A


###################################################
### chunk number 401: 
###################################################
names(A) <- c("Fred","Mary","Sue")
A


###################################################
### chunk number 402: 
###################################################
A <- as.data.frame(Titanic)
head(A)


###################################################
### chunk number 403: 
###################################################
library(reshape)
B <- with(A, untable(A, Freq))
head(B)


###################################################
### chunk number 404: 
###################################################
C <- B[, -5]
rownames(C) <- 1:dim(C)[1]
head(C)


###################################################
### chunk number 405: 
###################################################
tab <- matrix(1:6, nrow = 2, ncol = 3)
rownames(tab) <- c('first', 'second')
colnames(tab) <- c('A', 'B', 'C')
tab  # Counts


###################################################
### chunk number 406: 
###################################################
p <- c("milk","tea")
g <- c("milk","tea")
catgs <- expand.grid(poured = p, guessed = g)
cnts <- c(3, 1, 1, 3)
D <- cbind(catgs, count = cnts)
xtabs(count ~ poured + guessed, data = D)


###################################################
### chunk number 407:  eval=FALSE
###################################################
## library(foreign)
## read.spss("foo.sav")


###################################################
### chunk number 408: 
###################################################
Tmp <- Puromycin[order(Puromycin$conc), ]
head(Tmp)


###################################################
### chunk number 409:  eval=FALSE
###################################################
## with(Puromycin, Puromycin[order(conc), ])


###################################################
### chunk number 410:  eval=FALSE
###################################################
## with(Puromycin, Puromycin[order(state, conc), ])


###################################################
### chunk number 411: 
###################################################
Tmp <- with(Puromycin, Puromycin[order(-conc), ])
head(Tmp)


###################################################
### chunk number 412: 
###################################################
Tmp <- with(Puromycin, Puromycin[order(-xtfrm(state)), ])
head(Tmp)


###################################################
### chunk number 413:  eval=FALSE
###################################################
## library(odfWeave)
## odfWeave(file = "infile.odt", dest = "outfile.odt")


###################################################
### chunk number 414: 
###################################################
library(Hmisc)
summary(cbind(Sepal.Length, Sepal.Width) ~ Species, data = iris)


###################################################
### chunk number 415: 
###################################################
set.seed(095259)


###################################################
### chunk number 416: 
###################################################
options(digits = 16)
runif(1)


###################################################
### chunk number 417: 
###################################################
rm(.Random.seed)
try(dir.create("../../data"), silent = TRUE)
save.image(file = "../../data/IPSUR.RData")
Stangle(file="IPSUR.Rnw", output="../IPSUR.R", annotate=TRUE)



#--------------------Setup-------------------- 

#Data setup
data = read.table("hcmv.txt", header=TRUE)
head(data)
summary(data)


#--------------------Problem 1--------------------
#Setting Variables
n =  296
N = 229354
k = 23

#Sample 1
set.seed(2)
sample1 <- sample.int(N, size = n, replace = FALSE)
hist(sample1, breaks = 50, col = 3, main = "Uniform Distribution Random Sample 1", 
     xlab = "Location", ylab = "Counts", ylim = range(c(0,20)), xlim = range(c(0,250000)))

#Sample 2
set.seed(52)
sample2 <- sample.int(N, size = n, replace = FALSE)
hist(sample2, breaks = 50, col = 3, main = "Uniform Distribution Random Sample 2", 
     xlab = "Location", ylab = "Counts", ylim = range(c(0,20)), xlim = range(c(0,250000)))

#Sample 3
set.seed(34)
sample3 <- sample.int(N, size = n, replace = FALSE)
hist(sample3, breaks = 50, col = 3, main = "Uniform Distribution Random Sample 3", 
     xlab = "Location", ylab = "Counts", ylim = range(c(0,20)), xlim = range(c(0,250000)))

#Location Data
hist(data$location, breaks = 50, col = 6, main = "Palindrome Location Data", xlab = "Location", ylab = "Counts",
     ylim = range(c(0,20)), xlim = range(c(0,250000)))

#Stripplot of randoms against actual data
a <- list("Rand1" = sample1,
          "Rand2" = sample2, 
          "Rand3" = sample3,
          "Locations" = data$location)

stripchart(a, method = "jitter", col = c(3,3,3,6), pch = 16, xlab = "Location")

#CDFS of Uniform Randoms and the actual Data
plot(ecdf(data$location), main = "CDF of Palindrome Location Data", xlab = "Location")
abline(0, 1/N, col = 'red', lwd = 2)

plot(ecdf(sample1), main = "CDF of Uniform Distribution Random Sample 1", xlab = "Location")
abline(0, 1/N, col = 'red', lwd = 2)

plot(ecdf(sample2), main = "CDF of Uniform Distribution Random Sample 2", xlab = "Location")
abline(0, 1/N, col = 'red', lwd = 2)

plot(ecdf(sample3), main = "CDF of Uniform Distribution Random Sample 3", xlab = "Location")
abline(0, 1/N, col = 'red', lwd = 2)


#--------------------Problem 2--------------------
#Distances for pairs doubles and triples for the data
pairs <- diff(data$location)
skipone <- diff(data$location, lag = 2)
skiptwo <- diff(data$location, lag = 3)

#Distances for pairs doubles and triples for the three uniform random samples
sample1order <- sort(sample1, decreasing = FALSE)
pairs1 <- diff(sample1order)
skipone1 <- diff(sample1order, lag = 2)
skiptwo1 <- diff(sample1order, lag = 3)

sample2order <- sort(sample2, decreasing = FALSE)
pairs2 <- diff(sample2order)
skipone2 <- diff(sample2order, lag = 2)
skiptwo2 <- diff(sample2order, lag = 3)

sample3order <- sort(sample3, decreasing = FALSE)
pairs3 <- diff(sample3order)
skipone3 <- diff(sample3order, lag = 2)
skiptwo3 <- diff(sample3order, lag = 3)

#Summaries of all distances 
summary(pairs)
summary(skipone)
summary(skiptwo)


summary(pairs1)
summary(skipone1)
summary(skiptwo1)

summary(pairs2)
summary(skipone2)
summary(skiptwo2)

summary(pairs3)
summary(skipone3)
summary(skiptwo3)


#Stripplot of randoms against actual data for PAIRS
x <- list("Rand1" = pairs1,
          "Rand2" = pairs2, 
          "Rand3" = pairs3,
          "Data" = pairs)

stripchart(x, method = "jitter", col = c(3,3,3,6), pch = 16, xlab = "Distances between Pairs")

#Stripplot of randoms against actual data for DOUBLES
y <- list("Rand1" = skipone1,
          "Rand2" = skipone2, 
          "Rand3" = skipone3,
          "Data" = skipone)

stripchart(y, method = "jitter", col = c(3,3,3,6), pch = 16, xlab = "Distances between Doubles")

#Stripplot of randoms against actual data for TRIPLES
z <- list("Rand1" = skiptwo1,
          "Rand2" = skiptwo2, 
          "Rand3" = skiptwo3,
          "Data" = skiptwo)

stripchart(z, method = "jitter", col = c(3,3,3,6), pch = 16, xlab = "Distances between Triples")


#Histograms for Distribution of Spacings for pairs (data and uniforms)

#Data
hist(pairs, main = "Distances between Pairs (Data)", xlab = "Distance", ylab = "Count", col = 6)

#Uniform 1
hist(pairs1, main = "Distances between Pairs (Rand1)", xlab = "Distance", ylab = "Count", col = 3)

#Uniform 2
hist(pairs2, main = "Distances between Pairs (Rand2)", xlab = "Distance", ylab = "Count", col = 3)

#Uniform 3
hist(pairs3, main = "Distances between Pairs (Rand3)", xlab = "Distance", ylab = "Count", col = 3)

par(mfrow = c(1,1))
#EXP OR MAYBE GAMMA 
exp_pairs <- rexp(295, rate = 1/775.5119)

b <- list("Data Distances" = pairs, "Exponential Distribution" = exp_pairs)

stripchart(b, method = "jitter", col = c(1,2), pch = 1)


#--------------------Problem 3--------------------

#Sample 1 Counts
tab1 <- table(cut(sample1, breaks = seq(0, 229354, length.out = k+1), include.lowest = TRUE))
counts1 <- as.vector(tab1)

#Sample 2 Counts
tab2 <- table(cut(sample2, breaks = seq(0, 229354, length.out = k+1), include.lowest = TRUE))
counts2 <- as.vector(tab2)

#Sample 3 Counts
tab3 <- table(cut(sample3, breaks = seq(0, 229354, length.out = k+1), include.lowest = TRUE))
counts3 <- as.vector(tab3)

#Data Counts
tabdata <- table(cut(data$location, breaks = seq(0, 229354, length.out = k+1), include.lowest = TRUE))
countsdata <- as.vector(tabdata)

#Sample 1 vs. Poisson Histogram
set.seed(11)
hist(counts1, breaks = 7, col = rgb(1,0,0,0.5), probability = TRUE, xlab = "Number of points inside an interval", ylim = c(0,0.25), 
     main = "Poisson vs Uniform Random Sample 1 Comparison of Density")
lines(density(counts1, adjust = 2), col = rgb(1,0,0,0.5))
Pois1 <- rpois(n, lambda = mean(counts1))
hist(Pois1, breaks = 10, col = rgb(0,0,1,0.5), probability = TRUE, add = TRUE)
lines(density(Pois1, adjust = 2), col = rgb(0,0,1,0.5))
legend(x = 19, y = 0.20, legend = c("sample", "Poisson"), lty = c(1,1), col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

#Sample 1 vs. Poisson Chi-Square Test
chisq.test(counts1)
par(mfrow = c(1,1))
#Sample 2 vs. Poisson Histogram
set.seed(13)
hist(counts2, breaks = 5, col = rgb(1,0,0,0.5), probability = TRUE, xlab = "number of points inside an interval", ylim = c(0,0.25), 
     main = "Poisson vs Uniform Random Sample 2 Comparison of Density")
lines(density(counts2, adjust = 2), col = rgb(1,0,0,0.5))
Pois2 <- rpois(n, lambda = mean(counts2))
hist(Pois2, breaks = 10, col = rgb(0,0,1,0.5), probability = TRUE, add = TRUE)
lines(density(Pois2, adjust = 2), col = rgb(0,0,1,0.5))
legend(x = 15, y = 0.24, legend = c("sample", "Poisson"), lty = c(1,1), col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

#Sample 2 vs. Poisson Chi-Square Test
chisq.test(counts2)

#Sample 3 vs. Poisson Histogram
set.seed(15)
hist(counts3, breaks = 7, col = rgb(1,0,0,0.5), probability = TRUE, xlab = "number of points inside an interval", ylim = c(0,0.25), 
     main = "Poisson vs Uniform Random Sample 3 Comparison of Density")
lines(density(counts3, adjust = 2), col = rgb(1,0,0,0.5))
Pois3 <- rpois(n, lambda = mean(counts3))
hist(Pois3, breaks = 10, col = rgb(0,0,1,0.5), probability = TRUE, add = TRUE)
lines(density(Pois3, adjust = 2), col = rgb(0,0,1,0.5))
legend(x = 16, y = 0.20, legend = c("sample", "Poisson"), lty = c(1,1), col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

#Sample 3 vs. Poisson Chi-Square Test
chisq.test(counts3)

#Sample 1 vs. Data Histogram USING THIS ONE
set.seed(6)
hist(counts1, breaks = 7, col = rgb(1,0,0,0.5), xlab = "Number of points inside an interval", 
     main = "Data vs Uniform Random Sample 1 Comparison of Counts", ylim = c(0,8))
hist(tabdata, breaks = 10, col = rgb(0,0,1,0.5), add = TRUE)
legend(x = 19, y = 6, legend = c("Sample", "Data"), lty = c(1,1), col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

#Sample 1 vs. Data Chi-Square Test
chisq.test(counts1, tabdata)

#Sample 2 vs. Data Histogram
set.seed(7)
hist(counts2, breaks = 7, col = rgb(1,0,0,0.5), xlab = "number of points inside an interval", ylim = c(0,8), 
     main = "Data vs Uniform Random Sample 2 Comparison of Counts")
hist(tabdata, breaks = 10, col = rgb(0,0,1,0.5), add = TRUE)
legend(x = 6, y = 8, legend = c("Sample", "Data"), lty = c(1,1), col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

#Sample 2 vs. Data Chi-Square Test
chisq.test(counts2, tabdata)

#Sample 3 vs. Data Histogram
set.seed(8)
hist(counts3, breaks = 7, col = rgb(1,0,0,0.5), xlab = "number of points inside an interval", ylim = c(0,8), 
     main = "Data vs Uniform Random Sample 3 Comparison of Counts")
hist(tabdata, breaks = 10, col = rgb(0,0,1,0.5), add = TRUE)
legend(x = 17, y = 8, legend = c("Sample", "Data"), lty = c(1,1), col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

#Sample 3 vs. Data Chi-Square Test
chisq.test(counts3, tabdata)

par(mfrow = c(2,2))
#Testing out different interval lengths
#Interval length of roughly 10000
hist(data$location, breaks = 23, col = 2, main = "Palindrome Location Data", xlab = "Location", ylab = "Counts")

#Interval length of roughly 4000 OPTIMAL
hist(data$location, breaks = 57, col = 4, main = "Palindrome Location Data", xlab = "Location", ylab = "Counts")

#Interval length of roughly 2300
hist(data$location, breaks = 100, col = 7, main = "Palindrome Location Data", xlab = "Location", ylab = "Counts")


int1 = length(which(data$location > 76000 & data$location < 80000))
int2 = length(which(data$location > 80001 & data$location < 84000))
int3 = length(which(data$location > 87500 & data$location < 91500))
int4 = length(which(data$location > 91501 & data$location < 95500))
int5 = length(which(data$location > 196000 & data$location < 2000000))
int6 = length(which(data$location > 200001 & data$location < 204000))

setup = as.table(c(int1, int2, int3, int4, int5, int6))
setup 

chisq.test(setup)


#--------------------Problem 4--------------------

#Interval Length
M = 4000
nsim = 2000

#Simulate data
set.seed(123)
max.count = rep(NULL, nsim)
for(i in 1:nsim) {
  Xmax = sample(1:N, size = n, replace = F)
  Xmax.hist = hist(Xmax, breaks = seq(0, 232000, by = M), plot = F)
  max.count[i] = max(Xmax.hist$counts)
}

#Distribution of Maximums
hist(max.count, main = 'Simulated Max Counts of Palindromes', 
     xlab = 'Max Number of Palindromes Per Simulation', col=3)


#Finding p-value
max.count.obs = max(hist(data$location, breaks = 57, plot = F)$counts)
pval = sum(max.count >= max.count.obs)/nsim
pval


#--------------------Advanced Analysis--------------------
diffdf = diff(data$location)
advdataframe <- data.frame (space = c(sum(diffdf[1:5]), sum(diffdf[6:10]), sum(diffdf[11:15]), 
  sum(diffdf[16:20]), sum(diffdf[21:25]), sum(diffdf[26:30]), sum(diffdf[31:35]), sum(diffdf[36:40]), 
  sum(diffdf[41:45]), sum(diffdf[46:50]), sum(diffdf[51:55]), sum(diffdf[56:60]), sum(diffdf[61:65]), 
  sum(diffdf[66:70]), sum(diffdf[71:75]), sum(diffdf[76:80]), sum(diffdf[81:85]), sum(diffdf[86:90]), 
  sum(diffdf[91:95]), sum(diffdf[96:100]), sum(diffdf[101:105]), sum(diffdf[106:110]), sum(diffdf[111:115]), 
  sum(diffdf[116:120]), sum(diffdf[121:125]), sum(diffdf[126:130]), sum(diffdf[131:135]), sum(diffdf[136:140]), 
  sum(diffdf[141:145]), sum(diffdf[146:150]), sum(diffdf[151:155]), sum(diffdf[156:160]), sum(diffdf[161:165]), 
  sum(diffdf[166:170]), sum(diffdf[171:175]), sum(diffdf[176:180]), sum(diffdf[181:185]), sum(diffdf[186:190]), 
  sum(diffdf[191:195]), sum(diffdf[196:200]), sum(diffdf[201:205]), sum(diffdf[206:210]), sum(diffdf[211:215]), 
  sum(diffdf[216:220]), sum(diffdf[221:225]), sum(diffdf[226:230]), sum(diffdf[231:235]), sum(diffdf[236:240]), 
  sum(diffdf[241:245]), sum(diffdf[246:250]), sum(diffdf[251:255]), sum(diffdf[256:260]), sum(diffdf[261:265]), 
  sum(diffdf[266:270]), sum(diffdf[271:275]), sum(diffdf[276:280]), sum(diffdf[281:285]), sum(diffdf[286:290]), 
  sum(diffdf[291:295])))

diffdataframe <- (1/5) * advdataframe

spacedata <- diffdataframe$space

#Histogram of average distances
hist(spacedata, main = 'Distribution of Average Distance Between Palindromes',
     xlab = 'Distance (Base Pairs)', col = 6)

#minimum is at 116-120
#92570-92783 this is our biggest peak, avg distance (43.2)

#next min is at 91-95
#75622 - 76043 this is not a peak, worth looking into, avg distance (100.4)

#next min is at 251-255
#194111 - 195151 this is our second peak, avg distance (222)

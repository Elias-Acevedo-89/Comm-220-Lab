#Problem 2: Binomial r.v.
# Parameters
n <- 8
p <- 0.7

# Calculate the probabilities for X = 5, 6, 7, 8
P_X_5 <- dbinom(5, size = n, prob = p)
P_X_6 <- dbinom(6, size = n, prob = p)
P_X_7 <- dbinom(7, size = n, prob = p)
P_X_8 <- dbinom(8, size = n, prob = p)

# Sum the probabilities to get P(X >= 5)
P_X_ge_5 <- P_X_5 + P_X_6 + P_X_7 + P_X_8
P_X_ge_5

#Height data
set.seed(123)
heights <- rnorm(1000, mean = 170, sd = 10) # Heights in cm

#Histogram
hist(heights, breaks = 20, col = "skyblue", main = "Distribution of Heights", 
     xlab = "Height (cm)", ylab = "Frequency", border = "black")

#Grades data
set.seed(123)
grades <- sample(c("A", "B", "C", "D", "F"), size = 500, replace = TRUE, 
                 prob = c(0.2, 0.3, 0.25, 0.15, 0.1))

#Barplot
barplot(table(grades), col = "lightgreen", main = "Distribution of Grades", 
        xlab = "Grades", ylab = "Frequency", border = "black")

#Pass/fail data
set.seed(123)
pass_fail <- sample(c("Pass", "Fail"), size = 500, replace = TRUE, prob = c(0.8, 0.2))

#Barplot
barplot(table(pass_fail), col = "coral", main = "Pass/Fail Distribution", 
        xlab = "Outcome", ylab = "Frequency", border = "black")

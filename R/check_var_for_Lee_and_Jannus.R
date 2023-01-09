# Using Chapter 4 of Borenstein.

# Lee:
n1 <- n2 <- 50
m1 <- 2.2
sd1 <- 0.5
m2 <- 2
sd2 <- 0.4

s <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))
vd <- (n1+n2)*(s^2)/(n1*n2)
vd

# Alternative:
sd1^2/n1 + sd2^2/n2

# Both give 0.0082.



# Jannus
n1 <- n2 <- 40
m1 <- 2.3
sd1 <- 0.5
m2 <- 2
sd2 <- 0.3

s <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))
vd <- (n1+n2)*(s^2)/(n1*n2)
vd

# Alternative:
sd1^2/n1 + sd2^2/n2

# Both give 0.0085.

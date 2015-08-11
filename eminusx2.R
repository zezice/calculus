## Approximate integral of e^(-x^2) with a power series

eminusx2 <- function(numterms,t) {
    s <- 0
    for (i in 0:numterms) {
        f2 <- function(n) {
            f1 <- function(x) {
                (-1)^(n)*(x)^(2*n)/factorial(n)
            }
        return(f1)
        }
        s <- s + integrate(f2(i),0,t)$value
    }
    s
}
e <- Vectorize(eminusx2)

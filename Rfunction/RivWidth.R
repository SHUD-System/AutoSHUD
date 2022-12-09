AA = seq(1e3, AA1, length.out = 10)[-1]
AA = 10 ^ (0:6 ) * 1e6
a = 3.2
ord=rbind(1:5)
b= max(ord) * .05

# W = a*log(AA)^(b*ord)
fx.W <- function(x) {return(  (log(AA) / 1) ^ (1.2^x) )}

fx.W <- function(x, a, b) {return( log10(AA/1000) ^ a /(1 + exp(- b * 2^x)) )}
fx.D <- function(w){return( log(w) ^ (1+b) ) }


# fx.W <- function(x) {return(log(AA) * log(AA) ^  (b * x) )}
# 
# fx.W <- function(x) {return( a + b ^ (1/ log(AA) ^  (b * x) ) )}
w =  round(t(apply(ord, 2, fx.W, a=a, b=b)), 0)
d = round(fx.D(w), 1)

dim(w)
par(mfrow=c(2,1))
matplot(w, type='l', log=''); grid()
matplot(d, type='l', log=''); grid()

print(round(AA/1e6))
apply(d, 1, summary)
apply(w, 1, summary)
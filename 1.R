#Program oblicza podstawowe statystyki opisowe dla zbioru danych
library(MASS)

is.positive.defined = function(x) {
  x = as.matrix(x)
  n = nrow(x)
  if (n == 1) { 
    return(x[1,1] > 0)
  } else {
    if (isSymmetric.matrix(x)) {
      for(i in 2:n) {
        if (det(x[1:i,1:i]) <= 0) { #wszystkie minory <= 0
          return(FALSE)
        }
      }
      return(x[1,1] > 0)
    } else {
      return(FALSE)
    }
  }
}

T = 10             #liczba obserwacji
k = 5              #ilosc zmiennych
mu = matrix(0, 1, k)
sigma = matrix(scan(textConnection(
" 1.0  0.0  0.5 -0.3  0.2 
  0.0  1.0  0.1  0.0  0.0 
  0.5  0.1  1.0  0.3  0.7 
 -0.3  0.0  0.3  1.0  0.4 
  0.2  0.0  0.7  0.4  1.0 
"
  )), k, byrow=TRUE) 

all( eigen(sigma)$values > 0 )

if (!is.positive.defined(sigma)) {
  stop('Sigma not positive defined! \n\n') 
}

dane = mvrnorm(T, mu, sigma) #losujemy dane

sred = colMeans(dane)
print("średnie")
print(sred)

data(swiss)
y = swiss$Fertility
x = as.matrix(swiss[,-1])

#1. decomp: principal component

decomp = princomp(x,cor = TRUE)
#Set the cor = TRUE will make the decomposition on the correlation/covariance matrix.
#correlation matrix get rids the unit of variance compared with covariance matrix.
#This will make the decomposition more comparable.
plot(cumsum(decomp$sdev^2) / sum(decomp$sdev^2), type = "l")
#The accumulative percentage of the variance explained.

#2. decomp2: eigen value decomposition of the correlation matrix.
decomp2 = eigen(cor(x))
t(decomp2$vector) %*% decomp2$vectors #The identity matrix

#3. decomp3: singular value decomposition of the normalized x matrix.
xnorm = apply(x, 2, function(z) (z - mean(z)) / sd(z))
decomp3 = svd(xnorm)

#4. compare the basis of the principal subspaces
round(rbind(decomp2$vectors,
            decomp$loadings,
            decomp3$v),3)

#The 3 different approaches give us identical orthonomal basis

#5. compare the eigen values
n = nrow(swiss)

#P.S. the singular values / 1/(n-1) = sqrt (eigen values of S)
round(rbind(decomp2$values, decomp$sdev^2, decomp3$d^2/(n-1)),3)

#6. compare the principal components
plot(decomp3$u[,1],decomp$scores[,1])
plot(decomp3$u[,1],
     xnorm %*% decomp2$vectors %*% diag(1/sqrt(decomp2$values))[,1])

#They are the same, exept the eigen value of S approach need to recalculate it by hand.

#7. Application of the principal components.
u = decomp3$u[,1:4]
summary(lm(y ~ u))$r.squared
summary(lm(y ~ x))$r.squared

#basically svd find out a linear combination on X that retains as much information in X.

#The parameter of lm can be calculated in short cut because u is orthonormal
summary(lm(y ~ u))

t(u) %*% y

#home work
decomp <- princomp(mtcars[,c("mpg","hp","drat","wt","qsec")],cor = TRUE)
decomp$sdev^2 / sum(decomp$sdev^2)


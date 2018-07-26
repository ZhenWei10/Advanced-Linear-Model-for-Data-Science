###Coefficient table
fit = lm(Fertility ~ ., data = swiss)
round(summary(fit)$coef,3)

##Generate the same results manually
x = cbind(1, as.matrix(swiss[,-1]))
y = swiss$Fertility
beta = solve(t(x) %*% x, t(x) %*% y) #The LSE
e = y - x %*% beta #The residuals (errors)
n = nrow(x); p = ncol(x)
s = sqrt(sum(e^2)/(n-p)) #Sample variance estimate
#Compare with lm
c(s,summary(fit)$sigma) 


#calculate the t statistics
#this calculate a variance covariance matrix of the random vector of beta.
betaVar = solve(t(x) %*% x) * s ^ 2

##Show that standard errors agree with lm
#P.S. standard error is just a fancy term for sdandard deviation of estimators.
cbind(summary(fit)$coef[,2], sqrt(diag(betaVar)))

# Show that the tstats agree
#P.S. t statistics is the standardized estimate by its own standard deviation.
tstat = beta / sqrt(diag(betaVar))
cbind(summary(fit)$coef[,3], tstat)

#Show that the P-values agree
cbind(summary(fit)$coef[,4], 2 * pt(- abs(tstat), n - p))

#Get the F statistic
#Set K to grab everything except the intercept
k = cbind(0, diag(rep(1, p - 1)))
kvar = k %*% solve(t(x) %*% x) %*% t(k)
fstat = t(k %*% beta) %*% solve(kvar) %*% (k %*% beta) / (p-1) / s ^ 2
#Show that it's equal to what lm is giving
cbind(summary(fit)$fstat[1], fstat)

#Calculate the p-value
pf(fstat, p - 1, n - p, lower.tail = FALSE)
summary(fit)
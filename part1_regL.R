plot(Salary_Data$YearsExperience,Salary_Data$Salary,xlab = "YearsExperience",ylab="salary")

modele=lm(Salary ~YearsExperience,data = data)
coeff=modele$coefficients
plot(data$YearsExperience,data$Salary,xlab = "YearsExperience",ylab="salary")
lines(data$YearsExperience,data$YearsExperience *coeff[2]+coeff[1],col="green")


summary(modele)
print(" On rejette l'hypothèse que l'un des coeff est nul ")


x = seq(0,20,0.01)
intC = predict(modele, newdata=list(YearsExperience=x), interval="confidence")
intP = predict(modele, newdata=list(YearsExperience=x), interval="prediction")
plot(data$YearsExperience,data$Salary,xlab = "YearsExperience",ylab="salary")
abline(coeff[1], coeff[2], col="blue")
lines(x, intC[, 2], col="red")
lines(x, intC[, 3], col="red")
lines(x, intP[, 2], col="green")
lines(x, intP[, 3], col="green")

X = c(1.1 , 1.6 , 1.9 , 1.4 , 1.7 , 1.2 , 1.6 , 1.8 , 2.3)
Y = c(3 , 2.96 , 5.28 , 3.72 , 3.76 , 2.91 , 4.34 , 4.28 , 5.71)

#estimation alpha et beta 

plot(X,Y)

print(paste("Moyenne de X:", mean(X)))
print(paste("Moyenne de Y:", mean(Y)))
print(paste("Covariance non corrigée de X et Y:", cov(X,Y)*8/9))
print(paste("Variance non corrigée de X:", var(X)*8/9))


a = cov(X,Y)/var(X)
b = mean(Y) - a * mean(X)
print(paste("Estimation de alpha:", a))
print(paste("Estimation de beta:", b))


plot(X,Y)
abline(b,a)


modele = lm(Y~X)
print(coefficients(modele))

confint(modele, level=0.99)

residu = Y - (a * X + b)
print(round(residu, 3))
print(round(modele$residuals, 3))

sigma = sqrt(sum(residu^2)/7)
print(paste("Estimation de sigma:", sigma))
summary(modele)


R2 = cor(X, Y)^2
print(paste("Calcul de R²:", R2))
summary(modele)
```











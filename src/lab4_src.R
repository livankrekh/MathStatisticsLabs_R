
x1 <- c(52.06, 73.8, 83.85, 84.28, 98.84, 103.3, 105.3, 108.4, 114.5, 148.8, 181.3, 206.6, 236, 274.7)
x2 <- c(25.17, 27.33, 26.17, 27.39, 37.17, 27.2, 28.17, 30.47, 29.17, 30.48, 30.17, 31.92, 31.17, 33.95)
y <- c(68.26, 73.07, 70.62, 73.88, 74.16, 74.75, 75.59, 80.15, 79.34, 82.63, 81.01, 86.12, 84.06, 90.04)

lab4_MultiRegression <- function(x1=x1, x2=x2, y=y) {

	cor(data.frame(x1, x2, y))

	fm <- lm(y ~ x1+x2)
	fm
	summary(fm)

	library(lmtest)
	dwtest(fm)

	fm <- lm(y~x1)
	fm
	summary(fm)
	dwtest(fm)

	fm <- lm(y~x2)
	fm
	summary(fm)
	dwtest(fm)
}
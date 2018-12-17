
x <- c(2.17, 2.9, 3.29, 4.13, 5.25, 4.92, 5.79, 5.87, 6.99, 7.04, 8.14, 8.06, 8.57, 9.45, 9.06)
y <- c(16.21, 17.75, 18.39, 18.87, 19.6, 21.21, 21.84, 23, 24.44, 25.36, 25.54, 27.14, 27.95, 28.99, 30.8)

lab3_Regression <- function(x=x, y=y) {
	fm <- lm(y ~ x)

	print("Regression model:")
	print(fm)

	print("Model details")
	print(summary(fm))
}
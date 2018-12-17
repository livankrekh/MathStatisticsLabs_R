
xi <- c(rep(1.5 ,2 + 2*13), rep(1.53, 4 + 2*13), rep(1.55, 6 + 2*13), rep(1.59, 8 + 2*13), rep(1.6, 2 + 2*13), rep(1.62, 2*13))
yi <- c(rep(1.58, 2 + 2*13), rep(1.6, 3 + 2*13), rep(1.65, 5 + 2*13), rep(1.74, 7 + 2*13), rep(1.8, 4 + 2*13), rep(1.82, 2 + 2*13))

lab2_TStudent <- function(xi=xi, yi=yi) {
	xi <- xi + 13/12
	yi <- yi + 13/12

	print("xi:")
	print(xi)
	print("_____________________")
	print("yi:")
	print(yi)

	print("Result:")
	print(t.test(xi, yi))
}

xi2 <- c(rep(10.1, 4 + 2*13), rep(10.2, 5 + 2*13), rep(10.3, 9 + 2*13), rep(10.4, 6 + 2*13), rep(10.5, 5 + 2*13), rep(10.7, 2*13))
yi2 <- c(rep(10.2, 2 + 2*13), rep(10.3, 2 + 2*13), rep(10.6, 6 + 2*13), rep(10.7, 10 + 2*13), rep(10.9, 7 + 2*13), rep(11.2, 3*13))

lab2_Fisher <- function(xi=xi2, yi=yi2) {
	xi <- xi + 13/12
	yi <- yi + 13/12

	print("xi:")
	print(xi)
	print("_____________________")
	print("yi:")
	print(yi)

	print("Result: ")
	print(var.test(xi,yi))
}

xi3 <- c(rep(9.82, 2 + 2*13), rep(9.9, 1 + 2*13), rep(10.01, 2 + 2*13), rep(10.24, 3 + 2*13), rep(10.5, 2 + 2*13), rep(10.82, 2*13))
yi3 <- c(rep(9.5, 2 + 2*13), rep(9.56, 2 + 2*13), rep(9.68, 1 + 2*13), rep(9.9, 2 + 2*13), rep(10.03, 3 + 2*13), rep(10.5, 2*13), rep(10.52, 2*13))

lab2_Wilcoxon <- function(xi=xi3, yi=yi3) {
	print("xi:")
	print(xi)
	print("_____________________")
	print("yi:")
	print(yi)

	print("Result:")
	print(wilcox.test(xi3,yi3))
}

xi4 <- c(0.37,0.36,0.34,0.36,0.33,0.35,0.37,0.36,0.35,0.38,0.34, 0.33,0.36,0.35,0.37)
yi4 <- c(0.35, 0.34, 0.33, 0.37, 0.36, 0.32, 0.34, 0.35, 0.32, 0.34,0.32,0.33,0.38,0.35,0.36)

lab2_Binom <- function(xi=xi3, yi=yi3) {
	print("xi:")
	print(xi)
	print("_____________________")
	print("yi:")
	print(yi)

	diff <- yi4 - xi4

	print("Result: ")
	print(binom.test(length(diff[diff > 0]), length(diff)))
}

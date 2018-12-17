
x<-c(175,178,172,175,175,180,165,178,186,175,172,175,175,165,186,180,180,178,172,180)

# Print general statistics parameters information about vector

first_lab <- function(x_vec=x) {
	print("Sorted x:")
	print(sort(x))

	print("Variants in x vector:")
	xi <- unique(x)
	print(xi)

	print("Min:")
	print(min(x))
	print("Max:")
	print(max(x))
	print("Mean: ")
	print(mean(x))
	print("Median:")
	print(median(x))
	print("Standart deviation: ")
	print(sd(x))
	print("Variative koff: ")
	print(var(x))
	print("Quantiles: ")
	print(quantile(x))
	plot(x, main="Simple Plot by index")
	boxplot(x, main="Boxplot")
	hist(x, main="Histogram")

	h_p <- hist(xi,main="Histogram with Poligons")
	lines(h_p$counts ~ h_p$mids, col="blue")
}
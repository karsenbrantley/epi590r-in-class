x <- c(1, 3, 5, 7, 9)

n <- length(x)
sum(x)/n

new_mean <- function(x){
	n <- length(x)
	mean_val <- sum(x)/n
	return(mean_val)

}
new_mean(x=x)

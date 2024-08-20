library(tidyverse)
library(gtsummary)
nlsy <- read_csv(here::here("data","raw","nlsy.csv"))

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))



x <- c(1, 3, 5, 7, 9)

n <- length(x)
sum(x)/n

new_mean <- function(x){
	n <- length(x)
	mean_val <- sum(x)/n
	return(mean_val)

}

new_mean(x=x)
new_mean(x=c(100,200,300))

square <- function(x){
	square_val <- x^2
	return(square_val)
}

square(x=53)


x <- c(0,1,1)
multiplier <- 100
multiplier*sum(x)/length(x)


prop <- function(x, multiplier = 1) {
	n <- length(x)
	mean_val <- multiplier*sum(x) / n
	return(mean_val)
}

# multiplier = 1 sets this as default argument

prop(x, 100)
prop(x, 1)

# function to raise some number x to any power, with default set to squaring it
raise <- function(x, power = 2){
	raise_val <- x^power
	return(raise_val)
}

raise(x=2, power=4)
raise(x=2)

# fitting models with functions
logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial()
)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat,
										 data = nlsy, family = poisson()
)
logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
												 data = nlsy, family = binomial(link = "log")
)

new_table_function <- function(model, tidy_fun = NULL) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"
		),
		tidy_fun = tidy_fun
	)
}

new_table_function(logistic_model)
new_table_function(poisson_model)
new_table_function(logbinomial_model, tidy_fun = partial)

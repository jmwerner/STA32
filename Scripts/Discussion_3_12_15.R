# Bootstrap examples

data(mtcars)

data = rep(mtcars$mpg, 5) + rnorm(length(mtcars$mpg) * 5, sd = 5)


# Bootstrap estimate and sd of A = range / IQR

B = 1000
bootstrap_estimates = sapply(1:B, function(i, data_set){
	
	boot_data = sample(data_set, length(data_set), replace = TRUE)

	range_estimate = diff(range(boot_data))
	iqr_estimate = IQR(boot_data)

	return(range_estimate / iqr_estimate)
}, data_set = data)

Ahat = mean(bootstrap_estimates)
actual_A = diff(range(data))/IQR(data) # The value of the statistic A for the actual data

Ahat_sd = sd(bootstrap_estimates)



# Returns bootstrap estimate and sd of xbar
xbar_boot = function(B_in, data_in){
	results = sapply(1:B_in, function(i, data_set){
		boot_data = sample(data_set, length(data_set), replace = TRUE)
		return(mean(boot_data))
	}, data_set = data_in)
	return(c(mean(results), sd(results)))
}


# Compare bootstrap confidence intervals to t-based confidence intervals

compare_CI = function(alpha_in, N_in, B_in, n_in, real_mean_in, new_data_pop){
	results = sapply(1:N_in, function(i){
		new_data_in = sample(new_data_pop, n_in)
		xbar = mean(new_data_in)
		tscore = qt(1-alpha_in/2, length(new_data_in)-1)
		interval_length = tscore * sd(new_data_in) / sqrt(length(new_data_in))	

		boot_ests = xbar_boot(B_in, new_data_in)

		regular_flag = real_mean_in - xbar > -interval_length & real_mean_in - xbar < interval_length
		boot_flag = real_mean_in - boot_ests[1] > -boot_ests[2] * tscore & real_mean_in - boot_ests[1] < boot_ests[2] * tscore

		return(c(regular_flag, boot_flag))
	})
	fractions = apply(t(results), 2, mean)
	return(round(fractions, 5))
}



new_data_population = c(rnorm(1000, 10, 200), rnorm(1000, 10, 300), rnorm(1000, 10, 400))

real_mean = mean(new_data_population)

compare_CI(alpha_in = .05, N_in = 500, B_in = 100, n_in = 200, real_mean, new_data_population)

compare_CI(alpha_in = .05, N_in = 500, B_in = 1000, n_in = 200, real_mean, new_data_population)

compare_CI(alpha_in = .05, N_in = 500, B_in = 100, n_in = 15, real_mean, new_data_population)

compare_CI(alpha_in = .05, N_in = 500, B_in = 1000, n_in = 15, real_mean, new_data_population)


####
# Compare interval width and mean differences if time allows 
####

compare_mean_and_sd = function(alpha_in, N_in, B_in, n_in, real_mean_in, new_data_pop){
	results = sapply(1:N_in, function(i){
		new_data_in = sample(new_data_pop, n_in)
		xbar = mean(new_data_in)
		tscore = qt(1-alpha_in/2, length(new_data_in)-1)
		interval_length = tscore * sd(new_data_in) / sqrt(length(new_data_in))	

		boot_ests = xbar_boot(B_in, new_data_in)

		# Returns differences in mean estimates as well as a TRUE if the bootstrap has a tighter
		# interval or FALSE otherwise
		return(c(xbar - boot_ests[1], boot_ests[2] * tscore < interval_length))
	})
	return(t(results))
}

comparison_1 = compare_mean_and_sd(alpha_in = .05, N_in = 500, B_in = 500, n_in = 200, real_mean, new_data_population)

summary(comparison_1)

plot(comparison_1[,1])


# Compare 
length_differences = comparison_1[which(abs(comparison_1[,1]) > 1),2]
summary(length_differences)

# Conclusion: mean(length_differences) ~ .5 so if a confidence interval misses the true mean, it's
#  most likely due to the estimate of the mean (or bootstrap estimate of the mean) and not so much the 
#  length of the interval. Hence, if a bootstrap is burned in enough, this can be avoided

# Example:  Probabilities end up being almost exactly the same even for a higher alpha (higher alpha <=> smaller t score
# 			and a more slim interval for both bootstrap and theory-based interval, hence placing more importance on the point estimate
#			of the mean - being xbar or bootstrap)
compare_CI(alpha_in = .2, N_in = 250, B_in = 10000, n_in = 100, real_mean, new_data_population)

# Again, with a small number of bootstrap iterations, the bootstrap under-performs, as expected 
compare_CI(alpha_in = .2, N_in = 250, B_in = 10, n_in = 100, real_mean, new_data_population)


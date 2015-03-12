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
	results = sapply(1:B, function(i, data_set){
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
		se = tscore * sd(new_data_in) / sqrt(length(new_data_in))	

		boot_ests = xbar_boot(B_in, new_data_in)

		regular_flag = real_mean_in - xbar > -se & real_mean_in - xbar < se
		boot_flag = real_mean_in - boot_ests[1] > -boot_ests[2] * tscore & real_mean_in - boot_ests[1] < boot_ests[2] * tscore

		return(c(regular_flag, boot_flag))
	})
	fractions = apply(t(results), 2, mean)
	return(round(fractions, 5))
}


real_mean = 10

new_data_population = c(rnorm(1000, real_mean, 200), rnorm(1000, real_mean, 300), rnorm(1000, real_mean, 400))


compare_CI(alpha_in = .05, N_in = 500, B_in = 500, n_in = 200, real_mean, new_data_population)

compare_CI(alpha_in = .05, N_in = 500, B_in = 100000, n_in = 200, real_mean, new_data_population)

compare_CI(alpha_in = .05, N_in = 200, B_in = 1000, n_in = 15, real_mean, new_data_population)

compare_CI(alpha_in = .05, N_in = 200, B_in = 10000, n_in = 15, real_mean, new_data_population)


# Compare SE and mean differences if time allows 


# Code snippets for 2/26/15 discussion

poisson_means = function(N, n, lambda_in){
	# Will call n, lambda_in from function's scope inside sapply
	means = sapply(1:N, function(i){
		return(mean(rpois(n, lambda_in)))
	})
	return(means)
}

plot_hist = function(input_data, n, lambda){
	x = seq(min(input_data) - 1, max(input_data) + 1, length = 10000)
	y = dnorm(x, mean = lambda, sd = sqrt(lambda/n))

	hist(input_data, freq = FALSE, ylim = range(c(y, hist(input_data)$density)),
		main = "Histogram of Means")
	abline(v = mean(input_data), col = "red", lwd = 2, lty = 2)

	points(x, y, type = "l", col = "blue")
}

plot_density = function(input_data, n, lambda, plot_flag = TRUE){
	y_den = density(input_data)
	y = dnorm(y_den$x , mean = lambda, sd = sqrt(lambda/n))

	if(plot_flag){

		plot(y_den, ylim = range(c(y, y_den$y)), main = "Sample density vs Normal Distribution", col = "grey")
		abline(v = mean(input_data), col = "red", lwd = 2, lty = 2)

		points(y_den$x, y, type = "l", col = "blue")
		legend("topright", c("Sample", "Normal"), col = c("grey", "blue"), lty = c(1,1))
	}
	# Return squared deviations between two curves
	return(sum((y_den$y - y)^2))
}



persp_plots = function(n_sequence, N_sequence, RowVsCol = FALSE){
	errors = matrix(0, length(n_sequence), length(N_sequence))
	for(row in 1:length(n_sequence)){
		for(col in 1:length(N_sequence)){
			errors[row, col] = plot_density(poisson_means(N_sequence[col], n_sequence[row], lambda_input),
											n_sequence[row],
											lambda_input,
											plot_flag = FALSE)
		}
	}

	if(RowVsCol){
		if(RowVsCol == 1){
			matplot(t(errors), type = "l", , xlab = "N_sequence")
		}else{
			matplot(errors, type = "l", xlab = "n_sequence")
		}
	}else{
		par(mfrow = c(1,2))
		persp(1:length(n_sequence), 1:length(N_sequence), errors, xlab = "n_sequence", ylab = "N_sequence")
		persp(1:length(n_sequence), 1:length(N_sequence), errors, theta = -90, xlab = "n_sequence", ylab = "N_sequence")

	}
}


# Main block and examples

lambda_input = 5
n_input = 20
plot_hist(poisson_means(50, n_input, lambda_input), n_input, lambda_input)

t1 = plot_density(poisson_means(50, n_input, lambda_input), n_input, lambda_input)

n_sequence_in = (3:18)*4
N_sequence_in = (5:25)*4
persp_plots(n_sequence_in, N_sequence_in)

n_sequence_in = (3:8)*2
N_sequence_in = (5:15)*2
persp_plots(n_sequence_in, N_sequence_in)


n_sequence_in = (3:18)*2
N_sequence_in = (5:25)*2
persp_plots(n_sequence_in, N_sequence_in)


n_sequence_in = (3:18)*4
N_sequence_in = (5:25)*4
persp_plots(n_sequence_in, N_sequence_in)


n_sequence_in = (3:18)*10
N_sequence_in = (3:18)*10
persp_plots(n_sequence_in, N_sequence_in)




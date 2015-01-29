
# Here are some book problems that we will solve empirically. Note: This is just for practice,
# please don't use this as a guide on how to do your book problems!!

# 2-4-19

density_function = function(x){
	return(3/64*x^2*(4-x))
}

# graph of density - only defined on 0 to 4

curve(density_function, 0, 4)


# Repeated sampling to approximate the probability (monte carlo integration)
probability_approx_gt = function(n, bottom_value){
	results = sapply(1:n, function(i, number){
		random_x = sample(((number* 10000):40000)/10000, 1)
		return(density_function(random_x))
	}, number = bottom_value)
	return((sum(results) / n) * (4-bottom_value))
}


probability_approx_gt(10000,3)
integrate(density_function, lower = 3, upper = 4)

probability_approx_gt(10000,2.5)
integrate(density_function, lower = 2.5, upper = 4)


# B

between_2_and_3 = probability_approx_gt(10000,2) - probability_approx_gt(10000,3)
real_answer = integrate(density_function, lower = 2, upper = 3)

# C

find_approx_mean = function(n){
	results = sapply(1:n, function(i){
		random_x = sample((0:40000)/10000,1)
		return(random_x * density_function(random_x))
	})
	return(4 * sum(results)/n) # Times 4 because that's the interval length (mc integration)
}

approx_mean = find_approx_mean(10000)
real_mean = integrate(function(i){return(i*density_function(i))}, 0, 4)

# D


find_approx_ex2 = function(n){
	results = sapply(1:n, function(i){
		random_x = sample((0:40000)/10000,1)
		return(random_x^2 * density_function(random_x))
	})
	return(4 * sum(results)/n) # Times 4 because that's the interval length (mc integration)
}

approx_var = find_approx_ex2(10000) - approx_mean^2
real_var = integrate(function(i){return(i^2*density_function(i))}, 0, 4)$value - (real_mean$value)^2


# E

cdf_fun = function(a){
	return(integrate(density_function, 0, a)$value)
}

approx_cdf_fun = function(n, a){
	return(1 - probability_approx_gt(n, a))
}


cdf_fun_approx = approx_cdf_fun(10000, 2)
real_cdf_fun = cdf_fun(2)


# Graph it
x_vals = (0:40)/10
y_cdf = sapply(x_vals, cdf_fun)
plot(x_vals, y_cdf, lwd = 2, type = "l")

iter = 2
for(n_num in c(100, 500, 1000)){
	y_out = sapply(x_vals, function(k){approx_cdf_fun(n_num, k)})
	points(x_vals, y_out, type = "l", col = iter, lty = iter, lwd = 2)
	iter = iter + 1
}




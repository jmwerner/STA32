# Rough and purposely non-commented code for 1/22/15 discussion

roll_di = function(){
	return(sample(1:6, 1))
}

rolls = function(n){
	out = sapply(1:n, function(i){
		return(roll_di())
	})
	return(out)
}

yahtzee = function(n){
	turns = sapply(1:n, function(i){
		one_turn = rolls(5)

		#return(one_turn[1] == one_turn[2] & one_turn[2] == one_turn[3] & one_turn[3] == one_turn[4] & one_turn[4] == one_turn[5])

		return(sum(one_turn[1] == one_turn[2:5]) == 4)
	})
	return(turns)
}

a = yahtzee(50000)
sum(a)/length(a)

yahtzee_prob = function(N){
	return(sum(yahtzee(N)) / N)
}


actual_answer = choose(5,5) * choose(6,1) / 6 ^ 5
trial_n = c(100, 1000, 10000, 100000, 500000)
# This might take a little time
empirical_probs = sapply(trial_n, yahtzee_prob)


pair = function(n){
	turns = sapply(1:n, function(i){
		one_turn = rolls(2)
		return(one_turn[1] == one_turn[2])
	})
	return(turns)
}

pair_prob = function(N){
	return(sum(pair(N)) / N)
}

actual_pair_prob = choose(2,2) * choose(6,1) / 6 ^ 2

trial_n = seq(1, 5001, 25)
# This might take a little time
empirical_pair_probs = sapply(trial_n, pair_prob)


plot(trial_n, empirical_pair_probs, xlab = "N trials", ylab = "Probability")
abline(h = actual_pair_prob, col = "RED", lty = 2)
legend("bottomright", c("Calculated Probability", "Actual Probability"), pch = c(1,NA), lty = c(NA, 2), col = c("Black","Red"))


plot(trial_n, empirical_pair_probs, xlab = "N trials", ylab = "Probability", type = "l")
abline(h = actual_pair_prob, col = "RED", lty = 2)
legend("bottomright", c("Calculated Probability", "Actual Probability"), lty = c(1, 2), col = c("Black","Red"))




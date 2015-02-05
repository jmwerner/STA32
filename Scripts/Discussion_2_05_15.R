
CardValue = rep(c("A",2:10,"J","Q","K"),times = 4)
CardSuit = rep(c("S","C","H","D"), each = 13)
CardDeck = t(rbind(CardValue,CardSuit))

suitfactor = factor(CardSuit)
valuefactor = factor(CardValue)

prob_1 = function(n){
	results = sapply(1:n, function(i){
		Cards = sample(1:52, 2, replace= FALSE)
		MyHand = CardDeck[Cards,]
		return(sum(MyHand[,2] == "D") == 1 & sum(MyHand[,2] == "H") == 1 & MyHand[1,1] == MyHand[2,1])
	})
	return(sum(results) / n)
}


prob_2 = function(n){
	results = sapply(1:n, function(i){
		suits = sample(CardSuit, 2, replace = FALSE)
		values = sample(CardValue, 2, replace = FALSE)
		return(values[1] == values[2] & sum(suits == "H") == 1 & sum(suits == "D") == 1)
	})
	return(sum(results) / n)
}


# Results
Actual_prob = 13/choose(52,2)
prob_1(100000)
prob_2(100000)


card_drawing_test = function(n, method){
	if(method == 1){
		draws = sapply(1:n, function(i){
			Cards = sample(1:52, 2, replace= FALSE)
			return(c(CardDeck[Cards,]))
		})
	}else{
		draws = sapply(1:n, function(i){
			suits = sample(CardSuit, 2, replace = FALSE)
			values = sample(CardValue, 2, replace = FALSE)
			return(cbind(values, suits))
		})
	}
	
	col_vect = c("black", "red")
	pch_vect = c("o", "X")

	equal_draws = draws[1,] == draws[2,] & draws[3,] == draws[4,]
	index_vector = as.numeric(equal_draws) + 1

	plot(as.numeric(factor(draws[1,], levels = levels(valuefactor))), 
		 as.numeric(factor(draws[3,], levels = levels(suitfactor))), xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
		 col = col_vect[index_vector], pch = pch_vect[index_vector])
	points(as.numeric(factor(draws[2,], levels = levels(valuefactor))), 
		 as.numeric(factor(draws[4,], levels = levels(suitfactor))), col = col_vect[index_vector], pch = pch_vect[index_vector])
	axis(1, at=1:13, labels=levels(valuefactor)) # out of order, but that's ok!
	axis(2, at=1:4, labels=levels(suitfactor))
}

# Results
card_drawing_test(1000,1)
card_drawing_test(1000,2)


# Other misc examples


# Recursive function example
fact = function(input){
	if(input < 1){stop("Must input number 1 or greater")}
	if(input == 1){
		return(1)
	}else{
		return(input * fact(input - 1))
	}
}

factorial(5)
fact(5)

iter = 1
while(iter < 10){
	print(paste("iter value:", iter, "   factorial value: ", fact(iter)))
	iter = iter + 1
}




# Basic functions to simulate a craps game and fire bet winnings
# These will be used to make a beautiful, well documented .Rmd document during class

roll = function(){
  return(sample(1:6,1))
}

list_append = function(input, val){
  n = length(input)
  input[[n+1]] = val
  return(input)
}

game = function(){
	point = 0
	craps = FALSE
	all_rolls = list()
	all_points = list()

	while(!craps){
	  r1 = roll()
	  r2 = roll()
	  if(point > 0){
	    if((r1+r2) == 7){
	      craps = TRUE
	    }
		  if((r1+r2) == point){
		    all_points = list_append(all_points, point)
		    point = 0
		  }
	  }else{
	      if((r1+r2) %in% c(4,5,6,8,9,10)){
	        point = r1+r2
	      }
	  }
	  all_rolls = list_append(all_rolls, c(r1,r2)) 
	}
	return(list("rolls" = unlist(lapply(all_rolls, sum)), "points" = unique(unlist(all_points))))
}

fire_bet = function(bet){
	win = 0
	game_data = game()
	unique_p = length(game_data$points)
	if(unique_p > 3){
		win = bet * c(25, 250, 1000)[unique_p - 3]
	}
	return(win)
}

fire_bet_winnings = function(rounds, in_bet){
	win_vector = sapply(1:rounds, function(i, bet2){
		return(fire_bet(bet2))
	}, bet2 = in_bet)
	net_winnings = -1 * rounds * in_bet + sum(win_vector)
	return(net_winnings)
}


# Crude Plot to visualize these trials
x = 1:1000
y = sapply(x, function(i){return(fire_bet(5))})


indices = as.numeric(y > 0) + 1
plot(x,y, col = c("black", "red")[indices], pch = c(".","W")[indices], xlab = "Game Number", ylab = "Fire Bet Winnings")




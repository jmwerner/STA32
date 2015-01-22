
# Load mtcars data for playing
install.packages("MASS") #if MASS is not installed, run this and select a server for download  (CA 2 is advised)
library(MASS) #(assuming MASS is already installed)
data(mtcars)

attach(mtcars)
names(mtcars)

dim(mtcars)
head(mtcars)
tail(mtcars)

cyl == 4
indexes_4 = which(cyl == 4)
sub_4 = mtcars[indexes_4,]
indexes_6 = which(cyl == 6)
sub_6 = mtcars[indexes_6,]

boxplot(sub_4$mpg, sub_6$mpg, names = c("4 Cyl","6 Cyl"), xlab = "Cylinders", ylab = "mpg")

cyl_factor = factor(cyl)
boxplot(mpg ~ cyl_factor) # Works without converting cyl to factor in this case (but not always!)

# These are two examples of MANY different ways this can be done.

m = mean(hp)
round(m, digits = 2)
floor(m)
ceiling(m)

detach(mtcars)


test = function(x,y){
  return(x*y)
}

test(5,2)

test2 = function(x,y){
  out_value = x * y
  return(out_value)
}

test2(5,3)

# Function scope example
a = 10
test3 = function(){
  print(a)
  a = 5
  print(a)
}

test3()
print(a)

# n is the number of x,y random pairs to return
make_data = function(n){
  x = rnorm(n)
  y = 4 * x + rnorm(n, 1, 2)
  return(data.frame(x,y))
}

test_data = make_data(100)
plot(test_data)

# Basically a pipe into the sample function - note the default argument as well
make_data_2 = function(vector, n, replacement = FALSE){
  out_data = sample(vector, n, replacement)
  return(out_data)
}

people = c("Sam", "Mark", "Heidi", "Kyle")
sample(people, 2, replace = TRUE)
sample(people, 2) # replace = false by default

pair = make_data_2(people, 2)


twoplots = function(x,y){
  par(mfrow = c(1,2)) # Sets global graphical parameters - must set back afterwards
  
  # First graph
  plot(x,y)
  lm = lm(y~x)
  abline(lm$coeff, col = "Red", lty = 2, lwd = 3)
  
  # Second graph
  plot(1:length(y), y[order(y)])
}

a = rnorm(10)
order(a)
a[order(a)]


big_data = make_data(500) # Not actually 'big data' (haha)
twoplots(big_data$x, big_data$y)

hist(big_data$x)
# par options are global
par(mfrow = c(1,1))
hist(big_data$x)


########

roll_dice = function(){
  return(sample(1:6,1))
}

####
N = 50
####

rolls1 <- rep(0, N)
for(i in 1:N){
  rolls1[i] <- roll_dice()
}

hist(rolls) # Bigger N  = more flat histogram

rolls2 = sapply(1:N, function(i){
  return(roll_dice())
})


rolls3 = rep(0,N)
i = 1
while(i <= N){
  rolls3[i] = roll_dice()
  i = i + 1
}


# Don't worry about this function too much, just made for a fun speed comparison
rollall_time_test = function(N){
  rolls1 <- rep(0, N)
  t1 = system.time(
    for(i in 1:N){
      rolls1[i] <- roll_dice()
    }
  )
  
  t2 = system.time(
    rolls2 <- sapply(1:N, function(i){
      return(roll_dice())
    })
  )
  
  rolls3 = rep(0,N)
  i = 1
  
  t3 = system.time(
    while(i <= N){
      rolls3[i] <- roll_dice()
      i <- i + 1
    }
  )
  
  return(c(t1[3], t2[3], t3[3]))
}


# Dirty and quick way of creating a deck of cards. Dirty, but functional!
create_deck = function(){
  cards = c(as.character(1:10), "J", "Q", "K")
  suits = c(rep("D", length(cards)), rep("C", length(cards)), rep("H", length(cards)), rep("S", length(cards)))
  allcards = rep(cards, 4)
  deck = paste(allcards, suits)
  return(deck)
}


# Draws n cards without replacement
draw_cards = function(n){
  deck = create_deck()
  sample(deck, n, replace = FALSE)
}


# Simple illustration of if() logical function with default argument
draw_or_roll = function(decision = "Roll"){
  return_value = NA
  if(decision == "Roll"){
    return_value = roll_dice()
  }else{
    return_value = draw_cards(1)
  }
  return(return_value)
}


draw_or_roll("Roll")
draw_or_roll()
draw_or_roll("Beans")








n_stay <- 0       #initializing variables stay
n_switch <- 0     #initializing variables switch
n_random <- 0     #initializing random

for ( i in 1:100) { 
  door <- c(1,2,3)    #variable door which has numbers 1,2,3
  random <- c(1,2)    #variable random 
  cardoor <- sample(door,1)   #randomly selects one of the doors to have thew car behind it
  choice <- sample(door,1)    #randomly selects the contestants choice of door 
  goatdoors <- setdiff(door, cardoor)     #vector that holds the corresponding values to goats
  reveal_options <- setdiff(goatdoors, choice)    #identify the options we have for the reveal
  if (choice == cardoor) { 
    reveal <- sample(reveal_options,1)  #select one randomly and assign it to the variable reveal.
  }else {
    reveal <- reveal_options    #single element in reveal_options which assign to reveal
  }
  remaining_doors <-setdiff(door, reveal)   #new vector - identifies the two remainung unrevealed doors
  newchoice <- setdiff(remaining_doors, choice)   #records yhe final choice of door if the contestant switches  
  randomchoice <- sample(random, 1)
  
  if (choice == cardoor) {
    n_stay <- n_stay + 1
    print("Stay: You got a car.")   #print results if the choice is to stay and is equal to a car
    if(randomchoice==1) {
      print("Random choice won")
      n_random <- n_random +1       #first random choice 
    }
  }else { 
    print("Stay: You've got a goat!")
    }
  if (newchoice == cardoor) {
    n_switch <- n_switch + 1
    print("Switch: You got a car.") #print results if the choice is to switch and is equal to a car
    if(randomchoice==2) {
      print("Random choice won")
      n_random <- n_random +1       #second random choice
  }
  }else {
  print("Switch: You've got a goat!")
  }
}
print(n_stay/100)     #probability of staying
print(n_switch/100)   #probability of switching
print(n_random/100)   #probability of random


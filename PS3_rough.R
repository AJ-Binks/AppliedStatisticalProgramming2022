#1
#Define a new class:door. Objects of this class simply take on one numeric value: 1, 2, or 3 – indicating which door a candidate chooses
chosenDoor <- list(choice=sample(1:3,1))
class(chosenDoor) <- "door"
#check class
class(chosenDoor)
#check value is a single number
chosenDoor

#2
#Create a method for door objects that is called PlayGame.
#Advanced R would call this the low-level constructor

#first generic
PlayGame <- function(chosenDoor){
  UseMethod("PlayGame")
}

#take the numeric value that is stored in the door object
PlayGame <- function(chosenDoor){
  #draw a random number between 1 and 3 that presents the door behind which the car is hidden
  car <- sample(1:3,1)
  #compare the two numbers
  if (chosenDoor == car){
    #print a message congratulating a winning candidate that chose the correct door
    print("Huzzah! You did it! Folks, you're looking at the owner of a brand new car!")
  }else{
    #express sympathies for a losing candidate that chose the wrong door
    print("Ah shucks! So close!")
  }
}

#test it - I reran the code until I had successfully received both messages
PlayGame(chosenDoor)



#3
#write a construction function that allows the user to create a door object
#Advanced R would call this the helper, recommends calling it new_myclass()
new_chosenDoor <- function(choice){
  output <- list(chosenDoor = choice)
  class(output) <- "door"
  return(output)
}


#test helper
door1 <- new_chosenDoor(1)
PlayGame(door1)


# validation function that checks whether the value stored in door is actually an integer
#Advanced R would call this the validator
validateDoor <- function(choice){
  `%notin%` <- Negate(`%in%`)
  #must be an integer
  if(! is.numeric(choice)){
    stop ("Not an integer")
  }
  if(choice %notin% c(1:3)){
    stop("Not a valid door, choose 1, 2, or 3 ")
  }
}


#test validator
validateDoor(choice=6) #doesnt work
validateDoor(choice="meow") #doesnt work
validateDoor(choice=1) #works
validateDoor(choice=2) #works
validateDoor(choice=3) #works

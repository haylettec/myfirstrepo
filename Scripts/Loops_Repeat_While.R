# Demonstrates basic repeat and while loops
# Last updated 2019-08-27 @ 08:00pm
# Update: modified/added additional examples


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# roll a double (identical numbers) on two die
# stop rolling when the first double occurs
# how many "rolls" will this take to happen?

# Create a virtual die
DIE  <- 1:6

# Initialize roll to 1
roll <- 1
repeat {
    DIE.1 <- sample(DIE, 1);   # Roll the first die
    DIE.2 <- sample(DIE, 1);   # Roll the other die
    if (DIE.1 == DIE.2) break; # Doubles stops loop
    roll <- roll + 1   # If not doubles, roll again
}

# Print out the turn on which you rolled the double
print(roll)

# You can print the result as a sentence w/ sprintf

sprintf("We got doubles on roll %d.", roll)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# roll a double (identical numbers) on two die
# print out the results of each die roll (repeat)

# Create a virtual die
DIE  <- 1:6

# Initialize roll to 1
roll <- 1
repeat {
  DIE.1 <- sample(DIE, 1);   # Roll the first die
  DIE.2 <- sample(DIE, 1);   # Roll the other die
  print(c(DIE.1, DIE.2));
  if (DIE.1 == DIE.2) break; # Doubles stops loop
  roll <- roll + 1   # If not doubles, roll again
}

# Print out the turn on which you rolled the double
print(roll)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How long will it take you to save $10,000
# You input interest rate and first deposit

interest <- .025  # interest rate as a decimal (e.g., 2.5% = 0.025)
starting <- 1000  # initial deposit in dollars
month    <- 1     # initialize month counter

savings <- starting
while (savings < 10000) {
  savings <- savings + interest * savings; 
  month <- month + 1
}

print(month)

sprintf("It would take %d months (or %.2f years) to save $10,000 with an initial deposit of $%.0f and an interest rate of %.2f%%.", month, month/12, starting, 100 * interest)




library(tidyverse)

simulate_craps_game <- function() {
  # store data in a dataframe ...
  # I searched this on web
  craps_data <- data.frame(
    # referring to requirement: id, roll, outcome
    # also checking my own table.
    id    = integer(),
    dice1 = integer(),
    dice2 = integer(),
    roll  = integer(),
    # before outcome: roll_outcome
    point = character(),
    # outcome is character: win, loss,
    outcome = character(),
    stringsAsFactors = FALSE
  )

  # Setup: roll the die, generate a number from 1 of 6, integer
  roll_die <- function() sample(1:6, 1)
  # Input: set up initial num, including id (rolling number), roll (Total points),
  # Input: outcome (win, loss (crapping out), continue), point
  # keep lowercase for consistent
  rolling_number <- 1
  outcome <- "continue" # win or loss will be direct output
  point <- NA

  # write a loop referring to the provided code previously
  repeat {
    # rolling two dice
    dice1 <- roll_die()
    dice2 <- roll_die()
    total_points <- dice1 + dice2

    # using this if it occurs error / checking error / progress
    # print(paste("Roll:", rolling_number, "| Dice1:", dice1, "| Dice2:", dice2,
    #             "| Total:", total_points, "| Point:", ifelse(is.na(point), "-", point),
    #             "| Outcome:", outcome))

    ## Be careful for the loop, it's easy to mess up and take more time to fix
    # multiple conditions
    # come-out/crapping-out
    if (is.na(point)) {
      # win
      if (total_points == 7 || total_points == 11) {
        outcome <- "win"
        break
      } else if (total_points == 2 || total_points == 3 || total_points == 12) {
        outcome <- "loss(crapping out)"
        break
      } else {
        # if the total_points shows4,5,6,8,9,10
        point <- total_points
        outcome <- "continue"
      }
    } else {
      # another loop for multiple conditions
      if (total_points == point) {
        outcome <- "win"
        break
      } else if (total_points == 7) {
        outcome <- "loss(7-out)"
        break
      } else {
        outcome <- "continue"
      }
    }
    # store output in the loop
    craps_data <- rbind(craps_data, data.frame(
      id    = rolling_number,
      dice1 = dice1,
      dice2 = dice2,
      roll  = total_points,
      point = ifelse(is.na(point), "-", as.character(point)),
      outcome = outcome))
    # keep rolling ...
    rolling_number <- rolling_number + 1
  }
  ## repeat this again, otherwise `continue` show all columns in `outcome`
  # store output in the loop
  craps_data <- rbind(craps_data, data.frame(
    id    = rolling_number,
    dice1 = dice1,
    dice2 = dice2,
    roll  = total_points,
    point = ifelse(is.na(point), "-", as.character(point)),
    outcome = outcome))

  # return to beginning
  return(craps_data)
}


# try differen seed number
# set.seed(20201)
# # set.seed(123)
simulate_craps_game()
#


## `summarize_craps_game`

summarize_craps_game <- function(craps_data) {
  # let's recall requirement 2: n_rolls, outcome, point
  # n_rolls = id; outcome = win,loss,continue; point = point
  ## referring to previous step, we have a dataframe - craps_data, is an output of simulating the game

  # Input:
  n_rolls <- nrow(craps_data)  # I wanna all content in each row
  # if the first roll is not win or loss(crapping out), the `Point` exists and `outcome` shows `continue`
  first_roll_point <- ifelse(n_rolls > 1, craps_data$point[2], NA) # change None to NA in order to calculate probability
  # process <-
  last_outcome <- tail(craps_data$outcome, 1)
  # Output:
  # summarize the whole process, similar to Requirement 1
  craps_summary <- data.frame(
    n_rolls = n_rolls,
    point   = first_roll_point,
    outcome = last_outcome,
    stringsAsFactors = FALSE
  )
  return(craps_summary)
}




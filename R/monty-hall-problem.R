#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Select a Door
#'   
#' @description
#'   This function selects one of the available vectors randomly.
#'   
#' @details
#'   The contestant makes their first selection. The function will 
#'   select one door at random.
#'   
#' @param ... no arguments are used by the function.
#'   
#'   
#' @return
#'   This function returns a number for doors 1-3 as 
#'   "a.pick"
#'   
#' @examples
#'   select_door
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open the Goat Door
#'   
#' @description
#'   This function opens the goat door - assuming the user didn't
#'   select the goat door during their first choice.
#'   
#' @details
#'   The host will always open a door with a goat behind it. 
#'   But it can't be a door the contestant has already selected. 
#'   So it must be a door that is not a car and not a current 
#'   contestant selection.
#'   Note that if the contestant selects the car on the first 
#'   guess the host can open either door, but if the contestant 
#'   selects a goat the host only has one option.
#'   
#' @param ... no arguments are used by the function.
#'   
#' @return
#'   This function opens the goat door to show the user one of
#'   the two final available options.
#'   
#' @examples
#'   Number between 1 and 3.
#'   
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change final door choice
#'   
#' @description
#'   This function switches to the other available door if the
#'   goat door is shown to the user and shows the final door 
#'   pick for the game.
#'   
#' @details
#'   The contestant is given the option to change from their 
#'   initial selection to the other door that is still closed. 
#'   The function will represent the game-playing strategy as 
#'   the argument stay=TRUE or stay=FALSE.
#'   
#' @param ... no arguments are used by the function.
#'   
#' @return
#'   Number between 1 and 3.
#'   
#' @examples
#'   change_door
#'   
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine Winner
#'   
#' @description
#'   This function lets the user know if they won either the
#'   car or the goat. If the selected the car, the game prints
#'   "WIN". If they selected the goat, the game prints "LOSE."
#'   
#' @details
#'   This function checks the user's pick "a.pick" and 
#'   determines if it is equal to a car or a goat.
#'   If the selected the car, the game prints
#'   "WIN". If they selected the goat, the game prints 
#'   "LOSE."
#'   
#' @param ... no arguments are used by the function.
#'   
#' @return
#'   This will return a 3 or 4 character string.
#'   
#' @examples
#'   determine_winner
#'   
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play Game
#'   
#' @description
#'   This function plays an entire round of the game.
#'   
#' @details
#'   This function will auto-select and entire round of
#'   the game using random choices. It then returns the
#'   outcome of the game.
#'   
#' @param ... no arguments are used by the function.
#'   
#' @return
#'   Win or Lose
#'   
#' @examples
#'   play_game()
#'   
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play 100 Games
#'   
#' @description
#'   The function will play 100 rounds of the game choosing
#'   doors at random.
#'   
#' @details
#'   This function will play 100 rounds of the Monty Hall 
#'   game to calculate odds of choosing the "goat" door as
#'   compared to staying on the first pick.
#'   
#' @param ... no arguments are used by the function.
#'   
#' @return
#'   This function returns a data frame of the results.
#'   
#' @examples
#'   play_n_games()
#'   
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}

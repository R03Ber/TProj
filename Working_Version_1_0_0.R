#BIOME
BIOME <- new.env()

BIOME$state <- list()
BIOME$savestate <- list()

#HELLO
HELLO <- function(){
  state <- BIOME$state
  
  state$x <- list(0)
  state$y <- list(0)
  state$angle <- list(90)
  state$drawturtle <- list(1)
  
  plot.new()
  plot.window(xlim = c(-50, 50), ylim = c(-50, 50), pty = "s")
  
  state$plot_records[[1]] <- recordPlot()
  
  state$savemode <- "OFF"
  
  BIOME$state <- state
}

#REMEMBER
REMEMBER <- function(){
  state <- BIOME$state
  
  state$plot_records[[length(state$plot_records)+1]] <- recordPlot()
  
  BIOME$state <- state
}

#DEJAVU
DEJAVU <- function(){
  state <- BIOME$state
  
  replayPlot(state$plot_records[[length(state$plot_records)]])
}

#SHOWTURTLE
SHOWTURTLE <- function(){
  state <- BIOME$state
  
  state$drawturtle <- 1
  
  BIOME$state <- state
  
  return("Turtle is now awake!")
}

#HIDETURTLE
HIDETURTLE <- function(){
  state <- BIOME$state
  
  state$drawturtle <- 0
  
  BIOME$state <- state
  
  return("Turtle is now asleep!")
}

#TURTLE
TURTLE <- function(){
  state <- BIOME$state
  
  if(state$drawturtle == 1){
    if(state$angle[[length(state$angle)]]==90){
      arrows(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]]-0.02,
        x1 = state$x[[length(state$x)]],
        y1 = state$y[[length(state$y)]]+0.02,
        angle = 65,
        length = 0.1, lwd = 9, col = "darkslategrey", code = 3
      )
      arrows(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        x1 = state$x[[length(state$x)]],
        y1 = state$y[[length(state$y)]]+0.9,
        length = 0, lwd = 12, col = "olivedrab4"
      )
      arrows(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]]-1,
        x1 = state$x[[length(state$x)]],
        y1 = state$y[[length(state$y)]],
        length = 0, lwd = 7, col = "darkslategrey", code = 1
      )
      points(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        pch = 19, cex = 2.5, col = "darkgreen"
      )
      points(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        pch = 5, cex = 1.75, col = "black"
      )
      points(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        pch = 10, cex = 2.5, col = "black"
      )
    }else if(state$angle[[length(state$angle)]]==270){
      arrows(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]]+0.02,
        x1 = state$x[[length(state$x)]],
        y1 = state$y[[length(state$y)]]-0.02,
        angle = 65,
        length = 0.1, lwd = 9, col = "darkslategrey", code = 3
      )
      arrows(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        x1 = state$x[[length(state$x)]],
        y1 = state$y[[length(state$y)]]-0.9,
        length = 0, lwd = 12, col = "olivedrab4"
      )
      arrows(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]]+1,
        x1 = state$x[[length(state$x)]],
        y1 = state$y[[length(state$y)]],
        length = 0, lwd = 7, col = "darkslategrey", code = 1
      )
      points(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        pch = 19, cex = 2.5, col = "darkgreen"
      )
      points(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        pch = 5, cex = 1.75, col = "black"
      )
      points(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        pch = 10, cex = 2.5, col = "black"
      )
    }else{
      arrows(state$x[[length(state$x)]]-0.02*cos(state$angle[[length(state$angle)]]*(pi/180)),
             state$y[[length(state$y)]]-0.02*sin(state$angle[[length(state$angle)]]*(pi/180)),
             x1 = state$x[[length(state$x)]]+0.02*cos(state$angle[[length(state$angle)]]*(pi/180)),
             y1 = state$y[[length(state$y)]]+0.02*sin(state$angle[[length(state$angle)]]*(pi/180)),
             angle = 65,
             length = 0.1, lwd = 9, col = "darkslategrey", code = 3)
      arrows(state$x[[length(state$x)]],
             state$y[[length(state$y)]],
             x1 = state$x[[length(state$x)]]+0.85*cos(state$angle[[length(state$angle)]]*(pi/180)),
             y1 = state$y[[length(state$y)]]+0.85*sin(state$angle[[length(state$angle)]]*(pi/180)),
             length = 0, lwd = 12, col = "olivedrab4")
      arrows(state$x[[length(state$x)]]-1*cos(state$angle[[length(state$angle)]]*(pi/180)),
             state$y[[length(state$y)]]-1*sin(state$angle[[length(state$angle)]]*(pi/180)),
             x1 = state$x[[length(state$x)]],
             y1 = state$y[[length(state$y)]],
             length = 0, lwd = 7, col = "darkslategrey", code = 1)
      points(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        pch = 19, cex = 2.5, col = "darkgreen"
      )
      points(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        pch = 5, cex = 1.75, col = "black"
      )
      points(
        state$x[[length(state$x)]],
        state$y[[length(state$y)]],
        pch = 10, cex = 2.5, col = "black"
      )
    }}
}

#TRACE
TRACE <- function(){
  state <- BIOME$state
  
  lines(c(state$x[[length(state$x)-1]], state$x[[length(state$x)]]), 
        c(state$y[[length(state$y)-1]], state$y[[length(state$y)]]),
        lwd = 2)
}

#WHERE
WHERE <- function(){
  state <- BIOME$state
  
  x <- state$x[[length(state$x)]]
  y <- state$y[[length(state$y)]]
  return(c(x, y))
}

#PREVIOUS
PREVIOUS <- function(n_back){
  state <- BIOME$state
  
  if(length(state$x)<=1){
    return("The Turtle is still in it's shell")
  }
  else if(missing(n_back)){
    x <- state$x[[length(state$x)-1]]
    y <- state$y[[length(state$y)-1]]
    return(c( x, y))
  }
  else{
    x <- state$x[[length(state$x)-1*n_back]]
    y <- state$y[[length(state$y)-1*n_back]]
    return(c(x, y))
  }
}

#SETPOS
SETPOS <- function(x_coords, y_coords){
  state <- BIOME$state
  
  if(missing(y_coords)){
    state$x <- append(state$x, x_coords[1])
    state$y <- append(state$y, x_coords[2])
  }else{
    state$x <- append(state$x, x_coords)
    state$y <- append(state$y, y_coords)
  }
  
  BIOME$state <- state
  
  DEJAVU()
  TURTLE()
  
  return(list(x = state$x[[length(state$x)]], y = state$y[[length(state$y)]]))
}

#HOME
HOME <- function(){
  state <- BIOME$state
  
  state$x[[length(state$x)+1]] <- 0
  state$y[[length(state$y)+1]] <- 0
  
  BIOME$state <- state
  
  DEJAVU()
  TURTLE()
  
  return(list(x = state$x[[length(state$x)]], y = state$y[[length(state$y)]]))
}

#SETX
SETX <- function(x_coord){
  state <- BIOME$state
  
  state$x <- append(state$x, x_coord)
  state$y <- append(state$y, state$y[[length(state$y)]])
  
  BIOME$state <- state
  
  DEJAVU()
  TURTLE()
  
  return(list(x = state$x[[length(state$x)]], y = state$y[[length(state$y)]]))
}

#SETY
SETY <- function(y_coord){
  state <- BIOME$state
  
  state$x <- append(state$x, state$x[[length(state$x)]])
  state$y <- append(state$y, y_coord)
  
  BIOME$state <- state
  
  DEJAVU()
  TURTLE()
  
  return(list(x = state$x[[length(state$x)]], y = state$y[[length(state$y)]]))
}

#HEADING
HEADING <- function(){
  state <- BIOME$state
  
  return(paste("Turtle is currently facing", state$angle[[length(state$angle)]],
               "degrees!"))
}

#SETHEADING
SETHEADING <- function(degrees){
  state <- BIOME$state
  
  state$angle <- append(state$angle, degrees%%360)
  
  BIOME$state <- state
  
  DEJAVU()
  TURTLE()
  
  return(paste("Turtle has turned to", state$angle[[length(state$angle)]], "degrees!"))
}

#RIGHT
RIGHT <- function(n_degrees_to_turn){
  state <- BIOME$state
  
  state$angle <- append(state$angle,
                        (state$angle[[length(state$angle)]]
                         -n_degrees_to_turn)%%360)
  
  BIOME$state <- state
  
  DEJAVU()
  TURTLE()
  
  return(paste("Turtle has turned", n_degrees_to_turn, "degrees to the right and is now facing", state$angle[[length(state$angle)]], "degrees!"))
}

#LEFT
LEFT <- function(n_degrees_to_turn){
  state <- BIOME$state
  
  state$angle <- append(state$angle,
                        (state$angle[[length(state$angle)]]
                         +n_degrees_to_turn)%%360)
  
  BIOME$state <- state
  
  DEJAVU()
  TURTLE()
  
  return(paste("Turtle has turned", n_degrees_to_turn, "degrees to the left and is now facing", state$angle[[length(state$angle)]], "degrees!"))
}

#FORWARD
FORWARD<- function(n){
  state <- BIOME$state
  DEJAVU()
  RAD <- state$angle[[length(state$angle)]]*(pi/180)
  
  if(missing(n)){
    state$x[[length(state$x)+1]] <- state$x[[length(state$x)]]+cos(RAD)
    state$y[[length(state$y)+1]] <- state$y[[length(state$y)]]+sin(RAD)
  }else{
    state$x[[length(state$x)+1]] <- state$x[[length(state$x)]]+n*cos(RAD)
    state$y[[length(state$y)+1]] <- state$y[[length(state$y)]]+n*sin(RAD)
  }
  BIOME$state <- state
  TRACE()
  REMEMBER()
  TURTLE()
}

#BACK
BACK <- function(n){
  state <- BIOME$state
  DEJAVU()
  RAD <- state$angle[[length(state$angle)]]*(pi/180)
  
  if(missing(n)){
    state$x[[length(state$x)+1]] <- state$x[[length(state$x)]]-cos(RAD)
    state$y[[length(state$y)+1]] <- state$y[[length(state$y)]]-sin(RAD)
  }else{
    state$x[[length(state$x)+1]] <- state$x[[length(state$x)]]-n*cos(RAD)
    state$y[[length(state$y)+1]] <- state$y[[length(state$y)]]-n*sin(RAD)
  }
  BIOME$state <- state
  TRACE()
  REMEMBER()
  TURTLE()
}

#CLEARSCREEN
CLEARSCREEN <- function(){
  
  plot.new()
  plot.window(xlim = c(-50, 50), ylim = c(-50, 50), pty = "s")
  REMEMBER()
  
  state <- BIOME$state
  state$angle[[length(state$angle)]] <- 90
  BIOME$state <- state
  
  HOME()
}

#Comment from Luca

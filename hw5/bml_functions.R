#################################################################################
#### Functions for BML Simulation Study
#represents the traffic grid as list of matrices.
gcd = function(u, v) {ifelse(u %% v != 0, gcd(v,(u%%v)), v)}
lcm = function(u, v) { abs(u*v)/gcd(u,v)}


random_cars=function(m, n, n_cars){
  cars=rep(c(1,2),each=floor(n_cars))
  empty=rep(0,times=(m*n-(2*n_cars)))
  random_placement = sample(c(cars,empty),size = m*n, replace = FALSE)
  return(random_placement)

#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  n_cars = floor(m*n*p/2) # number of cars of each red and blue
  car_vector = random_cars(r,c,n_cars)
  if (all(car_vector == 0)){stop('no cars in grid.')}
  
  occupied = matrix(FALSE, nrow = r, ncol = c)
  red_x = numeric(n_cars) 
  red_y = numeric(n_cars)
  blue_x = numeric(n_cars)
  blue_y = numeric(n_cars)
  
  i = 1
  red_count = 1
  blue_count = 1
  
  while (i <= r * c){
    if (car_vector[i] == 1){
      x = (i-1) %% r + 1
      y = (i-1) %/% r + 1
      red_x[red_count] = x
      red_y[red_count] = y
      occupied[x, y] = TRUE
      red_count = red_count + 1
      
    }
    else if(car_vector[i] == 2){
      x = (i-1) %% r + 1
      y = (i-1) %/% r + 1
      blue_x[blue_count] = x 
      blue_y[blue_count] = y
      occupied[x, y] = TRUE
      blue_count = blue_count + 1
    }
    i = i + 1
  }
  reds = cbind(x = red_x, y = red_y)
  blues = cbind(x = blue_x, y = blue_y)
  return(list(reds = reds, blues = blues, occupied=occupied))
}


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m, color= "red"){
  i = row(grid)[m != ""]
  j = col(grid)[m != ""]
  pos = cbind(i, j)
  colors = m[pos]
  cars = data.frame(i = i, j = j, colors = colors)
  
  w = which(cars$colors == color)
  for(idx in w) {
    curPos = c(cars$i[ idx ], cars$j[idx])
    nextPos = if(color == "blue")
      c(cars$i[ idx ] , 
        if(cars$j[idx] == 1)
          nrow(m)
        else
          cars$j[idx] - 1L)
    else
      c(if(cars$i[ idx ] == ncol(m))
        1L
        else
          cars$i + 1L,
        cars$j[idx])
    
    # check if nextPos is empty
    if(grid[ nextPos[1], nextPos[2] ] == "")  {
      grid[nextPos[1], nextPos[2]] = color
      grid[curPos[1], curPos[2]] = ""      
    }
  }
  
  return(m)
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  i = 0
  names(i)="Number of Steps"
  
  if(!summary) {plot_traffic(traffic_list, i, pch_size)}
  
  freeflow_instances = 0
  freeflow_condition = lcm(nrow(traffic_list$occupied), ncol(traffic_list$occupied))
  # Free flow is defined as above, when traffic is able to cycle through the grid n times, for n
  # is the LCM of the number of columns and rows.
  
  while (i < iterations){
    next_traffic_list = step(traffic_list)
    
    allbluesmove=(!(any(traffic_list$blues[,2]==next_traffic_list$blues[,2]))) #true if all blue cars moved
    nobluesmove=all(traffic_list$blues[,2]==next_traffic_list$blues[,2]) #true if none moved
    
    allredsmove=(!(any(traffic_list$reds[,1]==next_traffic_list$reds[,1]))) #true if all blue cars moved
    noredsmove=all(traffic_list$reds[,1]==next_traffic_list$reds[,1]) #true if none moved
    
    if(allredsmove&allbluesmove)
    {
      freeflow_instances = freeflow_instances + 1
      
      if (freeflow_instances == freeflow_condition){
        plot_traffic(next_traffic_list, i, sample_number, p, 0.7)
        return(c(i, paste(freeflow_condition," Consecutive Instances of Free Flow Found")))
      }
    }
    else if(noredsmove&nobluesmove) {
      freeflow_instances = 0
      plot_traffic(next_traffic_list, i, sample_number, p, 0.7)
      return(c(i,"Traffic Jam"))
    }
    
    else {
      freeflow_instances = 0
    }
    i = i + 1 
    
    traffic_list = next_traffic_list
    if(!summary) {plot_traffic(traffic_list, i, pch_size)}
  }
  plot_traffic(next_traffic_list, i, sample_number, p, 0.7)
  return(c(i, "Mix of Free Flow and Traffic"))
}

sampling=function(total_samples,m,n,p,iterations=100,summary=TRUE,pch_size=.7) {
  simulation=function(m,n,p,sample_number,iterations=50,summary=TRUE,pch_size=.7){
    traffic_list=make_car_list(m,n,p)
    return(simulate(traffic_list, iterations, sample_number, p, summary,pch_size=.7))
  }
  samples = list(total_samples)
  for (i in 1:total_samples){
    samples[[i]] = simulation(m,n,p,i,iterations)
  }
  save(samples, file = 'samples.RData') 
  #save the sample information in the working directory as well.
  return(samples)
}



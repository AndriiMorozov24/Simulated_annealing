main <- function(.name,.k){
  (WD <- getwd())
  if (!is.null(WD)) setwd(WD)
  name <- .name
  df <- read.csv(name, sep = ";", row.names = "Cities")
  
  N = nrow(df) # number of cities
  k = .k # number of probes in the epoch
  T0 = 100 # start temperature
  Tk = 0.01 # end temperature
  a = 0.85 # for temperature change
  
  Swap <- function(.vector){
    tempR <- sample(1:N,2,replace = F) # two random generated cities
    
    # for (j in 1:N){
    #   if (vectorM.[j] == vector.[1]){
    #     Find <- j
    #   }
    #   if (vectorM.[j] == vector.[2]){
    #     Sind <- j
    #   }
    # }
    # temp <- vectorM.[Find]
    # vectorM.[Find] <- vectorM.[Sind]
    # vectorM.[Sind] <- temp
    # return(vectorM.)
  } # function that swap 2 random cities
  
  Fitness <- function(.vector){
    sum <- 0
    for(i in 1:N){
      if(i != N){
        sum <- sum + df[[.vector[i]]][.vector[i+1]]
      }else{
        sum <- sum + df[[.vector[i]]][.vector[1]]
      }
    }
    return(sum)
  } 
  
  cat("Current file = ", name, "\n", file = "output_TSP.txt", append = TRUE)
  cat("Output for the number of probes = ",k, "\n", file = "output_TSP.txt", append = TRUE)
  Schedule <- sample(1:N,N,replace = F) # random generated scheduling
  curMin <- Fitness(Schedule) # current scheduling time
  
  repeat{
    for(i in 1:k){
      Kandydat <- Swap(Schedule) # swaping them
      Kandydat.Min <- Fitness(Kandydat) # checking new scheduling time
      dE <- Kandydat.Min - curMin 
      if (dE < 0){ # if we found better scheduling time
        Schedule <- Kandydat # new scheduling
        curMin <- Kandydat.Min # override with the new optimal scheduling time
      }else{ # if not
        .rand <- runif(1,0,1) # still we want to give it a try
        if (.rand < exp(-dE/T0)){ # condition
          Schedule <- Kandydat
          curMin <- Kandydat.Min
        }
      }
    }
    T0 = T0 * a # changing temperature
    cat("Current temperature = ",round(T0,3)," Current best = ",curMin,"\n", file = "output_TSP.txt", append = TRUE) # new temperature
    if(T0 <= Tk){ # stop condition
      break
    }
  }
  cat("Optimal schedule", "\n", file = "output_TSP.txt", append = TRUE)
  capture.output(Schedule, file = "output_TSP.txt", append = TRUE)
  print("end")
}

Names <- c("TSP_48.csv","TSP_76.csv","TSP_127.csv")
nprobes <- c(1000,5000,10000)

for(i in 1:length(Names)){
  for(j in 1:length(Names)){
    main(Names[i],nprobes[j])
  }
}

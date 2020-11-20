(WD <- getwd())
if (!is.null(WD)) setwd(WD)
df <- read.csv("Dane_S2_100_20.csv", sep = ";", row.names = "Zadanie")

N = nrow(df) # number of tasks
NM = ncol(df) # number of machines
k = 1000 # number of probes in the epoch
T0 = 100 # start temperature
Tk = 0.01 # end temperature
a = 0.85 # for temperature change

Swap <- function(vectorM.,vector.){ 
  for (j in 1:N){
    if (vectorM.[j] == vector.[1]){
      Find <- j
    }
    if (vectorM.[j] == vector.[2]){
      Sind <- j
    }
  }
  temp <- vectorM.[Find]
  vectorM.[Find] <- vectorM.[Sind]
  vectorM.[Sind] <- temp
  return(vectorM.)
} # function that swap 2 random tasks

Fitness <- function(.vector){
  temp <- rep(0,NM) # how much time each machine will work with given schedule
  for (i in 1:N){
    ind <- .vector[i] # number of current task
    for(j in 1:NM){
      if (j!=1) { # for other machines
        if (temp[j] < temp[j-1]){ # calculating "makespan"
          temp[j] <- temp[j-1] + df[[j]][[ind]] # starting time of the next task on the machine [j]
        }else {
          temp[j] <- temp[j] + df[[j]][[ind]] # starting time of the next task on the machine [j]
        }
      }else{ # for the first machine
        temp[j] <- temp[j] + df[[j]][[ind]] # starting time of the next task on the first machine
      }
    }
  }
  return(max(temp)) # we are interested to accomplish all tasks, so we choose max of time 
} 

R0 <- sample(1:N,N,replace = F) # random generated scheduling
curMin <- Fitness(R0) # current scheduling time

repeat{
  for(i in 1:k){
    tempR <- sample(1:N,2,replace = F) # two random generated tasks
    Kandydat <- Swap(R0, tempR) # swaping them
    Kandydat.Min <- Fitness(Kandydat) # checking new scheduling time
    dE <- Kandydat.Min - curMin 
    if (dE < 0){ # if we found better scheduling time
      R0 <- Kandydat # new scheduling
      curMin <- Kandydat.Min # override with the new optimal scheduling time
    }else{ # if not
      .rand <- runif(1,0,1) # still we want to give it a try
      if (.rand < exp(-dE/T0)){ # condition
        R0 <- Kandydat
        curMin <- Kandydat.Min
      }
    }
  }
  T0 = T0 * a # changing temperature
  cat("Current temperature = ",round(T0,3),"\n") # new temperature
  cat("Current best = ",curMin,"\n") # current optimal scheduling time
  if(T0 <= Tk){ # stop condition
    break
  }
}
KolejnoscZadan <- data.frame(R0)
KolejnoscZadan # final optimal scheduling


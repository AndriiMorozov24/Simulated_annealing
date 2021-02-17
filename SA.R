library(rlist)
main <- function(.name, .k, .t) {
  (WD <- getwd())
  if (!is.null(WD))
    setwd(WD)
  df <- read.csv(.name, sep = ";", row.names = "Zadanie")
  
  N = nrow(df) # number of tasks
  NM = ncol(df) # number of machines
  k = .k # number of iterations
  T0 = .t # start temperature
  Tk = 0.01 # end temperature
  a = 0.85 # for temperature change
  
  Swap <- function(vector) {
    tempR <- sample(1:length(vector), 2, replace = FALSE)
    index <- which(vector %in% tempR)
    if ((index[1] == 1) && (index[2] == length(vector))) {
      final <- rev(vector)
    } else if (index[1] == 1) {
      temp2 <- list.subset(vector, c((index[2] + 1):length(vector)))
      .rev <- rev(list.subset(vector, c(index[1]:index[2])))
      final <- c(.rev, temp2)
    } else if (index[2] == length(vector)) {
      temp1 <- list.subset(vector, c(1:(index[1] - 1)))
      .rev <- rev(list.subset(vector, c(index[1]:index[2])))
      final <- c(temp1, .rev)
    } else{
      temp1 <- list.subset(vector, c(1:(index[1] - 1)))
      temp2 <- list.subset(vector, c((index[2] + 1):length(vector)))
      .rev <- rev(list.subset(vector, c(index[1]:index[2])))
      final <- c(temp1, .rev, temp2)
    }
    return(final)
  }
  
  Fitness <- function(.vector) {
    temp <-
      rep(0, NM) # how much time each machine will work with given schedule
    for (i in 1:N) {
      ind <- .vector[i] # number of current task
      for (j in 1:NM) {
        if (j != 1) {
          # for other machines
          if (temp[j] < temp[j - 1]) {
            # calculating "makespan"
            temp[j] <-
              temp[j - 1] + df[[j]][[ind]] # starting time of the next task on the machine [j]
          } else {
            temp[j] <-
              temp[j] + df[[j]][[ind]] # starting time of the next task on the machine [j]
          }
        } else{
          # for the first machine
          temp[j] <-
            temp[j] + df[[j]][[ind]] # starting time of the next task on the first machine
        }
      }
    }
    return(max(temp)) # we are interested to accomplish all tasks, so we choose max of time
  }
  
  # Fitness <- function(.vector){
  #   sum <- 0
  #   for(i in 1:N){
  #     if(i != N){
  #       sum <- sum + df[[.vector[i]]][.vector[i+1]]
  #     }else{
  #       sum <- sum + df[[.vector[i]]][.vector[1]]
  #     }
  #   }
  #   return(sum)
  # } for the TSP schedule problem
  
  cat("Current file = ",
      .name,
      "\n",
      file = "output1.txt",
      append = TRUE)
  cat("Start temperature = ",
      T0,
      "\n",
      file = "output1.txt",
      append = TRUE)
  cat("Output for the number of probes = ",
      k,
      "\n",
      file = "output1.txt",
      append = TRUE)
  Schedule <-
    sample(1:N, N, replace = F) # random generated scheduling
  curMin <- Fitness(Schedule) # current scheduling time
  
  repeat {
    for (i in 1:k) {
      Kandydat <- Swap(Schedule) # swap
      Kandydat.Min <-
        Fitness(Kandydat) # checking new scheduling time
      dE <- Kandydat.Min - curMin
      if (dE < 0) {
        # if we found better scheduling time
        Schedule <- Kandydat # new scheduling
        curMin <-
          Kandydat.Min # override with the new optimal scheduling time
      } else{
        # if not
        .rand <- runif(1, 0, 1) # still we want to give it a try
        if (.rand < exp(-dE / T0)) {
          # condition
          Schedule <- Kandydat
          curMin <- Kandydat.Min
        }
      }
    }
    T0 = T0 * a # changing temperature
    cat(
      "Current temperature = ",
      round(T0, 3),
      " Current best = ",
      curMin,
      "\n",
      file = "output1.txt",
      append = TRUE
    ) # new temperature
    if (T0 <= Tk) {
      # stop condition
      break
    }
  }
  cat("Optimal schedule", "\n", file = "output1.txt", append = TRUE)
  capture.output(Schedule, file = "output1.txt", append = TRUE)
  print("end")
}

Names <- c("Dane_S2_100_20.csv")
niter <- c(5000, 10000, 20000)

for (i in 1:length(Names)) {
  for (j in 1:length(niter)) {
    for (x in 1:10) {
      main(Names[i], niter[j], 100)
    }
  }
}

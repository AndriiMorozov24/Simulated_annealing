library(rlist)
(WD <- getwd())
if (!is.null(WD))
  setwd(WD)
T0 = 100 # start temperature
Tk = 0.01 # end temperature
a = 0.85 # for temperature change

main <- function(.name, .k) {
  name <- .name
  df <- read.csv(name, sep = ";", row.names = "Cities")
  
  N = nrow(df) # number of cities
  k = .k # number of probes in the epoch
  
  Swap <- function(vector) {
    tempR <- sample(1:length(vector), 2, replace = FALSE)
    index <- which(vector %in% tempR)
    if((index[1]==1) && (index[2]==length(vector))){
      final <- rev(vector)
    }else if(index[1]==1){
      temp2 <- list.subset(vector, c((index[2]+1):length(vector)))
      .rev <- rev(list.subset(vector, c(index[1]:index[2])))
      final <- c(.rev,temp2)
    }else if(index[2]==length(vector)){
      temp1 <- list.subset(vector, c(1:(index[1]-1)))
      .rev <- rev(list.subset(vector, c(index[1]:index[2])))
      final <- c(temp1,.rev)
    }else{
      temp1 <- list.subset(vector, c(1:(index[1]-1)))
      temp2 <- list.subset(vector, c((index[2]+1):length(vector)))
      .rev <- rev(list.subset(vector, c(index[1]:index[2])))
      final <- c(temp1, .rev, temp2)
    }
    return(final)
  }
  # function that swap 2 random cities
  
  Fitness <- function(.vector) {
    sum <- 0
    for (i in 1:length(.vector)) {
      if (i != length(.vector)) {
        sum <- sum + df[[.vector[i]]][.vector[i + 1]]
      } else{
        sum <- sum + df[[.vector[i]]][.vector[1]]
      }
    }
    return(sum)
  }
  
  cat("Current file = ",
      name,
      "\n",
      file = "output_TSP.txt",
      append = TRUE)
  cat("Output for the number of probes = ",
      k,
      "\n",
      file = "output_TSP.txt",
      append = TRUE)
  
  Schedule <- sample(1:N, N, replace = F) # random generated scheduling
  curMin <- Fitness(Schedule) # current scheduling time
  
  repeat {
    for (i in 1:k) {
      Kandydat <- Swap(Schedule) # swaping them
      Kandydat.Min <- Fitness(Kandydat) # checking new scheduling time
      dE <- Kandydat.Min - curMin
      if (dE < 0) {
        # if we found better scheduling time
        Schedule <- Kandydat # new scheduling
        curMin <- Kandydat.Min # override with the new optimal scheduling time
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
      file = "output_TSP.txt",
      append = TRUE
    ) # new temperature
    if (T0 <= Tk) {
      # stop condition
      break
    }
  }
  cat("Optimal schedule", "\n", file = "output_TSP.txt", append = TRUE)
  capture.output(Schedule, file = "output_TSP.txt", append = TRUE)
  print("end")
}

Names <- c("TSP_48.csv", "TSP_76.csv", "TSP_127.csv")
nprobes <- c(1000, 5000, 10000)

for (i in 1:length(Names)) {
  for (j in 1:length(nprobes)) {
    main(Names[i], nprobes[j])
  }
}

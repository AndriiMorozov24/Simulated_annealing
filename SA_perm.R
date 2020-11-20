(WD <- getwd())
if (!is.null(WD)) setwd(WD)
df <- read.csv("Dane_S2_100_20.csv", sep = ";", row.names = "Zadanie")

N = nrow(df) #ogolna liczba zadan
NM = ncol(df) #ogolna liczba maszyn
k = 10000 #liczba prob w epoce
T0 = 100 #temperatura poczatkowa
Tk = 0.01 #temperatura koncowa
a = 0.85 # dla zmiany temperatury

Switch <- function(vectorM.,vector.){ 
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
} # funkcja zamienia 2 wygenorawane losowo zadania

Fcelu <- function(.vector){
  temp <- rep(0,NM) 
  for (i in 1:N){
    ind <- .vector[i] #numer zadania
    for(j in 1:NM){
      if (j!=1) { # dla pozostalych maszyn
        if (temp[j] < temp[j-1]){ #ustawiamy bufor czasowy
          temp[j] <- temp[j-1] + df[[j]][[ind]] #czas rozpoczecia kolejnego zadania na maszynie [j]
        }else {
          temp[j] <- temp[j] + df[[j]][[ind]] #czas rozpoczecia kolejnego zadania na maszynie [j]
        }
      }else{
        temp[j] <- temp[j] + df[[j]][[ind]] #czas rozpoczecia kolejnego zadania na maszynie 1
      }
    }
  }
  return(max(temp)) #interesuje nas czas koncowy uszeregowania zadan, czyli maximum maszyn
} 

R0 <- sample(1:N,N,replace = F) #stan zerowy
curMin <- Fcelu(R0) #obecna wartosc FC

repeat{
  for(i in 1:k){
    tempR <- sample(1:N,2,replace = F) # generowanie 2 losowych zadan
    Kandydat <- Switch(R0, tempR) 
    Kandydat.Min <- Fcelu(Kandydat)
    dE <- Kandydat.Min - curMin
    if (dE < 0){
      R0 <- Kandydat
      curMin <- Kandydat.Min
    }else{
      .rand <- runif(1,0,1)
      if (.rand < exp(-dE/T0)){
        R0 <- Kandydat
        curMin <- Kandydat.Min
      }
    }
  }
  T0 = T0 * a
  print(round(T0,3)) #nowa temperatura 
  print(curMin) #obecna wartosc FC
  if(T0 <= Tk){
    break
  }
}
KolejnoscZadan <- data.frame(R0)
KolejnoscZadan


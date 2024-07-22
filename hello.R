# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

###########
# SHAPLEY #
###########

# Ejemplo paquete ya existente
n <- 3
coalitions(n)

v <- c(1,1,2,1,2,2,2)

shapley(v, method = "exact")
shapley(v, method = "appro", n_rep = 10000)


# A symmetric voting game
n <- 10

v <- function(coalition) {
  if (length(coalition) > 5) {
    return(1)
  } else {
    return(0)
  }
}

shapley(v, method = "exact", n_players = n)

tic <- Sys.time()
shapley(v, method = "appro", n_rep = 10000, n_players = n)
toc <- Sys.time()
toc-tic


###########
# BANZHAF #
###########

v<-c(0,0,0,1,2,1,3)
banzhaf(v, method = "exact")
banzhaf(v, method = "appro", n_rep = 10000, replace = FALSE)
# 1.25 0.75 1.25

# A symmetric voting game
n <- 10

v <- function(coalition) {
  if (length(coalition) > 5) {
    return(1)
  } else {
    return(0)
  }
}
banzhaf(v, method = "exact", n_players = n)

tic <- Sys.time()
banzhaf(v, method = "appro", n_rep = 10000, n_players = n, replace = TRUE)
toc <- Sys.time()
toc-tic

tic <- Sys.time()
banzhaf(v, method = "appro", n_rep = 10000, n_players = n, replace = FALSE)
toc <- Sys.time()
toc-tic


#####################
# EGALITARIAN VALUE #
#####################
n <- 3
v <- c(1,1,2,1,2,2,2)
egalitarian(v)
equal_surplus_division(v)

n <- 10
v <- function(coalition) {
  if (length(coalition) > 5) {
    return(1)
  } else {
    return(0)
  }
}
egalitarian(v,n)
equal_surplus_division(v,n)


##########
# OWEN
##########
#[1] "Owen Value"
#  0.25 0.25 1.5
characteristic_func <- c(1,1,2,1,2,2,2)
union <- list(c(1,2),c(3))
owen(characteristic_func, union)
owen(characteristic_func, union, method = "appro", n_rep = 10000)



#[1] "Owen Value"
#  20 20  30  30
characteristic_func <- c(0,0,0,0,30,30,40,40,50,50,60,70,80,90,100)
union <- list(c(1,3),c(2),c(4))
owen(characteristic_func, union)
owen(characteristic_func, union, method = "appro", n_rep = 10000)


# A symmetric voting game
n <- 8
v <- function(coalition) {
  if (length(coalition) > n/2) {
    return(1)
  } else {
    return(0)
  }
}
u <- lapply(1:(n/2), function(i) c(2*i - 1, 2*i))
owen(v, union = u, method = "exact", n_players = n)
owen(v, union = u, method = "appro", n_rep = 10000, n_players = n)


##########
# BANZHAF-OWEN
##########
characteristic_func <- c(0,0,0,0,30,30,40,40,50,50,60,70,80,90,100)
union <- list(c(1,3),c(2),c(4))
banzhaf_owen(characteristic_func, union)
banzhaf_owen(characteristic_func, union, method = "appro", n_rep = 10000)
banzhaf_owen(characteristic_func, union, method = "appro", n_rep = 10000, replace = FALSE)


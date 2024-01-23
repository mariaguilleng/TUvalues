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
shapley(v, method = "appro", n_rep = 100000)


# A symmetric voting game
n <- 50

v <- function(coalition) {
  if (length(coalition) > 25) {
    return(1)
  } else {
    return(0)
  }
}

shapley(v, method = "exact", n_players = n)

tic <- Sys.time()
shapley(v, method = "appro", n_rep = 100000, n_players = n)
toc <- Sys.time()
toc-tic

tic <- Sys.time()
banzhaf(v, method = "appro", n_rep = 10000, n_players = n, replace = TRUE)
toc <- Sys.time()
toc-tic

tic <- Sys.time()
banzhaf(v, method = "appro", n_rep = 10000, n_players = n, replace = FALSE)
toc <- Sys.time()
toc-tic

###########
# BANZHAF #
###########

v<-c(0,0,0,1,2,1,3)
banzhaf(v, method = "appro", n_rep = 10000)
banzhaf(v, method = "exact")
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

shapley(v, method = "exact", n_players = n)
banzhaf(v, method = "appro", n_players = n, n_rep = 1000, replace = FALSE)
banzhaf(v, method = "exact", n_players = n)


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






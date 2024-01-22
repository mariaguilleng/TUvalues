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
banzhaf(v, method = "appro", n_rep = 10000, n_players = n)
toc <- Sys.time()
toc-tic

###########
# BANZHAF #
###########

v=c(0,0,0,1,2,1,3)
banzhaf(v, method = "appro", n_rep = 10000)
# 1.25 0.75 1.25



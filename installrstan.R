Sys.getenv('PATH')

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) 
  dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) 
  file.create(M)
cat("\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function", 
    file = M, sep = "\n", append = TRUE)

cat('Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")',
    file = file.path(Sys.getenv("HOME"), ".Rprofile"), 
    sep = "\n", append = TRUE)

cat(readLines(M), sep = "\n")

cat(M)

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)

fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
    return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
                           ' )
fx( 2L, 5 ) # should be 10

install.packages(c("coda","mvtnorm","devtools"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")

#links:
#https://www.youtube.com/channel/UCNJK6_DZvcMqNSzQdEkzvzA
library(rstan)
?rstan
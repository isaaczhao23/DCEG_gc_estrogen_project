check_packages = function(names){
    for(name in names){
        if (!(name %in% installed.packages()))
            install.packages(name, repos="http://cran.us.r-project.org") #if package not installed, install the package
        library(name, character.only=TRUE,warn.conflicts=FALSE,quietly=TRUE)
    }
}

# Function to re-arrange columns
arrange.variables <- function(data, vars){
  stopifnot(is.data.frame(data))
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  data <- data[ , out.vec]
  return(data)
}
# example: df %>% arrange.variables(c("var1"=1, "var2"=2))
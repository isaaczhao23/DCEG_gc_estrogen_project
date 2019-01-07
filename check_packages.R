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



# Name estrogens
estrogen_names = c("E1", "E2", "4OHE1", "4ME1","4ME2","2OHE1","3ME1" ,"2OHE2","2ME1","2ME2","16aE1","17epiE3","E3","16KE2","16epiE3") 
estrogen_names_long = c("Estrone (E1)", "Estradiol (E2)", "4-Hydroxyestrone (4OHE1)", "4-Methoxyestrone (4ME1)","4-Methoxyestradiol (4ME2)","2-Hydroxyestrone (2OHE1)","2-Hydroxyestrone-3-methyl ether (3ME1)" ,"2-Hydroxyestradiol (2OHE2)","2-Methoxyestrone (2ME1)","2-Methoxyestradiol (2ME2)","16a-Hydroxyestrone (16aE1)","17-Epiestriol (17epiE3)","Estriol (E3)","16-Ketoestradiol (16KE2)","16-Epiestriol (16epiE3)")
estrogen_names_long2 = c("Estrone", "Estradiol", "4-Hydroxyestrone", "4-Methoxyestrone","4-Methoxyestradiol","2-Hydroxyestrone","2-Hydroxyestrone-3-methyl ether" ,"2-Hydroxyestradiol","2-Methoxyestrone","2-Methoxyestradiol","16a-Hydroxyestrone","17-Epiestriol","Estriol","16-Ketoestradiol","16-Epiestriol")
estrogen_names_df = data.frame(Estrogen = estrogen_names, Nomenclature = estrogen_names_long2)
t.estrogen_names = paste0("t.",estrogen_names)
t.f.estrogen_names = paste0("t.f.",estrogen_names)
country_list = c("Iran", "Korea (KMCC)", "Germany", "Korea (SNU)", "Japan")


# in case dplyr functions are overwritten by another package
select <- dplyr::select; rename <- dplyr::rename; mutate <- dplyr::mutate; 
summarize <- dplyr::summarize; arrange <- dplyr::arrange; filter <- dplyr::filter

set.seed(1)


#' String operations and interpolation: magic edition
#' 
#' The stringmagic package introduces a new language tailored to create complex character strings. 
#' Use the function string_magic to interpolate a variable inside a character string and to apply, 
#' on-the-fly, any arbitrary string operation.
#' 
#' It supports over 50 basic operations, seamless customization, nesting, 
#' pluralization, and much more. Writing complex character strings has never been that easy!
#' 
#' To get an overview of the package: see the [Readme](https://lrberge.github.io/stringmagic/index.html). 
#' To get started with string_magic,
#'  see [the vignette](https://lrberge.github.io/stringmagic/articles/guide_string_magic.html).
#' 
#' This package also enhances regular string functions (like grep and co) with
#'  specialized functions to:
#' 
#' - detect combinations of regular expressions
#' - chain basic string operations
#' - clean character string vectors
#' 
#' See more in the [dedicated vignette](https://lrberge.github.io/stringmagic/articles/guide_string_tools.html).
#' 
#' @author 
#' Laurent Berge
#' 
#' For a few functions (e.g. string_split2df) I use internally an algorithm to turn vectors 
#' into indexes (i.e. integer vectors of the same length ranging to 1 to the number of groups).
#' This algorithm was inspired by Sebastian Krantz's [collapse](https://cran.r-project.org/package=collapse)
#' (if you don't know about it, you should definitely check it out!) who uses an algorithm
#' from Morgan Jacob's [kit](https://cran.r-project.org/package=kit) package.
#' 
#' 
#' 
"_PACKAGE"


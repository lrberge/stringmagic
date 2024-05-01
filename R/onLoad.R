
#
# compatibility issues
#



if(!exists("str2lang", asNamespace("base"))){
  str2lang = function(x){
    parse(text = x, keep.source = FALSE)
  }
}

if(is.factor(data.frame(x = "bonjour")$x)){
  data.frame = function(..., stringsAsFactors = FALSE){
    base::data.frame(..., stringsAsFactors = stringsAsFactors)
  }
  
  as.data.frame = function(..., stringsAsFactors = FALSE){
    base::as.data.frame(..., stringsAsFactors = stringsAsFactors)
  }
}


#
# startup
#


.onLoad = function(libname, pkgname){
  # setting some options
  
  # operations
  setup_operations()
  
  # generating functions (only if root)
  generate_help_extensive()

  # help
  setup_help_extensive()
  
  # To circumvent a peculiar behavior from pkgdown
  fix_pkgwdown_path()

  invisible()
}










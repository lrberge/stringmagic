
#
# compatibility issues
#

# Nota:
# - gregexec, used only in format_help(), does not exist in R 3.5.0
#   => we can live without it
#   


if(!exists("str2lang", asNamespace("base"))){
  str2lang = function(x){
    parse(text = x, keep.source = FALSE)[[1]]
  }
}

change_defaults = function(fun_name, ...){
  # I don't rewrite the function by attaching the body of the fun_name
  # because I'm a bit wary of namespaces
  #
  # it would work for base R stuff but would be dangerous if extended to
  # functions from imported packages
  # it would also be a problem if some functions I use are conflicted with internal base R funs
  # (although I think they're all exposed)
  #
  # So here it's pretty innocuous, it's a simple rewrite of the call
  # but a bit less efficient
  # 
  
  defaults = list(...)
  for(i in seq_along(defaults)){
    arg_val = parse(text = deparse(defaults[[i]], width.cutoff = 500), keep.source = FALSE)
    if(is.expression(arg_val)){
      arg_val = arg_val[[1]]
    }
    defaults[[i]] = arg_val
  }
  
  fun = parse(text = fun_name, keep.source = FALSE)
  if(is.expression(fun)){
    fun = fun[[1]]
  }
  
  function(...){
    mc = match.call()
    mc_names = names(mc)
    for(arg_name in setdiff(names(defaults), mc_names)){
      mc[[arg_name]] = defaults[[i]]
    }
    
    mc[[1]] = fun
    eval(mc, parent.frame())
  }
  
}

if(is.factor(data.frame(x = "bonjour")$x)){
  data.frame = change_defaults("base::data.frame", stringsAsFactors = FALSE)
  
  as.data.frame = change_defaults("base::as.data.frame", stringsAsFactors = FALSE)
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










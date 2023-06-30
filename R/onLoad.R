


.onLoad = function(libname, pkgname){
  # setting some options
  
  # operations
  setup_operations()
  
  # generating functions (only if root)
  generate_help_extensive()

  # help
  setup_help_extensive()

  invisible()
}




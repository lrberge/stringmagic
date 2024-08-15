# 
# Generated automatically with indexthis::indexthis_vendor
# this is indexthis version 1.1.0
# 


to_index = function(..., list = NULL, sorted = FALSE, items = FALSE,
                    items.simplify = TRUE){
  return_items = items
  IS_DOT = TRUE
  if(!missing(list) && !is.null(list)){
    if(!is.list(list)){
      stop("The argument `list` must be a list of vectors of the same length.",
           "\nPROBLEM: currently it is not a list.")
    } else if(length(list) == 0){
      stop("The argument `list` must be a list of vectors of the same length.",
           "\nPROBLEM: currently this list is empty.")
    }
    dots = list
    IS_DOT = FALSE
  } else {
    dots = list(...)
  }  
  Q = length(dots)
  n_all = lengths(dots)
  n = n_all[1]
  if(length(unique(n_all)) != 1){
    stop("All elements in `...` should be of the same length (current lenghts are ", 
         enum(n_all), ").")
  }
  if(n == 0){
    res = integer(0)
    if(return_items){
      items = integer(0)
      if(items.simplify){
        items = data.frame()
      }
      res = list(index = res, items = items)
    }
    return(res)
  }
  info = cpp_to_index(dots)
  index = info$index
  if(sorted || return_items){
    items_unik = vector("list", Q)
    for (q in 1:Q) {
      items_unik[[q]] = dots[[q]][info$first_obs]
    }
    if(sorted){
      x_order = do.call(order, items_unik)
      index = order(x_order)[index]
      for (q in 1:Q) {
        items_unik[[q]] = items_unik[[q]][x_order]
      }
    }
    items = NULL
    if(items.simplify && Q == 1){
      items = items_unik[[1]]
    } else {
      user_names = names(dots)
      if(is.null(user_names)){
        user_names = character(Q)
      }
      if(IS_DOT){
        mc_dots = match.call(expand.dots = FALSE)[["..."]]
      }
      for(q in 1:Q){
        if(nchar(user_names[q]) == 0){
          is_done = FALSE
          if(IS_DOT){
            mcq = mc_dots[[q]]
            if(is.name(mcq)){
              user_names[q] = as.character(mcq)[1]
              is_done = TRUE
            } else if(is.call(mcq) && as.character(mcq[[1]])[1] == "$"){
              user_names[q] = as.character(mcq[[3]])[1]
              is_done = TRUE
            }
          }
          if(!is_done){
            user_names[q] = paste0("x", q)
          }          
        }
      }
      names(items_unik) = user_names
      items = as.data.frame(items_unik)
      row.names(items) = 1:nrow(items)
    }
    if(return_items){
      res = list(index = index, items = items)
    } else {
      res = index
    }
  } else {
    res = index
  }
  res
}


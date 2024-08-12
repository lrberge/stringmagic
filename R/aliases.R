#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-06-27
# ~: Alias generators
#------------------------------------------------------------------------------#


#' Create `string_magic` aliases with custom defaults
#' 
#' Utility to easily create `string_magic` aliases with custom default
#' 
#' @inheritParams string_magic
#' 
#' @param .local_ops Named list or `NULL` (default). If provided, it must be a list 
#' of the form `list(alias1 = ops1, alias2 = ops2)` where alias is the name of the newly defined 
#' operator an `ops` is a character scalar representing the associated string_magic operations.
#' Ex: `list(add = "' + 'collapse")` creates the operation `add` which collapses the 
#' string with pluses. All operations created here are only available to the
#' generated function.
#' 
#' @details 
#' 
#' Use this function if you want to change `string_magic` default values. For example,
#' if you want the interpolation to be done with `"{{}}"` (instead of `{}`) or if you want the 
#' default separation to be the space (instead of the empty string). See the example.
#' 
#' @return 
#' This function returns a function which will behave in the same way as [string_magic()]
#' 
#' 
#' @inheritSection string_magic_register_fun Writing a package using `string_magic`
#' 
#' @author 
#' Laurent Berge
#' 
#' @family related to string_magic
#' @family tools with aliases
#' 
#' @examples 
#' 
#' # we create the function sma2 with different defaults
#' sma2 = string_magic_alias(.delim = ".[ ]", .sep = " ", .class = "string_magic")
#' 
#' person = "john doe"
#' sma2("Hello", ".[title ? person]")
#' 
#' # you can use the arguments whose default has been changed
#' sma2("Hello", ".[title ? person]", .sep = ": ")
#' 
#' 
#' 
string_magic_alias = function(.sep = "", .vectorize = FALSE, 
                              .delim = c("{", "}"), .last = NULL, 
                              .post = NULL, .default = NULL, .nest = FALSE,
                              .invisible = FALSE, .local_ops = NULL,
                              .collapse = NULL,  .check = TRUE, 
                              .class = NULL, .namespace = NULL){
  
  # checks
  check_character(.sep, scalar = TRUE)
  check_logical(.vectorize, scalar = TRUE)
  .delim = check_set_delimiters(.delim)
  check_last(.last)
  check_function(.post, null = TRUE)
  check_character(.default, scalar = TRUE, null = TRUE)
  check_logical(.nest, scalar = TRUE)
  check_logical(.invisible, scalar = TRUE)
  # .local_ops, see below
  check_character(.collapse, scalar = TRUE, null = TRUE)
  check_logical(.check, scalar = TRUE)
  check_character(.class, no_na = TRUE, null = TRUE)
  check_character(.namespace, scalar = TRUE, null = TRUE)
  
  # .local_ops
  if(!missnull(.local_ops)){
    info = .sma("\nINFO: names of the list = alias. Content = string_magic operations.",
                "\nEx: .local_ops = list(\"plus\" = \"' + 'collapse\")")
    
    if(!is.list(.local_ops)){
      stopi("The argument `.local_ops` must be a list. ",
            "Currently it is of class {enum.bq?class(.local_ops)}.", info)
    }
    if(length(.local_ops) == 0){
      stopi("The argument `.local_ops` must be a non-empty list. ",
            "Currently it is empty.", info)
    }
    if(is.null(names(.local_ops))){
      stopi("In the argument `.local_ops` require names.",
            "\nPROBLEM: the list has no names.", info)
    }
    if(string_any(names(.local_ops), "[^[:lower:]]")){
      stopi("In the argument `.local_ops`, the aliases must be composed of ",
            "only lower case letters.",
            "\nPROBLEM: this is not the case for ",
            "{'[^[:lower:]]'get, enum.bq ? names(.local_ops)}.", info)
    }
    if(!all(sapply(.local_ops, is.character))){
      i_pblm = which(!sapply(.local_ops, is.character))[1]
      stopi("In the argument `.local_ops`, each element of the list must ",
            "be a character scalar.",
            "\nPROBLEM: the {nth?i_pblm} element is of class ",
            "{enum.bq?class(.local_ops[i_pblm]}.", info)
    }
    if(any(lengths(.local_ops) != 1)){
      i_pblm = which(lengths(.local_ops) != 1)[1]
      stopi("In the argument `.local_ops`, each element of the list must ",
            "be a character scalar.",
            "\nPROBLEM: the {nth?i_pblm} element is of length {len?.local_ops[i_pblm]}.", 
            info)
    }
    
    if(is.null(.namespace)){
      .namespace = tag_gen()
    }   
    
    for(i in seq_along(.local_ops)){
      alias = names(.local_ops)[[i]]
      content = .local_ops[[i]]
      string_magic_register_ops(content, alias, .namespace)
    }    
  }
  
  
  # forcing evaluations (INDISPENSABLE)
  sep = .sep
  vectorize = .vectorize
  delim = .delim
  last = .last
  post = .post
  default = .default
  nest = .nest
  invisible = .invisible
  collapse = .collapse
  check = .check
  class = .class
  namespace = .namespace
  
  if(.nest && .vectorize){
    stop("You cannot have `.nest` and `.vectorize` at the same time. One of the two must be set to `FALSE`.")
  }
  
  res = function(..., .envir = parent.frame(), .sep = sep, .vectorize = vectorize, 
                 .delim = delim, .last = last, .post = post, .default = default,
                 .nest = nest, .invisible = invisible, .collapse = collapse, 
                 .check = check, .class = class, .help = NULL, 
                 .namespace = namespace){
                    
    string_magic(..., .envir = .envir, .sep = .sep, .vectorize = .vectorize, 
                 .delim = .delim, .last = .last, .post = .post, .default = .default,
                 .nest = .nest, .invisible = .invisible, .collapse = .collapse,
                 .check = .check, .class = .class, .help = .help,
                 .namespace = .namespace)
  }
  
  res
}


#' @describeIn cat_magic Create an alias of `cat_magic` with custom defaults
cat_magic_alias = function(.sep = "", .end = "", .width = FALSE, .leader = "", 
                           .delim = c("{", "}"), .last = NULL, 
                           .trigger = TRUE, .check = TRUE, 
                           .namespace = NULL){
  
  # checks
  check_character(.sep, scalar = TRUE)
  check_character(.end, scalar = TRUE)
  check_character(.leader, scalar = TRUE)
  .delim = check_set_delimiters(.delim)
  check_character(.last, scalar = TRUE, null = TRUE)
  check_logical(.trigger, scalar = TRUE)
  check_logical(.check, scalar = TRUE)
  check_character(.namespace, scalar = TRUE, null = TRUE)
  
  # width is special
  is_call = isTRUE(try(is.call(.width), silent = TRUE))
  if(!is_call){
    .width = substitute(.width)
    if(!".sw" %in% all.vars(.width)){
      .width = eval(.width, parent.frame())
    }
  }
  
  sep = .sep
  end = .end
  leader = .leader
  last = .last
  trigger = .trigger
  check = .check
  namespace = .namespace
  width = .width
  delim = .delim
  
  res = function(..., .sep = sep, .end = end, .width = width, .leader = leader, 
                 .envir = parent.frame(), delim = delim, .last = last, 
                 .trigger = trigger, .check = check, .help = NULL, 
                 .namespace = namespace){
    cat_magic(..., .sep = .sep, .end = .end, .width = .width, .leader = .leader, 
              .envir = .envir, .delim = .delim, .last = .last, 
              .trigger = .trigger, .check = .check, .help = .help, 
              .namespace = .namespace)
  }
  
  res
}

#' @describeIn cat_magic Create an alias of `message_magic` with custom defaults
message_magic_alias = function(.sep = "", .end = "\n", .width = "min(100, .sw)", .leader = "", 
                               .delim = c("{", "}"), .last = NULL, .trigger = TRUE, 
                               .check = TRUE, .namespace = NULL){
  
  # checks
  check_character(.sep, scalar = TRUE)
  check_character(.end, scalar = TRUE)
  check_character(.leader, scalar = TRUE)
  .delim = check_set_delimiters(.delim)
  check_character(.last, scalar = TRUE, null = TRUE)
  check_logical(.check, scalar = TRUE)
  check_logical(.trigger, scalar = TRUE)
  check_character(.namespace, scalar = TRUE, null = TRUE)
  
  # width is special
  is_call = isTRUE(try(is.call(.width), silent = TRUE))
  if(!is_call){
    .width = substitute(.width)
    if(!".sw" %in% all.vars(.width)){
      .width = eval(.width, parent.frame())
    }
  }
  
  # forcing the eval
  sep = .sep
  end = .end
  leader = .leader
  last = .last
  trigger = .trigger
  check = .check
  namespace = .namespace
  width = .width
  delim = .delim
  
  res = function(..., .sep = sep, .end = end, .width = width, .leader = leader, 
                 .envir = parent.frame(), .delim = delim, .last = last, 
                 .trigger = trigger,
                 .check = check, .help = NULL, 
                 .namespace = namespace){

    message_magic(..., .sep = .sep, .end = .end, .width = .width, .leader = .leader, 
                  .envir = .envir, .delim = .delim, .last = .last, 
                  .trigger = .trigger, .check = .check, .help = .help, 
                  .namespace = .namespace)
  }
  
  res
}

#' @describeIn string_ops `string_ops` alias with custom defaults
string_ops_alias = function(op = NULL, pre_unik = NULL, namespace = NULL){
  #
  
  check_character(op, scalar = TRUE, null = TRUE)  
  check_logical(pre_unik, null = TRUE, scalar = TRUE)
  check_character(namespace, scalar = TRUE, null = TRUE)  
  
  # forcing evaluations
  .op = op
  .pre_unik = pre_unik
  .namespace = namespace
  
  res = function(x, op = .op, pre_unik = .pre_unik, namespace = .namespace){                  
    string_ops(x, op = op, pre_unik = pre_unik, namespace = namespace)
  }
  
  res
}

#' @describeIn string_clean Create a `string_clean` alias with custom defaults
string_clean_alias = function(replacement = "", pipe = " => ", split = ",[ \n\t]+", 
                              ignore.case = FALSE, fixed = FALSE, word = FALSE, 
                              total = FALSE, single = FALSE, 
                              namespace = NULL){
  
  check_character(replacement, scalar = TRUE)
  check_character(pipe, scalar = TRUE)
  check_character(split, scalar = TRUE, null = TRUE)
  check_logical(ignore.case, scalar = TRUE)
  check_logical(fixed, scalar = TRUE)
  check_logical(word, scalar = TRUE)
  check_logical(total, scalar = TRUE)
  check_logical(single, scalar = TRUE)
  check_character(namespace, scalar = TRUE, null = TRUE)
  
  # forcing the evaluations
  .replacement = replacement
  .pipe = pipe
  .split = split
  .ignore.case = ignore.case
  .fixed = fixed
  .word = word
  .total = total
  .single = single
  .namespace = namespace
  
  res = function(x, ..., replacement = .replacement, pipe = .pipe, split = .split, 
                 ignore.case = .ignore.case, fixed = .fixed, word = .word, 
                 total = .total, single = .single, envir = parent.frame(), 
                 namespace = .namespace){                  

    string_clean(x, ..., replacement = replacement, pipe = pipe, split = split, 
                 ignore.case = ignore.case, fixed = fixed, word = word, 
                 total = total, single = single, envir = envir, 
                 namespace = namespace)
  }
  
  res  
}

#' @describeIn string_vec Create `string_vec` aliases with custom defaults
string_vec_alias = function(.cmat = FALSE, .nmat = FALSE, .df = FALSE, .df.convert = TRUE, 
                            .last = NULL, .delim = c("{", "}"), .split = TRUE, 
                            .protect.vars = FALSE, .check = TRUE, .sep = NULL, 
                            .collapse = NULL, .namespace = NULL){
  
  .delim = check_set_delimiters(.delim)
  check_character(.sep, scalar = TRUE, null = TRUE)
  check_character(.collapse, scalar = TRUE, null = TRUE)
  check_character(.last, scalar = TRUE, null = TRUE)
  check_logical(.protect.vars, scalar = TRUE)
  check_logical(.check, scalar = TRUE)
  check_character(.namespace, scalar = TRUE, null = TRUE)
  .split = check_set_split(.split)
  check_set_mat(.cmat, .nmat, .df)
  check_logical(.df.convert, scalar = TRUE)
  
  # forcing evaluation
  nmat = .nmat
  cmat = .cmat
  df = .df
  df.convert = .df.convert
  last = .last
  delim = .delim
  split = .split
  protect.vars = .protect.vars
  check = .check
  sep = .sep
  collapse = .collapse
  namespace = .namespace  
  
  res = function(..., .cmat = cmat, .nmat = nmat, .df = df, .df.convert = df.convert,
                 .last = last, .delim = delim, .envir = parent.frame(), 
                 .split = split, .protect.vars = protect.vars, 
                 .check = check, .sep = sep, 
                 .collapse = collapse, .namespace = namespace){

    string_vec(..., .cmat = .cmat, .nmat = .nmat, .df = .df, .df.convert = .df.convert, 
               .last = last, .delim = .delim, .envir = .envir, 
               .split = .split, .protect.vars = .protect.vars, 
               .check = .check, .sep = .sep, 
               .collapse = .collapse, .namespace = .namespace)
  }
  
  res
}






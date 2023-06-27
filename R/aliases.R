#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-06-27
# ~: Alias generators
#------------------------------------------------------------------------------#


#' Create `smagic` aliases with custom defaults
#' 
#' Utility to easily create `smagic` aliases with custom default
#' 
#' @inheritParams smagic
#' 
#' @details 
#' 
#' Use this function if you want to change `smagic` default values. For example,
#' if you want the interpolation to be done with "{{}}" (instead of `{}`) or if you want the 
#' default separation to be the space (instead of the empty string). See the example.
#' 
#' 
#' @inheritSection smagic_register_fun Writing a package using `smagic`
#' 
#' @author 
#' Laurent Berge
#' 
#' @family related to `smagic`
#' @family tools with aliases
#' 
#' @examples 
#' 
#' # we create the function sma2 with different defaults
#' sma2 = smagic_alias(.delim = ".[ ]", .sep = " ", .class = "smagic")
#' 
#' person = "john doe"
#' sma2("Hello", ".[title ? person]")
#' 
#' # you can use the arguments whose default has been changed
#' sma2("Hello", ".[title ? person]", .sep = ": ")
#' 
#' 
#' 
smagic_alias = function(.sep = "", .vectorize = FALSE, 
                        .delim = c("{", "}"), .last = NULL, 
                        .collapse = NULL,  .check = TRUE, 
                        .class = NULL, .data.table = TRUE, .namespace = NULL){
  #
  
  check_character(.sep, scalar = TRUE)
  check_logical(.vectorize, scalar = TRUE)
  check_last(.last)
  check_character(.collapse, scalar = TRUE, null = TRUE)
  check_logical(.check, scalar = TRUE)
  check_character(.class, no_na = TRUE, null = TRUE)
  check_logical(.data.table, scalar = TRUE)
  check_character(.namespace, scalar = TRUE, null = TRUE)
  
  # forcing evaluations
  sep = .sep
  vectorize = .vectorize
  delim = check_set_delimiters(.delim)
  last = .last
  collapse = .collapse
  check = .check
  class = .class
  data.table = .data.table
  namespace = .namespace
  
  res = function(..., .envir = parent.frame(), .sep = sep, .vectorize = vectorize, 
                   .delim = delim, .last = last, 
                   .collapse = collapse, 
                   .check = check, .class = class, .help = NULL, 
                   .data.table = data.table, .namespace = namespace){
                    
    smagic(..., .envir = .envir, .sep = .sep, .vectorize = .vectorize, 
            .delim = .delim, .last = .last, .collapse = .collapse,
            .check = .check, .class = .class, .help = .help,
            .data.table = .data.table, .namespace = .namespace)
  }
  
  res
}

#' @describeIn string_ops `string_ops` alias with custom defaults
string_ops_alias = function(pre_unik = NULL, namespace = NULL){
  #
  
  check_logical(pre_unik, null = TRUE, scalar = TRUE)
  check_character(namespace, scalar = TRUE, null = TRUE)  
  
  # forcing evaluations
  force(pre_unik)
  force(namespace)
  
  res = function(x, op, pre_unik = pre_unik, namespace = namespace){                  
    string_ops(x, op, pre_unik = pre_unik, namespace = namespace)
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
  force(replacement)
  force(pipe)
  force(split)
  force(ignore.case)
  force(fixed)
  force(word)
  force(total)
  force(single)
  force(namespace)
  
  res = function(x, ..., replacement = replacement, pipe = pipe, split = split, 
                     ignore.case = ignore.case, fixed = fixed, word = word, 
                     total = total, single = single, envir = parent.frame(), 
                     namespace = namespace){                  
    string_clean(x, ..., replacement = replacement, pipe = pipe, split = split, 
                     ignore.case = ignore.case, fixed = fixed, word = word, 
                     total = total, single = single, envir = envir, 
                     namespace = namespace)
  }
  
  res  
}

#' @describeIn string_vec Create `string_vec` aliases with custom defaults
string_vec_alias = function(.delim = c("{", "}"), .split = TRUE, 
                         .protect.vars = TRUE, .sep = NULL, 
                         .collapse = NULL, .namespace = NULL){
  
  .delim = check_set_delimiters(.delim)
  check_character(.sep, scalar = TRUE, null = TRUE)
  check_character(.collapse, scalar = TRUE, null = TRUE)
  check_logical(.split, scalar = TRUE)
  check_logical(.protect.vars, scalar = TRUE)
  check_character(.namespace, scalar = TRUE, null = TRUE)
  
  # forcing evaluation
  force(.sep)
  force(.collapse)
  force(.split)
  force(.protect.vars)
  force(.namespace)
  
  res = function(..., .delim = .delim, .envir = parent.frame(), 
                   .split = .split, .protect.vars = .protect.vars, .sep = .sep, 
                   .collapse = .collapse, .namespace = .namespace){
    string_vec(..., .delim = .delim, .envir = .envir, 
                   .split = .split, .protect.vars = .protect.vars, .sep = .sep, 
                   .collapse = .collapse, .namespace = .namespace)
  }
  
  res
}






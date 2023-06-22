#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Jul 27 09:40:24 2022
# ~: string ops core functions
#----------------------------------------------#


####
#### User-level ####
####

#' Simple print function for objects of class `smagick`
#' 
#' Print `smagick` character vectors in a nice way.
#' 
#' @method print smagick
#' 
#' @param x A `smagick` object, obtained from the function `smagick`.
#' @param ... Not currently used.
#' 
#' @author 
#' Laurent Berge
#' 
#' @inherit str_clean seealso 
#' 
#' @examples 
#' 
#' cars = row.names(mtcars)
#' 
#' print(cars)
#' 
#' smagick("/The first cars are:, {6 first ? cars}")
#' 
#' 
print.smagick = function(x, ...){
  if(length(x) == 0){
    print(character(0))
  } else {
    cat(x, sep = "\n")
  }
}

#' Register custom operations to apply them in smagick
#' 
#' Extends the capabilities of [smagick()] by adding any custom operation
#' 
#' @param fun A function which must have at least the arguments 'x' and '...'. 
#' Additionnaly, it can have the arguments: 'argument', 'options', 'group', 'group_flag'.
#' This function must return a vector.
#' This function will be internally called by `smagick` in the form 
#' `fun(x, argument, options, group, group_flag)`.`x`: the value to which the 
#' operation applies. `argument`: the quoted `smagick` argument (always character). 
#' `options`: a character vector of `smagick` options. The two last arguments are of use
#' only in group-wise operations if `fun` changes the lengths of vectors. `group`: an index of
#' the group to which belongs each observation (integer). `group_flag`: value between 0
#' and 2; 0: no grouping operation requested; 1: keep track of groups; 2: apply grouping.
#' @param alias Character scalar, the name of the operation.
#' @param valid_options A character vector or NULL (default). Represents a list of 
#' valid options for the operation. This is used: a) to enable auto-completion,
#' b) for error-handling purposes.
#' 
#' 
#' @author 
#' Laurent R. Berge
#' 
#' @seealso 
#' `smagick()`
#' 
#' @examples
#' 
#' # let's create an operation that adds markdown emphasis
#' # to elements of a vector
#' 
#' # A) define the function
#' fun_emph = function(x, ...) paste0("*", x, "*")
#' 
#' # B) register it
#' smagick_register(fun_emph, "emph")
#' 
#' # C) use it
#' x = smagick("/right, now")
#' smagick("Take heed, {emph, c? x}.")
#' 
#' #
#' # now let's add the option "strong"
#' fun_emph = function(x, options, ...) {
#'   if("strong" %in% options){
#'     paste0("***", x, "***")
#'   } else {
#'     paste0("*", x, "*")
#'   }
#' }
#' 
#' smagick_register(fun_emph, "emph", "strong")
#' 
#' x = smagick("/right, now")
#' smagick("Take heed, {emph.strong, c? x}.")
#' 
#' #
#' # now let's add an argument
#' fun_emph = function(x, argument, options, ...){
#'   arg = argument
#'   if(nchar(arg) == 0) arg = "*"
#'   
#'   if("strong" %in% options){
#'     arg = paste0(rep(arg, 3), collapse = "")
#'   }
#'   
#'   paste0(arg, x, arg)
#' }
#' 
#' smagick_register(fun_emph, "emph", "strong")
#' 
#' x = smagick("/right, now")
#' smagick("Take heed, {'_'emph.s, c? x}.")
#' 
#' 
#' 
#' 
smagick_register = function(fun, alias, valid_options = NULL){
  # fun: must be a function with x and ... as arguments
  # the argument names must be in:
  # x, argument, options, group, conditonnal_flag

  if(missing(fun)){
    stop("The argument `fun` must be provided. PROBLEM: it is missing.")
  }

  if(!is.function(fun)){
    stop_hook("The argument `fun` must be a function. ",
              "PROBLEM: it is not a function, instead it is of class {enum.bq?class(fun)}.")
  }

  check_character(alias, scalar = TRUE, mbt = TRUE)
  check_character(valid_options, no_na = TRUE, null = TRUE)

  fun_args = names(formals(fun))
  
  arg_must = c("...", "x")
  arg_missing = setdiff(arg_must, fun_args)
  if(length(arg_missing) > 0){
    stop_hook("The argument `fun` must be a function with {enum.bq?arg_missing} in its arguments.",
              "\nPROBLEM: it has no argument {enum.bq.or?arg_missing}.")
  }

  valid_args = smagick("/x, argument, options, group, group_flag")
  arg_pblm = setdiff(setdiff(fun_args, "..."), valid_args)
  if(length(arg_pblm) > 0){
    stop_hook("The argument `fun` must have specific argument names. Valid arguments are {enum.bq.or?valid_args}.",
              "\nPROBLEM: the argument{$s, enum.bq, are?arg_pblm} invalid.")
  }

  OPERATORS = getOption("smagick_operations_origin")

  if(alias %in% OPERATORS){
    stop_hook("The argument `alias` must not be equal to an existing internal argument.",
              "\nPROBLEM: the operation {bq?alias} is already an internal operation.")
  }

  user_operators = getOption("smagick_user_ops")
  if(is.null(user_operators)){
    user_operators = list()
  }

  if(!is.null(valid_options)){
    attr(fun, "valid_options") = valid_options
  }

  user_operators[[alias]] = fun

  options("smagick_user_ops" = user_operators)
  options("smagick_operations" = c(OPERATORS, names(user_operators)))

}

#' Set defaults for smagick
#' 
#' Se the default values for a few arguments of the function [smagick()].
#' 
#' @inheritParams smagick
#' @param reset Logical scalar, default is `FALSE`. Whether to reset all values.
#' 
#' @details 
#' By default, each call to `setSmagick` adds modifications to the default values. 
#' To set a few default values and resetting the others, you need to use `reset = TRUE`.
#' 
#' @author 
#' Laurent Berge
#' 
#' @examples 
#' 
#' # we change the default display of the results of smagick
#' setSmagick(.smagick.class = TRUE)
#' smagick("{S!x, y}{2 each?1:2}")
#' 
#' # back to a regular character vector
#' setSmagick(reset = TRUE)
#' smagick("{S!x, y}{2 each?1:2}")
#' 
setSmagick = function(.smagick.class = FALSE, .delim = c("{", ."}"), 
                      .sep = "", .data.table = TRUE, reset = FALSE){

  check_logical(.smagick.class, scalar = TRUE)
  check_logical(.data.table, scalar = TRUE)
    
  check_character(.sep, scalar = TRUE)
  
  .delim = check_set_character(.delim)

  # Getting the existing defaults
  opts = getOption("smagick_options")

  if(reset || is.null(opts)){
    opts = list()
  } else if(!is.list(opts)){
    warning("Wrong formatting of option 'smagick_options', all options are reset.")
    opts = list()
  }

  # Saving the default values
  mc = match.call()
  args_default = setdiff(names(mc)[-1], "reset")

  # NOTA: only elements in opts will be later set to their default
  for(v in args_default){
    opts[[v]] = eval(as.name(v))
  }

  options(smagick_options = opts)

}

set_defaults = function(opts_name){

  opts = getOption(opts_name)
  if(is.null(opts) || length(opts) == 0){
    return(NULL)
  }

  sysOrigin = sys.parent()
  mc = match.call(definition = sys.function(sysOrigin), call = sys.call(sysOrigin), expand.dots = FALSE)
  args_in = names(mc)

  args2set = setdiff(names(opts), args_in)

  for(v in args2set){
    assign(v, opts[[v]], parent.frame())
  }

}

####
#### ... smagick ####
####


smagick = function(..., .envir = parent.frame(), .sep = "", .vectorize = FALSE, 
                   .delim = c("{", "}"), .check = TRUE, .smagick.class = FALSE, 
                   .default = TRUE,
                   .slash = TRUE, .collapse = NULL, .help = NULL, .data.table = TRUE){


  if(!missing(.vectorize)) check_logical(.vectorize, scalar = TRUE)
  if(!missing(.slash)) check_logical(.slash, scalar = TRUE)
  if(!missing(.default)) check_logical(.default, scalar = TRUE)
  if(!missing(.data.table)) check_logical(.data.table, scalar = TRUE)
  if(!missing(.collapse)) check_character(.collapse, null = TRUE, scalar = TRUE)
  if(!missing(.sep)) check_character(.sep, scalar = TRUE)
  if(!missing(.envir)) check_envir(.envir)
  
  set_pblm_hook()
  
  if(.default){
    set_defaults("smagick_options")
  }  
  
  check_character(.delim, no_na = TRUE)
  if(length(.delim) == 1){
    .delim = strsplit(.delim, " ", fixed = TRUE)[[1]]
  }
    
  if(.check){ 
    check_delimiters(.delim)
  }

  if(...length() == 0){
    if(missnull(.help)){
      return("")
    }
  } else if(identical(..1, "--help")){
    sc = sys.call()
    if(identical(sc[[2]], "--help")){
      .help = "_COMPACT_" # means full help
    }
  }

  res = smagick_internal(..., .delim = .delim, .envir = .envir, .sep = .sep,
                            .vectorize = .vectorize, .slash = .slash, .help = .help,
                            .collapse = .collapse, is_root = TRUE, 
                            .data.table = .data.table,
                            .check = .check)

  if(inherits(res, "help")){
      return(invisible(NULL))
  }

  if(isTRUE(.smagick.class)){
    class(res) = c("smagick", "character")
  }
  
  res
}

#' @describeIn smagick Like `smagick` but without any error handling to save a few micro seconds
.smagick = function(..., .envir = parent.frame(), .sep = "", .vectorize = FALSE,
                    .delim = c("{", "}"), .collapse = NULL,
                    .check = FALSE, .slash = FALSE, .data.table = FALSE){

  set_pblm_hook()
  
  if(length(.delim) == 1){
    .delim = strsplit(.delim, " ", fixed = TRUE)[[1]]
  }

  smagick_internal(..., .delim = .delim, .envir = .envir,
                      .slash = .slash, .sep = .sep,
                      .vectorize = .vectorize, .is_root = TRUE, 
                      .data.table = .data.table, .collapse = .collapse,
                      .check = .check)
}

.sma = .smagick

####
#### Internal ####
####

smagick_internal = function(..., .delim = c("{", "}"), .envir = parent.frame(),  .data = list(),
                               .sep = "", .vectorize = FALSE,
                               .slash = TRUE, .collapse = NULL,
                               .help = NULL, .is_root = FALSE, .data.table = FALSE,
                               .check = FALSE, .plural_value = NULL){
  
  # flag useful to easily identify this environment (used in error messages)
  is_smagick_internal = TRUE

  if(!is.null(.help)){
    if(identical(.help, "_COMPACT_")){
      msg = getOption("smagick_help_compact")
      
      msg = paste(msg, collapse = "\n")
      stop_up("Help requested.", msg = msg)
    } else {
      
      on.exit(smagick_dynamic_help(.help))
      
      stop_up("smagick: Help requested.")
    }    
  }

  if(.is_root && .data.table){
    # we check for data table calls
    is_dt = FALSE
    sc = sys.calls()
    n_sc = length(sc)
    if(n_sc >= 4){

      for(i in 2:(n_sc - 2)){
        ls_i = ls(envir = parent.frame(i), all.names = TRUE)
        is_dt = all(c(".SD", ".I", ".N") %in% ls_i)
        if(is_dt){
          break
        }
      }

      if(is_dt){
          is_by = ".BY" %in% ls_i && ".Random.seed" %in% ls(envir = parent.frame(i + 1), all.names = TRUE)

          if(is_by){
            # that's not great but it works somehow
            dt_call = sc[[n_sc - i - 1]]
            dt_name = dt_call[[2]]
            dt_data = unclass(eval(dt_name, parent.frame(i + 1)))
            for(v in c(".I", ".N", ".GRP", ".BY")){
              dt_data[[v]] = get(v, parent.frame(i))
            }
          } else {
            dt_data = unclass(get("x", parent.frame(i + 2)))
            for(v in c(".N", ".GRP")){
              dt_data[[v]] = get(v, parent.frame(i))
            }
          }

          attr(envir, "dt_data") = dt_data
      }
    }
  }

  if(...length() == 0){
    return("")

  } else if(...length() == 1){
    
    if(!is.null(...names())){
      stop_hook("`smagick` requires at least one character scalar to work.",
                "\nNamed arguments are only used as variables on which to apply interpolation.",
                "\nFIX: please provide at least one non-named argument.")
    }
    
    x = as.character(..1)
    
    if(length(x) > 1){
      stop_hook("`smagick` can only be applied to character scalars. ",
                "\nPROBLEM: the argument is not of length 1, it is of length {len.f ? x}.")
    }

  } else {

    if(.check){
      dots = check_expr(list(...), "In `smagick`, one element of ... could not be evaluated.")
    } else {
      dots = list(...)
    }
    
    # we check if some variables were passed in the arguments
    dot_names = names(dots)
    if(!is.null(dot_names)){
      is_var = dot_names != ""
      for(i in which(is_var)){
        .data[[dot_names[i]]] = dots[[i]]
      }
      dots[is_var] = NULL      
    }
    
    if(length(dots) == 0){
      stop_hook("`smagick` requires at least one character scalar to work.",
                "\nNamed arguments are only used as variables on which to apply interpolation.",
                "\nPROBLEM: all arguments are named.",
                "\nFIX: please provide at least one non-named argument.")
    }
    
    if(any(lengths(dots) > 1)){
      qui = which(lengths(dots) > 1)[1]
      stop_hook("`smagick` can only be applied to character scalars.",
                "\nPROBLEM: The ", n_th(qui),
                " elment in ... is of length ", length(dots[[qui]]), ".")
    }

    if(!.vectorize){
      # Note: using paste(..1, ..2, sep = .sep) explicitly only saves 2us vav do.call
      # not worth it.

      dots$sep = .sep
      x = do.call(paste, dots)
    } else {
      # vectorize
      n = length(dots)
      res = vector("list", n)
      for(i in 1:n){
        res[[i]] = smagick_internal(dots[[i]], .delim = .delim, .slash = .slash, .envir = .envir, 
                                    .data = .data, .check = .check)
      }

      return(unlist(res))
    }
  }

  if(is.na(x) || length(x) == 0){
    return(x)
  }

  BOX_OPEN = .delim[1]

  if(.slash && substr(x, 1, 1) == "/"){
    # we apply the "slash" operation
    x = sma_operators(substr(x, 2, nchar(x)), "/", NULL, "", .check = .check, .envir = .envir, 
                      group_flag = 0, .delim = .delim)
    return(x)
  } 
  
  if(!grepl(BOX_OPEN, x, fixed = TRUE)){
    return(x)

  } else {
    x_parsed = cpp_smagick_parser(x, .delim)
    if(length(x_parsed) == 1 && isTRUE(attr(x_parsed, "error"))){
      report_smagick_parsing_error(x, x_parsed, .delim)
    }
  }

  # NOTE on the parsing:
  # - parsing returns a list.
  #   If, eg: x = "str1.[stuff1]str2.[stuff2]"
  #   we will obtain a list of 4 elements
  #   note that the empty string between two boxes is an element
  # - an element can be either of length 1 or of length 2
  #   * l=1: means this is a regular string
  #   * l=2: this is a list of two elements:
  #     + the first element is a vector of operators, with ? or ! always at the end (when available)
  #     + the second element is the string to be interpolated (when available, some operators don't
  #       require interpolation, like pluralization)
  #   ex: "hello: .[u, C ! .[/luc, domi]]"
  #     [[1]]: "hello: "
  #     [[2]]:
  #           [[1]]: vector c("u", "C")
  #           [[2]]: scalar ".[/luc, domi]"
  # - note that nestedness is dealt with in here and NOT in the parser.
  #

  ANY_PLURAL = isTRUE(attr(x_parsed, "plural"))
  if(ANY_PLURAL){
    # we need to book keep the evaluations to 'know' to which the pluralization applies
    x_values_all = vector("list", length(x_parsed))
  }

  n_x_all = lengths(x_parsed)
  
  n = length(x_parsed)
  res = character(0)
  i_done = FALSE
  for(i in 1:n){

    if(i_done){
      i_done = FALSE
      next
    }

    xi = x_parsed[[i]]

    if(length(xi) == 1){
      # This is a regular string (no operations involved)

      if(i == 1){
        res = xi
      } else if(nchar(xi) > 0){
        res = paste0(res, xi)
      }

    } else {
      #
      # operations
      #

      operators = xi[[1]]
      xi = xi[[2]]

      if(length(operators) == 0){

        # we need to evaluate xi
        xi_call = check_set_smagick_parsing(xi, .check, .delim)

        if(is.character(xi_call)){
          xi = xi_call
        } else {
          xi = check_set_smagick_eval(xi_call, .data, .envir, .check)
        }

        if(ANY_PLURAL){
          # we save
          x_values_all[[i]] = xi
        }

      } else {

        n_op = length(operators)
        verbatim = operators[n_op] %in% c("!", "/")
        
        if(operators[1] %in% c("&", "&&")){
          .data[["len"]] = function(x) length(x)
        }

        is_ifelse = FALSE
        concat_nested = FALSE
        is_xi_done = FALSE
        is_plural = ANY_PLURAL && operators[1] %in% c("$", "#")
        i_candidate = i # => used when evaluating later xi by anticipation
        if(ANY_PLURAL){

          if(!is_plural){
            # if the xi has already been evaluated by anticipation, we catch it
            if(lengths(x_values_all)[i] > 0){
              xi = x_values_all[[i]]
              is_xi_done = TRUE
            }
          }

          if(is_plural){
            #
            # Finding out candidates for pluralization
            #

            if(identical(xi, "")){

              if(!is.null(.plural_value)){
                # we are in a nested pluralization call:
                # ex: "{$(;; Maybe you meant: {enum.bq.4}?) ? sugg)}"
                # here 'sugg' would be in `plural_value`
                # we would be in the evaluation of 
                # " Maybe you meant: {enum.bq.4}?"
                # if we did not pass it along, there would have been an error

                xi = .plural_value
                is_xi_done = TRUE

              } else {
                if(length(operators) == 1){
                  example = 'Example: x = c("Mark", "Sarah"); smagick("{$enum, is ? x} away.")'
                  .stop_hook("In `smagick`, the pluralization operator (`", operators[1], 
                             "`) must contain operations. ", '\n', example)
                }

                # We need to find it out!!!!!!
                xl = lengths(x_values_all)
                if(all(xl == 0)){
                  # We need to evaluate the next ones
                  ok = FALSE
                  if(i < n){
                    for(j in (i + 1):n){
                      if(length(x_parsed[[j]]) == 2 && !identical(x_parsed[[j]][[2]], "")){
                        ok = TRUE
                        break
                      }
                    }
                  }

                  if(!ok){
                    pblm = paste0(operators[1], paste0(operators[-1], collapse = ", "))
                    example = 'Example: x = c("Mark", "Sarah"); smagick("{$enum, is ? x} away.")'

                    .stop_hook("In `smagick`, the pluralization (`", pblm, "`) did not find any value to pluralize on. ",
                            "Please provide one by adding it after a question mark.\n", example)
                  }

                  i_candidate = j
                  xi = x_parsed[[j]][[2]]

                } else {
                  # Means that stg has been evaluated before
                  qui = tail(which(xl > 0), 1)
                  xi = x_values_all[[qui]]
                  is_xi_done = TRUE
                }
              }

            } else {
              # xi != ""

              # last item is ? or !, we trim it
              operators = operators[-n_op]
            }
          }
        }

        if(!is_plural) {
          # regular operators

          if(operators[1] %in% c("&", "&&")){
            # if else operator
            is_ifelse = TRUE
            xi_raw = xi = operators[2]
            verbatim = FALSE
          } else if(operators[n_op] == "/"){
            operators = "/"
          } else {
            operators = operators[-n_op]
            # The two separators ? and ! have no default operation
          }

          # If split operator, we need to concatenate
          concat_nested = length(operators) > 0 && grepl("(?:^|[^[:alpha:]])(?:s|S)$", operators[[1]])
        }


        #
        # evaluation of xi
        #


        if(!is_xi_done){
          if(verbatim){
            # for the slash operation, we delay the interpolation
            if((length(operators) == 0 || operators[1] != "/") && grepl(BOX_OPEN, xi, fixed = TRUE)){
              xi = smagick_internal(xi, .delim = .delim, .envir = .envir, .data = .data, .slash = FALSE,
                                      .vectorize = concat_nested, .check = .check)
            }
          } else if(!verbatim){
            # evaluation
            xi_call = check_set_smagick_parsing(xi, .check, .delim)
            xi = check_set_smagick_eval(xi_call, .data, .envir, .check)
          }
        }

        if(ANY_PLURAL){
          # we save the results
          x_values_all[[i]] = xi
          if(i != i_candidate){
            x_values_all[[i_candidate]] = xi
          }
        }


        #
        # Applying the operators
        #

        if(is_plural){
          xi = sma_pluralize(operators, xi, .delim, .envir, .data, .check)

        } else if(is_ifelse){

          xi_val = NULL
          vars = all.vars(str2lang(xi_raw))

          if(length(vars) == 0){
            if(operators[1] == "&&"){
              # ERROR
              form = "{&& cond ; true}"
              example = '\nEXAMPLE: x = c(5, 700); smagick("Value: {&&x > 20 ; > 20}")'

              .stop_hook("The if-else operator `&&`, of the form ", form,
                      ", requires at least one variable to be evaluated in the condition.",
                      "\nPROBLEM: no variable could be found.", example)
            }
            
          } else {
            do_eval = TRUE
            if(operators[1] == "&"){
              do_eval = grepl(BOX_OPEN, operators[3], fixed = TRUE)
              if(!do_eval && length(operators) == 4){
                do_eval = grepl(BOX_OPEN, operators[4], fixed = TRUE)
              }
            }
            
            if(do_eval){
              xi_call = str2lang(vars[1])
              xi_val = check_set_smagick_eval(xi_call, .data, .envir, .check)
            }
            
            if(operators[1] == "&&" && length(xi) != length(xi_val)){
              form = "{&& cond ; true}"
              example = 'EXAMPLE: x = c(5, 700); smagick("Value: {&&x > 20 ; > 20}")'

              stop_hook("The if-else operator `&&`, of the form {form} ",
                      "requires that the first variable used in the condition ",
                      "(here {bq?vars[1]}) is of the same length as the condition.",
                      "\nPROBLEM: the condition is of length {len.f?xi} while the ",
                      "variable is of length {len.f?xi_val}.",
                      "\n{example}")
            }
          }
          
          if(ANY_PLURAL){
            # we save the variable over which the ifelse is done
            x_values_all[[i]] = xi_val
            if(i != i_candidate){
              x_values_all[[i_candidate]] = xi_val
            }
          }

          xi = sma_ifelse(operators, xi, xi_val, .envir = .envir, .data = .data,
                          .check = .check, .delim = .delim)
        } else {
          #
          # REGULAR OPERATORS
          #

          group_flag = any(str_x(operators, 1) == "~")
          
          for(j in seq_along(operators)){
            opi = operators[[j]]
            op_parsed = sma_char2operator(opi)

            if(op_parsed$eval){
              argument_call = check_set_oparg_parse(op_parsed$argument, opi, .check)
              argument = check_set_oparg_eval(argument_call, .data, .envir, opi, .check)
            } else {
              argument = op_parsed$argument
            }

            if(.check){
              xi = check_expr(sma_operators(xi, op_parsed$operator, op_parsed$options, argument,
                                              .check = .check, .envir = .envir, group_flag = group_flag,
                                              .delim = .delim),
                                get_smagick_context(), " See error below:",
                                verbatim = TRUE, up = 1)
            } else {
              xi = sma_operators(xi, op_parsed$operator, op_parsed$options, argument, .check = .check,
                                 .envir = .envir, group_flag = group_flag,
                                 .delim = .delim)
            }
          }
        }

        extra = attr(xi, "extra")
        if(length(extra) > 0){

          if(length(extra$element_add_first) > 0){
            if(length(extra$element_add_last) > 0){
              xi = c(extra$element_add_first, xi, extra$element_add_last)
            } else {
              xi = c(extra$element_add_first, xi)
            }
          } else if(length(extra$element_add_last) > 0){
            xi = c(xi, extra$element_add_last)
          }

          if(length(extra$str_add_first) > 0){
            if(length(xi) > 0){
              xi[1] = paste0(extra$str_add_first, xi[1])
            } else {
              xi = extra$str_add_first
            }
          }

          if(length(extra$str_add_last) > 0){
            if(length(xi) > 0){
              xi[length(xi)] = paste0(xi[length(xi)], extra$str_add_last)
            } else {
              xi = extra$str_add_last
            }
          }

          attr(xi, "extra") = NULL
        }
      }
      
      if(i == 1){
        res = xi
      } else{
        if(i < n && n_x_all[i + 1] == 1){
          if(.vectorize){
            res = c(res, xi, x_parsed[[i + 1]])
          } else {
            res = paste0(res, xi, x_parsed[[i + 1]])
          }

          i_done = TRUE
        } else {
          if(.vectorize){
            res = c(res, xi)
          } else {
            res = paste0(res, xi)
          }
        }
      }
    }
  }

  if(!is.null(.collapse) && length(res) > 1){
    res = paste0(res, collapse = .collapse)
  }

  return(res)
}


sma_char2operator = function(x){

  op_parsed = cpp_parse_operator(x)

  OPERATORS = getOption("smagick_operations")
  
  ok = FALSE
  op = op_parsed$operator

  if(nchar(op) == 0){
    .stop_hook("In `smagick`, if a quoted value is present, the operators must ",
              "be of the form 'value'op, ",
              "with 'op' an operator. ",
              "\nPROBLEM: In `", x, "` the operator is missing.")
  }
  
  argument = op_parsed$argument
  do_eval = op_parsed$eval
  
  # we partially match operators if needed
  if(!op %in% OPERATORS){
    op = check_set_options(op, OPERATORS, case = TRUE, free = TRUE)
    op_parsed$operator = op
  }

  if(nchar(argument) == 0 && !substr(x, 1, 1) %in% c("'", "\"", "`")){
    # no argument provided: we need to:
    # - set default values if available
    # - send error if operator requires argument
    
    # default values
    argument = switch(op,
                      s = " ", S = ",[ \t\n]*", split = " ",
                      x = "[[:alnum:]]+", X = "[[:alnum:]]+", extract = "[[:alnum:]]+",
                      c = " ", C = ", | and ", collapse = " ",
                      times = 1, each = 1,
                      first = 1, last = 1,
                      cfirst = 1, clast = 1,
                      trim = 1, 
                      "")
    
    op_parsed$argument = argument

    if(op %in% c("R", "r", "%", "k", "K", "paste", "get", "is")){
      ex = c("R" = 'x = "She loves me."; smagick("{\'s\\b => d\'R ? x}")',
            "r" = 'x = "Amour"; smagick("{\'ou => e\'r ? x}...")',
            "replace" = 'x = "Amour"; smagick("{\'ou => e\'r ? x}...")',
            "%" = 'smagick("pi is: {%.03f ? pi}")',
            "k" = 'smagick("The first 8 letters of the longuest word are: {8k, q ! the longuest word}.")',
            "K" = 'x = 5:9; smagick("The first 2 elements of `x` are: {2K, C ? x}.")',
            "get" = 'x = row.names(mtcars) ; smagick("My fav. cars are {\'toy\'get.ignore, \'the \'app, enum ? x}.")',
            "is" = 'x = c("Bob", "Pam") ; smagick("{\'am\'is ? x}")',
            "paste" = 'x = "those, words"; smagick("Let\'s emphasize {S, \'**\'paste.both, c ? x}.")')

      .stop_hook("The operator `", op, "` has no default value, you must provide values explicitly.", 
                 " EXAMPLE: ", ex)
    }
  }

  if(op %in% c("Ko", "KO")){
    # special case
    text = if(op == "Ko") "||:rest: others" else "||:REST: others"
    argument = paste0(argument, text)
    op_parsed$operator = op = "K"
    op_parsed$argument = argument
  }
    
  if(!ok && !op %in% OPERATORS){
    
    op = check_set_options(op, OPERATORS, case = TRUE, free = TRUE)
    op_parsed$operator = op
    
    if(!op %in% OPERATORS){
      
      context = get_smagick_context()

      sugg_txt = suggest_item(op, OPERATORS, newline = FALSE, info = "operator")
      
      op = gsub("\n", "\\n", op)
      op = gsub("\t", "\\t", op)

      msg = .sma("{context}",
              "\nPROBLEM: {bq?op} is not a valid operator. ", sugg_txt,
              "\n\nINFO: Type smagick('--help') for more help or smagick(.help = regex) or smagick(.help = TRUE).",
              "\nEx. of valid stuff: smagick(\"{10 first, `6/2`last, ''c, 'i => e'r, upper.first ? letters}!\") ")

      .stop_hook(msg)
    }

  }

  op_parsed
}


sma_operators = function(x, op, options, argument, .check = FALSE, .envir = NULL, .data = list(),
                         group_flag = 0, .delim = c("{", "}")){

  # group_flag:  0 nothing
  #                    1 keep track of conditional things
  #                    2 apply conditional

  extra = attr(x, "extra")
  group_index = attr(x, "group_index")

  if(op == "/"){
    # slash ####
    
    # What happens:
    # - string is split wrt commas
    # - any interpolation is vectorized
    
    res = cpp_parse_slash(x, .delim)
    
    BOX_OPEN = .delim[1]
    
    is_open = grepl(BOX_OPEN, res, fixed = TRUE)
    n_open = sum(is_open)
    if(n_open > 0){
      n_res = length(res)
      all_elements = vector("list", n_res)
      
      for(i in 1:n_res){
        if(is_open[i]){
          all_elements[[i]] = smagick_internal(res[i], .delim = .delim, .envir = .envir,
                                               .data = .data, .check = .check)  
        } else {
          all_elements[[i]] = res[i]
        }        
      }
      
      res = do.call(base::c, all_elements)      
    }    
    
    
  } else if(op %in% c("c", "C", "collapse")){
    # C, collapse ####
    # collapse

    # argument of the form: 'main_sep|last_sep'
    sep_last = ""
    is_last = FALSE
    if(length(x) > 1 && grepl("|", argument, fixed = TRUE)){
      info = extract_pipe(argument, op)
      is_last = info$is_pipe
      argument = info$value
      sep_last = info$extra
    }
    sep = argument

    if(!is.null(group_index) && group_flag == 2){
      if(!is.character(x)){
        x = as.character(x)
      }
      res = cpp_paste_conditional(x, group_index, sep, sep_last)
      group_index = seq_along(res)
      
    } else {
      n_x = length(x)
      if(is_last){
        # This is the "last" operator
        if(n_x == 2){
          res = paste(x, collapse = sep_last)
        } else {
          res = paste(x[-n_x], collapse = sep)
          res = paste0(res, sep_last, x[n_x])
        }
      } else {
        res = paste(x, collapse = sep)
      }

      if(group_flag == 1){
        group_index = NULL
      }

    }
    
  } else if(op %in% c("s", "S", "split", "r", "R", "replace", "clean", "get", 
                      "is", "which", "x", "X", "extract")){
    # split, replace, clean, extract, get, is, which ####
    
    valid_options = c("word", "ignore", "fixed")
    if(op == "extract"){
      valid_options = c(valid_options, "first")
    } else if(op %in% c("r", "R", "clean", "replace")){
      valid_options = c(valid_options, c("total", "first"))
    } else if(op %in% c("get", "is", "which")){
      valid_options = c(valid_options, "equal", "in")
    }

    options = check_set_options(options, valid_options)
    is_word = "word" %in% options
    is_ignore = "ignore" %in% options
    is_fixed = "fixed" %in% options

    if(op == "extract"){
      if("first" %in% options){
        op = "x"
      } else {
        op = "X"
      }
    }

    if(op %in% c("s", "S", "split", "X", "x", "extract")){
      # otherwise => dealt with in str_is or str_clean
      # we don't repeat the processing (otherwise => bugs)
      
      pat_parsed = format_simple_regex_flags(argument, fixed = is_fixed, word = is_word, 
                                             ignore = is_ignore)
      argument = pat_parsed$pattern
      is_fixed = pat_parsed$fixed
    } 

    #
    # now the operations
    #
    
    if(!is.character(x) && op != "which"){
      x = as.character(x)
    }
    
    if(op %in% c("s", "S", "split")){
      # strsplit applied to "" returns character(0)
      # if occurring: need to fix
      
      x_split = strsplit(x, argument, fixed = is_fixed, perl = !is_fixed)
      
      fix_empty = any(x == "")

      if(group_flag != 0){
        # we keep track of the group index
        x_len_all = lengths(x_split)
        
        if(fix_empty){
          x_len_all[x_len_all == 0] = 1
        }
        
        group_index = rep(seq_along(x_len_all), x_len_all)
      }
      
      # Note that unlist removes empty strings
      if(fix_empty){
        x_split[x == ""] = ""
      }

      res = unlist(x_split)
      
    } else if(op %in% c("r", "R", "replace", "clean")){
      is_total = "total" %in% options
      is_first = "first" %in% options

      pipe = "=>"
      if(grepl(" => ", argument, fixed = TRUE)){
        pipe = " => "
      }
      
      sep = if(op == "clean") ",[ \t\n]+" else ""

      res = str_clean(x, argument, pipe = pipe, sep = sep, 
                      ignore.case = is_ignore, fixed = is_fixed, word = is_word, 
                      total = is_total, first = is_first)

    } else if(op == 'x'){
      x_pat = regexpr(argument, x, fixed = is_fixed, perl = !is_fixed)

      res = substr(x, x_pat, x_pat - 1 + attr(x_pat, "match.length"))
    } else if(op == "X"){
      # extract all patterns

      x_list = regmatches(x, gregexpr(argument, x, fixed = is_fixed, perl = !is_fixed))

      if(group_flag != 0){
        # we keep track of the group index
        x_len_all = lengths(x_list)
        # I could avoid the last line... later
        group_index = rep(seq_along(x_len_all), x_len_all)
        group_index = cpp_recreate_index(group_index)
      }

      res = unlist(x_list)
    } else if(length(argument) == 1 && argument == "" && op == "which"){
      
      if(!is.logical(x)){
        stop_hook("The operation `which` must apply only to logical values.",
                 "\nPROBLEM: the current value to which it is applied is not logical ",
                 "(instead is is of class {enum.bq?class(x)}).")
      }

      res = which(x)

      if(group_flag == 1){
        group_index = group_index[res]
        group_index = cpp_recreate_index(group_index)
      } else if(group_flag == 2){
        stop_hook("The operation `which` cannot be applied in ",
                  "conditional statements (inside `~()`).",
                  "\nFIX: simply put this operation after the conditional statement.")
      }
      
    } else {
      # default is str_is
      if("equal" %in% options || "in" %in% options){
        if("equal" %in% options){
          res = x == argument
        } else {
          arg_split = strsplit(argument, ",[ \t\n]+")
          res = x %in% unlist(arg_split)
        }
        
      } else {
        res = str_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, word = is_word)
      }

      if(op %in% c("get", "which")){

        if(group_flag == 1){
          group_index = group_index[res]
          group_index = cpp_recreate_index(group_index)
        } else if(group_flag == 2){
          stop_hook("The operation `{q?argument}{op}` cannot be applied in ",
                    "conditional statements (inside `~()`).",
                    "\nFIX: simply put this operation after or before the conditional statement.")
        }

        if(op == "get"){
          res = x[res]
        } else if(op == "which"){
          res = which(res)
        }

      }
    } 
    

  } else if(op %in% c("each", "times")){
    # times/each ####

    if(!is_numeric_in_char(argument)){
      msg = paste0("In `smagick`: the operator `", op, "` must have numeric arguments, `",
                   argument, "` is not numeric.")
      .stop_hook(msg)
    }
    
    if(length(argument) != 1){
      msg = paste0("In `smagick`: the operator `", op, "` must have an argument of length 1.",
                   "\nPROBLEM: the argument is of length ", length(argument), ".")
      .stop_hook(msg)
    }
    
    if(op == "each"){
      res = rep(x, each = as.numeric(argument))
    } else {
      res = rep(x, as.numeric(argument))
    }

    if("c" %in% options){
      res = paste(res, collapse = "")
    }

    group_index = NULL

  } else if(op == "upper"){
    # upper, lower, title, Q, ####
    
    options = check_set_options(options, c("first", "sentence"))
        
    if("first" %in% options){
      # First letter only, if relevant
      res = as.character(x)
      substr(res, 1, 1) = toupper(substr(x, 1, 1))  
    } else if("sentence" %in% options){
      # we add upper case like in sentences
      # does not work for spanish for instance (with inverted !? coming first)
      res = gsub("(^|[.?!])(\\s*)(\\p{Ll})", "\\1\\2\\U\\3", x, perl = TRUE)
    } else {
      res = toupper(x)  
    }
    
  } else if(op == "lower"){
    res = tolower(x)

  } else if(op == "title"){
    # title case

    options = check_set_options(options, c("force", "ignore"))

    if("force" %in% options){
      # puts in lowercase first
      x = tolower(x)
    }

    is_ignore = "ignore" %in% options
    if(is_ignore){
      # we add __IGNORE in front of the items so that upper casing won't proc
      IGNORE = c("a", "the", "in", "on", "at", "of", 
                 "for", "to")
      pattern = paste0("(?<=[^:!?.]) (", paste0(IGNORE, collapse = "|"), ")(?=(\\s|$))")
      x = gsub(pattern, " __IGNORE\\1", x, perl = TRUE)
    }
    
    # should I add unicode awareness with (*UCP)? Option?
    # I would say yes by default bc you only apply this function
    # to small strings

    res = gsub("(*UCP)(^|[^\\p{L}])(\\p{Ll})", "\\1\\U\\2", x, perl = TRUE)
    
    if(is_ignore){
      res = gsub("__IGNORE", "", res, fixed = TRUE)
    }

  } else if(op == "q"){
    res = paste0("'", x, "'")

  } else if(op == "Q"){
    res = paste0("\"", x, "\"")

  } else if(op == "bq"){
    res = paste0("`", x, "`")

  } else if(op %in% c("format", "Format")){
    # Format, % ####

    options = check_set_options(options, c("0", "zero", "right", "center"))

    is_zero = any(options %in% c("0", "zero"))
    is_right = "right" %in% options || op == "Format" || is_zero
    is_center = any(options == "center")
    is_left = !is_right && !is_center

    if(is_right){
      res = format(x, justify = "right", big.mark = ",")
    } else {
      res = format(x, big.mark = ",")
    }   

    if(is.numeric(x)){
      if(is_zero){
        res = gsub(" ", "0", res, fixed = TRUE)
      } else if(!is_right){
        # we have to do that.... not sure the use cases need to be optimized
        pos = if(is_center) "centre" else "left"
        res = format(cpp_normalize_ws(res), justify = pos)
      }
    } else if(is_zero && is_numeric_in_char(x)){
      res = gsub(" ", "0", res, fixed = TRUE)
    }

  } else if(op == "%"){
    res = sprintf(paste0("%", argument), x)

  } else if(op == "ws"){
    # White spaces, tws, trim ####

    # w: only whitespaces
    # p: punct
    # d: digits
    # i: isolated
    
    options = check_set_options(options, c("punct", "digit", "isolated"))

    clean_punct = "punct" %in% options
    clean_digit = "digit" %in% options
    clean_isolated = "isolated" %in% options

    res = cpp_normalize_string(x, clean_punct, clean_digit, clean_isolated)
    
  } else if(op == "tws"){
    # trimming WS

    # note that the changes are 'in place' => it does not matter here
    # but I should not use cpp_trimws in other functions
    res = cpp_trimws_in_place(x)

  } else if(op == "trim"){
    # we trim

    nb = argument
    if(!is_numeric_in_char(nb)){
      msg = paste0("In `smagick`: the operator `", op, "` must first contain a numeric argument. PROBLEM: `",
                   argument, "` is not numeric.")
      .stop_hook(msg)
    }
    nb = as.numeric(nb)

    is_both = opt_equal(options, "both")
    is_right = any(c("r", "right") %in% options) || nb < 0
    
    if(!is.character(x)){
      x = as.character(x)
    }

    nx = nchar(x)
    if(is_both){
      # on both sides
      nb = abs(nb)

      res = substr(x, 1, nx - nb)
      res = substr(res, 1 + nb, nchar(res))

    } else if(is_right){
      # we revert: start from the end
      nb = abs(nb)
      res = substr(x, 1, nx - nb)
    } else {
      res = substr(x, 1 + nb, nx)
    }

  } else if(op %in% c("k", "K")){
    # keep: either the nber of characters (k) or the number of elements (K)

    #
    # Keep, enum ####
    #
    
    info = extract_pipe(argument, op, double = TRUE, numeric = TRUE, mbt = TRUE)
    nb = info$value
    add = info$extra
    is_included = info$is_double

    if(op == "k"){
      if(!is.character(x)){
        x = as.character(x)
      }
      qui = nchar(x) > nb
      res = substr(x, 1, nb)

      if(is_included){
        res[qui] = substr(res[qui], 1, nb - nchar(add))
      }

      if(nchar(add) > 0){
        res[qui] = paste0(res[qui], add)
      }

    } else if(op == "K"){

      if(nb == 0){
        res = character(0)
      } else {
        res = if(nb < length(x)) x[1:nb] else x
        
        if(group_flag == 2){
          stop_hook("The operation `{q?argument}{op}` cannot be applied in ",
                    "conditional statements (inside `~()`).",
                    "\nFIX: simply put this operation after or before the conditional statement.")
        }

        if(any(grepl(":(?:n|N|rest|REST):", add))){
          n = length(x)
          N = ""
          if(grepl(":N:", add, fixed = TRUE)){
            N = n_letter(n)
            add = gsub(":N:", N, add, fixed = TRUE)
          }

          add = gsub(":n:", n, add, fixed = TRUE)
          n_rest = n - nb + is_included
          if(n_rest > 0){
            add = gsub(":rest:", n_rest, add, fixed = TRUE)
            add = gsub(":REST:", n_letter(n_rest), add, fixed = TRUE)
          }
        }

        if(length(x) > nb){
          if(is_included){
            res = res[-nb]
          }

          if(nchar(add) > 0){
            res = c(res, add)
          }
        }
      }
    }
  } else if(op == "enum"){
    # NOTE:
    # for conditional operations, the code is slow.
    # but I don't see a use case for large vectors....
    # we use enum for smallish things so it should be OK.

    if(!is.null(group_index) && group_flag == 2){

      res = unname(tapply(x, group_index, enum_main, options = options))
      group_index = seq_along(group_index)

    } else {
      res = enum_main(x, options = options)

      if(group_flag == 1){
        group_index = NULL
      }

    }

  } else if(op %in% c("first", "last", "cfirst", "clast")){
    # first, last, cfirst, clast ####

    nb = argument
    
    nb_extra = NULL
    if(op == "first" && grepl("|", argument, fixed = TRUE)){
      arg_split = strsplit(argument, "|", fixed = TRUE)[[1]]
      nb = arg_split[1]
      nb_extra = arg_split[2]
      
      if(!is.null(nb_extra) && !is_numeric_in_char(nb_extra)){
        msg = paste0("In `smagick`: in the operator `", op, "` the argument can be of the form ",
                     "'n1|n2' with `n1` and `n2` numbers. ",
                     "\nPROBLEM: the second element `", nb_extra, "` is not numeric.")
        .stop_hook(msg)
      }
      
      nb_extra = as.numeric(nb_extra)
    }
    
    if(!is_numeric_in_char(nb)){
      msg = paste0("In `smagick`: the operator `", op, "` must have a numeric argument. ",
                   "\nPROBLEM: `", argument, "` is not numeric.")
      .stop_hook(msg)
    }    

    # the numeric argument can also be passed in options
    qui_num = which(grepl("^\\d+$", options))
    if(length(qui_num)){
      nb = options[qui_num[1]]
    }

    nb = as.numeric(nb)

    if(str_x(op, 1) == "c"){
      # we select the first/last characters
      
      if(!is.character(x)){
        x = as.character(x)
      }
      
      if(nb < 0){
        nb = abs(nb)
        nx = nchar(x)
        if(op == "cfirst"){
          res = substr(x, nb + 1, nx)
        } else {
          res = substr(x, 1, nx - nb)
        }
      } else {
        if(op == "cfirst"){
          res = substr(x, 1, nb)
        } else {
          nx = nchar(x)
          res = substr(x, nx - nb + 1, nx)
        }
      }
      
    } else {
      # we select the first/last elements

      if(!is.null(group_index) && group_flag == 2){
        is_last = op == "last"
        qui = cpp_find_first_index(group_index, nb, is_last)
        
        res = x[qui]
        group_index = group_index[qui]
        
        if(!is.null(nb_extra)){
          qui = cpp_find_first_index(group_index, nb, TRUE)
          res = c(res, x[qui])
          group_index = c(group_index, group_index[qui])
          
          my_order = order(group_index)
          res = res[my_order]
          group_index = group_index[my_order]
        }
        
      } else {
        if(op == "first"){
          
          if(nb < 0){
            
            if(!is.null(nb_extra)){
              msg = paste0("In `smagick`: in the operator `", op, "` the argument can be of the form ",
                          "'n1|n2' with `n1` and `n2` positive numbers. ",
                          "\nPROBLEM: the first element `", nb, "` is negative.")
              .stop_hook(msg)
            }
            
            nb = abs(nb)
            if(nb > length(x)){
              res = character(0)
            } else {
              res = tail(x, length(x) - nb)
            }            
          } else {
            res = head(x, nb)
            
            if(!is.null(nb_extra)){
              if(nb_extra < 0){
                msg = paste0("In `smagick`: in the operator `", op, "` the argument can be of the form ",
                            "'n1|n2' with `n1` and `n2` positive numbers. ",
                            "\nPROBLEM: the second element `", nb_extra, "` is negative.")
                .stop_hook(msg)
              }
              
              res = c(res, tail(x, nb_extra))
            }
            
          }    
        } else {
          if(nb < 0){
            nb = abs(nb)
            if(nb > length(x)){
              res = character(0)
            } else {
              res = head(x, length(x) - nb)
            }            
          } else {
            res = tail(x, nb)
          }
        }

        if(group_flag == 1){
          # this operation destroys the groups
          group_index = NULL
        }
      }
    }

  } else if(op == "rev"){
    # rev, sort, dsort ####
    
    if(!is.null(group_index) && group_flag == 2){
      qui = cpp_group_rev_index(group_index)
      res = x[qui]
    } else {
      res = rev(x)
    }

  } else if(op %in% c("sort", "dsort")){
    
    is_decreasing = op == "dsort"
    x2sort = x
    is_modified = FALSE
    if(nchar(argument) > 0){
      is_modified = TRUE
      x2sort = str_clean(x, argument)
    }
    
    options = check_set_options(options, "num")
    if("num" %in% options){
      is_modified = TRUE
      x2sort = suppressWarnings(as.numeric(x2sort))
    }
    
    if(group_flag == 1){
      # it makes no sense to keep track of the indexes when sorting the full string
      # ex: a1 b2 => split => a 1 b 2 => sort => a b 1 2
      # after that, the conditional operations (on a1 and b2) don't make much sense
      group_index = NULL
      group_flag = 0
    }

    if(!is.null(group_index) && group_flag == 2){
      
      if(is_decreasing){
        new_order = order(-group_index, x2sort, decreasing = TRUE)
      } else {
        new_order = order(group_index, x2sort)
      }

      res = x[new_order]
      group_index = cpp_recreate_index(group_index[new_order])
    } else {
      if(is_modified){
        new_order = order(x2sort, decreasing = is_decreasing)
        res = x[new_order]
      } else {
        res = sort(x, decreasing = is_decreasing)
      }
      
    }

  } else if(op %in% c("paste", "insert")){
    # paste, insert ####
    # paste: at the beginning/end of all strings
    # insert: element at the beginning/end of the vector 
    
    valid_options = c("both", "right")
    if(op == "paste"){
      valid_options = c(valid_options, "front", "back", "delete")
    }
    
    options = check_set_options(options, valid_options)

    if("delete" %in% options){
      x = character(length(x))
    }

    left = right = ""
    if("both" %in% options){
      left = right = argument
    } else if("right" %in% options){
      right = argument
    } else {
      left = argument
    }

    if(nchar(argument) == 0){
      res = x

    } else if(op == "paste"){

      if(length(x) == 0){
        res = x

      } else if(any(options %in% c("front", "back"))) {
        res = x
        if("front" %in% options){
          res[[1]] = paste0(left, x[1], right)
        }
        
        if("back" %in% options){
          n = length(x)
          res[[n]] = paste0(left, x[n], right)
        }        
        
      } else {

        # We replace the special values
        # :1:, :i:, :a:
        # we allow only one special value
        n_x = length(x)

        pat = c(":1:", ":01:", ":i:", ":a:", ":I:", ":A:")

        for(i in 1:2){
          tmp = if(i == 1) left else right
          any_done = FALSE

          for(p in pat){
            if(grepl(p, tmp, fixed = TRUE)){
              any_done = TRUE
              tmp_split = strsplit(tmp, p, perl = TRUE)[[1]]
              if(length(tmp_split) > 1){
                txt = switch(p,
                             ":1:" = 1:n_x,
                             ":01:" = str_fill(1:n_x, right = TRUE, symbol = "0"),
                             ":i:" = tolower(as.roman(1:n_x)),
                             ":I:" = as.character(as.roman(1:n_x)),
                             ":a:" = enum_letter(n_x),
                             ":A:" = toupper(enum_letter(n_x)))

                if(length(tmp_split) == 1){
                  tmp = paste0(tmp_split[[1]], txt)
                } else  {
                  tmp_new = paste0(tmp_split[[1]], txt, tmp_split[[2]])
                  k = 2
                  while(k + 1 <= length(tmp_split)){
                    k = k + 1
                    tmp_new = paste0(tmp_new, txt, tmp_split[[k]])
                  }
                  tmp = tmp_new
                }
              }

              break
            }
          }

          if(any_done){
            if(i == 1){
              left = tmp
            } else {
              right = tmp
            }
          }
        }

        if(any(nchar(left) > 0)){
          if(any(nchar(right) > 0)){
            res = paste0(left, x, right)
          } else {
            res = paste0(left, x)
          }
        } else {
          res = paste0(x, right)
        }
      }
    } else if(op == "insert"){
      # inserts an ELEMENT at the beginning/end
      
      if(group_flag == 2){
        stop_hook("The operation `{q?argument}{op}` cannot be applied in ",
                  "conditional statements (inside `~()`).",
                  "\nFIX: simply put this operation after or before the conditional statement.")
      }

      if(nchar(left) > 0){
        if(nchar(right) > 0){
          res = c(left, x, right)
        } else {
          res = c(left, x)
        }
      } else {
        res = c(x, right)
      }

    } else if(op %in% "Append"){
      # THIS IS DEPRECATED:
      # I give it just in case
      # appends **implicitly** at the beginning of the first/last string

      res = x

      if(any(grepl(":(?:n|N):", c(left, right)))){
        n = length(x)
        n_letter = ""
        if(any(grepl(":N:", c(left, right), fixed = TRUE))){
          n_letter = n_letter(n)
          left = gsub(":N:", n_letter, left)
          right = gsub(":N:", n_letter, right)
        }

        left = gsub(":n:", n, left)
        right = gsub(":n:", n, right)
      }

      if(is.null(extra)){
        extra = list()
      }

      if(nchar(left) > 0){
        extra$str_add_first = paste0(extra$str_add_first, left)
      }

      if(nchar(right) > 0){
        extra$str_add_last = paste0(extra$str_add_last, right)
      }

    }

    # END: paste/insert

  } else if(op == "fill"){
    # fill ####
    
    valid_options = c("right", "center")
    options = check_set_options(options, valid_options, op = op)
    
    info = extract_pipe(argument, op, double = FALSE, numeric = TRUE, mbt = FALSE)
    argument = info$value
    symbol = info$extra
    
    center = "center" %in% options
    right = "right" %in% options
    
    if(symbol == ""){
      symbol = " "
    }
    
    if(nchar(symbol) != 1){
      stop_hook("In the operator `fill`, the symbol used to fill must be of length 1.",
                "\nPROBLEM: the symbol, equal to {bq?symbol}, is of length {n?nchar(symbol)}.",
                "\nEXAMPLE: to fill with 0s: `fill.0`; of length 10 with underscores and right-aligned text: `'10|_'fill.right`.")
    }
    
    if(argument == ""){
      argument = NULL
    }
    
    if(!is.null(argument)){
      if(is_numeric_in_char(argument)){
        argument = as.numeric(argument)
      } else {
        stop_hook("In the operator `fill`, the argument (giving the length of the fill) must be numeric.",
                "\nPROBLEM: the argument, equal to {bq?argument}, is not numeric ",
                "(instead it is of class {enum ? class(argument)}.",
                "\nEXAMPLE: a fill of size 10: `10 fill`; of size 10 (using quotes) on the right: `'10'fill.right`.")
      }
    }
    
    res = str_fill(x, argument, symbol = symbol, right = right, center = center)
    
  } else if(op == "unik"){
    # unik ####

    if(!is.null(group_index) && group_flag == 2){

      # not really fast 
      # => to be improved
      df = data.frame(g = group_index, x = x, stringsAsFactors = FALSE)
      df_unik = unique(df)

      group_index = df_unik$g
      res = df_unik$x

    } else {
      
      res = unique(x)

      if(group_flag == 1){
        group_index = NULL
      }

    }

  } else if(op %in% c("nth", "Nth", "ntimes", "Ntimes", "n", "N", "len", "Len")){
    #
    # nth, ntimes, n, len ####
    #
    
    # parsing the default values of the options and if we display in letters
    is_letter = substr(op, 1, 1) %in% c("N", "L")
    if(is_letter){
      op = tolower(op)
    }
    
    if(op == "nth"){
      opt_default = c("letter", "upper", "compact")
    } else if(op == "n"){
      opt_default = c("letter", "upper", "0", "zero", "roman", "Roman")
    } else if(op == "len"){
      opt_default = c("letter", "upper", "format")
    } else {
      opt_default = c("letter", "upper")
    }

    options = check_set_options(options, opt_default)
    
    if(!is_letter){
      is_letter = any(c("letter", "upper") %in% options)
    }

    # operations
    if(op == "nth"){
      res = n_th(x, letters = is_letter, compact = "compact" %in% options)

    } else if(op == "ntimes"){
      res = n_times(x, letters = is_letter)

    } else if(op == "n"){
      is_zero = any(options %in% c("0", "zero"))
      # we force the conversion to numeric
      mark = if(is_zero) "" else ","
      if(is.numeric(x)){
        if(any(options %in% c("roman", "Roman"))){
          res = as.roman(x)
          if("roman" %in% options){
            res = tolower(res)
          }
        } else if(is_letter){
          res = n_letter(x)
        } else {
          res = format(x, big.mark = mark)
          
          if(is_zero){
            res = gsub(" ", "0", res, fixed = TRUE)
          } else {            
            res = cpp_trimws_in_place(res)
          }
        }
      } else {
        x_num = suppressWarnings(as.numeric(x))
        is_x_num = which(!is.na(x_num))
        num_val = x_num[is_x_num]

        if(any(options %in% c("roman", "Roman"))){
          res_num = as.roman(num_val)
          if("roman" %in% options){
            res_num = tolower(res_num)
          }
        } else if(is_letter){
          res_num = n_letter(num_val)
        } else {
          res_num = format(num_val, big.mark = mark)
          if(is_zero){
            res_num = gsub(" ", "0", res_num, fixed = TRUE)
          } else {            
            res_num = cpp_trimws_in_place(res_num)
          }
        }

        res = x
        res[is_x_num] = res_num
      }
      
    } else if(op == "len"){

      # if conditional: lengths of all the groups
      if(!is.null(group_index) && group_flag == 2){
        res = tabulate(group_index)
        group_index = seq_along(res)
      } else {
        if(group_flag == 1){
          group_index = 1
        }

        res = length(x)
      }

      if(is_letter){
        res = n_letter(res)
      } else if(opt_equal(options, "format")){
        res = format(res, big.mark = ",")
      }
    }

    if("upper" %in% options){
      res = gsub("^(.)", "\\U\\1", res, perl = TRUE)
    }
  } else if(op == "width"){
    # width, dtime ####
    
    sw = getOption("width")
    data = list(.sw = sw)

    info = extract_pipe(argument, op, double = FALSE, numeric = TRUE, data = data, mbt = FALSE)
    nb = info$value
    comment = info$extra
    
    if(nb == ""){
      nb = NULL
    }

    if(str_x(comment, -1) == "_"){
      # the underscore means that we don't add a space
      comment = str_trim(comment, -1)
    } else if(comment != ""){
      comment = paste0(comment, " ")
    }

    # code is slow but I don't see why would one want it to be super fast
    # (only for user-content, not performance/data-analysis content)

    res = character(length(x))
    for(i in seq_along(x)){
      res[i] = fit_screen(x[i], width = nb, leader = comment)
    }
    
  } else if(op == "dtime"){
    res = format_difftime(x, options)

  } else if(op == "erase"){
    # erase, rm, nuke, num ####
    #

    options = check_set_options(options, c("fixed", "ignore", "word"))

    if(argument == ""){
      res = character(length(x))
    } else {
      is_fixed = "fixed" %in% options
      is_word = "word" %in% options
      is_ignore = "ignore" %in% options

      qui = str_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, word = is_word)

      res[qui] = ""
    }

  } else if(op %in% c("rm", "nuke")){
    options = check_set_options(options, c("empty", "blank", "noalpha", "noalnum", "all", 
                                           "fixed", "ignore", "word"))
                                           
    if(op == "nuke"){
      res = character(0)
      return(res)
    }
    
    res = NULL
    if(argument != ""){
      is_fixed = "fixed" %in% options
      is_word = "word" %in% options
      is_ignore = "ignore" %in% options

      qui = !str_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, word = is_word)
      
      options = setdiff(options, c("fixed", "ignore", "word"))
      if("blank" %in% options){
        # 10 times faster than grepl("\\S", x, perl = TRUE)
        qui_blank = cpp_which_empty(x)
        if(length(qui_blank) > 0){
          qui[qui_blank] = FALSE
        } 
        
      } else if("empty" %in% options){
        qui = qui & x != ""
      }

    } else {
      options = setdiff(options, c("fixed", "ignore", "word"))
      if(length(options) == 0){
        opt = "empty"
      } else {
        opt = options[1]
      }

      # beware I define here the ones we should keep, **not* the ones we should drop

      if(opt == "empty"){
        qui = x != ""
      } else if(opt == "blank"){
        # 10 times faster than grepl("\\S", x, perl = TRUE)
        qui = cpp_which_empty(x)
        if(length(qui) == 0){
          qui = seq_along(x)
        } else {
          qui = -qui
        }
      } else if(opt == "noalpha"){
        qui = grepl("[\\p{L}]", x, perl = TRUE)
      } else if(opt == "noalnum"){
        qui = grepl("[\\p{L}[:digit:]]", x, perl = TRUE)
      } else if(opt == "all"){
        qui = integer(0) 
      }
    }

    if(is.logical(qui)){
      qui = which(qui)
    }

    res = x[qui]
    if(!is.null(group_index) && group_flag != 0){
      group_index = group_index[qui]
      group_index = cpp_recreate_index(group_index)
    }

  } else if(op == "num"){
    options = check_set_options(options, c("warn", "soft", "rm", "clear"))

    # warn: warning if failed conversions
    is_warn = "warn" %in% options
    # soft: if some numbers couldn't be parsed to numeric: no conversion is done
    is_soft = "soft" %in% options
    # rm: the on numeric are removed
    is_rm = "rm" %in% options
    # clear: the non numeric are turned into empty strings
    is_clear = "clear" %in% options

    is_valid_num = TRUE
    if(is_warn || is_soft){
      is_valid_num = is_numeric_in_char(x)
      if(!is_valid_num && is_warn){
        msg = if(!is_soft) "" else " No conversion is performed."
        warning("In operation `num`: when trying to convert to numeric, NAs were created.", 
                 msg, call. = FALSE)
      }
    }

    if(is_valid_num || !is_soft){
      res = suppressWarnings(as.numeric(x))
      if(is_rm || is_clear){
        if(anyNA(res)){
          qui_na = which(is.na(res))
          if(is_rm){
            res = res[-qui_na]
            if(!is.null(group_index) && group_flag != 0){
              group_index = group_index[-qui_na]
              group_index = cpp_recreate_index(group_index)
            }
          } else { 
            # is_clear
            res[qui_na] = ""
          }
        }
      }
    } else {
      res = x
    }

  } else if(op == "ascii"){
    # ascii, stop ####
    res = str_to_ascii(x, options)

  } else if(op == "stopwords"){

    # current limitation: does not work for quoted words
    #                     but quoted words are not stopwords usually
    # + the algo is quite inefficient

    # Snowball stopwords
    # These come from http://snowballstem.org/algorithms/english/stop.txt
    stopwords = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves",
                  "you", "your", "yours", "yourself", "yourselves", "he", "him",
                  "his", "himself", "she", "her", "hers", "herself", "it", "its",
                  "itself", "they", "them", "their", "theirs", "themselves", "what",
                  "which", "who", "whom", "this", "that", "these", "those", "am", "is",
                  "are", "was", "were", "be", "been", "being", "have", "has", "had",
                  "having", "do", "does", "did", "doing", "would", "should",
                  "could", "ought", "i'm", "you're", "he's", "she's", "it's",
                  "we're", "they're", "i've", "you've", "we've", "they've",
                  "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll",
                  "you'll", "he'll", "she'll", "we'll", "they'll", "isn't",
                  "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't",
                  "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't",
                  "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's",
                  "that's", "who's", "what's", "here's", "there's", "when's", "where's",
                  "why's", "how's", "a", "an", "the", "and", "but", "if", "or",
                  "because", "as", "until", "while", "of", "at", "by", "for",
                  "with", "about", "against", "between", "into", "through",
                  "during", "before", "after", "above", "below", "to", "from",
                  "up", "down", "in", "out", "on", "off", "over", "under",
                  "again", "further", "then", "once", "here", "there", "when",
                  "where", "why", "how", "all", "any", "both", "each", "few",
                  "more", "most", "other", "some", "such", "no", "nor", "not",
                  "only", "own", "same", "so", "than", "too", "very")

    n = length(x)
    x_split = strsplit(x, "(?<=[[:alnum:]])(?=[^[:alnum:]'])|(?<=[^[:alnum:]'])(?=[[:alnum:]])", perl = TRUE)
    x_len = lengths(x_split)
    x_vec = unlist(x_split)

    id = rep(1:n, x_len)

    # Lowering is costly, checking costs a bit less
    if(any(grepl("[[:upper:]]", x))){
      qui_drop = which(tolower(x_vec) %in% stopwords)
    } else {
      qui_drop = which(x_vec %in% stopwords)
    }

    x_vec = x_vec[-qui_drop]
    id = id[-qui_drop]
    
    res = cpp_paste_conditional(x_vec, id)

  } else if(op %in% c("if", "vif")){
    #
    # Conditions: if, vif ####
    #
    
    info = strsplit(argument, "_;;;_", fixed = TRUE)[[1]]
    
    cond_raw = info[[1]]
    true_section = info[[2]]
    is_false_section = length(info) == 3
    if(is_false_section){
      false_section = info[[3]]  
    } else {
      false_section = NULL
    }

    # computing the condition
    # a) parsing
    if(.check){
      cond_parsed = check_expr(str2lang(cond_raw), 
            .sma("The string operation {bq?op} should be of the form ",
                 "{op}(condition ; true ; false). ",
                 "\nPROBLEM: the condition {bq?cond_raw} could not be parsed, see error below:"), 
            verbatim = TRUE)
    } else {
      cond_parsed = str2lang(cond_raw)
    }
    
    # b) evaluating
    if(is.character(cond_parsed)){
      # REGEX
      
      if(.check){
        cond = check_expr(str_is(x, cond_parsed), 
                  .sma("The operation is of the form {bq?op}(cond ; true ; false). ",
                  "When `cond` is a pure character string, the function `str_is()` ",
                  "is applied with `cond` being the searched pattern.",
                  "\nPROBLEM: in {op}({'_;;;_ => ;'R ? argument}), the condition ",
                  "{bq?cond_raw} led to an error, see below:"),
                  verbatim = TRUE)
      } else {
        cond = str_is(x, cond_parsed)
      }      
      
    } else {
      vars = all.vars(cond_parsed)
      my_data = list()
      for(v in vars){
        if(v %in% c(".N", ".len")){
          my_data[[v]] = length(x)
        } else if(v %in% c(".nchar", ".C")){
          if(!is.character(x)){
            x = as.character(x)
          }
          my_data[[v]] = nchar(x)
        } else if(v == "."){
          my_data[[v]] = x
        }
      }
      
      if(.check){
        cond = check_expr(eval(cond_parsed, my_data, .envir), 
                  .sma("The string operation {bq?op} should be of the form ",
                       "{op}(condition ; true ; false). ",
                       "\nPROBLEM: the condition {bq?cond_raw} could not be ",
                       "evaluated, see error below:"), 
                   verbatim = TRUE)  
      } else {
        cond = eval(cond_parsed, my_data, .envir)
      }
    }
    
    # further error checking
    if(!length(cond) %in% c(1, length(x))){
      stop_hook("In {bq?op} operations, the condition (here {bq?cond_raw}) must ",
                "be either of length 1 (applying to the full string), ",
                "either of the length of the interpolated value.", 
                "\nPROBLEM: the condition is of length {len.f?cond} while the ",
                "interpolated value is of length {len?x}.")
    }
    
    if(is.numeric(cond)){
      # numeric values are converted to logical
      cond = as.logical(cond)
    }
    
    if(!is.logical(cond)){
      stop_hook("In {bq?op} operations, the condition (here {bq?cond_raw}) must be logical.",
                "\nPROBLEM: the condition is not logical, instead it is of class ",
                "{enum.bq ? class(cond)}.")
    }
    
    # NA = FALSE
    if(anyNA(cond)){
      cond[is.na(cond)] = FALSE
    }
    
    #
    # Computing the operations
    #    
    
    is_elementwise = length(cond) > 1
    
    if(is_elementwise){
      x_true = x[cond]
      x_false = x[!cond]
    } else {
      if(cond){
        x_true = x
        x_false = character(0)
      } else {
        x_true = character(0)
        x_false = x
      }
    }
    
    # All operations that change the length of the element should be forbidden
    # we check that on the fly

    for(state in c("true", "false")){

      if(state == "true"){
        xi = x_true
      } else {
        xi = x_false
      }
      
      if(length(xi) == 0){
        next
      }
      
      if(state == "false" && !is_false_section){
        break
      }
      
      instruction = if(state == "true") true_section else false_section
      
      if(op == "if"){
        n_old = length(xi)
        cond_flag = +grepl("~(", instruction, fixed = TRUE)
        xi = apply_simple_operations(xi, op, instruction, .check, .envir, 
                                     group_flag = cond_flag, .delim)
        
        if(is_elementwise && !length(xi) %in% c(0, n_old)){
          stop_hook("In {bq?op} operations, when conditions are of length > 1, the ",
                   "operations either a) can remove the string completely ",
                   "(e.g. with `nuke`), or b) must not change the length of the element.",
                   "\nPROBLEM: the original vector is of length {n?n_old} while after ",
                   "the operations it becomes of length {len.f ? xi}")
        }
      } else {
        # verbatim if
        if(grepl(".", instruction, fixed = TRUE)){
          .data[["."]] = xi
        }
        
        xi = smagick_internal(instruction, .delim = .delim, .envir = .envir, 
                              .data = .data, .check = .check)
      }
      
      if(state == "true"){
        x_true = xi
      } else {
        x_false = xi
      }
    }
    
    if(is_elementwise){
      
      if(length(x_true) == 0 && length(x_false) == 0){
        res = character(0)
        if(group_flag != 0){
          group_index = NULL
        }
      } else if(length(x_true) == 0){
        if(op == "vif" && length(x_false) == 1){
          res = x
          res[!cond] = x_false
        } else {
          res = x_false  
        }
        
        if(group_flag != 0){
          group_index = group_index[!cond]
        }
      } else if(length(x_false) == 0){
        if(op == "vif" && length(x_true) == 1){
          res = x
          res[cond] = x_true
        } else {
          res = x_true
        }
        
        if(group_flag != 0){
          group_index = group_index[cond]
        }
      } else {
        res = x
        res[cond] = x_true
        res[!cond] = x_false
      }
    } else {
      
      res = if(cond) x_true else x_false

      # we can add extra stuff, if needed

      extra_if = attr(res, "extra")
      if(length(extra_if) > 0){
        if(is.null(extra)){
          extra = list()
        }

        if(length(extra_if$element_add_first) > 0){
          extra$element_add_first = c(extra$element_add_first, extra_if$element_add_first)
        }

        if(length(extra_if$element_add_last) > 0){
          extra$element_add_last = c(extra$element_add_last, extra_if$element_add_last)
        }

        if(length(extra_if$str_add_first) > 0){
          extra$str_add_first = paste0(extra$str_add_first, extra_if$str_add_first)
        }

        if(length(extra_if$str_add_last) > 0){
          extra$str_add_last = paste0(extra$str_add_last, extra_if$str_add_last)
        }
      }
    }

  } else if(op == "~"){
    # Conditional: ~ ####
    res = apply_simple_operations(x, op, argument, .check, .envir, 
                                  group_flag = 2, .delim)
                                  
    group_index = attr(res, "group_index")

  } else {
    # user-defined operations ####
    
    user_ops = getOption("smagick_user_ops")
    if(op %in% names(user_ops)){
      fun = user_ops[[op]]
      valid_options = attr(fun, "valid_options")

      if(!is.null(valid_options)){
        options = check_set_options(options, valid_options, free = any(valid_options == ""))
      }

      tmp = fun(x = x, argument = argument, options = options, group = group_index, 
                group_flag = group_flag)

      if(is.list(tmp)){
        if(!all(c("x", "group") %in% names(tmp))){
          pblm  = setdiff(c("x", "group") %in% names(tmp))
          stop_hook("The user-defined operation {bq?alias} is not well formed. The function should",
                    " return either a vector, either a list of exactly two elements 'x' and 'group'.",
                    "\nPROBLEM: it returns a list but the item{$s, are ? pblm} missing.")
        }
        res = tmp$x
        group_index = tmp$group
      } else {
        res = tmp
      }

    } else {
      msg = paste0("In `smagick`: the operator `", op, "` is not recognized. ",
                  "Internal error: this problem should have been spotted beforehand.")
      .stop_hook(msg)
    }
  }

  if(length(extra) > 0){
    attr(res, "extra") = extra
  }

  if(length(group_index) > 0){
    attr(res, "group_index") = group_index
  }

  return(res)
}



sma_pluralize = function(operators, xi, .delim, .envir, .data, .check){

  plural_len = operators[1] == "$"

  if(!plural_len){
    # xi should be a number
    is_pblm = length(xi) != 1 || !is.atomic(xi) || any(is.na(xi))

    if(!is_pblm && !is.numeric(xi)){
      xi = suppressWarnings(as.numeric(xi))
      is_pblm = any(is.na(xi))
    }

    if(!is_pblm){
      is_pblm = xi < 0
    }

    if(is_pblm){
      example = 'Example: x = 5; smagick("There {#is, N ? x} cat{#s} in the room.")'

      extra = ""
      reason = NULL
      if(length(xi) != 1){
        extra = " single"
        reason = "PROBLEM: it is not of length 1."
      } else if(!is.atomic(xi)){
        reason = paste0("PROBLEM: it is not atomic (it is of class", class(xi)[1], ").")
      } else if(is.na(xi)){
        reason = "PROBLEM: It is NA."
      } else if(xi < 0){
        extra = " positive"
        reason = "PROBLEM: It is negative."
      }

      .stop_hook("The pluralization operator `#` applies to a", extra, " number. ", reason, ".\n",
              example)
    }
  }

  n = if(plural_len) length(xi) else xi
  IS_ZERO = n == 0
  IS_PLURAL = n > 1

  operators = operators[-1]
  n_op = length(operators)
  res = rep(NA_character_, n_op)
  for(i in 1:n_op){
    op_parsed = cpp_parse_operator(operators[i])

    op = op_parsed$operator
    options = op_parsed$options
    argument = op_parsed$argument

    if(op %in% c("es", "s", "y", "ies")){
      zero_case = opt_equal(options, c("zero", "0"))

      is_really_plural = IS_PLURAL || (zero_case && IS_ZERO)

      if(op %in% c("s", "es")){
        if(is_really_plural){
          res[i] = op
        } else {
          res[i] = ""
        }
      } else {
        if(is_really_plural){
          res[i] = "ies"
        } else {
          res[i] = "y"
        }
      }

    } else if(op %in% c("n", "N", "len", "Len")){

      is_letter = opt_equal(options, c("letter", "upper")) || substr(op, 1, 1) %in% c("N", "L")
      is_upper = opt_equal(options, "upper")

      if(opt_equal(options, "no") && IS_ZERO){
        if(is_upper){
          res[i] = "No"
        } else {
          res[i] = "no"
        }
      } else if(is_letter){
        val = n_letter(n)

        if(is_upper){
          val = gsub("^(.)", "\\U\\1", val, perl = TRUE)
        }

        res[i] = val
      } else {
        res[i] = format(n, big.mark = ",")
      }
      
    } else if(op %in% c("nth", "Nth")){
      is_compact = opt_equal(options, "compact") 
      is_letter = opt_equal(options, c("letter", "upper")) || substr(op, 1, 1) == "N"
      val = n_th(n, letters = is_letter, compact = is_compact)
      if(opt_equal(options, "upper")){
        val = gsub("^(.)", "\\U\\1", val, perl = TRUE)
      }
      res[i] = val

    } else if(op %in% c("ntimes", "Ntimes")){
      is_letter = opt_equal(options, "letter") || substr(op, 1, 1) == "N"
      res[i] = n_times(n, letters = is_letter)

    } else if(op %in% c("zero", "singular", "plural")){
      if((op == "zero" && IS_ZERO) || 
         (op == "singular" && !IS_PLURAL && !(any(grepl("zero$", operators)) && IS_ZERO)) || 
         (op == "plural" && IS_PLURAL)){
          
          value = smagick_internal(argument, .delim = .delim, .envir = .envir, .data = .data,
                                      .slash = FALSE, .check = .check,
                                      .plural_value = xi)
        if(value != ""){
          res[i] = value
        }        
      }
    } else if(op == "enum"){

      res[i] = enum_main(xi, options = options)

    } else {
      # The verb is always last

      if(nchar(op) < 2){
        example = 'Example: x = c("Charles", "Alice"); smagick("{$Is, enum ? x} crazy? Yes {$(he:they), are}.")'
        .stop_hook("In pluralization, `", op, "` is expected to be a verb ",
                   "and verbs must always be composed of at least two letters.\n", example)
      }

      zero_case = opt_equal(options, c("zero", "0"))
      is_really_plural = IS_PLURAL || (zero_case && IS_ZERO)

      res[i] = conjugate(op, is_really_plural)
    }

  }

  res = res[!is.na(res)]
  paste(res, collapse = " ")
}

sma_ifelse = function(operators, xi, xi_val, .envir, .data, .delim, .check){

  if(is.numeric(xi)){
    xi = xi != 0
  }
  
  amp = operators[1]
  is_double_amp = amp == "&&"
  if(!is.logical(xi)){
    form = "{&cond ; true ; false}"
    if(is_double_amp) form = "{&&cond ; true}"
    example = '\nEXAMPLE: x = Sys.time(); smagick("Hello {&format(x, \'%H\') < 20 ; Sun ; Moon}!")'

    .stop_hook("The if-else operator `", amp, "`, of the form ", form,
            ", accepts only logical values in the condition. ",
            "PROBLEM: the value is not logical, but of class `",
            class(xi)[1], "`.", example)
  }

  if(anyNA(xi)){
    form = "{&cond ; true ; false}"
    if(is_double_amp) form = "{&&cond ; true}"
    example = '\nEXAMPLE: x = Sys.time(); smagick("Hello {&format(x, \'%H\') < 20 ; Sun ; Moon}!")'

    .stop_hook("The if-else operator `", amp, "`, of the form ", form,
              ", accepts only non-NA logical values.\n",
              "PROBLEM: the condition contains NA values.", example)
  }

  BOX_OPEN = .delim[1]
  true = operators[3]
  false = operators[4]
  
  if(is_double_amp && !is.na(false)){
    .stop_hook("The if-else operator `&&`, of the form ", 
               "{&&cond ; true} does not accept a 'false' statement (since it will ",
               "be filled with the value of the variable in the condition). ",
              "\nFIX: remove the false statement or use the if-else operator `&` (single ampersand).",
              "\nEXAMPLE: x = 1:5",
              "           compare smagick(\"{x} = {&x %% 2;odd;even}\")",
              "\n              to smagick(\"{x} = {&x %% 2;odd}\")",
              "\n              to smagick(\"{x} = {&&x %% 2;odd}\")")
  }
  
  if(is.na(false)) false = ""
  
  true = cpp_extract_quote_from_op(true)
  false = cpp_extract_quote_from_op(false)
  
  new_values_done = FALSE
  n_x = length(xi)
  is_vector = n_x > 1
  
  if(is_vector){
    # if vector: true/false can be vectors
    for(i in 1:2){
      log_op = if(i == 1) true else false
      if(grepl(BOX_OPEN, log_op, fixed = TRUE)){
        if(!new_values_done){
          new_values_done = TRUE
          .data[["."]] = xi_val
          .data[[".N"]] = length(xi_val)
          .data[[".len"]] = length(xi_val)
        }
        
        log_op_eval = smagick_internal(log_op, .delim = .delim, .envir = .envir, .data = .data,
                              .slash = FALSE, .check = .check,  
                              .plural_value = xi_val)
        
        if(!length(log_op) %in% c(1, n_x)){
          form = "{&cond ; true ; false}"
          if(is_double_amp) form = "{&&cond ; true}"
          .stop_hook("The if-else operator of the form {form} accepts values in ",
                      "{&i==1;true;false} which can be interpolated, as is the case here ",
                      "for {bq?log_op}.",
                      "\nThe length of the interpolated value must be equal to 1 or the length ",
                      "of the condition.",
                      "PROBLEM: the length of the condition is {n?n_x} while the length of ",
                      "`{&i==1;true;false}` is {len.format?log_op_eval}.")
        }
        
        if(i == 1){
          true = log_op_eval
        } else {
          false = log_op_eval
        }        
      }
    }
  }

  if(operators[1] == "&"){
    # we replace TRUE and FALSE with the strings, even empty
    
    true_long = length(true) > 1
    false_long = length(false) > 1
    
    if(true_long || false_long){
       if(true_long){
        res = true
        if(false_long){
          res[xi] = false[xi]
        } else {
          res[xi] = false
        }
       } else {
        res = false
        res[xi] = true
       }
       
       if(anyNA(xi)){
        res[is.na(xi)] = NA
       }
    } else {
      res = c(false, true)[xi + 1]
    }
    
  } else {
    # we change only the ones that need to be changed
    res = xi_val
    
    if(length(true) > 1){
      res[xi] = true[xi]
    } else {
      res[xi] = true
    }
  }

  # we allow nestedness only for single values
  if(length(res) == 1 && grepl(BOX_OPEN, res, fixed = TRUE)){
    if(!new_values_done){
        new_values_done = TRUE
        .data[["."]] = xi_val
        .data[[".N"]] = length(xi_val)
        .data[[".len"]] = length(xi_val)
      }
    res = smagick_internal(res, .delim = .delim, .envir = .envir, .data = .data,
                           .slash = FALSE, .check = .check,  
                           .plural_value = xi_val)
  }

  res
}


apply_simple_operations = function(x, op, operations_string, .check = FALSE, .envir = NULL, 
                                 group_flag = 0, .delim = c("{", "}")){
                                  
  op_all = cpp_parse_simple_operations(operations_string, .delim)
    
  if(length(op_all) == 0){
    return(x)
  }
  
  if(identical(op_all[1], "_ERROR_")){
    op_msg = if(op =='~') "~(op1, op2)" else "if(cond ; op1, op2 ; op3, op4)"
    last = gsub("_;;;_", ";", tail(op_all, 1))
    extra = ""
    if(last %in% c("!", "?")){
      extra = smagick("The character {bq?last} is forbidden in operations.")
    } else if(last != "_ERROR_"){
      extra = smagick("Operations must be of the form `'arg'operator.option` but the value {bq?last} is ill formed.")
    } else {
      extra = smagick("The value {bq?operations} could not be parsed.")
    }
      
    msg = smagick("The operator {bq?op} expects a suite of valid operations (format: {bq?op_msg}). ",
               "\nPROBLEM: the operations were not formatted correctly. ", extra)
    
    .stop_hook(msg)
  }
  
  #
  # applying the operations
  #
  
  xi = x

  for(i in seq_along(op_all)){
    opi = op_all[i]

    op_parsed = sma_char2operator(opi)

    if(op_parsed$eval){
      if(.check){
        argument_call = check_expr(str2lang(op_parsed$argument), 
                                   .sma("In the operation `{op}()`, the ",
                                        "{&length(op_all) == 1 ; operation ; chain of operations} ",
                                        " {bq?operations_string} led to a problem.", 
                                        "In operation {bq?opi}, the argument in backticks ", 
                                        "(equal to {bq?op_parsed$argument}) is evaluated from the calling environment.",
                                        "\nPROBLEM: the argument could not be parsed."),
                                    verbatim = TRUE)

        argument = check_expr(eval(argument_call, .envir),
                              "In the operation `{op}()`, the ",
                              "{&length(op_all) == 1 ; operation ; chain of operations} ",
                              " {bq?operations_string} led to a problem.", 
                              "In operation {bq?opi}, the argument in backticks ", 
                              "(equal to {bq?op_parsed$argument}) is evaluated from the calling environment.",
                              "\nPROBLEM: the argument could not be evaluated.")
      } else {
        argument = eval(str2lang(op_parsed$argument), .envir)
      }

    } else {
      argument = op_parsed$argument
    }

    if(.check){
      xi = check_expr(sma_operators(xi, op_parsed$operator, op_parsed$options, argument, 
                                      group_flag = group_flag, .delim = .delim),
                                     "In the operation `{op}()`, the ",
                                     "{&length(op_all) == 1 ; operation ; chain of operations} ",
                                     " {bq?operations_string} led to a problem.", 
                                     "\nPROBLEM: the operation {opi} failed. Look up the doc?")
    } else {
      xi = sma_operators(xi, op_parsed$operator, op_parsed$options, argument, 
                         group_flag = group_flag, .delim = .delim)
    }
  }

  res = xi

}

setup_operations = function(){
  OPERATORS = c("/", "s", "S", "split", "x", "X", "extract", 
                "c", "C", "collapse", "r", "R", "replace", "clean",
                "times", "each", "fill",
                "~", "if", "vif",
                "upper", "lower", "q", "Q", "bq", 
                "format", "Format", "%",
                "erase", "rm", "nuke", "paste", "insert", 
                "k", "K", "last", "first",
                "cfirst", "clast", "unik", "num", "enum",
                "rev", "sort", "dsort", "ascii", "title",
                "ws", "tws", "trim", "get", "is", "which",
                "n", "N", "len", "Len", "width", "dtime",
                "stopwords", "nth", "Nth", "ntimes", "Ntimes")
  options("smagick_operations" = sort(OPERATORS))
  options("smagick_operations_origin" = sort(OPERATORS))
}




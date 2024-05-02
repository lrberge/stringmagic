#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Jul 27 09:40:24 2022
# ~: string ops core functions
#----------------------------------------------#


####
#### User-level ####
####


#' Register custom operations to apply them in `string_magic`
#' 
#' Extends the capabilities of [string_magic()] by adding any custom operation
#' 
#' @param fun A function which must have at least the arguments 'x' and '...'. 
#' Additionnaly, it can have the arguments: 'argument', 'options', 'group', 'group_flag'.
#' This function must return a vector.
#' This function will be internally called by `string_magic` in the form 
#' `fun(x, argument, options, group, group_flag)`.`x`: the value to which the 
#' operation applies. `argument`: the quoted `string_magic` argument (always character). 
#' `options`: a character vector of `string_magic` options. The two last arguments are of use
#' only in group-wise operations if `fun` changes the lengths of vectors. `group`: an index of
#' the group to which belongs each observation (integer). `group_flag`: value between 0
#' and 2; 0: no grouping operation requested; 1: keep track of groups; 2: apply grouping.
#' @param ops Character scalar representing a valid chain of `string_magic` operations. It should
#' be of the form `"op1, 'arg'op2, etc"`. For example `"'80|-'fill"` fills the line
#' with dashes up to 80 characters. 
#' @param alias Character scalar, the name of the operation.
#' @param valid_options A character vector or NULL (default). Represents a list of 
#' valid options for the operation. This is used: a) to enable auto-completion,
#' b) for error-handling purposes.
#' @param namespace Character scalar or `NULL` (default). **Only useful for package developers.**
#' As a regular end-user you shouldn't care! If your package uses `string_magic`, you should care. 
#' If the function `string_magic_register_*` is located in the `onLoad` function (see `help("onLoad")`), 
#' there is nothing to do. Otherwise, pass the name of your package in this argument to 
#' make all the new operation definitions scoped (i.e. only your package can access it and 
#' it can't be messed up by end users).
#' 
#' 
#' @details 
#'  
#' We try to strongly check the new operations since it's always better to find out problems
#' sooner than later. This means that when the function is defined, it is also 
#' tested.
#' 
#' If you pass a function, note that it should work for non-character arguments in `x`.
#' 
#' @section Writing a package using `string_magic`:
#' 
#' If you want to use `string_magic` in your package and want to make use of custom operations:
#' 
#' - place any `string_magic_register_fun` and `string_magic_register_ops` in your `.onLoad` function
#' (see `help("onLoad")`). The .onLoad function is run whenever the package is loaded 
#' for the first time. It's a function that you can place anywhere in your `R/*` files 
#' and which looks like this:
#' ```{r}
#' .onLoad = function(libname, pkgname){
#'   # string_magic custom operations
#'   string_magic_register_ops("'80|-'fill", "h1")
#' 
#'   invisible()
#' }
#' ```
#' - if you don't want to place the `string_magic_register_*` functions in the .onLoad function, 
#' you can, but then you **must** provide the argument `namespace`:
#' ```{r}
#' string_magic_register_ops("'80|-'fill", "h1", namespace = "myPackageName")
#' ```
#' - you must create an [string_magic_alias()] to create an alias to [string_magic()] and use the 
#' argument `.namespace = "myPackageName"`. Use this opportunity to change the 
#' defaults if you wish. You can even override the `string_magic` function:
#'  ```{r}
#' # creating an alias with the same name + changing the delimiter
#' string_magic = stringmagic::string_magic_alias(.namespace = "myPackageName", .delim = "{{ }}")
#' ```
#' 
#' @author 
#' Laurent R. Berge
#' 
#' @return 
#' These function do not return anything. They register new operations to be used in the 
#' `string_magic` family of functions by placing them in the options (later fetched by 
#' `string_magic()` at run-time).
#' 
#' @family related to string_magic
#' 
#' @inherit string_clean seealso
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
#' string_magic_register_fun(fun_emph, "emph")
#' 
#' # C) use it
#' x = string_vec("right, now")
#' string_magic("Take heed, {emph, c? x}.")
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
#' string_magic_register_fun(fun_emph, "emph", "strong")
#' 
#' x = string_vec("right, now")
#' string_magic("Take heed, {emph.strong, c? x}.")
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
#' string_magic_register_fun(fun_emph, "emph", "strong")
#' 
#' x = string_vec("right, now")
#' string_magic("Take heed, {'_'emph.s, c? x}.")
#' 
#' #
#' # using string_magic_register_ops
#' #
#' 
#' # create a 'header' maker
#' string_magic_register_ops("tws, '# 'paste, ' 'paste.right, '40|-'fill", "h1")
#' cat_magic("{h1 ! My title}\n my content")
#' 
#' 
#' 
#' 
string_magic_register_fun = function(fun, alias, valid_options = NULL, namespace = NULL){
  # fun: must be a function with x and ... as arguments
  # the argument names must be in:
  # x, argument, options, group, conditonnal_flag

  if(missing(fun)){
    stop("The argument `fun` must be provided. PROBLEM: it is missing.")
  }

  if(!is.function(fun)){
    stopi("The argument `fun` must be a function. ",
          "PROBLEM: it is not a function, instead it is of class {enum.bq?class(fun)}.")
  }

  check_character(alias, scalar = TRUE, mbt = TRUE)
  check_character(valid_options, no_na = TRUE, null = TRUE)

  fun_args = names(formals(fun))
  
  arg_must = c("...", "x")
  arg_missing = setdiff(arg_must, fun_args)
  if(length(arg_missing) > 0){
    stopi("The argument `fun` must be a function with {enum.bq?arg_missing} in its arguments.",
              "\nPROBLEM: it has no argument {enum.bq.or?arg_missing}.")
  }

  valid_args = string_vec("x, argument, options, group, group_flag")
  arg_pblm = setdiff(setdiff(fun_args, "..."), valid_args)
  if(length(arg_pblm) > 0){
    stopi("The argument `fun` must have specific argument names. Valid arguments are {enum.bq.or?valid_args}.",
              "\nPROBLEM: the argument{$s, enum.bq, are?arg_pblm} invalid.")
  }
  
  # we only test for simple cases
  if(!"argument" %in% fun_args){
    options = head(valid_options, 1)
    run = try(fun(x = 1:5, options = options, group_flag = 0, group = rep(1, 5)), silent = TRUE)
    if(isError(run)){
      mc = match.call()
      fun_dp = if(length(mc$fun) == 1) deparse_short(fun) else ""
      stopi("The function defined in argument `fun` failed to run for a simple case.",
            "\n{&nchar(fun_dp)>0 ; {.}(x = 1:5) ; Running it on x = 1:5} leads to an error,",
            " see below:",
            "\n{run}")
    }  
  }
  
  
  #
  # adding attributes
  #
  
  if(!is.null(valid_options)){
    attr(fun, "valid_options") = valid_options
  }
  
  if("group_flag" %in% fun_args){
    attr(fun, "group_flag") = TRUE
  }
  
  #
  # saving within a namespace
  #
  
  if(get_function_above() == ".onLoad"){
    namespace = get_namespace_above()
  }  
  
  save_user_fun(fun, alias, namespace)
}

#' @describeIn string_magic_register_fun Create new combinations of `string_magic` operations
string_magic_register_ops = function(ops, alias, namespace = NULL){
  
  #
  # checking
  #
  
  check_character(ops, scalar = TRUE, mbt = TRUE)
  check_character(alias, scalar = TRUE, mbt = TRUE)
  
  run = try(string_magic("x{1:5}", .last = ops), silent = TRUE)
  
  if(isError(run)){
    stopi("The argument `ops` must refer to valid `string_magic` operations.", 
          "\nPROBLEM: running `string_magic(\"x\\{1:5}\", .last = {Q?ops})` leads ",
          "to an error, see below:",
          "\n{run}")
  }
  
  #
  # adding attributes
  #
  
  if(!is.null(ops)){
    attr(ops, "simple_ops") = TRUE
  }
  
  #
  # saving within a namespace
  #
  
  if(get_function_above() == ".onLoad"){
    namespace = get_namespace_above()
  } 
  
  save_user_fun(ops, alias, namespace)  
}

save_user_fun = function(fun, alias, namespace){
  # We save the user function in the global options
  
  if(is.null(namespace)){
    namespace = "R_GlobalEnv"
  }
  
  # We forbid the redefinition of internal operations
  # => only for V1, so that later versions don't break code
  #    if user did defined operations with the same name of the new ones
  OPERATORS_v1.0.0 = getOption("string_magic_operations_v1.0.0")
  if(alias %in% OPERATORS_v1.0.0){
    stopi("The argument `alias` must not be equal to an existing internal argument.",
              "\nPROBLEM: the operation {bq?alias} is already an internal operation.")
  }
  
  user_ops_all = getOption("string_magic_user_ops")
  if(is.null(user_ops_all) || !is.list(user_ops_all)){
    user_ops_all = list()
  }
  
  user_info = user_ops_all[[namespace]]
  if(is.null(user_info)){
    funs = list()
    funs[[alias]] = fun
    operators_default = getOption("string_magic_operations_default")
    operators = c(operators_default, alias)
  } else {
    funs = user_info$funs
    funs[[alias]] = fun
    operators = user_info$operators
    operators = unique(c(operators, alias))
  }
  
  user_info = list(funs = funs, operators = operators)
  user_ops_all[[namespace]] = user_info

  options("string_magic_user_ops" = user_ops_all)
}

#' Display messages using interpolated strings
#' 
#' Utilities to display messages using `string_magic` interpolation and operations to generate the message.
#' 
#' @inheritParams string_magic
#' 
#' @param .end Character scalar, default is `""` (the empty string). This string 
#' will be collated at the end of the message (a common alternative is `"\n"`).
#' @param .width Can be 1) a positive integer, 2) a number in (0;1), 3) `FALSE` (default 
#' for `cat_magic`), or 4) `NULL` (default for `message_magic`). It represents the target 
#' width of the message on the user console. Newlines will be added *between words* to fit the
#' target width.
#' 
#' 1. positive integer: number of characters
#' 2. number (0;1): fraction of the screen
#' 3. `FALSE`: does not add newlines
#' 4. `NULL`: the min between 120 characters and 90% of the screen width
#' 
#' Note that you can use the special variable `.sw` to refer to the screen width. Hence the value
#' `NULL` is equivalent to using `min(120, 0.9*.sw)`.
#' @param .leader Character scalar, default is `TRUE`. Only used if argument `.width` is not `FALSE`. 
#' Whether to add a leading character string right after the extra new lines.
#' 
#' @details 
#' These functions are [base::cat()]/[message()] wrappers aroung [string_magic()]. There is one notable difference
#' with respect to `cat`/`message`. It's the ability to add newlines after words for 
#' the message to fit a target width. This is controlled with the argument `.width`. This is 
#' active by default for `message_magic` (default is `.width = NULL` which leads to the 
#' minimum betwen 120 characters and 90% of the screen width).
#' 
#' You can very easily change the default values with the alias generators `cat_magic_alias` and 
#' `message_magic_alias`.
#' 
#' `[Advanced]` A note for package developers who would use these functions **and**
#' also use custom `string_magic` operations created with [string_magic_register_fun()] or
#' [string_magic_register_ops()]. To ensure forward compatibility the new operations created 
#' should be defined in the package namespace (see the *ad hoc* section in [string_magic_register_fun()] help).
#' To access these operators in their specific namespaces, you must use an alias with 
#' `cat_magic_alias` or `message_magic_alias` with the argument `.namespace = "myPackageName"` 
#' (to avoid having to provide the `.namespace` argument repeatedly).
#' 
#' @return 
#' The functions `cat_magic()` and `message_magic()` do not return anything, they simply print on the console. 
#' 
#' The function `cat_magic_alis()` returns a function behaving identically to [cat_magic()] but for which the
#' default values have been altered.
#' Same for `message_magic_alias()`.
#' 
#' @family tools with aliases
#' 
#' @inherit string_clean seealso
#' 
#' @examples 
#' 
#' start = Sys.time()
#' Sys.sleep(0.05)
#' message_magic("This example has run in {difftime ? start}.")
#' 
#' cat_magic("Let's write a very long message to illustrate how .width work.", 
#'           .width = 40)
#' 
#' # Let's add a leader
#' cat_magic("Let's write a very long message to illustrate how `.width` work.", 
#'           "And now we add `.leader`.", .width = 40, .leader = "#> ")
#' 
#' # newlines respect the introductory spaces
#' cat_magic("Here's a list:", 
#'           "    + short item", 
#'           "    + this is a very long item that likely overflows", 
#'          .width = 30, .sep = "\n")
#' 
#' #
#' # define custom defaults
#' #
#' 
#' # Unhappy about the default values? Create an alias!
#' 
#' # Here we change the defaults to mimic the printing of a column
#' cat_column = cat_magic_alias(.sep = "\n", .end = "\n", .vectorize = TRUE, 
#'                             .last = "fill.center, ' + 'paste.both")
#'
#' cat_column("code string_magic", "write the docs", "write the vignettes")
#' 
cat_magic = function(..., .sep = "", .end = "", .width = FALSE, .leader = "", 
                     .envir = parent.frame(), 
                     .vectorize = FALSE, .delim = c("{", "}"), .last = NULL, 
                     .collapse = NULL, .trigger = TRUE, 
                     .check = TRUE, .help = NULL, 
                     .namespace = NULL){
  
  if(!isTRUE(.trigger)) return(invisible(NULL))
  
  set_pblm_hook()
  txt = string_magic(..., .envir = .envir, .sep = .sep, .vectorize = .vectorize, 
                     .delim = .delim, .last = .last, .collapse = .collapse,
                     .check = .check, .help = .help,
                     .namespace = .namespace)
            
  check_character(.end, scalar = TRUE)
  
  # all this is needed to implement lazy default values with  yet to be evaluated expressions (.sw)
  is_call = isTRUE(try(is.call(.width), silent = TRUE))
  .width = if(is_call) .width else substitute(.width)
  .width = check_set_width(.width)
  if(is.finite(.width)){
    txt = fit_screen(txt, .width, leader = .leader)
  }
  
  if(length(txt) > 1){
    txt = paste0(txt, collapse = .sep)
  }
  
  if(nchar(.end) > 0){
    txt = paste0(txt, .end)
  }

  cat(txt)
}

#' @describeIn cat_magic Alias to `cat_magic`
catma = cat_magic

#' @describeIn cat_magic Display messages using interpolated strings
message_magic = function(..., .sep = "", .end = "\n", .width = NULL, .leader = "", 
                         .envir = parent.frame(), 
                         .vectorize = FALSE, .delim = c("{", "}"), 
                         .last = "'min(100, .sw)'swidth", 
                         .collapse = NULL, .trigger = TRUE,
                         .check = TRUE, .help = NULL, 
                         .namespace = NULL){
  
  if(!isTRUE(.trigger)) return(invisible(NULL))
  
  set_pblm_hook()
  txt = string_magic(..., .envir = .envir, .sep = .sep, .vectorize = .vectorize, 
                     .delim = .delim, .last = .last, .collapse = .collapse,
                     .check = .check, .help = .help,
                     .namespace = .namespace)


  check_character(.end, scalar = TRUE)
  
  is_call = isTRUE(try(is.call(.width), silent = TRUE))
  .width = if(is_call) .width else substitute(.width)
  .width = check_set_width(.width)
  if(is.finite(.width)){
    txt = fit_screen(txt, .width, leader = .leader)
  }
  
  if(length(txt) > 1){
    txt = paste0(txt, collapse = .sep)
  }
  
  if(nchar(.end) > 0){
    txt = paste0(txt, .end)
  }

  message(txt, appendLF = FALSE)
}

#' @describeIn cat_magic Alias to `message_magic`
mema = message_magic

#' Sets up a timer that can be used within `_magic` functions
#' 
#' Sets up a timer which can later be summoned by [string_magic()] functions via
#' the `.timer`, `.timer_lap` and `.timer_total` variables. Useful to report 
#' timings within functions with the function [cat_magic()] or [message_magic()].
#' 
#' @details
#' This functions sets up a timer with [base::Sys.time()]. This timer can then be tracked 
#' and modified with the `.timer`, `.timer_lap` and `.timer_total` variables within 
#' [cat_magic()] or [message_magic()].
#' 
#' Note that the timer is precise at +/- 1ms, hence it should **not** be used to time 
#' algorithms with very short execution times. 
#' 
#' It works by saving the current system time in R options (`stringmagic_timer` and `stringmagic_timer_origin`).
#' Hence, since it uses options, it should not be used in parallel processes.
#' 
#' @return 
#' This function does not return anything and is only intended to be used in
#' conjunction with future calls of [string_magic()]. 
#' 
#' 
#' @author 
#' Laurent Berge
#' 
#' @inherit cat_magic seealso
#' 
#' @examples 
#' 
#' # simple example where we time the execution of some elements in a function
#' # we trigger the message conditionally on the value of the argument `debug`.
#' rnorm_crossprod = function(n, mean = 0, sd = 1, debug = FALSE){
#'   # we set the timer
#'   timer_magic()
#'   # we compute some stuff
#'   x = rnorm(n, mean, sd)
#'   # we can report the time with .timer
#'   message_magic("{15 align ! Generation}: {.timer}", .trigger = debug)
#'   
#'   res = x %*% x
#'   message_magic("{15 align ! Product}: {.timer}",
#'                 "{15 align ! Total}: {.timer_total}", 
#'                 .sep = "\n", .trigger = debug)
#'   res
#' }
#' 
#' rnorm_crossprod(1e5, TRUE)
#' 
#' 
timer_magic = function(){
  tm = Sys.time()
  options(stringmagic_timer = tm)
  options(stringmagic_timer_origin = tm)
}

####
#### ... string_magic ####
####


#' @describeIn string_magic String interpolation with operation chaining
string_magic = function(..., .envir = parent.frame(), .sep = "", .vectorize = FALSE, 
                        .delim = c("{", "}"), .last = NULL, .post = NULL, .nest = FALSE,
                        .collapse = NULL, .invisible = FALSE, .default = NULL,
                        .trigger = TRUE, 
                        .check = TRUE, .class = NULL, .help = NULL, 
                        .namespace = NULL){
  
  if(!isTRUE(.trigger)) return(invisible(NULL))
  
  if(.check){
    set_pblm_hook()
    
    if(!missing(.envir)) check_envir(.envir)
    if(!missing(.sep)) check_character(.sep, scalar = TRUE)
    if(!missing(.vectorize)) check_logical(.vectorize, scalar = TRUE)
    if(!missing(.nest)) check_logical(.nest, scalar = TRUE)
    if(!missing(.delim)){
      .delim = check_set_delimiters(.delim)
    }
    if(!missing(.last)) check_last(.last)
    if(!missing(.collapse)) check_character(.collapse, null = TRUE, scalar = TRUE)
    if(!missing(.invisible)) check_logical(.invisible, scalar = TRUE)
    if(!missing(.default)) check_character(.default, null = TRUE, scalar = TRUE)
  }
    
  if(length(.delim) == 1){
    .delim = strsplit(.delim, " ", fixed = TRUE)[[1]]
    if(.check){ 
      check_delimiters(.delim)
    }
  }  

  if(...length() == 0){
    if(missnull(.help)){
      return("")
    }
  } 
  
  res = string_magic_internal(..., .delim = .delim, .envir = .envir, .sep = .sep,
                              .vectorize = .vectorize, .help = .help, .nest = .nest,
                              .collapse = .collapse, .is_root = TRUE, 
                              .namespace = .namespace, .default = .default,
                              .check = .check, .last = .last)
  
  if(!is.null(attr(res, "group_index"))){
    # cleaning artifacts
    attr(res, "group_index") = NULL
  }
  
  if(!is.null(.post)){
    # .post must be a function
    # we catch the arguments
    res = check_set_eval_fun(.post, res, ...)
  }
  
  if(!is.null(.class)){
    class(res) = c(.class, class(res))
  }
  
  if(.invisible){
    return(invisible(res))
  }
  
  res
}

#' @describeIn string_magic A simpler version of `string_magic` without any error handling to save a few micro seconds
.string_magic = function(..., .envir = parent.frame(), .sep = "", .vectorize = FALSE,
                         .delim = c("{", "}"), .collapse = NULL, .last = NULL, .nest = FALSE,
                         .trigger = TRUE, .namespace = NULL){
  
  if(!.trigger) return(invisible(NULL))
  
  if(length(.delim) == 1){
    .delim = strsplit(.delim, " ", fixed = TRUE)[[1]]
  }

  string_magic_internal(..., .delim = .delim, .envir = .envir, 
                        .sep = .sep, .namespace = .namespace, .nest = .nest,
                        .vectorize = .vectorize, .is_root = TRUE, 
                        .collapse = .collapse,
                        .check = FALSE, .last = .last)
}

# This is an internal alias (not exported)
.sma = .string_magic

#' @describeIn string_magic Alias to `string_magic`
sma = string_magic

####
#### Internal ####
####


string_magic_internal = function(..., .delim = c("{", "}"), .envir = parent.frame(), 
                                 .data = list(),
                                 .sep = "", .vectorize = FALSE,
                                 .collapse = NULL, .last = NULL, .nest = FALSE,
                                 .help = NULL, .is_root = FALSE, 
                                 .namespace = NULL, .user_funs = NULL,
                                 .valid_operators = NULL, .default = NULL,
                                 .check = FALSE, .plural_value = NULL){
  
  # flag useful to easily identify this environment (used in error messages)
  is_string_magic_internal = TRUE

  if(!is.null(.help)){
    on.exit(string_magic_dynamic_help(.help))
    stop_up("string_magic: Help requested.")  
  }
  
  if(...length() == 0){
    return("")
  }
  
  if(.is_root){
    if(is.null(.namespace)){
      .namespace = "R_GlobalEnv"
    }
    user_ops_all = getOption("string_magic_user_ops")
    # beware the sneaky assignment!
    if(!is.null(user_ops_all) && !is.null(user_info <- user_ops_all[[.namespace]])){
      #                                             ^^ sneaky!
      .user_funs = user_info$funs
      .valid_operators = user_info$operators          
    } else {
      .valid_operators = getOption("string_magic_operations_default")
    }
  }

  if(...length() == 1){
    
    # Note: using ...names() conditionnally adds 110us!!!! (getRversion() >= "4.1.3" is slow)
    # otherwise the line below is about half a micro second
    dots_nm = names(list(...))
    is_no_names = !is.null(dots_nm)
    
    if(is_no_names){
      stop_hook("`string_magic` requires at least one character scalar to work.",
                "\nNamed arguments are only used as variables on which to apply interpolation.",
                "\nFIX: please provide at least one non-named argument.")
    }
    
    x = as.character(..1)
    
    if(length(x) > 1){
      stop_hook("`string_magic` can only be applied to character scalars. ",
                "\nPROBLEM: the argument is not of length 1, it is of length {len ? x}.")
    }

  } else {

    if(.check){
      dots = check_expr(list(...), "In `string_magic`, one element of ... could not be evaluated.")
    } else {
      dots = list(...)
    }
    
    # we check if some variables were passed in the arguments
    if(.is_root){
      # ONLY HAPPENS IN THE ROOT CALL
      dot_names = names(dots)
      if(!is.null(dot_names)){
        is_var = dot_names != ""
        for(i in which(is_var)){
          .data[[dot_names[i]]] = dots[[i]]
        }
        dots[is_var] = NULL      
      }
    }    
    
    if(length(dots) == 0){
      stop_hook("`string_magic` requires at least one character scalar to work.",
                "\nNamed arguments are only used as variables on which to apply interpolation.",
                "\nPROBLEM: all arguments are named.",
                "\nFIX: please provide at least one non-named argument.")
    }
    
    if(any(lengths(dots) > 1)){
      qui = which(lengths(dots) > 1)[1]
      stop_hook("`string_magic` can only be applied to character scalars.",
                "\nPROBLEM: The ", n_th(qui),
                " elment in ... is of length ", length(dots[[qui]]), ".")
    }

    if(!.vectorize){
      # Note: using paste(..1, ..2, sep = .sep) explicitly only saves 2us vav do.call
      # not worth it.

      dots$sep = .sep
      x = do.call(paste, dots)
    } else {
      
      if(.nest){
        stop_hook("The argument `.nest` is not compatible with the argument `.vectorize`.",
                  "One of the two must be set to `FALSE`.")
      }
      
      # vectorize
      n = length(dots)
      res = vector("list", n)
      
      now_done = FALSE
      date_done = FALSE
      
      for(i in 1:n){
        
        di = dots[[i]]
        if(.is_root && length(di) == 1 && is.character(di)){
          if(!now_done && grepl(".now", di, fixed = TRUE)){
            if(grepl(".now(", di, fixed = TRUE)){
              .data[[".now"]] = function(x) format(Sys.time(), x)
            } else {
              .data[[".now"]] = Sys.time()
            }
            
            now_done = TRUE
          }
          
          if(grepl(".timer", di, fixed = TRUE)){
            if(grepl(".timer([^_]|$)", di)){
              .data[[".timer"]] = timer("simple")
            }
            
            if(grepl(".timer_lap", di, fixed = TRUE)){
              .data[[".timer_lap"]] = timer("lap")
            }
            
            if(grepl(".timer_total", di, fixed = TRUE)){
              .data[[".timer_total"]] = timer("total")
            }
          }
          
          if(!date_done && grepl(".date", di, fixed = TRUE)){
            .data[[".date"]] = Sys.Date()
            date_done = TRUE
          }
        }
        
        res[[i]] = string_magic_internal(dots[[i]], .delim = .delim, .envir = .envir, 
                                         .data = .data, .check = .check,
                                         .user_funs = .user_funs, 
                                         .valid_operators = .valid_operators)
      }
      
      res = unlist(res)
      
      if(!true_character(res)){
        res = as.character(res)
      }
      
      if(!is.null(.last)){
        if(is.function(.last)){
          res = .last(res)
        } else {
          res = apply_simple_operations(res, ".last", .last, .check, .envir, .data,
                                        group_flag = 1 * grepl("~", .last, fixed = TRUE), 
                                        .delim, .user_funs = .user_funs, 
                                        .valid_operators = .valid_operators)
        }
      }

      return(res)
    }
  }

  if(is.na(x) || length(x) == 0){
    return(x)
  }
  
  if(.nest){
    x = paste0(.delim[1], x, .delim[2])
  }

  BOX_OPEN = .delim[1]
  
  if(!grepl(BOX_OPEN, x, fixed = TRUE)){
    if(is.null(.last)){
      return(x)
    } else {
      x_parsed = list(x)
    }    
  } else {
    x_parsed = cpp_string_magic_parser(x, .delim)
    if(length(x_parsed) == 1 && isTRUE(attr(x_parsed, "error"))){
      report_string_magic_parsing_error(x, x_parsed, .delim)
    }
    
    # we add extra variables
    if(.is_root){
      if(grepl(".now", x, fixed = TRUE)){
        if(grepl(".now(", x, fixed = TRUE)){
          .data[[".now"]] = function(x) format(Sys.time(), x)
        } else {
          .data[[".now"]] = Sys.time()
        }
      }
      
      if(grepl(".timer", x, fixed = TRUE)){
        if(grepl(".timer([^_]|$)", x)){
          .data[[".timer"]] = timer("simple")
        }
        
        if(grepl(".timer_lap", x, fixed = TRUE)){
          .data[[".timer_lap"]] = timer("lap")
        }
        
        if(grepl(".timer_total", x, fixed = TRUE)){
          .data[[".timer_total"]] = timer("total")
        }
      }
      
      if(grepl(".date", x, fixed = TRUE)){
        .data[[".date"]] = Sys.Date()
      }
    }
  }
  

  # NOTE on the parsing:
  # - parsing returns a list.
  #   If, eg: x = "str1{stuff1}str2{stuff2}"
  #   we will obtain a list of 4 elements
  #   note that the empty string between two boxes is an element
  # - an element can be either of length 1 or of length 2
  #   * l=1: means this is a regular string
  #   * l=2: this is a list of two elements:
  #     + the first element is a vector of operators, with ? or ! always at the end (when available)
  #     + the second element is the string to be interpolated (when available, some operators don't
  #       require interpolation, like pluralization)
  #   ex: "hello: {u, C ! {S!luc, domi}}"
  #     [[1]]: "hello: "
  #     [[2]]:
  #           [[1]]: vector c("u", "C")
  #           [[2]]: scalar "{S!luc, domi}"
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
        xi_call = check_set_string_magic_parsing(xi, .check, .delim)

        if(is.character(xi_call)){
          xi = xi_call
        } else {
          xi = check_set_string_magic_eval(xi_call, .data, .envir, .check)
        }

        if(ANY_PLURAL){
          # we save
          x_values_all[[i]] = xi
        }
        
        if(!is.null(.default)){
          # We apply the default operations
          xi = apply_simple_operations(xi, ".default", .default, .check, .envir, .data,
                                       group_flag = 1 * grepl("~", .default, fixed = TRUE), 
                                       .delim, .user_funs = .user_funs, 
                                       .valid_operators = .valid_operators)
        }

      } else {

        n_op = length(operators)
        verbatim = operators[n_op] == "!"
        
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
                  example = 'Example: x = c("Mark", "Sarah"); string_magic("{$enum, is ? x} away.")'
                  .stop_hook("In `string_magic`, the pluralization operator (`", operators[1], 
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
                    example = 'Example: x = c("Mark", "Sarah"); string_magic("{$enum, is ? x} away.")'

                    .stop_hook("In `string_magic`, the pluralization (`", pblm, "`) did not find any value to pluralize on. ",
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
            if(grepl(BOX_OPEN, xi, fixed = TRUE)){
              xi = string_magic_internal(xi, .delim = .delim, .envir = .envir, .data = .data, 
                                         .vectorize = concat_nested, .check = .check, 
                                         .user_funs = .user_funs, 
                                         .valid_operators = .valid_operators)
            }
          } else if(!verbatim){
            # evaluation
            xi_call = check_set_string_magic_parsing(xi, .check, .delim)
            xi = check_set_string_magic_eval(xi_call, .data, .envir, .check)
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
          xi = sma_pluralize(operators, xi, .delim, .envir, .data, .check, 
                             .user_funs, .valid_operators)

        } else if(is_ifelse){

          xi_val = NULL
          xi_expr = str2lang(xi_raw)
          vars = all.vars(xi_expr)

          if(length(vars) == 0){
            if(operators[1] == "&&"){
              # ERROR
              form = "{&& cond ; true}"
              example = '\nEXAMPLE: x = c(5, 700); string_magic("Value: {&&x > 20 ; > 20}")'

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
              xi_call = extract_first_variable(xi_expr)
              xi_val = check_set_string_magic_eval(xi_call, .data, .envir, .check)
            }
            
            if(operators[1] == "&&" && length(xi) != length(xi_val)){
              form = "{&& cond ; true}"
              example = 'EXAMPLE: x = c(5, 700); string_magic("Value: {&&x > 20 ; > 20}")'

              stop_hook("The if-else operator `&&`, of the form {form} ",
                      "requires that the first variable used in the condition ",
                      "(here {bq?vars[1]}) is of the same length as the condition.",
                      "\nPROBLEM: the condition is of length {len?xi} while the ",
                      "variable is of length {len?xi_val}.",
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
                          .check = .check, .delim = .delim, 
                          .user_funs = .user_funs, .valid_operators = .valid_operators)
        } else {
          #
          # REGULAR OPERATORS
          #

          group_flag = any(string_x(operators, 1) == "~")
          
          for(j in seq_along(operators)){
            opi = operators[[j]]
            op_parsed = sma_char2operator(opi, .valid_operators)

            if(op_parsed$eval){
              argument_call = check_set_oparg_parse(op_parsed$argument, opi, .check)
              argument = check_set_oparg_eval(argument_call, .data, .envir, opi, .check)
            } else {
              argument = op_parsed$argument
            }

            if(.check){
              xi = check_expr(sma_operators(xi, op_parsed$operator, op_parsed$options, argument,
                                            .check = .check, .envir = .envir, 
                                            group_flag = group_flag,
                                            .delim = .delim, .user_funs = .user_funs, 
                                            .valid_operators = .valid_operators),
                              get_string_magic_context(), " See error below:",
                              verbatim = TRUE, up = 1)
            } else {
              xi = sma_operators(xi, op_parsed$operator, op_parsed$options, argument, 
                                 .check = .check,
                                 .envir = .envir, group_flag = group_flag,
                                 .delim = .delim, .user_funs = .user_funs, 
                                 .valid_operators = .valid_operators)
            }
          }
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
  
  if(!true_character(res)){
    # we always return something in character form
    res = as.character(res)
  }  
  
  if(!is.null(.last)){
    if(is.function(.last)){
      res = .last(res)
    } else {
      res = apply_simple_operations(res, ".last", .last, .check, .envir, .data,
                                    group_flag = 1 * grepl("~", .last, fixed = TRUE), 
                                    .delim, .user_funs = .user_funs, 
                                    .valid_operators = .valid_operators)
    }
  }

  if(!is.null(.collapse) && length(res) > 1){
    res = paste0(res, collapse = .collapse)
  }

  return(res)
}


sma_char2operator = function(x, .valid_operators){

  op_parsed = cpp_parse_operator(x)
  
  ok = FALSE
  op = op_parsed$operator

  if(nchar(op) == 0){
    .stop_hook("In `string_magic`, if a quoted value is present, the operators must ",
              "be of the form 'value'op, ",
              "with 'op' an operator. ",
              "\nPROBLEM: In `", escape_newline(x), "` the operator is missing.")
  }
  
  argument = op_parsed$argument
  do_eval = op_parsed$eval
  
  # we partially match operators if needed
  if(!op %in% .valid_operators){
    op = check_set_options(op, .valid_operators, case = TRUE, free = TRUE)
    op_parsed$operator = op
  }

  if(nchar(argument) == 0 && !substr(x, 1, 1) %in% c("'", "\"", "`")){
    # no argument provided: we need to:
    # - set default values if available
    # - send error if operator requires argument
    
    # default values
    argument = switch(op,
                      s = " ", S = ",[ \t\n]*", split = " ", Split = ",[ \t\n]*",
                      x = "[[:alnum:]]+", X = "[[:alnum:]]+", extract = "[[:alnum:]]+",
                      c = " ", C = ", | and ", collapse = " ", Collapse = ", | and ",
                      head = "6",
                      times = 1, each = 1,
                      first = 1, last = 1,
                      firstchar = 1, lastchar = 1,
                      trim = 1, 
                      "")
    
    op_parsed$argument = argument

    if(op %in% c("R", "r", "%", "k", "K", "paste", "get", "is")){
      ex = c("R" = 'x = "She loves me."; string_magic("{\'s\\b => d\'R ? x}")',
            "r" = 'x = "Amour"; string_magic("{\'ou => e\'r ? x}...")',
            "replace" = 'x = "Amour"; string_magic("{\'ou => e\'r ? x}...")',
            "%" = 'string_magic("pi is: {%.03f ? pi}")',
            "k" = 'string_magic("The first 8 letters of the longuest word are: {8k, q ! the longuest word}.")',
            "shorten" = 'string_magic("The first 8 letters of the longuest word are: {8 shorten, q ! the longuest word}.")',
            "Shorten" = 'string_magic("The first 8 letters of the longuest word are: {8 Shorten, q ! the longuest word}.")',
            "K" = 'x = 5:9; string_magic("The first 2 elements of `x` are: {2K, C ? x}.")',
            "get" = 'x = row.names(mtcars) ; string_magic("My fav. cars are {\'toy\'get.ignore, \'the \'app, enum ? x}.")',
            "is" = 'x = c("Bob", "Pam") ; string_magic("{\'am\'is ? x}")',
            "paste" = 'x = "those, words"; string_magic("Let\'s emphasize {S, \'**\'paste.both, c ? x}.")')

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
    
  if(!ok && !op %in% .valid_operators){
    
    op = check_set_options(op, .valid_operators, case = TRUE, free = TRUE)
    op_parsed$operator = op
    
    if(!op %in% .valid_operators){
      
      context = get_string_magic_context()

      sugg_txt = suggest_item(op, .valid_operators, newline = FALSE, info = "operator")
      
      op = escape_newline(op)
      op = gsub("\t", "\\\\t", op)

      msg = .sma("{context}",
              "\nPROBLEM: {bq?op} is not a valid operator. ", sugg_txt,
              "\n\nINFO: Type string_magic(.help = \"regex\") or string_magic(.help = TRUE) for help.",
              "\nOr look at the vignette: https://lrberge.github.io/stringmagic/articles/guide_string_magic.html")

      .stop_hook(msg)
    }

  }

  op_parsed
}


sma_operators = function(x, op, options, argument, .check = FALSE, .envir = NULL, 
                         .data = list(), group_flag = 0, .delim = c("{", "}"),  
                         .user_funs = NULL, .valid_operators = NULL){

  # group_flag:  0 nothing
  #                    1 keep track of conditional things
  #                    2 apply conditional

  group_index = attr(x, "group_index")
  
  # beware the sneaky assignment!!!!!
  if(!is.null(.user_funs) && !is.null(fun <- .user_funs[[op]])){
    #                                     ^ sneaky (but saves half a us)
    # user-defined operations ####
    
    # EXPLANATION: why do we check user operations first?
    # - I want this package to last and be stable
    # - I also want forward stability for packages that use stringmagic as a dependency
    # - example of use case:
    #   * pkg matchmaker uses stringmagic and defines the operation 'h1'
    #   * all works fine with SM 1.0.0
    #   * now SM 1.1.0 introduces the operator h1!!!!
    #   * what happens?
    #     + matchmaker should work for both SM 1.0.0 and SM 1.1.0
    #     + it should work even if h1 has a new definition in SM 1.1.0 => the h1 used
    #       should be the one of the user, defined in the package.
    # - hence I need to:
    #   * allow users to redefine existing operations
    #   * check user defined version first
    #
    
    if(isTRUE(attr(fun, "simple_ops"))){
      res = apply_simple_operations(x, op, fun, .check, .envir,  .data,
                                    group_flag = group_flag, .delim, 
                                    .user_funs = .user_funs, 
                                    .valid_operators = .valid_operators)
                                
      # res already has a group_index attribute
      
    } else {
      # This is a regular function
      valid_options = attr(fun, "valid_options")
      if(!is.null(valid_options)){
        options = check_set_options(options, valid_options)
      }

      res = fun(x = x, argument = argument, options = options, group = group_index, 
                group_flag = group_flag)

      # NOTA: for the users I use 'group', internally I use 'group_index'
      group_index = attr(res, "group")
      if(group_flag != 0 && is.null(group_index) && !isTRUE(attr(fun, "group_flag")) 
          && length(x) == length(res)){
        # This is the case in which there is grouping but we don"t impose
        # to the user to define the grouping behavior
        # if the lengths are different: it's messed up and so be it
        attr(res, "group_index") = attr(x, "group_index")
      }
    }
    
    return(res)
  } 
  
  # We carry on
  
  if(op %in% c("c", "C", "collapse", "Collapse")){
    # C, collapse ####
    # collapse

    # argument of the form: 'main_sep|last_sep'
    sep_last = ""
    is_last = FALSE
    if(length(x) > 1 && grepl("|", argument, fixed = TRUE) && nchar(argument) > 1){
      info = extract_pipe(argument, op)
      is_last = info$is_pipe
      argument = info$value
      sep_last = info$extra
    }
    sep = argument

    if(!is.null(group_index) && group_flag == 2){
      x = as.character(x)
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
    
  } else if(op %in% c("s", "S", "split", "Split", "r", "R", "replace", "clean", "get", 
                      "is", "which", "x", "X", "extract")){
    # split, replace, clean, extract, get, is, which ####
    
    valid_options = c("word", "ignore", "fixed")
    if(op == "extract"){
      valid_options = c(valid_options, "first")
    } else if(op %in% c("r", "R", "clean", "replace")){
      valid_options = c(valid_options, c("total", "single"))
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
      # otherwise => dealt with in string_is or string_clean
      # we don't repeat the processing (otherwise => bugs)
      
      pat_parsed = format_simple_regex_flags(argument, fixed = is_fixed, word = is_word, 
                                             ignore = is_ignore, magic = TRUE, 
                                             envir = .envir)
      argument = pat_parsed$pattern
      is_fixed = pat_parsed$fixed
    } 

    #
    # now the operations
    #
    
    if(!true_character(x) && op != "which"){
      x = as.character(x)
    }
    
    if(op %in% c("s", "S", "split", "Split")){
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
      is_single = "single" %in% options

      pipe = "=>"
      if(grepl(" => ", argument, fixed = TRUE)){
        pipe = " => "
      }
      
      split = if(op == "clean") ",[ \t\n]+" else ""
      
      res = string_clean(x, argument, pipe = pipe, split = split, 
                      ignore.case = is_ignore, fixed = is_fixed, word = is_word, 
                      total = is_total, single = is_single, envir = .envir)

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
      # default is string_is
      if("equal" %in% options || "in" %in% options){
        if("equal" %in% options){
          res = x == argument
        } else {
          arg_split = strsplit(argument, ",[ \t\n]+")
          res = x %in% unlist(arg_split)
        }
        
      } else {
        res = string_is(x, pattern = argument, fixed = is_fixed, 
                     ignore.case = is_ignore, word = is_word, envir = .envir)
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
      msg = paste0("In `string_magic`: the operator `", op, "` must have numeric arguments, `",
                   argument, "` is not numeric.")
      .stop_hook(msg)
    }
    
    if(length(argument) != 1){
      msg = paste0("In `string_magic`: the operator `", op, "` must have an argument of length 1.",
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
    
    pos = "left"
    if(is_right) pos = "right"
    if(is_center) pos = "center"
    
    if(nchar(argument) > 0){
      res = format(x, argument, justify = "right", big.mark = ",")
    } else {
      res = format(x, justify = "right", big.mark = ",")
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
      msg = paste0("In `string_magic`: the operator `", op, "` must first contain a numeric argument. PROBLEM: `",
                   argument, "` is not numeric.")
      .stop_hook(msg)
    }
    nb = as.numeric(nb)

    is_both = opt_equal(options, "both")
    is_right = any(c("r", "right") %in% options) || nb < 0
    
    if(!true_character(x)){
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

  } else if(op %in% c("k", "shorten", "Shorten", "K", "head")){
    # keep: either the nber of characters (k) or the number of elements (K)

    #
    # Keep, shorten, head ####
    #
    
    if(op %in% c("k", "shorten", "Shorten")){
      options = check_set_options(options, c("include", "dots"))
      is_dots = "dots" %in% options || substr(op, 1, 1) == "S"
      op = "k"
    } else {
      options = check_set_options(options, "include")
    }
    
    info = extract_pipe(argument, op, double = TRUE, numeric = TRUE, mbt = TRUE)
    nb = info$value
    add = info$extra
    is_included = info$is_double

    if(op == "k"){
      if(!true_character(x)){
        x = as.character(x)
      }
      
      n_longer = n_rm = 0
      if(is_dots){
        n_longer = 1
        n_rm = 1
        add = ".."
      } else if(is_included || "include" %in% options){
        n_rm = nchar(add)
      }
      
      qui = nchar(x) > (nb + n_longer)
      res = x
      
      res[qui] = substr(res[qui], 1, nb - n_rm)

      if(nchar(add) > 0){
        res[qui] = paste0(res[qui], add)
      }

    } else {
      # K or head

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
  } else if(op %in% c("dp", "deparse")){
    
    options = check_set_options(options, "long")
    
    if(nchar(argument) > 0){
      argument = as.numeric(argument)
    } else {
      argument = 35
    }
    
    if("long" %in% options){
      x_dp = paste0(deparse(x, nlines = 100), collapse = " ")
    } else {
      x_dp = deparse(x, nlines = 1)
    }
    
    
    if(nchar(x_dp) > argument){
      x_dp = substr(x_dp, 1, argument - 2)
      x_dp = gsub(" *$", "...", x_dp)
    }
    
    res = x_dp
    
  } else if(op == "enum"){
    # enum ####
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

  } else if(op %in% c("first", "last", "firstchar", "lastchar")){
    # first, last, firstchar, lastchar ####

    nb = argument
    
    nb_extra = NULL
    if(op == "first" && grepl("|", argument, fixed = TRUE)){
      arg_split = strsplit(argument, "|", fixed = TRUE)[[1]]
      nb = arg_split[1]
      nb_extra = arg_split[2]
      
      if(!is.null(nb_extra) && !is_numeric_in_char(nb_extra)){
        msg = paste0("In `string_magic`: in the operator `", op, "` the argument can be of the form ",
                     "'n1|n2' with `n1` and `n2` numbers. ",
                     "\nPROBLEM: the second element `", nb_extra, "` is not numeric.")
        .stop_hook(msg)
      }
      
      nb_extra = as.numeric(nb_extra)
    }
    
    if(!is_numeric_in_char(nb)){
      msg = paste0("In `string_magic`: the operator `", op, "` must have a numeric argument. ",
                   "\nPROBLEM: `", argument, "` is not numeric.")
      .stop_hook(msg)
    }    

    # the numeric argument can also be passed in options
    qui_num = which(grepl("^\\d+$", options))
    if(length(qui_num)){
      nb = options[qui_num[1]]
    }

    nb = as.numeric(nb)

    if(string_x(op, -4) == "char"){
      # we select the first/last characters
      
      if(!true_character(x)){
        x = as.character(x)
      }
      
      if(nb < 0){
        nb = abs(nb)
        nx = nchar(x)
        if(op == "firstchar"){
          res = substr(x, nb + 1, nx)
        } else {
          res = substr(x, 1, nx - nb)
        }
      } else {
        if(op == "firstchar"){
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
              msg = paste0("In `string_magic`: in the operator `", op, "` the argument can be of the form ",
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
                msg = paste0("In `string_magic`: in the operator `", op, "` the argument can be of the form ",
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
      x2sort = string_clean(x, argument, envir = .envir)
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

  } else if(op %in% c("paste", "append", "insert")){
    # paste, append, insert ####
    # paste: at the beginning/end of all strings
    # insert: element at the beginning/end of the vector 
    
    if(op == "append"){
      op = "paste"
    }
    
    valid_options = c("both", "right")
    if(op == "paste"){
      valid_options = c(valid_options, "front", "back", "delete")
    }
    
    options = check_set_options(options, valid_options)
    
    left = right = ""
    if(!any(c("both", "right") %in% options) && grepl("|", argument, fixed = TRUE)){
      args = extract_pipe(argument, op)
      left = args$value
      right = args$extra
    } else {
      if("both" %in% options){
        left = right = argument
      } else if("right" %in% options){
        right = argument
      } else {
        left = argument
      }
    }
    
    if("delete" %in% options){
      x = character(length(x))
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
                             ":01:" = string_fill(1:n_x, right = TRUE, symbol = "0"),
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

    }

    # END: paste/insert

  } else if(op %in% c("fill", "align", "width")){
    # fill, align ####
    
    # all are equivalent
    
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
    
    res = string_fill(x, argument, symbol = symbol, right = right, center = center)
    
  } else if(op == "join"){
    # join, escape ####
    res = gsub(" *\\\\ *\n *", " ", x, perl = TRUE)    
  } else if(op == "escape"){
    # later, I can inclue latex, markdown, etc
    options = check_set_options(options, c("nl", "tab"))
    if(length(options) == 0){
      options = c("nl", "tab")
    }
    
    res = x
    if("nl" %in% options){
      res = gsub("\n", "\\\\n", res, perl = TRUE)
    }
    
    if("tab" %in% options){
      res = gsub("\t", "\\\\t", res, perl = TRUE)
    }
    
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
      opt_default = c("letter", "upper", "num")
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
      } else if(opt_equal(options, "num")){
        # we do nothing => we keep it as numeric
      } else {
        res = format(res, big.mark = ",")
      }
    }

    if("upper" %in% options){
      res = gsub("^(.)", "\\U\\1", res, perl = TRUE)
    }
  } else if(op == "swidth"){
    # width, difftime ####
    
    sw = getOption("width")
    data = list(.sw = sw)

    info = extract_pipe(argument, op, double = FALSE, numeric = TRUE, data = data, mbt = FALSE)
    nb = info$value
    comment = info$extra
    
    if(nb == ""){
      nb = NULL
    }

    if(string_x(comment, -1) == "_"){
      # the underscore means that we don't add a space
      comment = string_trim(comment, -1)
    } else if(comment != ""){
      comment = paste0(comment, " ")
    }

    # code is slow but I don't see why would one want it to be super fast
    # (only for user-content, not performance/data-analysis content)

    res = character(length(x))
    for(i in seq_along(x)){
      res[i] = fit_screen(x[i], width = nb, leader = comment)
    }
    
  } else if(op == "difftime"){
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

      qui = string_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, 
                   word = is_word, envir = .envir)

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

      qui = !string_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, 
                    word = is_word, envir = .envir)
      
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
    res = string_to_ascii(x, options)

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
        cond = check_expr(string_is(x, cond_parsed, envir = .envir), 
                  .sma("The operation is of the form {bq?op}(cond ; true ; false). ",
                  "When `cond` is a pure character string, the function `string_is()` ",
                  "is applied with `cond` being the searched pattern.",
                  "\nPROBLEM: in {op}({'_;;;_ => ;'R ? argument}), the condition ",
                  "{bq?cond_raw} led to an error, see below:"),
                  verbatim = TRUE)
      } else {
        cond = string_is(x, cond_parsed, envir = .envir)
      }      
      
    } else {
      vars = all.vars(cond_parsed)
      my_data = list()
      for(v in vars){
        if(v %in% c(".N", ".len")){
          my_data[[v]] = length(x)
        } else if(v %in% c(".nchar", ".C")){
          if(!true_character(x)){
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
                "\nPROBLEM: the condition is of length {len?cond} while the ",
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
        xi = apply_simple_operations(xi, op, instruction, .check, .envir, .data,
                                     group_flag = cond_flag, .delim,
                                     .user_funs = .user_funs, 
                                     .valid_operators = .valid_operators)
        
        if(is_elementwise && !length(xi) %in% c(0, n_old)){
          stop_hook("In {bq?op} operations, when conditions are of length > 1, the ",
                   "operations either a) can remove the string completely ",
                   "(e.g. with `nuke`), or b) must not change the length of the element.",
                   "\nPROBLEM: the original vector is of length {n?n_old} while after ",
                   "the operations it becomes of length {len ? xi}")
        }
      } else {
        # verbatim if
        if(grepl(".", instruction, fixed = TRUE)){
          .data[["."]] = xi
        }
        
        xi = string_magic_internal(instruction, .delim = .delim, .envir = .envir, 
                             .data = .data, .check = .check, 
                             .user_funs = .user_funs, .valid_operators = .valid_operators)
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
    }

  } else if(op == "~"){
    # Group wise: ~ ####
    res = apply_simple_operations(x, op, argument, .check, .envir, .data,
                                  group_flag = 2, .delim, .user_funs = .user_funs, 
                                  .valid_operators = .valid_operators)
                                  
    group_index = attr(res, "group_index")

  } else {
    msg = paste0("In `string_magic`: the operator `", op, "` is not recognized. ",
                "Internal error: this problem should have been spotted beforehand.")
    .stop_hook(msg)
  }

  if(length(group_index) > 0){
    attr(res, "group_index") = group_index
  }

  return(res)
}



sma_pluralize = function(operators, xi, .delim, .envir, .data, .check, 
                         .user_funs, .valid_operators){

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
      example = 'Example: x = 5; string_magic("There {#is, N ? x} cat{#s} in the room.")'

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
    if(op_parsed$eval){
      argument_call = check_set_oparg_parse(argument, op, .check)
      argument = check_set_oparg_eval(argument_call, .data, .envir, op, .check)
    }

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
      
      options = check_set_options(options, c("letter", "upper", "no", "No"))

      is_letter = any(options %in% c("letter", "upper")) || substr(op, 1, 1) %in% c("N", "L")
      is_upper = "upper" %in% options
      
      if(IS_ZERO && nchar(argument) > 0){
        res[i] = argument
      } else if(IS_ZERO && any(options %in% c("no", "No"))){
        if(is_upper || "No" %in% options){
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
          
          value = string_magic_internal(argument, .delim = .delim, .envir = .envir, .data = .data,
                                        .check = .check, .plural_value = xi, 
                                        .user_funs = .user_funs, 
                                        .valid_operators = .valid_operators)
        if(value != ""){
          res[i] = value
        }        
      }
    } else if(op == "enum"){

      res[i] = enum_main(xi, options = options)

    } else {
      # The verb is always last

      if(nchar(op) < 2){
        example = 'Example: x = c("Charles", "Alice"); string_magic("{$Is, enum ? x} crazy? Yes {$(he:they), are}.")'
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

sma_ifelse = function(operators, xi, xi_val, .envir, .data, .delim, .check, 
                      .user_funs, .valid_operators){

  if(is.numeric(xi)){
    xi = xi != 0
  }
  
  amp = operators[1]
  is_double_amp = amp == "&&"
  if(!is.logical(xi)){
    form = "{&cond ; true ; false}"
    if(is_double_amp) form = "{&&cond ; true}"
    example = '\nEXAMPLE: x = Sys.time(); string_magic("Hello {&format(x, \'%H\') < 20 ; Sun ; Moon}!")'

    .stop_hook("The if-else operator `", amp, "`, of the form ", form,
            ", accepts only logical values in the condition. ",
            "PROBLEM: the value is not logical, but of class `",
            class(xi)[1], "`.", example)
  }

  if(anyNA(xi)){
    form = "{&cond ; true ; false}"
    if(is_double_amp) form = "{&&cond ; true}"
    example = '\nEXAMPLE: x = Sys.time(); string_magic("Hello {&format(x, \'%H\') < 20 ; Sun ; Moon}!")'

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
              "           compare string_magic(\"{x} = {&x %% 2;odd;even}\")",
              "\n              to string_magic(\"{x} = {&x %% 2;odd}\")",
              "\n              to string_magic(\"{x} = {&&x %% 2;odd}\")")
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
        
        log_op_eval = string_magic_internal(log_op, .delim = .delim, .envir = .envir, .data = .data,
                                            .check = .check, .plural_value = xi_val, 
                                            .user_funs = .user_funs, 
                                            .valid_operators = .valid_operators)
        
        if(!length(log_op) %in% c(1, n_x)){
          form = "{&cond ; true ; false}"
          if(is_double_amp) form = "{&&cond ; true}"
          .stop_hook("The if-else operator of the form {form} accepts values in ",
                      "{&i==1;true;false} which can be interpolated, as is the case here ",
                      "for {bq?log_op}.",
                      "\nThe length of the interpolated value must be equal to 1 or the length ",
                      "of the condition.",
                      "PROBLEM: the length of the condition is {n?n_x} while the length of ",
                      "`{&i==1;true;false}` is {len?log_op_eval}.")
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
    res = string_magic_internal(res, .delim = .delim, .envir = .envir, .data = .data,
                                .check = .check, .plural_value = xi_val, 
                                .user_funs = .user_funs, .valid_operators = .valid_operators)
  }

  res
}


apply_simple_operations = function(x, op, operations_string, .check = FALSE, .envir = NULL, 
                                   .data = list(), group_flag = 0, .delim = c("{", "}"), 
                                   .valid_operators = NULL, .user_funs = NULL){
                                  
  op_all = cpp_parse_simple_operations(operations_string, .delim)
    
  if(length(op_all) == 0){
    return(x)
  }
  
  if(identical(op_all[1], "_ERROR_")){
    op_msg = "'arg'op1, op2"
    if(op =='~') op_msg = "~(op1, op2)" 
    if(op == "if") op_msg = "if(cond ; op1, op2 ; op3, op4)"
    
    last = gsub("_;;;_", ";", tail(op_all, 1))
    extra = ""
    if(last %in% c("!", "?")){
      extra = string_magic("The character {bq?last} is forbidden in operations.")
    } else if(last != "_ERROR_"){
      extra = string_magic("Operations must be of the form `'arg'operator.option` but",
                     " the value {bq?last} is ill formed.")
    } else {
      extra = string_magic("The value {bq?operations_string} could not be parsed.")
    }
      
    msg = string_magic("The operator {bq?op} expects a suite of valid operations (format: {bq?op_msg}). ",
               "\nPROBLEM: the operations were not formatted correctly. ", extra)
    
    .stop_hook(msg)
  }
  
  #
  # applying the operations
  #
  
  xi = x

  for(i in seq_along(op_all)){
    opi = op_all[i]

    op_parsed = sma_char2operator(opi, .valid_operators)

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
                                    group_flag = group_flag, .delim = .delim, .envir = .envir,
                                    .user_funs = .user_funs, .valid_operators = .valid_operators),
                      "In the operation `{op}()`, the ",
                      "{&length(op_all) == 1 ; operation ; chain of operations} ",
                      " {bq?operations_string} led to a problem.", 
                      "\nPROBLEM: the operation {opi} failed. Look up the doc?")
    } else {
      xi = sma_operators(xi, op_parsed$operator, op_parsed$options, argument, 
                         group_flag = group_flag, .delim = .delim, .envir = .envir,
                         .user_funs = .user_funs, .valid_operators = .valid_operators)
    }
  }

  res = xi

}

setup_operations = function(){
  OPERATORS = c("s", "S", "split", "Split", "x", "X", "extract", 
                "c", "C", "collapse", "Collapse", "r", "R", "replace", "clean",
                "times", "each", "align", "fill", "join", "escape",
                "~", "if", "vif",
                "upper", "lower", "q", "Q", "bq", 
                "format", "Format", "%",
                "erase", "rm", "nuke", "append", "paste", "insert", 
                "k", "shorten", "Shorten", 
                "K", "head", "last", "first",
                "firstchar", "lastchar", "unik", "num", "enum",
                "rev", "sort", "dsort", "ascii", "title",
                "ws", "tws", "trim", "get", "is", "which",
                "n", "N", "len", "Len", "width", "difftime",
                "stopwords", "nth", "Nth", "ntimes", "Ntimes")
                
  options("string_magic_operations_v1.0.0" = sort(OPERATORS))
  
  # dp: v1.1.0
  # swidth: v1.1.3
  OPERATORS = c(OPERATORS, "dp", "deparse", "swidth")
  
  options("string_magic_operations_default" = sort(OPERATORS))
  
  options("string_magic_user_ops" = NULL)
}

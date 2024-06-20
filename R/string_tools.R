#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Aug  8 16:26:42 2022
# ~: string tools
#----------------------------------------------#


#' Chains basic operations to character vectors
#'
#' Simple tool to perform multiple operations to character vectors.
#' 
#' @inheritParams string_clean
#'
#' @param x A character vector. If not a character vector but atomistic (i.e. not a list), 
#' it will be converted to a character vector.
#' @param ... Character **scalars**. Character scalar containing the comma separated values 
#' of operations to perform to the vector. The 50+ operations are detailed in the help
#' page of [string_magic()].
#' @param op Character **vector** or `NULL` (default). Character scalar containing the comma separated values 
#' of operations to perform to the vector. The 50+ operations are detailed in the help
#' page of [string_magic()]. Note that if this argument is provided, then the values in 
#' `...` are ignored.
#' @param pre_unik Logical scalar, default is `NULL`. Whether to first unique the vector 
#' before applying the possibly costly string operations, and merging back the result. 
#' For very large vectors with repeated values the time gained can be substantial. By 
#' default, this is `TRUE` for vector of length 1M or more.
#' 
#' @details 
#' This function is a simple wrapper around string_magic. Formally, `string_ops(x, "op1, op2")`
#' is equivalent to `string_magic("{op1, op2 ? x}")`.
#'
#' @return
#' In general it returns a character vector. It may be of a length different from the original
#'  one, depending on the operations performed. 
#'
#' @author
#' Laurent R. Berge
#' 
#' @inherit string_clean seealso
#' 
#' @family tools with aliases
#'
#' @examples
#' 
#' # data on car models
#' cars = row.names(mtcars)
#' 
#' # let's get the brands starting with an "m"
#' string_ops(cars, "'i/^m'get, x, unik")
#' 
#' # Explainer:
#' # 'i/^m'get: keeps only the elements starting with an m,
#' #            i/ is the 'regex-flag' "ignore" to ignore the case
#' #            ^m means "starts with an m" in regex language
#' # x: extracts the first pattern. The default pattern is "[[:alnum:]]+"
#' #    which means an alpha-numeric word
#' # unik: applies unique() to the vector
#' # => see help in ?string_magic for more details on the operations
#' 
#' 
#' # let's get the 3 largest numbers appearing in the car models
#' string_ops(cars, "'\\d+'x, rm, unik, num, dsort, 3 first")
#' 
#' # Explainer:
#' # '\\d+'x: extracts the first pattern, the pattern meaning "a succession"
#' #          of digits in regex language
#' # rm: removes elements equal to the empty string (default behavior)
#' # unik: applies unique() to the vector
#' # num: converts to numeric
#' # dsort: sorts in decreasing order
#' # 3 first: keeps only the first three elements
#' 
#' # You can use several character vectors as operations:
#' string_ops(cars, 
#'            "'\\d+'x, rm, unik",
#'            "num, dsort, 3 first")
#' 
string_ops = function(x, ..., op = NULL, pre_unik = NULL, namespace = NULL, envir = parent.frame()){

  if(missing(x)){
    stop("Argument `x` must be provided. PROBLEM: it is currently missing.")
  }

  if(!is.atomic(x)){
    stop("Argument `x` must be atomic. Currently it is of class `", class(x)[1], "`")
  }
  
  if(length(x) == 0){
    return(x)
  }

  if(!true_character(x)){
    x = as.character(x)
  }
  
  set_pblm_hook()
  
  check_character(op, null = TRUE, no_na = TRUE)
  if(!is.null(op)){
    all_ops = op
  } else {
    all_ops = check_set_dots(..., mbt = TRUE, scalar = TRUE, character = TRUE)
    all_ops = unlist(all_ops)
  }
  
  check_logical(pre_unik, null = TRUE, scalar = TRUE)

  # For very large vectors, we unique
  n = length(x)
  if(is.null(pre_unik)){
    pre_unik = n > 1e6
  }

  if(pre_unik){
    x_int = to_integer(x)
    x_small = x[!duplicated(x_int)]

    res_small = string_ops(x_small, op = all_ops, pre_unik = FALSE, 
                           namespace = namespace, envir = envir)
    res = res_small[x_int]
  } else {
    
    if(is.null(namespace)){
      namespace = "R_GlobalEnv"
    }
    
    user_ops_all = getOption("string_magic_user_ops")
    # beware the sneaky assignment!
    if(!is.null(user_ops_all) && !is.null(user_info <- user_ops_all[[namespace]])){
      .user_funs = user_info$funs
      .valid_operators = user_info$operators          
    } else {
      .user_funs = NULL
      .valid_operators = getOption("string_magic_operations_default")
    }
    
    res = x
    for(op in all_ops){
      group_flag = 1 * grepl("~", op, fixed = TRUE)
      res = apply_simple_operations(res, "string_ops", op, .check = TRUE, .envir = envir,
                                    .data = list(), group_flag = group_flag, 
                                    .delim = c("{", "}"), .user_funs = .user_funs, 
                                    .valid_operators = .valid_operators)  
    }
    
  }

  if("group_index" %in% names(attributes(res))){
    attr(res, "group_index") = NULL
  }

  res
}





#' Detects whether a pattern is in a character string
#'
#' Function that detects if one or more patterns are in a string. The patterns can be 
#' chained, by default this is a regex search but special flags be triggered with a 
#' specific syntax, supports negation.
#'
#' @param x A character vector.
#' @param ... Character scalars representing the patterns to be found. By default they are (perl) regular-expressions.
#' Use ' & ' or ' | ' to chain patterns and combine their result logically (ex: `'[[:alpha:]] & \\d'` gets strings
#' containing both letters and numbers). You can negate by adding a `!` first (ex: `"!sepal$"` will 
#' return `TRUE` for strings that do not end with `"sepal"`).
#' Add flags with the syntax 'flag1, flag2/pattern'. Available flags are: 'fixed', 'ignore', 'word' and 'magic'.
#' Ex: "ignore/sepal" would get "Sepal.Length" (wouldn't be the case w/t 'ignore'). 
#' Shortcut: use the first letters of the flags. Ex: "if/dt[" would get `"DT[i = 5]"` (flags 'ignore' + 'fixed').
#' For 'word', it adds word boundaries to the pattern. The `magic` flag first interpolates
#' values directly into the pattern with "{}".
#' @param or Logical, default is `FALSE`. In the presence of two or more patterns, 
#' whether to combine them with a logical "or" (the default is to combine them with a logical "and").
#' @param pattern (If provided, elements of `...` are ignored.) A character vector representing the 
#' patterns to be found. By default a (perl) regular-expression search is triggered. 
#' Use ' & ' or ' | ' to chain patterns and combine their result logically (ex: `'[[:alpha:]] & \\d'` gets strings
#' containing both letters and numbers). You can negate by adding a `!` first (ex: `"!sepal$"` will 
#' return `TRUE` for strings that do not end with `"sepal"`).
#' Add flags with the syntax 'flag1, flag2/pattern'. Available flags are: 'fixed', 'ignore', 'word' and 'magic'.
#' Ex: "ignore/sepal" would get "Sepal.Length" (wouldn't be the case w/t 'ignore'). 
#' Shortcut: use the first letters of the flags. Ex: "if/dt[" would get `"DT[i = 5]"` (flags 'ignore' + 'fixed').
#' For 'word', it adds word boundaries to the pattern. The `magic` flag first interpolates
#' values directly into the pattern with "{}".
#' @param fixed Logical scalar, default is `FALSE`. Whether to trigger a fixed search instead of a 
#' regular expression search (default).
#' @param word Logical scalar, default is `FALSE`. If `TRUE` then a) word boundaries are added to the pattern, 
#' and b) patterns can be chained by separating them with a comma, they are combined with an OR logical operation.
#' Example: if `word = TRUE`, then pattern = "The, mountain" will select strings containing either the word
#' 'The' or the word 'mountain'.
#' @param ignore.case Logical scalar, default is `FALSE`. If `TRUE`, then case insensitive search is triggered. 
#' @param last A function or `NULL` (default). If a function, it will be applied to the vector 
#' just before returning it.
#' @param envir Environment in which to evaluate the interpolations if the flag `"magic"` is provided.
#' Default is `parent.frame()`.
#' 
#' @details
#' The internal function used to find the patterns is [base::grepl()] with `perl = TRUE`.
#' 
#' @section Generic regular expression flags:
#' 
#' All `stringmagic` functions support generic flags in regular-expression patterns. 
#' The flags are useful to quickly give extra instructions, similarly to *usual* 
#' [regular expression flags](https://javascript.info/regexp-introduction).
#' 
#' Here the syntax is "flag1, flag2/pattern". That is: flags are a comma separated list of flag-names 
#' separated from the pattern with a slash (`/`). Example: `string_which(c("hello...", "world"), "fixed/.")` returns `1`. 
#' Here the flag "fixed" removes the regular expression meaning of "." which would have otherwise meant *"any character"*.
#' The no-flag verion `string_which(c("hello...", "world"), ".")` returns `1:2`.
#' 
#' Alternatively, and this is recommended, you can collate the initials of the flags instead of using a
#' comma separated list. For example: "if/dt[" will apply the flags "ignore" and "fixed" to the pattern "dt[".
#' 
#' The four flags always available are: "ignore", "fixed", "word" and "magic". 
#' 
#' + "ignore" instructs to ignore the case. Technically, it adds the perl-flag "(?i)" 
#' at the beginning of the pattern.
#' 
#' + "fixed" removes the regular expression interpretation, so that the characters ".", "$", "^", "[" 
#' (among others) lose their special meaning and are treated for what they are: simple characters. 
#' 
#' + "word" adds word boundaries (`"\\b"` in regex language) to the pattern. Further, the comma (`","`) 
#' becomes a word separator. Technically, "word/one, two" is treated as "\\b(one|two)\\b". Example: 
#' `string_clean("Am I ambushed?", "wi/am")` leads to " I ambushed?" thanks to the flags "ignore" and "word".
#'
#' + "magic" allows to interpolate variables inside the pattern before regex interpretation. 
#' For example if `letters = "aiou"` then `string_clean("My great goose!", "magic/[{letters}] => e")` 
#' leads to `"My greet geese!"`
#'
#' @return
#' It returns a logical vector of the same length as `x`.
#' 
#' The function `string_which` returns a numeric vector. 
#' 
#' @author 
#' Laurent R. Berge
#' 
#' @inherit string_clean seealso
#'
#' @examples
#' 
#' # NOTA: using `string_get` instead of `string_is` may lead to a faster understanding 
#' #       of the examples 
#'
#' x = string_vec("One, two, one... two, microphone, check")
#'
#' # default is regular expression search
#' # => 3 character items
#' string_is(x, "^...$")
#'
#' # to trigger fixed search use the flag 'fixed'
#' string_is(x, "fixed/...")
#' # you can just use the first letter
#' string_is(x, "f/...")
#'
#' # to negate, use '!' as the first element of the pattern
#' string_is(x, "f/!...")
#'
#' # you can combine several patterns with " & " or " | "
#' string_is(x, "one & c")
#' string_is(x, "one | c")
#' 
#' #
#' # word: adds word boundaries
#' #
#' 
#' # compare
#' string_is(x, "one")
#' # with
#' string_is(x, "w/one")
#' 
#' # words can be chained with commas (it is like an OR logical operation)
#' string_is(x, "w/one, two")
#' # compare with
#' string_is(x, "w/one & two")
#' # remember that you can still negate
#' string_is(x, "w/one & !two")
#' 
#' # you can combine the flags
#' # compare
#' string_is(x, "w/one")
#' # with
#' string_is(x, "wi/one")
#' 
#' #
#' # the `magic` flag
#' #
#' 
#' p = "one"
#' string_is(x, "m/{p}")
#' # Explanation:
#' # - "p" is interpolated into "one"
#' # - we get the equivalent: string_is(x, "one")
#' 
#'
#' #
#' # string_which
#' #
#'
#' # it works exactly the same way as string_is
#' # Which are the items containing an 'e' and an 'o'?
#' string_which(x, "e", "o")
#' # equivalently
#' string_which(x, "e & o")
#'
#'
string_is = function(x, ..., fixed = FALSE, ignore.case = FALSE, word = FALSE, 
                     or = FALSE, pattern = NULL, envir = parent.frame(), last = NULL){

  x = check_set_character(x, mbt = TRUE, l0 = TRUE)
  if(length(x) == 0){
    return(logical(0))
  }
  
  check_logical(ignore.case, scalar = TRUE)
  check_logical(fixed, scalar = TRUE)
  check_logical(word, scalar = TRUE)
  check_logical(or, scalar = TRUE)
  check_character(pattern, null = TRUE, no_na = TRUE)
  check_function(last, null = TRUE)

  if(missnull(pattern)){
    dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)
    if(!is.null(names(dots))){
      if(is.null(last)){
        warn_no_named_dots(dots)
      } else {
        # we remove the arguments that go to "last"
        dot_names = names(dots)
        args_last = names(formals(args(last)))
        if(!is.null(args_last)){
          args_ok = intersect(dot_names, args_last)
          dots[args_ok] = NULL
          if(sum(nchar(dot_names) > 0) != length(args_ok)){
            warn_no_named_dots(dots, extra_args = args_last, extra_funName = "last")
          }
        }        
      }
    }
    
    pattern = unlist(dots)
  }

  or_origin = or
  negate = FALSE
  logical_op = function(a, b, or) if(or) a | b else a & b

  res = NULL
  for(i in seq_along(pattern)){
    pat = pattern[i]
    first_char = substr(pat, 1, 1)
    
    pat_parsed = parse_regex_pattern(pat, c("fixed", "word", "ignore", "magic"), 
                                     envir = envir)
    is_or = pat_parsed$is_or
    flags = pat_parsed$flags
    all_patterns = pat_parsed$patterns
    negate_all = pat_parsed$is_not

    is_fixed_origin = fixed || "fixed" %in% flags
    is_word = word || "word" %in% flags
    is_ignore = ignore.case || "ignore" %in% flags
    
    res_current = NULL
    n_pat = length(all_patterns)
    for(j in 1:n_pat){
      is_fixed = is_fixed_origin
      p = all_patterns[j]
      sub_negate = negate_all[j]
      
      p = format_pattern(p, fixed = is_fixed, word = is_word, ignore = is_ignore) 
      is_fixed = attr(p, "fixed")
      
      res_tmp = tryCatch(grepl(p, x, perl = !is_fixed, fixed = is_fixed), 
                         error = function(e) structure(conditionMessage(e), class = "try-error"),
                         warning = function(w) structure(conditionMessage(w), class = "try-warning"))
        
      is_warn = inherits(res_tmp, "try-warning")
      warn_msg = ""
      if(is_warn){
        # is there an error?
        warn_msg = res_tmp
        res_tmp = tryCatch(suppressWarnings(grepl(p, x, perl = !is_fixed, fixed = is_fixed)), 
                           error = function(e) structure(conditionMessage(e), class = "try-error"))
      }
      
      if(inherits(res_tmp, "try-error")){
        pat_raw = pattern[i]
        stopi("CONTEXT: {&p != pat_raw;evaluation of {Q?pat_raw}\n         }",
              "pattern = {Q?p} ",
              "\nEXPECTATION: the pattern must be a valid regular expression",
              "\nPROBLEM: `grepl` led to an error, see below:",
              "\n{res_tmp}",
              "{&nchar(warn_msg) > 0;\n{.}}")
      } else if(is_warn){
        pat_raw = pattern[i]
        warni("CONTEXT: {&p != pat_raw;evaluation of {Q?pat_raw}\n         }",
              "pattern = {Q?p} ",
              "\nA warning was raised when evaluating the pattern:",
              "\n{warn_msg}", immediate. = TRUE)
      }
      
      if(sub_negate){
        res_tmp = !res_tmp
      }
      if(is.null(res_current)){
        res_current = res_tmp
      } else {
        res_current = logical_op(res_current, res_tmp, is_or[j])
      }
    }

    if(is.null(res)){
      res = res_current
    } else {
      res = logical_op(res, res_current, or)
    }
  }
  
  if(!is.null(last)){
    res = check_set_eval_fun(last, res, ...)
  }

  res
}

#' @describeIn string_is Detects if at least one element of a vector matches a regex pattern
string_any = function(x, ..., fixed = FALSE, ignore.case = FALSE, word = FALSE, 
                     or = FALSE, pattern = NULL, envir = parent.frame()){

  check_character(pattern, null = TRUE, no_na = TRUE)

  if(missnull(pattern)){
    dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)
    warn_no_named_dots(dots)
    pattern = unlist(dots)
  }

  any(string_is(x, fixed = fixed, ignore.case = ignore.case, word = word, 
               or = or, pattern = pattern, envir = envir))
}

#' @describeIn string_is Detects if all elements of a vector match a regex pattern
string_all = function(x, ..., fixed = FALSE, ignore.case = FALSE, word = FALSE, 
                     or = FALSE, pattern = NULL, envir = parent.frame()){

  check_character(pattern, null = TRUE, no_na = TRUE)

  if(missnull(pattern)){
    dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)
    warn_no_named_dots(dots)
    pattern = unlist(dots)
  }

  all(string_is(x, fixed = fixed, ignore.case = ignore.case, word = word, 
               or = or, pattern = pattern, envir = envir))
}

#' @describeIn string_is Returns the indexes of the values in which a pattern is detected
string_which = function(x, ..., fixed = FALSE, ignore.case = FALSE, word = FALSE, 
                     or = FALSE, pattern = NULL, envir = parent.frame()){

  check_character(pattern, null = TRUE, no_na = TRUE)

  if(missnull(pattern)){
    dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)
    warn_no_named_dots(dots)
    pattern = unlist(dots)
  }

  which(string_is(x, fixed = fixed, ignore.case = ignore.case, word = word, 
               or = or, pattern = pattern, envir = envir))
}

#' Gets elements of a character vector
#'
#' Convenient way to get elements from a character vector.
#'
#' @inheritParams string_is
#'
#' @param x A character vector.
#' @param seq Logical, default is `FALSE`. The argument `pattern` accepts a vector of 
#' patterns which are combined with an `and` by default. If `seq = TRUE`, then it is like 
#' if `string_get` was called sequentially with its results stacked. See examples.
#' @param seq.unik Logical, default is `FALSE`. The argument `...` (or the argument `pattern`) accepts 
#' a vector of patterns which are combined with an `and` by default. If `seq.unik = TRUE`, then 
#' `string_get` is called sequentially with its results stacked, and `unique()` is 
#' applied in the end. See examples.
#' 
#' @details 
#' This function is a wrapper to [string_is()].
#' 
#' @inheritSection string_is Generic regular expression flags
#' 
#' @section Caching:
#' 
#' In an exploratory stage, it can be useful to quicky get values from a vector with the 
#' least hassle as possible. Hence `string_get` implements caching, so that users do not need
#' to repeat the value of the argument `x` in successive function calls, and can concentrate
#' only on the selection patterns.
#' 
#' Caching is a feature only available when the user calls `string_get` from the global environment.
#' If that feature were available in regular code, it would be too dangerous, likely leading to hard to debug bugs. 
#' Hence caching is disabled when used within code (i.e. inside a function or inside an 
#' automated script), and function calls without the main argument will lead to errors in such scripts.
#' 
#' @author 
#' Laurent R. Berge
#'
#' @return
#' It always return a character vector.
#' 
#' @inherit string_clean seealso
#' 
#'
#' @examples
#'
#' x = rownames(mtcars)
#'
#' # find all Mazda cars
#' string_get(x, "Mazda")
#' # same with ignore case flag
#' string_get(x, "i/mazda")
#' 
#' # all cars containing a single digit (we use the 'word' flag)
#' string_get(x, "w/\\d")
#'
#' # finds car names without numbers AND containing `u`
#' string_get(x, "!\\d", "u")
#' # equivalently
#' string_get(x, "!\\d & u")
#'
#' # Stacks all Mazda and Volvo cars. Mazda first
#' string_get(x, "Mazda", "Volvo", seq = TRUE)
#'
#' # Stacks all Mazda and Volvo cars. Volvo first
#' string_get(x, "Volvo", "Mazda", seq = TRUE)
#'
#' # let's get the first word of each car name
#' car_first = string_ops(x, "extract.first")
#' # we select car brands ending with 'a', then ending with 'i'
#' string_get(car_first, "a$", "i$", seq = TRUE)
#' # seq.unik is similar to seq but applies unique()
#' string_get(car_first, "a$", "i$", seq.unik = TRUE)
#' 
#' #
#' # flags
#' #
#' 
#' # you can combine the flags
#' x = string_magic("/One, two, one... Two!, Microphone, check")
#' # regular
#' string_get(x, "one")
#' # ignore case
#' string_get(x, "i/one")
#' # + word boundaries
#' string_get(x, "iw/one")
#' 
#' # you can escape the meaning of ! with backslashes
#' string_get(x, "\\!")
#' 
#' #
#' # Caching
#' #
#' 
#' # Caching is enabled when the function is used interactively
#' # so you don't need to repeat the argument 'x'
#' # Mostly useful at an exploratory stage
#' 
#' if(interactive() && is.null(sys.calls())){
#'    
#'    # first run, the data is cached
#'    string_get(row.names(mtcars), "i/vol")
#' 
#'    # now you don't need to specify the data
#'    string_get("i/^m & 4")
#' }
#'
#'
#'
#'
string_get = function(x, ..., fixed = FALSE, ignore.case = FALSE, word = FALSE, 
                   or = FALSE, seq = FALSE, seq.unik = FALSE, 
                   pattern = NULL, envir = parent.frame()){

  x = check_set_character(x, mbt = TRUE, l0 = TRUE)
  if(length(x) == 0){
    return(character(0))
  }
  
  # data caching in interactive mode
  is_caching = FALSE
  is_forced_caching = isTRUE(getOption("string_magic_string_get_forced_caching"))
  if(is_forced_caching || (interactive() && identical(parent.frame(), .GlobalEnv))){
    mc = match.call()
    if(is.character(mc$x) && !is.null(getOption("stringmagic_string_get_cache"))){
      is_caching = TRUE
      x_pattern = x
      x = getOption("stringmagic_string_get_cache")
    } else if(length(x) > 1){
      options(stringmagic_string_get_cache = x)
    }
  }

  check_character(pattern, null = TRUE, no_na = TRUE)
  check_logical(or, scalar = TRUE)
  check_logical(seq, scalar = TRUE)
  check_logical(seq.unik, scalar = TRUE)

  if(missnull(pattern)){
    if(is_caching){
      dots = check_set_dots(..., mbt = FALSE, character = TRUE, scalar = TRUE, no_na = TRUE)
      dots = append(dots, x_pattern, 0)
    } else {
      dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)  
    }
    
    warn_no_named_dots(dots)
    pattern = unlist(dots)
  }

  if(seq.unik){
    seq = TRUE
  }

  if(seq){
    for(i in seq_along(pattern)){
      value = string_get(x, pattern = pattern[i], or = or, seq = FALSE, 
                      fixed = fixed, ignore.case = ignore.case, 
                      word = word, envir = envir)
      if(i == 1){
        res = value
      } else {
        res = c(res, value)
      }
    }

    if(seq.unik){
      res = unique(res)
    }

    return(res)
  }

  index = string_is(x, fixed = fixed, ignore.case = ignore.case, word = word, 
                 pattern = pattern, or = or, envir = envir)
  x[index]
}




#' Splits a character string wrt a pattern
#' 
#' Splits a character string with respect to pattern
#' 
#' @inheritParams string_split2df
#' 
#' @param x A character vector.
#' @param simplify Logical scalar, default is `TRUE`. If `TRUE`, then when the vector input `x`
#'  is of length 1, a character vector is returned instead of a list.
#' 
#' @inheritSection string_is Generic regular expression flags
#' 
#' @return 
#' If `simplify = TRUE` (default), the object returned is:
#' + a character vector if `x`, the vector in input, is of length 1: the character vector contains
#' the result of the split.
#' + a list of the same length as `x`. The ith element of the list is a character vector
#' containing the result of the split of the ith element of `x`.
#' 
#' If `simplify = FALSE`, the object returned is always a list.
#' 
#' @examples 
#' 
#' time = "This is the year 2024."
#' 
#' # we break the sentence
#' string_split(time, " ")
#' 
#' # simplify = FALSE leads to a list
#' string_split(time, " ", simplify = FALSE)
#' 
#' # let's break at "is"
#' string_split(time, "is")
#' 
#' # now breaking at the word "is"
#' # NOTE: we use the flag `word` (`w/`)
#' string_split(time, "w/is")
#' 
#' # same but using a pattern from a variable
#' # NOTE: we use the `magic` flag
#' pat = "is"
#' string_split(time, "mw/{pat}")
#' 
#' 
string_split = function(x, split, simplify = TRUE, fixed = FALSE,
                        ignore.case = FALSE, word = FALSE, 
                        envir = parent.frame()){
  
  x = check_set_character(x, mbt = TRUE, l0 = TRUE)

  check_logical(simplify, scalar = TRUE)
  check_logical(fixed, scalar = TRUE)
  check_logical(ignore.case, scalar = TRUE)
  check_logical(word, scalar = TRUE)
  
  pat_parsed = format_simple_regex_flags(split, ignore = ignore.case, 
                                         fixed = fixed, word = word, 
                                         magic = TRUE, envir = envir)
  split = pat_parsed$pattern
  is_fixed = pat_parsed$fixed

  x_split = strsplit(x, split, fixed = is_fixed, perl = !is_fixed)
  
  if(simplify && length(x) == 1){
    x_split = x_split[[1]]
  }
  
  x_split
}


#' Splits a character vector into a data frame
#'
#' Splits a character vector and formats the resulting substrings into a data.frame
#' 
#' @inheritParams string_is
#'
#' @param x A character vector or a two-sided formula. If a two-sided formula, then the 
#' argument `data` must be provided since the variables will be fetched in there. 
#' A formula is of the form `char_var ~ id1 + id2` where `char_var` on the left is a 
#' character variable and on the right `id1` and `id2` are identifiers which will be 
#' included in the resulting table. Alternatively, you can provide identifiers via 
#' the argument `id`.
#' @param data Optional, only used if the argument `x` is a formula. It should 
#' contain the variables of the formula.
#' @param split A character scalar. Used to split the character vectors. By default 
#' this is a regular expression. You can use flags in the pattern in the form `flag1, flag2/pattern`.
#' Available flags are `ignore` (case), `fixed` (no regex), word (add word boundaries), 
#' magic (add interpolation with `"{}"`). Example:
#' if "ignore/hello" and the text contains "Hello", it will be split at "Hello". 
#' Shortcut: use the first letters of the flags. Ex: "iw/one" will split at the word 
#' "one" (flags 'ignore' + 'word').
#' @param id Optional. A character vector or a list of vectors. If provided, the 
#' values of `id` are considered as identifiers that will be included in the resulting table.
#' @param add.pos Logical, default is `FALSE`. Whether to include the position of each split element.
#' @param id_unik Logical, default is `TRUE`. In the case identifiers are provided, 
#' whether to trigger a message if the identifiers are not unique. Indeed, if
#'  the identifiers are not unique, it is not possible to reconstruct the original texts.
#' @param fixed Logical, default is `FALSE`. Whether to consider the argument `split` 
#' as fixed (and not as a regular expression).
#' @param dt Logical, default is `FALSE`. Whether to return a `data.table`. See also the function `string_split2dt`.
#' @param ... Not currently used.
#'
#' @return
#' It returns a `data.frame` or a `data.table` which will contain: i) `obs`: the observation index, 
#' ii) `pos`: the position of the text element in the initial string (optional, via add.pos), 
#' iii) the text element, iv) the identifier(s) (optional, only if `id` was provided).
#' 
#' @inherit string_clean seealso
#' 
#' @examples
#'
#' x = c("Nor rain, wind, thunder, fire are my daughters.",
#'       "When my information changes, I alter my conclusions.")
#'
#' id = c("ws", "jmk")
#'
#' # we split at each word
#' string_split2df(x, "[[:punct:] ]+")
#'
#' # we add the 'id'
#' string_split2df(x, "[[:punct:] ]+", id = id)
#'
#' # TO NOTE:
#' # - the second argument is `data`
#' # - when it is missing, the argument `split` becomes implicitly the second
#' # - ex: above we did not use `split = "[[:punct:] ]+"`
#'
#' #
#' # using the formula
#'
#' base = data.frame(text = x, my_id = id)
#' string_split2df(text ~ my_id, base, "[[:punct:] ]+")
#'
#' #
#' # with 2+ identifiers
#'
#' base = within(mtcars, carname <- rownames(mtcars))
#'
#' # we have a message because the identifiers are not unique
#' string_split2df(carname ~ am + gear + carb, base, " +")
#'
#' # adding the position of the words & removing the message
#' string_split2df(carname ~ am + gear + carb, base, " +", id_unik = FALSE, add.pos = TRUE)
#'
#'
string_split2df = function(x, data = NULL, split = NULL, id = NULL, add.pos = FALSE,
                           id_unik = TRUE, fixed = FALSE, ignore.case = FALSE,
                           word = FALSE, envir = parent.frame(), dt = FALSE, ...){

  if(missing(x)){
    stop("Argument 'x' must be provied. PROBLEM: it is missing.")
  }

  if(missnull(data) && missnull(split)){
    stop("Argument `split` must be provided. PROBLEM: it is missing.")
  }

  if(!missnull(data) && is.character(data) && length(data) == 1 && missnull(split)){
    # argument sliding
    split = data
    data = NULL
  }

  #
  # Formatting the input data
  #

  # used for the names
  dots = list(...)
  internal = FALSE
  if("mc" %in% names(dots)){
    mc = dots$mc
    internal = TRUE
  } else {
    mc = match.call()
  }

  id_names = NULL
  x_name = NULL
  if(inherits(x, "formula")){

    fml = x

    if(!missnull(data) && !is.list(data)){
      stop("When `x` is a formula, the argument `data` must be a list containing ",
           "the variables in the formula.",
           "\nPROBLEM: `data` is not a list.")
    }

    if(length(fml) < 3){
      stop("When `x` is a formula, it must be two sided (on the left the variable,",
           " on the right the identifier).\nPROBLEM: it is currently only one sided.")
    }

    x = try(eval(fml[[2]], data, enclos = parent.frame()), silent = TRUE)

    # error handling
    if(inherits(x, "try-error") || !is.atomic(x)){
      vars = all.vars(fml[[2]])

      if(!is.null(names(data))){
        var_pblm = setdiff(vars, names(data))
        if(length(var_pblm) > 0){
          stop("The evaluation of the left side of `x` raised an error:\n",
               string_magic("PROBLEM: the variable{$s, enum.bq, is ? var_pblm} not in the data set (`"),
               deparse_short(mc$data), "`).")
        }
      }

      if(inherits(x, "try-error")){
        stop("The evaluation of the left side of `x` raised an error:\n", x)
      }

      stop("The evaluation of the left side of `x` raised an error:\n",
           "PROBLEM: `x` is not a character vector, instead it is of class ", class(x)[1])
    }

    #
    # elements from id
    # we don't apply terms: we only separate with sum

    terms_right = fml_extract_elements(fml)

    id = list()
    for(i in seq_along(terms_right)){
      term = terms_right[[i]]
      id_name = deparse_short(term)

      val = try(eval(term, data, enclos = parent.frame()), silent = TRUE)

      # error handling
      if(inherits(val, "try-error") || !is.atomic(val)){
        vars = all.vars(term)

        intro = string_magic("The evaluation of the right side of `x` raised an error.\n",
                    "VALUE TO EVAL: {bq ? id_name}\n")

        if(!is.null(names(data))){
          var_pblm = setdiff(vars, names(data))
          if(length(var_pblm) > 0){
            stop(intro,
                 string_magic("PROBLEM: the variable{$s, enum.bq, is ? var_pblm} not in the data set (`"),
                 deparse_short(mc$data), "`).")
          }
        }

        if(inherits(val, "try-error")){
          stop(intro, "ERROR: ", val)
        }

        stop(intro,
             "PROBLEM: this identifier is not atomic, instead it is of class ", class(x)[1])
      }

      id[[id_name]] = val
    }
    
    if(!is.character(x)){
      x = as.character(x)
    }

    x_name = deparse_short(fml[[2]])
    id_names = names(id)

  } else {
    check_set_character(x)
    x_name = "x"
  }

  n = length(x)
  if(!missnull(id)){
    # id: a) a vector, b) a data frame. Same length as x

    if(is.list(id)){
      n_id = unique(lengths(id))
      if(length(n_id) != 1 || max(n_id) != n){
        extra = ""
        if(max(n_id) != n) extra = string_magic("\nPROBELM: len x: {#n ? n} len id: {#n ? max(n_id)}.")
        stop("The argument `id` must be either a vector of identifiers or a data.frame ",
             "of identifiers of the same length as `x`.", extra)
      }

      id_names = names(id)
      if(is.null(id_names)){
        id_txt = deparse_short(mc$id)
        if(length(id_names) > 1){
          id_names = paste0(id_txt, "..", 1:length(id))
        } else {
          id_names = id_txt
        }
      }

    } else {
      n_id = length(id)
      if(n != n_id){
        stopi("The argument `id` must be either a vector of identifiers or a data.frame ",
             "of identifiers of the same length as `x`.",
             "\nPROBELM: len x: {n ? n}; len id: {n ? n_id}.")
      }
      if(!is.atomic(id)){
        stop("The argument `id` must be either a vector of identifiers or a data.frame ",
             "of identifiers of the same length as `x`.",
             "\nPROBLEM: `id` is not an atomic vector")
      }

      id_names = deparse_short(mc$id)
    }
  }

  add.id = FALSE
  obs = 1:n
  if(!missnull(id)){
    add.id = TRUE
    id_raw = id
    id = to_integer(id)

    if(id_unik && max(id) != length(id)){
      message("The identifiers are not unique, you will not be able to reconstruct the data using only them.")
    }

  }
  
  # flags
  pat_parsed = format_simple_regex_flags(split, ignore = ignore.case, 
                                         fixed = fixed, word = word, 
                                         magic = TRUE, envir = parent.frame(1 + internal))
  split = pat_parsed$pattern
  is_fixed = pat_parsed$fixed

  x_split = strsplit(x, split, fixed = is_fixed, perl = !is_fixed)

  n_all = lengths(x_split)

  # the result table
  if(add.pos){
    obs_all = rep(obs, n_all)
    pos = cpp_create_pos(obs_all)
    res = data.frame(obs = obs_all, pos = pos, x = unlist(x_split),
                     row.names = NULL)
  } else {
    res = data.frame(obs = rep(obs, n_all), x = unlist(x_split),
                     row.names = NULL)
  }


  # we add the names
  names(res)[names(res) == "x"] = x_name

  # we add the ID
  if(add.id){
    # we add the user-provided identifiers

    if(!is.list(id_raw)) id_raw = list(id = id_raw)

    id2bind = lapply(id_raw, function(x) x[res$obs])
    names(id2bind) = id_names

    res = cbind(res, id2bind)
  }

  if(dt){
    if(requireNamespace("data.table", quietly = TRUE)){
      data.table::setDT(res)
    } else {
      stop("To return a `data.table`, you need the `data.table` package to be installed. Currently this is not the case.",
           "\nUse install.packages('data.table')?")
    }
  }

  res
}

#' @describeIn string_split2df Splits a string vector and returns a `data.table`
string_split2dt = function(x, data = NULL, split = NULL, id = NULL, add.pos = FALSE,
                        id_unik = TRUE, fixed = FALSE){

  mc = match.call()
  string_split2df(x, data = data, split = split, id = id, add.pos = add.pos,
               id_unik = id_unik, fixed = fixed, mc = mc, dt = TRUE)
}

#' Paste a string vector conditionally 
#' 
#' Easily reconstruct a string vector that has been split with [string_split2df()].
#' 
#' @param x A character vector or a formula. If a vector: it represents the 
#' values to be pasted together.
#' If a formula, it must be of the form
#' `my_string ~ id1 + id2` with on the left the character vector and on the right
#' the (possibly many) identifiers. If a formula, the argument `id` must be a 
#' data frame.
#' @param id A vector of identifiers, a list of identifiers (can be a data frame),
#' or a `data.frame`. The identifiers can be a vector or a list of vectors. They represent
#' which elements of `x` are to be pasted together. 
#' 
#' When `x` is a formula, then `id` must be a data.frame containing the variables in 
#' the formula.
#' @param sep A character scalar, default is `" "`. The value used to paste together 
#' the elements of `x`.
#' @param names Logical scalar, default is `TRUE`. Whether to add, as names, the values of 
#' the identifiers.
#' @param sort Logical scalar, default is `TRUE`. Whether to sort the results according to 
#' the identifiers.
#' 
#' @return 
#' Returns a character vector. If the argument `names` is `TRUE` (default), the vector will have
#' names equal to the values of the identifiers.
#' 
#' @examples 
#' 
#' #
#' # let's paste together the letters of the alphabet
#' 
#' # first we create the identifier
#' id = rep(1:2, each = 13)
#' setNames(id, letters)
#' 
#' # now we conditionally paste together the letters
#' paste_conditional(letters, id, "")
#' 
#' #
#' # using a formula
#' 
#' # we create a small data set based on mtcars
#' base_cars = within(mtcars, carname <- row.names(mtcars))
#' base_cars = head(base_cars, 10)
#' 
#' # we use two identifiers
#' paste_conditional(carname ~ gear + carb, base_cars, sep = ", ")
#' 
#' 
#' 
paste_conditional = function(x, id, sep = " ", names = TRUE, sort = TRUE){
  
  check_logical(names, scalar = TRUE)
  check_logical(sort, scalar = TRUE)
  
  check_character(sep, scalar = TRUE)
  if(missing(x)) stop("The argument `x` must be provided. Problem: it is missing.")
  if(missing(id)) stop("The argument `id` must be provided. Problem: it is missing.")
  
  if(is.character(x)){
    n_x = length(x)
    # we expect id to be a vector or a list 
    if(is.atomic(id)){
      if(length(id) != n_x){
        stopi("The argument `id` must be of the same length as `x`.",
              "\nPROBLEM: `x` is {len?x} vs `id` is {len?id}.")
      }
    } else if(is.list(id)){
      n_id_all = lengths(id)
      if(any(n_id_all != n_x)){
        i_pblm = which(n_id_all != n_x)[1]
        stopi("The argument `id` must be of the same length as `x`.",
              "\nPROBLEM: `x` is {len?x} vs `id[[{i_pblm}]]` is {n?n_id_all[i_pblm]}.")
      }
    } else {
      stopi("When `x` is a vector, `id` must be i) a vector or ii) a list of vectors.",
            "\nPROBLEM: instead `id` is of class {enum.bs?class(id)}.")
    }
    
  } else if(inherits(x, "formula")){
    if(!is.data.frame(id)){
      stopi("When `x` is a vector, `id` must be a `data.frame`.",
            "\nPROBLEM: instead `id` is of class {enum.bs?class(id)}.")
    }
    
    fml = x
    df = id
    x = check_expr(eval(fml[[2]], df), "The left-hand-side of the formula, equal to `", 
                   deparse(fml[[2]]), 
                   "` could not be evaluated. See error below:")
    x = as.character(x)
    
    term_to_eval = attr(terms(fml), "term.labels")
    n_id = length(term_to_eval)
    id = vector("list", n_id)
    for(i in 1:n_id){
      term = term_to_eval[i]
      val = check_expr(eval(str2lang(term), df), "The right-hand-side of the formula, equal to `", 
                       term, "` could not be evaluated. See error below:")
      id[[i]] = val
    }
    names(id) = term_to_eval
    
  } else {
    stopi("The argument `x` must be a character vector or a formula.",
          "\nPROBLEM: instead it is of class {enum.bq?class(x)}.")
  }
  
  id_raw = id
  id = to_integer(id, sort)
  
  # obs_first: used in names
  obs_first = which(!duplicated(id))
  obs_first_order = order(id[obs_first])
  obs_first = obs_first[obs_first_order]  
  
  # we need to order before applying the cpp function
  id_order = order(id)
  id = id[id_order]
  x = x[id_order]
  
  res = cpp_paste_conditional(x, id, sep = sep, sep_last = sep)
  
  # adding names
  if(is.atomic(id_raw) || (is.null(names(id_raw)) && length(id_raw) == 1)){
    if(!is.atomic(id_raw)){
      id_raw = id_raw[[1]]
    }
    nm = id_raw[obs_first]
  } else {
    id_raw = unclass(id_raw)
    id_names = names(id_raw)
    is_names = !is.null(id_names)
    
    for(i in 1:length(id_raw)){
      val = id_raw[[i]][obs_first]
      if(is_names){
        val = paste0(id_names[i], ": ", val) 
      }
      id_raw[[i]] = val
    }
    
    names(id_raw) = NULL
    id_raw$sep = ", "
    
    nm = do.call(paste, id_raw)
  }
  names(res) = nm
  
  res  
}




#' Cleans a character vector from multiple patterns
#'
#' Recursively cleans a character vector from several patterns. Quickly handle the 
#' tedious task of data cleaning by taking advantage of the syntax.
#' You can also apply all sorts of cleaning operations by summoning [string_ops()] operations.
#' 
#' @inheritParams string_is
#'
#' @param x A character vector.
#' @param ... Character scalars representing patterns. A pattern is of the form
#' "flags/pat1, pat2 => replacement". This means that patterns 'pat1' and 'pat2' will be replaced
#' with the string 'replacement'. By default patterns are comma separated and the replacement comes 
#' after a ' => ' (see args `split` and `pipe` to change this). By default the replacement is the empty string 
#' (so "pat1, pat2" *removes* the patterns).
#' 
#' Available regex flags are: 'word' (add word boundaries), 'ignore' (the case), 'fixed' (no regex), 
#' 'total', 'single' and 'magic'. 
#' The flag `total` leads to a *total replacement* of the string if the pattern is found. 
#' The flag 'magic' allows to interpolate variables within the pattern.  Use flags
#' with comma separation ("word, total/pat") or use only their initials ("wt/pat").
#' 
#' Starting with an '@' leads to operations in [string_ops()]. Ex: "@ascii, lower, ws" turns
#' the string into ASCII, lowers the case and normalizes white spaces (see help of [string_ops()]).
#' @param pipe Character scalar, default is `" => "`. If thevalue of `pipe` is found in a pattern,
#' then the string is split w.r.t. the pipe and anything after the pipe becomes the replacement.
#' 
#' For example in `string_clean(x, "e => a")` the default pipe is found in "e => a", so the pattern 
#' "e" will be replaced with "a". In other terms, this is equivalent to `string_clean(x, "e", replacement = "a")`.
#' Example changing the pipe: you can obtain the previous result with `string_clean(x, "e|>a", pipe = "|>")`.
#' @param split Character scalar, default is `",[ \t\n]+"` (which means a comma followed with spaces 
#' and/or new lines). By default the patterns to be replaced are comma separated, that is 
#' the pattern is split w.r.t. the argument `split` and a replacement is done for each sub-pattern.
#' 
#' Use `NULL` or the empty string to disable pattern separation.
#' 
#' For example: let's look at `string_clean(x, "w/one, two => three")`. First the flag "word" is extracted from
#' the pattern (see arg. `...`) as well as the replacement (see arg. `pipe`), leading to "one, two" the 
#' pattern to be replaced. Then the pattern is split w.r.t. `split`, leading 
#' to two patterns "one" and "two". Hence the words (thanks to the flag "w") "one" and "two" from
#' the string `x` will be replaced with "three".
#' @param replacement Character scalar, default is the empty string. It represents the default 
#' value by which the patterns found in the character strings will be replaced. For example
#' `string_clean(x, "e", replacement = "a")` turn all letters "e" in `x` into "a".
#' @param total Logical scalar, default is `FALSE`. If `TRUE`, then when a pattern is found 
#' in a string, the full string is replaced (instead of just the pattern). Note, *importantly*, 
#' that when `total = TRUE` you can use logical operators in the patterns.
#' 
#' Example: `string_clean(x, "wi/ & two, three & !four => ", total = TRUE)`
#' @param single Logical scalar, default is `FALSE`. Whether, in substitutions, to stop at 
#' the first match found. Ex: `string_clean("abc", "[[:alpha:]] => _", single = TRUE)` leads 
#' to `"_bc"`, while `string_clean("abc", "[[:alpha:]] => _")` leads to `"___"`.
#' @param namespace Character scalar or `NULL` (default). **Only useful for package developers.**
#' As a regular end-user you shouldn't care! If your package uses `string_magic`, you should care. 
#' It is useful **only** if your package uses 'custom' `string_magic` operations, set with 
#' [string_magic_register_fun()] or [string_magic_register_ops()].
#' 
#' If so pass the name of your package in this argument so that your function can access 
#' the new `string_magic` operations defined within your package.
#' @param pattern A character scalar containing a regular expression pattern to be replaced.
#' You can write the replacement directly in the string after a pipe: ' => ' (see arg. `pipe` to change this). 
#' By default the replacement is the empty string (so "pat1" *removes* the pattern).
#' 
#' Available regex flags are: 'word' (add word boundaries), 'ignore' (the case), 'fixed' (no regex), 
#' 'total', 'single' and 'magic'. 
#' The flag `total` leads to a *total replacement* of the string if the pattern is found. 
#' The flag 'magic' allows to interpolate variables within the pattern.  Use flags
#' with comma separation ("word, total/pat") or use only their initials ("wt/pat").
#' 
#' 
#'
#' @return
#' The main usage returns a character vector of the same length as the vector in input.
#' Note, however, that since you can apply arbitrary [string_ops()] operations, the length and type
#' of the final vector may depend on those (if they are used).
#' 
#' @inheritSection string_is Generic regular expression flags
#' 
#' @section Regular expression flags specific to replacement:
#' 
#' This function benefits from two specific regex flags: "total" and "single".
#' 
#' + "total" replaces the *complete string* if the pattern is found (remember that the 
#' default behavior is to replace just the pattern). 
#' 
#' + "single" performs a single substitution for each string element and stops there. 
#' Only the first match of each string is replaced. Technically we use [base::sub()]
#' internally instead of [base::gsub()].
#' 
#' @author 
#' Laurent R. Berge
#' 
#' @family tools with aliases
#' 
#' @seealso 
#' String operations: [string_is()], [string_get()], [string_clean()], [string_split2df()]. 
#' Chain basic operations with [string_ops()]. Clean character vectors efficiently
#' with [string_clean()].
#' 
#' Use [string_vec()] to create simple string vectors.
#' 
#' String interpolation combined with operation chaining: [string_magic()]. You can change `string_magic`
#' default values with [string_magic_alias()] and add custom operations with [string_magic_register_fun()].
#' 
#' Display messages while benefiting from `string_magic` interpolation with [cat_magic()] and [message_magic()].
#'
#' @examples
#'
#' x = c("hello world  ", "it's 5 am....")
#'
#' # we clean the o's and the points (we use 'fixed' to trigger fixed-search)
#' string_clean(x, "o", "f/.")
#' # equivalently
#' string_clean(x, "fixed/o, .")
#' # equivalently
#' string_clean(x, "o, .", fixed = TRUE)
#' # equivalently
#' string_clean(x, "o", ".", fixed = TRUE)
#' 
#' #
#' # chaining operations: example using cars
#' #
#' 
#' cars = row.names(mtcars)
#' new = string_clean(cars, 
#'            # replace strings containing "Maz" with Mazda
#'            "total/Maz => Mazda", 
#'            # replace the word 'Merc' with Mercedes
#'            "wi/merc => Mercedes",
#'            # replace strings containing "Merc" and a digit followed with an 'S'
#'            "t/Merc & \\dS => Mercedes S!",
#'            # put to lower case, remove isolated characters and normalize white spaces
#'            "@lower, ws.isolated")
#' 
#' cbind(cars, new)
#'
#'
string_clean = function(x, ..., replacement = "", pipe = " => ", split = ",[ \n\t]+",
                        ignore.case = FALSE, fixed = FALSE, word = FALSE, 
                        total = FALSE, single = FALSE, envir = parent.frame(), 
                        namespace = NULL){

  x = check_set_character(x, l0 = TRUE)
  if(length(x) == 0){
    return(x)
  }

  check_character(replacement, scalar = TRUE)
  check_character(pipe, scalar = TRUE)
  check_character(split, scalar = TRUE, null = TRUE)

  is_split = length(split) > 0 && nchar(split) > 0
  is_pipe = length(pipe) > 0 && nchar(pipe) > 0

  check_logical(ignore.case, scalar = TRUE)
  check_logical(fixed, scalar = TRUE)
  check_logical(word, scalar = TRUE)
  check_logical(total, scalar = TRUE)
  check_logical(single, scalar = TRUE)

  rep_main = replacement

  dots = list(...)
  warn_no_named_dots(dots)

  dots = unlist(dots)

  res = x
  for(i in seq_along(dots)){
    di = di_raw = dots[[i]]
    
    first_char = substr(di, 1, 1)
    is_string_ops = FALSE
    if(first_char == "\\" && substr(di, 2, 2) == "@"){
      di = substr(di, 2, nchar(di))
    } else if(first_char == "@" && nchar(di) > 1){
      is_string_ops = TRUE
      di = substr(di, 2, nchar(di))
    }

    if(is_string_ops){
      res = string_ops(res, di, namespace = namespace)
      next
    }
    
    pattern_pipe = FALSE
    if(is_pipe && grepl(pipe, di)){
      # we apply the pipe to 
      pattern_pipe = TRUE
      di_split = strsplit(di, pipe)[[1]]
      replacement = di_split[2]
      di = di_split[1]
    } else {
      replacement = rep_main
    }
    
    # we parse the special flags
    is_total = total
    di_parsed = parse_regex_pattern(di, c("ignore", "fixed", "word", "total", "single", "magic"), 
                                    parse_logical = FALSE, envir = envir)
    flags = di_parsed$flags
    patterns = di_parsed$patterns

    is_total = total || "total" %in% flags
    is_fixed = fixed || "fixed" %in% flags
    is_ignore = ignore.case || "ignore" %in% flags
    is_word = word || "word" %in% flags
    is_single = single || "single" %in% flags
    
    if(pattern_pipe && "magic" %in% flags){
      # we interpolate the replacement
      replacement_new = try(string_magic(replacement, .envir = envir), silent = TRUE)
      if(isError(replacement_new)){
        stop_hook("CONTEXT: concerns the pattern {bq?replacement}",
                  "\nINFO: The `magic` flag expands the pattern with `string_magic`.",
                  "\nPROBLEM: the evaluation with `string_magic` failed, see error below:",
                  "\n{'^[^\n]+\n'r?replacement_new}")
      }
      
      if(length(replacement_new) != 1){
        stop_hook("CONTEXT: concerns the pattern {bq?replacement}",
                  "\nINFO: The `magic` flag expands the pattern with `string_magic`. It must return a vector of length 1.",
                  "\nPROBLEM: the vector returned is of length {len?replacement_new}.")
      }
      
      replacement = replacement_new
    }

    if(is_split){
      all_patterns = strsplit(patterns, split = split)[[1]]
    } else {
      all_patterns = patterns
    }

    for(j in seq_along(all_patterns)){
      pat = all_patterns[j]

      if(is_total){
        # we allow logical operations when the replacement is in full
        pat_parsed = parse_regex_pattern(pat, NULL, parse_logical = TRUE, parse_flags = FALSE)
        pat = pat_parsed$patterns
        is_or = pat_parsed$is_or
        is_negate_all = pat_parsed$is_not
      }

      who_current = NULL
      for(k in seq_along(pat)){
        p = pat[k]

        if(is_total){
          negate = is_negate_all[k]
        }
        
        p = format_pattern(p, fixed = is_fixed, word = is_word, ignore = is_ignore)
        is_fixed = attr(p, "fixed")

        if(is_total){
          who_tmp = tryCatch(grepl(p, res, perl = !is_fixed, fixed = is_fixed), 
                         error = function(e) structure(conditionMessage(e), class = "try-error"),
                         warning = function(w) structure(conditionMessage(w), class = "try-warning"))
            
          is_warn = inherits(who_tmp, "try-warning")
          warn_msg = ""
          if(is_warn){
            # is there an error?
            warn_msg = who_tmp
            who_tmp = tryCatch(suppressWarnings(grepl(p, res, perl = !is_fixed, fixed = is_fixed)), 
                           error = function(e) structure(conditionMessage(e), class = "try-error"))
          }
          
          if(inherits(who_tmp, "try-error")){
            di_raw = escape_newline(di_raw)
            p = escape_newline(p)
            replacement = escape_newline(replacement)
            stopi("CONTEXT: evaluation of {Q?di_raw}",
                  "\n         pattern     = {Q?p} ",
                  "\n         replacement = {Q?replacement}",
                  "\nEXPECTATION: the pattern must be a valid regular expression",
                  "\nPROBLEM: `grepl` led to an error, see below:",
                  "\n{who_tmp}",
                  "{&nchar(warn_msg) > 0;\n{.}}")
          } else if(is_warn){
            di_raw = escape_newline(di_raw)
            p = escape_newline(p)
            replacement = escape_newline(replacement)
            warni("CONTEXT: evaluation of {Q?di_raw}",
                  "\n         pattern     = {Q?p} ",
                  "\n         replacement = {Q?replacement}",
                  "\nA warning was raised when evaluating the pattern:",
                  "\n{warn_msg}", immediate. = TRUE)
          }
          
          if(negate){
            who_tmp = !who_tmp
          }
          if(is.null(who_current)){
            who_current = who_tmp
          } else {
            if(is_or[k]){
              who_current = who_tmp | who_current
            } else {
              who_current = who_tmp & who_current
            }
          }
        } else {
          # shallow copy for error handling
          # I'm not sure it's useful: I think P(error|warning) = 100%, but well
          # you never know... 
          res_save = res
          my_sub = if(is_single) base::sub else base::gsub          
          
          res = tryCatch(my_sub(p, replacement, res, perl = !is_fixed, fixed = is_fixed), 
                         error = function(e) structure(conditionMessage(e), class = "try-error"),
                         warning = function(w) structure(conditionMessage(w), class = "try-warning"))
            
          is_warn = inherits(res, "try-warning")
          warn_msg = ""
          if(is_warn){
            # is there an error?
            warn_msg = res
            res = tryCatch(suppressWarnings(my_sub(p, replacement, res_save, perl = !is_fixed, fixed = is_fixed)), 
                           error = function(e) structure(conditionMessage(e), class = "try-error"))
          }
          
          if(inherits(res, "try-error")){
            di_raw = escape_newline(di_raw)
            p = escape_newline(p)
            replacement = escape_newline(replacement)
            stopi("CONTEXT: evaluation of {Q?di_raw}",
                  "\n         pattern     = {Q?p} ",
                  "\n         replacement = {Q?replacement}",
                  "\nEXPECTATION: the pattern must be a valid regular expression",
                  "\nPROBLEM: `{&is_single;sub;gsub}` led to an error, see below:",
                  "\n{res}",
                  "{&nchar(warn_msg) > 0;\n{.}}")
          } else if(is_warn){
            di_raw = escape_newline(di_raw)
            p = escape_newline(p)
            replacement = escape_newline(replacement)
            warni("CONTEXT: evaluation of {Q?di_raw}",
                  "\n         pattern     = {Q?p} ",
                  "\n         replacement = {Q?replacement}",
                  "\nA warning was raised when evaluating the pattern:",
                  "\n{warn_msg}", immediate. = TRUE)
          }
        }
      }
      
      if(is_total){
        res[who_current] = replacement
      }
    }

  }

  res
}

#' @describeIn string_clean Simplified version of `string_clean`
string_replace = function(x, pattern, replacement = "", pipe = " => ", ignore.case = FALSE, 
                          fixed = FALSE, word = FALSE, 
                          total = FALSE, single = FALSE, envir = parent.frame()){
  # this is a simplified version of string_clean
  # I think it's still useful bc of the flags, the pipe and the absence of splitting
  
  set_pblm_hook()
  
  string_clean(x, pattern, replacement = replacement, pipe = pipe, ignore.case = ignore.case,
               fixed = fixed, word = word, total = total, single = single, 
               envir = envir, split = "")
  
}

#' Efficient creation of string vectors with optional interpolation
#' 
#' Create string vectors in multiple ways: 1) add successive string elements (like in `c()`),
#' or 2) write a character string that will be broken with respect to commas 
#' (`"hi, there"` becomes `c("hi", "there")`), or 3) interpolate variables in 
#' character strings (`"x{1:2}"` becomes `c("x1", "x2")`) with full access to 
#' [string_magic()] operations, or any combination of the three.
#' 
#' @inheritParams string_magic
#' @inheritParams string_clean
#' 
#' @param ... Character vectors that will be vectorized. If commas are present in the 
#' character vector, it will be split with respect to commas and following blanks. 
#' The vectors can contain any interpolation in the form `"{var}"` and 
#' any [string_magic()] operation can be applied. To change the delimiters for interpolation,
#' see `.delim`. Named arguments are used in priority for variable substitution,
#' otherwise the value of the variables to be interpolated are fetched in the calling environment 
#' (see argument `.envir`).
#' 
#' Note, importantly, that interpolation and comma splitting are performed on "natural" vectors only.
#' That is: `string_vec("x{1:5}")` will lead to a vector of length 5 ("x1" to "x5"), while `z = "x{1:5}"`
#' followed by `string_vec(z)` leads to a vector of length 1: `"x{1:5}"`. To change this behavior and 
#' obtain equivalent results, use `.protect.vars = FALSE`.
#' @param .protect.vars Logical scalar, default is `TRUE`. If `TRUE`, then only
#' arguments equal to a "natural" character scalar are comma-split and interpolated, 
#' other arguments are not touched. Ex: `string_vec("x{1:5}")` will lead to a vector of 
#' length 5 ("x1" to "x5"), while `z = "x{1:5}"` followed by `string_vec(z)` leads 
#' to a vector of length 1: `"x{1:5}"`. If `FALSE`, comma-splitting and interpolation
#' is performed on all variables. 
#' @param .split Logical or a character symbol, default is `TRUE`. If `TRUE`, 
#' the character vectors are split with respect to commas (default). If `FALSE`, no 
#' splitting is performed. If a character symbol, the string vector will be split
#' according to this symbol. Note that any space after the symbol (including tabs and 
#' newlines) is discarded.
#' 
#' Ex: by default `string_vec("hi, there")` leads to the vector `c("hi", "there")`.
#' @param .sep Character scalar or `NULL` (default). If not `NULL`, the function
#' [base::paste()] is applied to the resulting vector with `sep = .sep`.
#' @param .collapse Character scalar or `NULL` (default). If not `NULL`, the function
#' [base::paste()] is applied to the resulting vector with `collapse = .collapse`.
#' 
#' If so, pass the name of your package in this argument so that your function can access 
#' the new `string_magic` operations defined within your package.
#' @param .cmat Logical scalar (default is `FALSE`), integer or complex with integer values.
#' If `TRUE`, we try to coerce the result into a **character** matrix. The number of rows and columns
#' is deduced from the look of the arguments. An integer indicates the number of rows.
#' If a complex, the imaginary part represents the number of columns. Ex: `5i` means 
#' 5 columns, `3 + 2i` means 3 rows and 2 columns.
#' 
#' The matrix is always filled by row.
#' @param .nmat Logical scalar (default is `FALSE`), integer or complex with integer values.
#' If `TRUE`, we try to coerce the result into a **numeric** matrix. The number of rows and columns
#' is deduced from the look of the arguments. An integer indicates the number of rows.
#' If a complex, the imaginary part represents the number of columns. Ex: `5i` means 
#' 5 columns, `3 + 2i` means 3 rows and 2 columns.
#' 
#' The matrix is always filled by row. Non numbers are silently turned to NA.
#' @param .df Logical scalar (default is `FALSE`), integer, complex with integer values, or
#' character vector.
#' If `TRUE`, we try to coerce the result into a `data.frame`. The number of rows and columns
#' is deduced from the look of the arguments. An integer indicates the number of rows.
#' If a complex, the imaginary part represents the number of columns. Ex: `5i` means 
#' 5 columns, `3 + 2i` means 3 rows and 2 columns. 
#' 
#' If a character vector: it should give the column names. Note that if the vector
#' is of length 1, its values are comma separated (i.e. the value `"id, name"` is
#' identical to `c("id", "name")`).
#' 
#' Note that the columns that can be converted to numeric are converted to numeric.
#' The other columns are in string form. Monitor this behavior with `.df.convert`.
#' 
#' @param .df.convert Logical scalar, default is `TRUE`. Only used when the result is 
#' to be converted to a data frame (with the argument `.df`). If `TRUE`, any column
#' looking like a numeric vector is converted to numeric. Otherwise all columns 
#' are character strings.
#' 
#' @details 
#' The main objective of this function is to simplify the creation of small character vectors.
#' By default, you can pass a character string of length 1 with values separated with commas
#' and a character vector will be returned. 
#' 
#' You can use interpolation using curly brackets (see `string_magic()`). You can pass
#' values for the interpolation directly in the arguments (this is why all 
#' arguments start with a dot).
#' 
#' By default character values containing commas are split with respect to the commas
#' to create vectors. To change this behavior, see the argument `.split`.
#' 
#' The default of the argument `.protect.vars` is `FALSE` so as to avoid unwanted 
#' comma-splitting and interpolations. The main use case of this function is
#' the creation of small string vectors, which can be written directly at
#' function call. 
#' 
#' Customize the default of this function with [string_vec_alias()].
#' 
#' @return 
#' By default this function returns a string vector, the length of which depends on the arguments.
#' 
#' This result can be processed with the arguments `cmat`, `nmat` and `.df` which will 
#' try to coerce the result into a character matrix, a numeric matrix , or a data frame, respectively.
#' 
#' @author 
#' Laurent Berge
#' 
#' @family tools with aliases
#' 
#' @inherit string_clean seealso
#' 
#' @examples 
#' 
#' # illustrating comma-splitting and interpolation
#' string_vec("x1, y2, z{4:5}")
#' 
#' # variable protection
#' x = "x{1:5}"
#' string_vec(x, "y{1:2}")
#' 
#' # without protection => interpolation takes place
#' string_vec(x, "y{1:2}", .protect.vars = FALSE)
#' 
#' # removing comma splitting
#' string_vec("Hi, said Charles.", "Hi, said {girl}.", girl = "Julia", .split = FALSE)
#' 
#' # changing the delimiters for interpolation
#' pkg = "\\usepackage[usenames,dvipsnames]{xcolor}"
#' string_vec("\\usepackage{.[S!graphicx, fourier, standalone]}", 
#'            pkg, .delim = ".[ ]")
#' 
#' #
#' # Customization
#' #
#' 
#' # Unhappy about the defaults? Create an alias!
#' 
#' # we create a "matrix generator"
#' matgen = string_vec_alias(.nmat = TRUE, .last = "'\n'split")
#' 
#' matgen("5, 4, 3
#'         8, 6, 2")
#' 
string_vec = function(..., .cmat = FALSE, .nmat = FALSE, .df = FALSE,
                   .df.convert = TRUE,
                   .delim = c("{", "}"), .envir = parent.frame(), 
                   .split = TRUE, .protect.vars = TRUE, .sep = NULL, 
                   .last = NULL,
                   .collapse = NULL, .namespace = NULL){
  
  # checks
  set_pblm_hook()  
  .delim = check_set_delimiters(.delim)
  check_character(.sep, scalar = TRUE, null = TRUE)
  check_character(.collapse, scalar = TRUE, null = TRUE)
  check_character(.last, scalar = TRUE, null = TRUE)
  .split = check_set_split(.split)
  do_mat = check_set_mat(.cmat, .nmat, .df)
  
  check_logical(.protect.vars, scalar = TRUE)
  
  check_envir(.envir)
  
  # checking the dots + setting up the data
  dots = check_set_dots(..., mbt = TRUE, nofun = TRUE)
  
  # we check if some variables were passed in the arguments
  .data = list()
  dot_names = names(dots)
  if(!is.null(dot_names)){
    is_var = dot_names != ""
    for(i in which(is_var)){
      .data[[dot_names[i]]] = dots[[i]]
    }
    dots[is_var] = NULL      
  }
  
  n = length(dots)
  if(n == 0){
    stop_hook("`string_vec` requires at least one character scalar to work.",
              "\nNamed arguments are only used as variables on which to apply interpolation.",
              "\nPROBLEM: all arguments are named.",
              "\nFIX: please provide at least one non-named argument.")
  }
  
  is_to_split = rep(TRUE, n)
  if(.protect.vars){
    mc_dots = match.call(expand.dots = FALSE)[["..."]]
    mc_dots_nm = names(mc_dots)
    if(!is.null(mc_dots_nm)){
      # we keep only the non named arguments
      mc_dots = mc_dots[nchar(mc_dots_nm) == 0]
    }
    
    is_to_split = sapply(mc_dots, is.character)
  }
  
  #
  # now the algorithm
  #
  
  BOX_OPEN = .delim[1]
  
  res_list = vector("list", n)
  for(i in 1:n){
    di = dots[[i]]
    if(!true_character(di)){
      di = as.character(di)
    }
    if(length(di) == 1 && is_to_split[i]){
      
      if(isFALSE(.split)){
        di_expanded = di
      } else {
        di_expanded = cpp_magic_split(di, .split, .delim)
      }      
      
      is_open = grepl(BOX_OPEN, di_expanded, fixed = TRUE)
      n_open = sum(is_open)
      if(n_open > 0){
        n_di_xpd = length(di_expanded)
        all_elements = vector("list", n_di_xpd)
        
        for(j in 1:n_di_xpd){
          if(is_open[j]){
            all_elements[[j]] = string_magic_internal(di_expanded[j], .delim = .delim, 
                                                  .envir = .envir, .is_root = TRUE,
                                                  .data = .data, .check = TRUE, 
                                                  .namespace = .namespace)  
          } else {
            all_elements[[j]] = di_expanded[j]
          }        
        }
        
        di_expanded = do.call(base::c, all_elements)  
      }
      
      res_list[[i]] = di_expanded
    } else {
      res_list[[i]] = di
    }
  }
  
  if(!is.null(.sep) || !is.null(.collapse)){
    if(do_mat){
      stopi("The arguments `.sep` or `.collapse` are incompatible with the arguments ",
            "`.cmat` or `.nmat`. Please remove some of these arguments.")
    }
    res_list$.sep = .sep
    res_list$.collapse = .collapse
    res = do.call(base::paste, res_list)
  } else {
    res = do.call(base::c, res_list)
  }
  
  if(!is.null(.last)){
    res = string_ops(res, .last)
  }
  
  #
  # matrix formatting
  #
  
  if(do_mat){
    is_auto = isTRUE(attr(do_mat, "auto"))
    is_num = isTRUE(attr(do_mat, "num"))
    is_df = isTRUE(attr(do_mat, "df"))
    n_out = length(res)
    if(is_auto){
      done = TRUE
      if(n == 1){
        d = dots[[1]]
        nrows = length(strsplit(d, "\n")[[1]])
      } else {
        nrows = n
      }
      
      if((n_out / nrows) %% 1 != 0){
        done = FALSE
      }
      
      if(!done){
        nrows = floor(sqrt(n_out))
        while(nrows >= 1 && (n_out / nrows) %% 1 != 0){
          nrows = nrows - 1
        }
      }
      
      ncols = n_out / nrows
    } else {
      row_cols = attr(do_mat, "row_col")
      nrows = row_cols[1]
      ncols = row_cols[2]
      if(nrows == 0){
        nrows = n_out / ncols
        if(round(nrows) != nrows){
          arg = if(is_num) ".nmat" else if(is_df) ".df" else ".cmat"
          stopi("The number of columns in argument `{arg}` ", 
                "({ncols}) is not a divisor ",
                "of the final number of elements: {n?n_out}.")
        }
      } 
      
      if(ncols == 0){
        ncols = n_out / nrows
        if(round(ncols) != ncols){
          arg = if(is_num) ".nmat" else if(is_df) ".df" else ".cmat"
          stopi("The number of rows in argument `{arg}` ", 
                "({nrows}) is not a divisor ",
                "of the final number of elements: {n?n_out}.")
        }
      }
    }
    
    if(is_num){
      res = suppressWarnings(as.numeric(res))
    }
    
    res = matrix(res, nrows, ncols, byrow = TRUE)
    
    if(is_df){
      res = as.data.frame(res)
      colnames = attr(do_mat, "colnames")
      if(!is.null(colnames)){
        names(res) = colnames
      }
      # conversion of the columns to numeric
      check_logical(.df.convert, scalar = TRUE)
      if(.df.convert){
        for(i in 1:ncol(res)){
          if(is_numeric_in_char(res[[i]])){
            res[[i]] = as.numeric(res[[i]])
          }
        }
      }
    }
  }  
    
  res  
}



#' Fills a character string up to a size
#' 
#' Fills a character string up to a size and handles multibyte encodings 
#' (differently from sprintf).
#' 
#' @param x A character vector.
#' @param n Integer scalar, possibly equal to `NULL` (default). The size up to which the character 
#' vector will be filled. If `NULL` (default), it is set to the largest width in the character vector `x`.
#' To handle how the character is filled, see the arguments `symbol`, `right` and `center`.
#' @param symbol Character scalar of length 1, default is a space (" "). It is the symbol with which
#' the string will be filled.
#' @param right Logical scalar, default is `FALSE`. If `TRUE`, then the filling of the string is 
#' done from the left, leading to right-alignment.
#' @param center Logical scalar, default is `FALSE`. If `TRUE`, then the filling of the string will
#' be balanced so as to center the strings.
#' @param na Character scalar or `NA`. Default is "NA" (a character string!). What happens to NAs: by default 
#' they are replaced by the character string "NA".
#' 
#' @details 
#' If you use character filling of the form `sprintf("% 20s", x)` with `x``containing multibyte characters,
#' you may be suprised that all character strings do not end up at the same lenght (the occurrence of this problem
#' depends on many things: encodings are a mess). `string_fill`
#' uses only base R functions to compensate this. It is slightly slower but, in general, safer. 
#' 
#' It also looks a bit like [base::format()], but slightly different (and a bit faster, but more restrictive).
#' 
#' @return 
#' This functions returns a character vector of the same lenght as the vector in input.
#' 
#' @author 
#' Laurent R. Berge
#' 
#' @inherit string_clean seealso
#' 
#' @examples 
#' 
#' x = c("apple", "pineapple") 
#' 
#' # simple fill with blank
#' cat(paste0(string_fill(x), ":", c(3, 7), "€"), sep = "\n")
#' 
#' # center fill
#' cat(paste0(string_fill(x, center = TRUE), ":", c(3, 7), "€"), sep = "\n")
#' 
#' # changing the length of the fill and the symbol used for filling
#' cat(paste0(string_fill(x), ":", 
#'            string_fill(c(3, 7), 3, "0", right = TRUE), "€"), sep = "\n")
#' 
#' # na behavior: default/NA/other
#' x = c("hello", NA) 
#' string_fill(x)
#' string_fill(x, na = NA)
#' string_fill(x, na = "(missing)")
#' 
#' 
string_fill = function(x = "", n = NULL, symbol = " ", right = FALSE, center = FALSE, na = "NA"){
  # Character vectors starting with " " are not well taken care of

  x = check_set_character(x, l0 = TRUE)
  if(length(x) == 0) return(character(0))

  check_numeric(n, null = TRUE, scalar = TRUE, integer = TRUE)
  check_character(symbol, scalar = TRUE)
  check_logical(right, scalar = TRUE)

  if(anyNA(x)){
    x[is.na(x)] = na
  }  

  if(nchar(symbol) != 1){
    stop("Argument 'symbol' must be a single character ", 
         "(currenlty it is of length ", nchar(symbol), ").")
  }

  if(!is.null(n) && n == 0){
    return(x)
  }

  n_all = nchar(x)
  if(is.null(n)) n = max(n_all)

  n2fill = n - n_all
  qui = which(n2fill > 0)
  if(length(qui) == 0){
    return(x)
  }
  
  x_new = simple_string_fill(x[qui], n, symbol, right = right, center = center)

  res = x
  res[qui] = x_new
  res
}





#' Extracts a pattern from a character vector
#' 
#' Extracts the first, or several, patterns from a character vector.
#' 
#' @inheritParams string_is
#' 
#' @param x A character vector.
#' @param pattern A character scalar. It represents the pattern
#' to be extracted from `x`. By default 
#' this is a regular expression. You can use flags in the pattern in 
#' the form `flag1, flag2/pattern`.
#' Available flags are `ignore` (case), `fixed` (no regex), word (add word boundaries), 
#' single (select only the first element), and magic (add interpolation with `{}`) . Example:
#' if `"ignore/hello"` and `x = "Hello world` extracted text is `"Hello"`.
#' Shortcut: use the first letters of the flags. Ex: "iw/one" will extract the word 
#' "one" (flags 'ignore' + 'word').
#' @param single Logical scalar, default is `FALSE`. If `TRUE`, only the first pattern
#' that is detected will be returned. Note that in that case, a character vector is returned
#' of the same length as the vector in input. 
#' @param simplify Logical scalar, default is `TRUE`. If `TRUE`, then when the vector input `x`
#'  is of length 1, a character vector is returned instead of a list.
#' @param unlist Logical scalar, default is `FALSE`. If `TRUE`, the function `unlist` is applied
#' to the resulting list, leading to a character vector in output (instead of a list).
#' 
#' @inheritSection string_is Generic regular expression flags
#' 
#' @return 
#' The object returned by this functions can be a list or a character vector. 
#' 
#' If `single = TRUE`, a character vector is returned, containing the value of the first match.
#' If no match is found, an empty string is returned.
#' 
#' If `single = FALSE` (the default) and `simplify = TRUE` (default), the object returned is:
#' + a character vector if `x`, the vector in input, is of length 1: the character vector contains
#' all the matches and is of length 0 if no match is found.
#' + a list of the same length as `x`. The ith element of the list is a character vector
#' of the matches for the ith element of `x`.
#' 
#' If `single = FALSE` (default) and `simplify = FALSE`, the object returned is always a list.
#' 
#' @examples 
#' 
#' cars = head(row.names(mtcars))
#' 
#' # Let's extract the first word:
#' string_extract(cars, "\\w+", single = TRUE)
#' 
#' # same using flags
#' string_extract(cars, "s/\\w+")
#' 
#' # extract all words composed on only letters
#' # NOTE: we use the flag word (`w/`)
#' string_extract(cars, "w/[[:alpha:]]+")
#' 
#' # version without flag:
#' string_extract(cars, "\\b[[:alpha:]]+\\b")
#' 
#' # If a vector of length 1 => a vector is returned
#' greet = "Hi Tom, how's Mary doing?"
#' string_extract(greet, "w/[[:upper:]]\\w+")
#' 
#' # version with simplify = FALSE => a list is returned
#' string_extract(greet, "w/[[:upper:]]\\w+", simplify = FALSE)
#' 
string_extract = function(x, pattern, single = FALSE, simplify = TRUE, fixed = FALSE,
                          ignore.case = FALSE, word = FALSE, unlist = FALSE, 
                          envir = parent.frame()){
  
  x = check_set_character(x, mbt = TRUE, l0 = TRUE)

  check_logical(single, scalar = TRUE)
  check_logical(simplify, scalar = TRUE)
  check_logical(fixed, scalar = TRUE)
  check_logical(ignore.case, scalar = TRUE)
  check_logical(word, scalar = TRUE)
  check_logical(unlist, scalar = TRUE)
  
  pat_parsed = parse_regex_pattern(pattern, c("fixed", "word", "ignore", "magic", "single"),
                                   envir = envir, parse_logical = FALSE)
  
  flags = pat_parsed$flags
  
  is_single = single || "single" %in% flags
  is_fixed = fixed || "fixed" %in% flags
  is_word = word || "word" %in% flags
  is_ignore = ignore.case || "ignore" %in% flags
  is_magic = "magic" %in% flags
  
  info = format_simple_regex_flags(pat_parsed$patterns, 
                                   fixed = is_fixed, word = is_word, 
                                   ignore = is_ignore, magic = is_magic, envir = envir)
  
  pattern = info$pattern
  is_fixed = info$fixed
  
  if(length(x) == 0){
    if(is_single || simplify){
      return(character(0))
    } else {
      # simplify = FALSE and single = FALSE always return a list
      return(list())
    }    
  }
  
  if(is_single){
    x_pat = regexpr(pattern, x, fixed = is_fixed, perl = !is_fixed)
    
    res = substr(x, x_pat, x_pat - 1 + attr(x_pat, "match.length"))
  } else {
    res = regmatches(x, gregexpr(pattern, x, fixed = is_fixed, perl = !is_fixed))
    
    if(unlist || (simplify && length(x) == 1)){
      res = unlist(res)
    }
  }
  
  res
}


####
#### dedicated utilities ####
####



#' `stringmagic`'s regular expression parser
#' 
#' Parse regular expression with custom flags and obtain the final pattern
#' to be parsed as well as the vector of flags
#' 
#' @param pattern Character scalar, the regular expression pattern to parse.
#' @param authorized_flags Character vector representing the flags to be parsed.
#' Use the empty string if no flags are allowed.
#' @param parse_flags Logical scalar, default is `TRUE`. Whether to parse the 
#' optional regex flags.
#' @param parse_logical Logical scalar, default is `TRUE`. Whether to parse logical 
#' regex operations, similarly to [string_get()].
#' @param envir An environment, default is `parent.frame()`. Only used if the flag 
#' `magic` is present, it is used to find the variables to be interpolated.
#' 
#' @details 
#' This is an internal tool that is exposed in order to facilitate checking what's going on.
#' 
#' There is no error handling.
#' 
#' @return 
#' This function always returns a list of 4 elements:
#' 
#' + `flags`: the character vector of flags. If no flags were found, this is the empty string.
#' + `patterns`: the vector of regex patterns.
#' + `is_or`: logical vector of the same length as `patterns`. Indicates for each
#' pattern if it should be attached to the previous patterns with a logical OR (`FALSE`
#' means a logical `AND`).
#' + `is_not`: logical vector of the same length as `patterns`. Indicates for each pattern
#' if it should be negated.
#' 
#' @author 
#' Laurent Berge
#' 
#' @inherit string_clean seealso
#' 
#' @examples 
#' 
#' parse_regex_pattern("f/hello", c("fixed", "ignore"))
#' 
#' x = "john"
#' parse_regex_pattern("fm/{x} | Doe", c("fixed", "ignore", "magic"))
#' 
parse_regex_pattern = function(pattern, authorized_flags, parse_flags = TRUE, 
                               parse_logical = TRUE, envir = parent.frame()){
  # in: "fw/hey!, bonjour, a[i]"
  # common authorized_flags: c("fixed", "word", "ignore")

  info_pattern = cpp_parse_regex_pattern(pattern, parse_flags = parse_flags, 
                                         parse_logical = parse_logical)
  
  if(!is.null(info_pattern$error)){
    main_msg = .sma("Problem found in the regex pattern {'50|..'k, Q ? pattern}.",
                    "\nPROBLEM: ")
    extra = info_pattern$error_extra
    msg = switch(info_pattern$error,
                 "flag starting with space" = "flags cannot start with a space.", 
                 "flags list is too long" = 
                    .sma("the flag list is too long, ",
                         "most likely they are no flags.\nFIX? start with a slash or escape the slash",
                         "with a double backslash."), 
                 "non valid character at position" = 
                    .sma("the flag list contains a non ",
                        "valid character in position {extra} ",
                        "({`extra`firstchar, lastchar, bq ? pattern}). ",
                        "Flags must only consist of lower case letters followed by a comma or a slash."),
                "flag starts with !" = 
                    .sma("You cannot use the '!' operator before the flags."), 
                 "flag empty" = 
                    .sma("The {Nth ? extra} flag is empty. ", 
                         "{&extra==1;To use a regular slash, escape it with a double backslah.;",
                         "Flags must only consist of lower case letters followed by ",
                         "a comma or a slash.}"))
    
    note_rm = .sma("\n\nINFO: regex flags write 'flag1, flag2/pattern'",
                    " where a flag = lower case letters.",
                    "\n  Ex: 'ignore, fixed/dt[' leads",
                    " to the flags 'ignore' and 'fixed' for the pattern 'dt['.", 
                    "\n  Or simply collate the first letters of the flags:",
                    " 'if/dt['.", 
                    "\n  To use '/' without flag parsing, escape it. ",
                    "Ex: '\\\\/usr': leads to '/usr' without any flag on.")
    
    full_msg = .sma(main_msg, msg, note_rm)
    
    stop_hook(full_msg)
  }

  flags = info_pattern$flags
  if(length(flags) == 1){
    if(!flags %in% authorized_flags){
      # is it a flag made only of initials?
      flag_split = strsplit(flags, "")[[1]]
      authorized_initials = substr(authorized_flags, 1, 1)
      if(all(flag_split %in% authorized_initials)){
        flags = authorized_flags[authorized_initials %in% flag_split]
      }
    }
  }

  flags = check_set_options(flags, authorized_flags, free = TRUE)

  flag_pblm = setdiff(flags, authorized_flags)
  if(length(flag_pblm) > 0){
    info = setdiff(authorized_flags, flags)
    stop_hook("In the pattern {'20||...'k, bq?pattern} the flag {enum.bq?flag_pblm} is not authorized.",
              "\nFYI the authorized flags are: {enum.bq?authorized_flags}.",
              "\nNOTA: The syntax is \"flag1, flag2/regex\" or \"flag_initials/regex\".")
  }
  
  if("magic" %in% flags){
    for(i in seq_along(info_pattern$patterns)){
      p = info_pattern$patterns[i]
      p_escaped = gsub("(\\{( *\\d| *,))", "\\\\\\1", p)
      # we try the magic evaluation
      p_new = try(string_magic(p_escaped, .envir = envir), silent = TRUE)
      if(isError(p_new)){
        stop_hook("CONTEXT: concerns the pattern {bq?pattern}",
                  "{&len(info_pattern)>1;\n         when evaluating {bq?p}}",
                  "\nINFO: The `magic` flag expands the pattern with `string_magic`.",
                  "\nPROBLEM: the evaluation with `string_magic` failed, see error below:",
                  "\n{'^[^\n]+\n'r?p_new}")
      }
      
      if(length(p_new) != 1){
        stop_hook("CONTEXT: concerns the pattern {bq?pattern}",
                  "{&len(info_pattern)>1;\n         when evaluating {bq?p}}",
                  "\nINFO: The `magic` flag expands the pattern with `string_magic`. It must return a vector of length 1.",
                  "\nPROBLEM: the vector returned is of length {len?p_new}.")
      }
      
      info_pattern$patterns[i] = p_new
    }
  }

  res = list(flags = flags, patterns = info_pattern$patterns, is_or = info_pattern$is_or,
             is_not = info_pattern$is_not)

  res
}

format_simple_regex_flags = function(pattern, ignore = FALSE, word = FALSE, fixed = FALSE, 
                                     magic = FALSE, envir = parent.frame(2)){
  
  if(magic){
    new_regex = parse_regex_pattern(pattern, c("ignore", "word", "fixed", "magic"), 
                                    parse_logical = FALSE, envir = envir)
  } else {
    new_regex = parse_regex_pattern(pattern, c("ignore", "word", "fixed"), parse_logical = FALSE)
  }  
  
  pattern = new_regex$patterns
  is_ignore = "ignore" %in% new_regex$flags || ignore
  is_word = "word" %in% new_regex$flags || word
  is_fixed = "fixed" %in% new_regex$flags || fixed
  
  pattern = format_pattern(pattern, fixed = is_fixed, word = is_word, ignore = is_ignore)
  is_fixed = attr(pattern, "fixed")
    
  res = list(pattern = pattern, fixed = is_fixed)
  
  return(res)
}

format_pattern = function(pattern, fixed, word, ignore){

  if(word){
    items = strsplit(pattern, ",[ \t\n]+")[[1]]
    if(fixed){
      items = paste0("\\Q", items, "\\E")
      fixed = FALSE
    }
    pattern = paste0("\\b(", paste0(items, collapse = "|"), ")\\b")
  }

  if(ignore){
    if(fixed){
      fixed = FALSE
      pattern = paste0("(?i)\\Q", pattern, "\\E")
    } else {
      pattern = paste0("(?i)", pattern)
    }          
  }
  
  attr(pattern, "fixed") = fixed
  
  pattern
}

to_integer = function(x, sort = FALSE){
  # x: vector or a list of vectors

  if(!is.list(x) && !is.atomic(x)){
    stop("Argument `x` must be either a list or a vector.")
  }

  if(!is.list(x)){
    id = to_integer_single(x)
  } else {
    Q = length(x)

    if(Q == 1){
      id = to_integer_single(x[[1]])
    } else {

      x_int_all = list()
      g_all = numeric(Q)
      for(i in seq_along(x)){
        id_i = to_integer_single(x[[i]])
        x_int_all[[i]] = id_i
        g_all[i] = max(id_i)
      }

      # Then we combine
      power = floor(1 + log10(g_all))

      is_large = sum(power) > 14
      if(is_large){
        order_index = do.call(order, x_int_all)
        index = cpp_combine_clusters(x_int_all, order_index)
      } else {
        # quicker, but limited by the precision of doubles
        index = x_int_all[[1]]
        for(q in 2:Q){
          index = index + x_int_all[[q]] * 10 ** sum(power[1:(q-1)])
        }
      }

      id = cpp_to_integer(index)

    }
  }
  
  if(sort){
    # example:
    # x = c(5, 5, 8, 8, 1, 4)
    # x_first = c(5, 8, 1, 4)
    # id_first = c(1, 2, 3, 4)
    # id_all = c(1, 1, 2, 2, 3, 4) 
    
    obs_first = which(!duplicated(id))
    
    if(is.atomic(x)){
      x_list = list(x[obs_first])
    } else {
      x_list = unclass(x)
      for(i in 1:length(x)){
        x_list[[i]] = x_list[[i]][obs_first]
      }
    }
    
    new_order = do.call(base::order, x_list)
    order_new_order = order(new_order)
    id = order_new_order[id]
  }

  return(id)
}


to_integer_single = function(x){

  if(!is.numeric(x) && !is.character(x) && !is.factor(x)){
    # we're super conservative
    # otherwise, there can be error when underlying types or "wrongly" integer
    x = as.character(x)
  }

  cpp_to_integer(x)
}


####
#### Aliases ####
####


#' @describeIn string_ops Alias to `string_ops`
stops = string_ops

#' @describeIn string_is Alias to `string_is`
stis = string_is

#' @describeIn string_is Alias to `string_any`
stany = string_any

#' @describeIn string_is Alias to `string_all`
stall = string_all

#' @describeIn string_is Alias to `string_which`
stwhich = string_which

#' @describeIn string_get Alias to `string_get`
stget = string_get

#' @describeIn string_split Alias to `string_split`
stsplit = string_split

#' @describeIn string_clean Alias to `string_clean`
stclean = string_clean

#' @describeIn string_clean Alias to `string_replace`
streplace = string_replace

#' @describeIn string_vec Alias to `string_vec`
stvec = string_vec

#' @describeIn string_extract Alias to `string_extract`
stextract = string_extract













































































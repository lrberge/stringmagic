#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Aug  8 16:26:42 2022
# ~: string tools
#----------------------------------------------#


#' Chains basic operations to character vectors
#'
#' Simple tool to perform multiple operations to character vectors.
#'
#' @param x A character vector. If not a character vector but atomistic (i.e. not a list), 
#' it will be converted to a character vector.
#' @param op Character **scalar**. Character scalar containing the comma separated values 
#' of operations to perform to the vector. The 50+ operations are detailed in the help
#' page of [smagick()].
#' @param pre_unik Logical scalar, default is `NULL`. Whether to first unique the vector 
#' before applying the possibly costly string operations, and merging back the result. 
#' For very large vectors with repeated values the time gained can be substantial. By 
#' default, this is `TRUE` for vector of length 1M or more.
#' 
#' @details 
#' This function is a simple wrapper around smagick. Formally, `str_ops(x, "op1, op2")`
#' is equivalent to `smagick("{op1, op2 ? x}")`.
#'
#' @return
#' In general it returns a character vector. It may be of a length different from the original
#'  one, depending on the operations performed. 
#'
#' @author
#' Laurent R. Berge
#' 
#' @inherit str_clean seealso
#'
#' @examples
#' 
#' # data on car models
#' cars = row.names(mtcars)
#' 
#' # let's get the brands starting with an "m"
#' str_op(cars, "'i/^m'get, x, unik")
#' 
#' # Explainer:
#' # 'i/^m'get: keeps only the elements starting with an m,
#' #            i/ is the 'regex-flag' "ignore" to ignore the case
#' #            ^m means "starts with an m" in regex language
#' # x: extracts the first pattern. The default pattern is "[[:alnum:]]+"
#' #    which means an alpha-numeric word
#' # unik: applies unique() to the vector
#' # => see help in ?smagick for more details on the operations
#' 
#' 
#' # let's get the 3 largest numbers appearing in the car models
#' str_op(cars, "'\\d+'x, rm, unik, num, dsort, 3 first")
#' 
#' # Explainer:
#' # '\\d+'x: extracts the first pattern, the pattern meaning "a succession"
#'            of digits in regex language
#' # rm: removes elements equal to the empty string (default behavior)
#' # unik: applies unique() to the vector
#' # num: converts to numeric
#' # dsort: sorts in decreasing order
#' # 3 first: keeps only the first three elements
#' 
#' 
str_ops = function(x, op, pre_unik = NULL){

  if(missing(x)){
    stop("Argument `x` must be provided. PROBLEM: it is currently missing.")
  }

  if(!is.atomic(x)){
    stop("Argument `x` must be atomic. Currently it is of class `", class(x)[1], "`")
  }

  if(!is.character(x)){
    x = as.character(x)
  }

  check_character(op, mbt = TRUE, scalar = TRUE)
  check_logical(pre_unik, null = TRUE, scalar = TRUE)

  # For very large vectors, we unique
  n = length(x)
  if(is.null(pre_unik)) pre_unik = n > 1e6

  if(pre_unik){
    x_int = to_integer(x)
    x_small = x[!duplicated(x_int)]

    res_small = str_ops(x_small, op, pre_unik = FALSE)
    res = res_small[x_int]
  } else {
    operation = paste0("{", op, " ? x}")
    res = .smagick(operation, data = list(x = x), frame = parent.frame())
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
#' Use ' & ' or ' | ' to chain patterns and combine their result logically (ex: '[[:alpha:]] & \\d' gets strings
#' containing both letters and numbers). You can negate by adding a `!` first (ex: `"!sepal$"` will 
#' return `TRUE` for strings that do not end with `"sepal"`).
#' Add flags with the syntax 'flag1, flag2/pattern'. Available flags are: 'fixed', 'ignore', 'word', 'verbatim'.
#' Ex: "ignore/sepal" would get "Sepal.Length" (wouldn't be the case w/t 'ignore'). 
#' Shortcut: use the first letters of the flags. Ex: "if/dt[" would get "DT[i = 5]" (flags 'ignore' + 'fixed').
#' The flag 'verbatim' does not parse logical operations. For 'word', it adds word boundaries to the 
#' pattern. See the documentation of this argument.
#' @param or Logical, default is `FALSE`. In the presence of two or more patterns, 
#' whether to combine them with a logical "or" (the default is to combine them with a logical "and").
#' @param pattern (If provided, elements of `...` are ignored.) A character vector representing the 
#' patterns to be found. By default a (perl) regular-expression search is triggered. 
#' Use ' & ' or ' | ' to chain patterns and combine their result logically (ex: '[[:alpha:]] & \\d' gets strings
#' containing both letters and numbers). You can negate by adding a `!` first (ex: `"!sepal$"` will 
#' return `TRUE` for strings that do not end with `"sepal"`).
#' Add flags with the syntax 'flag1, flag2/pattern'. Available flags are: 'fixed', 'ignore', 'word', 'verbatim'.
#' Ex: "ignore/sepal" would get "Sepal.Length" (wouldn't be the case w/t 'ignore'). 
#' Shortcut: use the first letters of the flags. Ex: "if/dt[" would get "DT[i = 5]" (flags 'ignore' + 'fixed').
#' The flag 'verbatim' does not parse logical operations. For 'word', it adds word boundaries to the 
#' pattern. See the documentation of this argument.
#' @param fixed Logical scalar, default is `FALSE`. Whether to trigger a fixed search instead of a 
#' regular expression search (default).
#' @param word Logical scalar, default is `FALSE`. If `TRUE` then a) word boundaries are added to the pattern, 
#' and b) patterns can be chained by separating them with a comma, they are combined with an OR logical operation.
#' Example: if `word = TRUE`, then pattern = "The, mountain" will select strings containing either the word
#' 'The' or the word 'mountain'.
#' @param ignore.case Logical scalar, default is `FALSE`. If `TRUE`, then case insensitive search is triggered. 
#' 
#' @details
#' The internal function used to find the patterns is [base::grepl()] with `perl = TRUE`.
#' 
#' @section Generic pattern flags:
#' 
#' All `stringmagick` functions support generic flags in regular-expression patterns. 
#' The flags are useful to quickly give extra instructions, similarly to *usual* 
#' [regular expression flags](https://javascript.info/regexp-introduction).
#' 
#' Here the syntax is "flag1, flag2/pattern". That is: flags are a comma separated list of flag-names 
#' separated from the pattern with a slash (`/`). Example: `str_which(c("hello...", "world"), "fixed/.")` returns `1`. 
#' Here the flag "fixed" removes the regular expression meaning of "." which would have otherwise meant *"any character"*.
#' The no-flag verion `str_which(c("hello...", "world"), ".")` returns `1:2`.
#' 
#' Alternatively, and this is recommended, you can collate the initials of the flags instead of using a
#' comma separated list. For example: "if/dt[" will apply the flags "ignore" and "fixed" to the pattern "dt[".
#' 
#' The three flags always available are: "ignore", "fixed" and "word". 
#' + "ignore" instructs to ignore the case. Technically, it adds the perl-flag "(?i)" at the beginning of the pattern.
#' + "fixed" removes the regular expression interpretation, so that the characters ".", "$", "^", "[" 
#' (among others) lose their special meaning and are treated for what they are: simple characters. 
#' + "word" adds word boundaries (`"\\b"` in regex language) to the pattern. Further, the comma (`","`) 
#' becomes a word separator. Technically, "word/one, two" is treated as "\\b(one|two)\\b". Example: 
#' `str_clean("Am I ambushed?", "wi/am")` leads to " I ambushed?" thanks to the flags "ignore" and "word".
#'
#' @return
#' It returns a logical vector of the same length as `x`.
#' 
#' The function `str_which` returns a numeric vector. 
#' 
#' @author 
#' Laurent R. Berge
#' 
#' @inherit str_clean seealso
#'
#' @examples
#' 
#' # NOTA: using `str_get` instead of `str_is` may lead to a faster understanding 
#' #       of the examples 
#'
#' x = dsb("/one, two, one... two, microphone, check")
#'
#' # default is regular expression search
#' # => 3 character items
#' str_is(x, "^...$")
#'
#' # to trigger fixed search use the flag 'fixed'
#' str_is(x, "fixed/...")
#' # you can just use the first letter
#' str_is(x, "f/...")
#'
#' # to negate, use '!'
#' str_is(x, "!f/...")
#' # or directly in the pattern
#' str_is(x, "f/!...")
#'
#' # you can combine several patterns with "&" or "|"
#' str_is(x, "one & c")
#' str_is(x, "one | c")
#' 
#' #
#' # word: adds word boundaries
#' #
#' 
#' # compare
#' str_is(x, "one")
#' # with
#' str_is(x, "w/one")
#' 
#' # words can be chained with commas (it is like an OR logical operation)
#' str_is(x, "w/one, two")
#' # compare with
#' str_is(x, "w/one & two")
#' # remember that you can still negate
#' str_is(x, "w/one & !two")#' 
#' 
#' # you can combine the flags
#' # compare
#' str_is(x, "w/ONE")
#' # with
#' str_is(x, "wi/ONE")
#'
#' #
#' # str_which
#' #
#'
#' # it works exactly the same way as str_is
#' # Which are the items containing an 'e' and an 'o'?
#' str_which(x, "e", "o")
#' # equivalently
#' str_which(x, "e & o")
#'
#'
str_is = function(x, ..., fixed = FALSE, ignore.case = FALSE, word = FALSE, 
                  or = FALSE, pattern = NULL){

  x = check_set_character(x, mbt = TRUE, l0 = TRUE)
  if(length(x) == 0){
    return(logical(0))
  }

  check_logical(ignore.case, scalar = TRUE)
  check_logical(fixed, scalar = TRUE)
  check_logical(word, scalar = TRUE)
  check_logical(or, scalar = TRUE)
  check_character(pattern, null = TRUE, no_na = TRUE)

  if(missnull(pattern)){
    dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)
    warn_no_named_dots(dots)
    pattern = unlist(dots)
  }

  or_origin = or
  negate = FALSE
  logical_op = function(a, b, or) if(or) a | b else a & b

  res = NULL
  for(i in seq_along(pattern)){
    pat = pattern[i]
    first_char = substr(pat, 1, 1)
    main_negate = FALSE
    if(first_char == "\\" && substr(pat, 2, 2) == "!"){
      pat = substr(pat, 2, nchar(pat))
    } else if(first_char == "!" && nchar(pat) > 1){
      main_negate = TRUE
      pat = substr(pat, 2, nchar(pat))
    }
    
    pat_parsed = parse_regex_pattern(pat, c("fixed", "word", "ignore", "verbatim"))
    is_or = pat_parsed$is_or
    flags = pat_parsed$flags
    all_patterns = pat_parsed$patterns

    is_fixed_origin = fixed || "fixed" %in% flags
    is_word = word  || "word" %in% flags
    is_ignore = ignore.case || "ignore" %in% flags
    
    res_current = NULL
    n_pat = length(all_patterns)
    for(j in 1:n_pat){
      is_fixed = is_fixed_origin
      p = all_patterns[j]

      if(n_pat > 1 && main_negate && length(flags) == 0){
        main_negate = FALSE
        sub_negate = TRUE        
      } else {
        first_char = substr(p, 1, 1)
        sub_negate = FALSE
        if(first_char == "\\" && substr(p, 2, 2) == "!"){
          p = substr(p, 2, nchar(p))
        } else if(first_char == "!" && nchar(p) > 1){
          sub_negate = TRUE
          p = substr(p, 2, nchar(p))
        }
      }     
      
      p = format_pattern(p, fixed = is_fixed, word = is_word, ignore = is_ignore) 
      is_fixed = attr(p, "fixed")

      res_tmp = grepl(p, x, perl = !is_fixed, fixed = is_fixed)
      if(sub_negate){
        res_tmp = !res_tmp
      }
      if(is.null(res_current)){
        res_current = res_tmp
      } else {
        res_current = logical_op(res_current, res_tmp, is_or[j])
      }
    }

    if(main_negate){
      res_current = !res_current
    }
    
    if(is.null(res)){
      res = res_current
    } else {
      res = logical_op(res, res_current, or)
    }
  }

  res
}

#' @describeIn str_is Returns the indexes of the values in which a pattern is detected
str_which = function(x, ..., fixed = FALSE, ignore.case = FALSE, word = FALSE, 
                     or = FALSE, pattern = NULL){

  check_character(pattern, null = TRUE, no_na = TRUE)

  if(missnull(pattern)){
    dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)
    warn_no_named_dots(dots)
    pattern = unlist(dots)
  }

  which(str_is(x, fixed = fixed, ignore.case = ignore.case, word = word, or = or, pattern = pattern))
}

#' Gets elements of a character vector
#'
#' Convenient way to get elements from a character vector.
#'
#' @inheritParams str_is
#'
#' @param x A character vector.
#' @param seq Logical, default is `FALSE`. The argument `pattern` accepts a vector of 
#' patterns which are combined with an `and` by default. If `seq = TRUE`, then it is like 
#' if `str_get` was called sequentially with its results stacked. See examples.
#' @param seq.unik Logical, default is `FALSE`. The argument `...` (or the argument `pattern`) accepts 
#' a vector of patterns which are combined with an `and` by default. If `seq.unik = TRUE`, then 
#' `str_get` is called sequentially with its results stacked, and `unique()` is 
#' applied in the end. See examples.
#' 
#' @details 
#' This function is a wrapper to [str_is()].
#' 
#' @inheritSection str_is Generic pattern flags
#' 
#' @section Caching:
#' 
#' In an exploratory stage, it can be useful to quicky get values from a vector with the 
#' least hassle as possible. Hence `str_get` implements caching, so that users do not need
#' to repeat the value of the argument `x` in successive function calls, and can concentrate
#' only on the selection patterns.
#' 
#' Caching is a feature only available when the user calls `str_get` from the global environment.
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
#' @inherit str_clean seealso
#' 
#'
#' @examples
#'
#' x = rownames(mtcars)
#'
#' # find all Mazda cars
#' str_get(x, "Mazda")
#' # same with ignore case flag
#' str_get(x, "i/mazda")
#' 
#' # all cars containing a single digit (we use the 'word' flag)
#' str_get(x, "w/\\d")
#'
#' # finds car names without numbers AND containing `u`
#' str_get(x, "!\\d", "u")
#' # equivalently
#' str_get(x, "!\\d & u")
#'
#' # Stacks all Mazda and Volvo cars. Mazda first
#' str_get(x, "Mazda", "Volvo", seq = TRUE)
#'
#' # Stacks all Mazda and Volvo cars. Volvo first
#' str_get(x, "Volvo", "Mazda", seq = TRUE)
#'
#' # let's get the first word of each car name
#' car_first = str_op(x, "extract.first")
#' # we select car brands ending with 'a', then ending with 'i'
#' str_get(car_first, "a$", "i$", seq = TRUE)
#' # seq.unik is similar to seq but applies unique()
#' str_get(car_first, "a$", "i$", seq.unik = TRUE)
#' 
#' #
#' # flags
#' #
#' 
#' # you can combine the flags
#' x = dsb("/One, two, one... Two!, Microphone, check")
#' # regular
#' str_get(x, "one")
#' # ignore case
#' str_get(x, "i/one")
#' # + word boundaries
#' str_get(x, "iw/one")
#' 
#' # you can escape the meaning of ! with backslashes
#' str_get(x, "\\!")
#' 
#' #
#' # Caching
#' #
#' 
#' # Caching is enabled when the function is used interactively
#' # so you don't need to repeat the argument 'x'
#' # Mostly useful at an exploratory stage
#' 
#' if(interactive() && identical(sys.frame(), .GlobalEnv)){
#'    
#'    # first run, the data is cached
#'    str_get(row.names(mtcars), "i/vol")
#' 
#'    # now you don't need to specify the data
#'    str_get("i/^m & 4")
#' }
#'
#'
#'
#'
str_get = function(x, ..., fixed = FALSE, ignore.case = FALSE, word = FALSE, 
                   or = FALSE, seq = FALSE, seq.unik = FALSE, pattern = NULL){

  x = check_set_character(x, mbt = TRUE, l0 = TRUE)
  if(length(x) == 0){
    return(character(0))
  }
  
  # data caching in interactive mode
  is_caching = FALSE
  if(interactive() && identical(parent.frame(), .GlobalEnv)){
    mc = match.call()
    if(is.character(mc$x) && !is.null(getOption("stringmagick_str_get_cache"))){
      is_caching = TRUE
      x_pattern = x
      x = getOption("stringmagick_str_get_cache")
    } else if(length(x) > 1){
      options(stringmagick_str_get_cache = x)
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
      value = str_get(x, pattern = pattern[i], or = or, seq = FALSE, 
                      fixed = fixed, ignore.case = ignore.case, word = word)
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

  index = str_is(x, fixed = fixed, ignore.case = ignore.case, word = word, pattern = pattern, or = or)
  x[index]
}


#' Splits a character vector into a data frame
#'
#' Splits a character vector and formats the resulting substrings into a data.frame
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
#' Available flags are `ignore` (case), `fixed` (no regex), word (add word boundaries). Example:
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
#' @param dt Logical, default is `FALSE`. Whether to return a `data.table`. See also the function `str_split2dt`.
#' @param ... Not currently used.
#'
#' @return
#' It returns a `data.frame` or a `data.table` which will contain: i) `obs`: the observation index, 
#' ii) `pos`: the position of the text element in the initial string (optional, via add.pos), 
#' iii) the text element, iv) the identifier(s) (optional, only if `id` was provided).
#' 
#' @inherit str_clean seealso
#' 
#' @examples
#'
#' x = c("Nor rain, wind, thunder, fire are my daughters.",
#'       "When my information changes, I alter my conclusions.")
#'
#' id = c("ws", "jmk")
#'
#' # we split at each word
#' str_split2df(x, "[[:punct:] ]+")
#'
#' # we add the 'id'
#' str_split2df(x, "[[:punct:] ]+", id = id)
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
#' str_split2df(text ~ my_id, base, "[[:punct:] ]+")
#'
#' #
#' # with 2+ identifiers
#'
#' base = within(mtcars, carname <- rownames(mtcars))
#'
#' # we have a message because the identifiers are not unique
#' str_split2df(carname ~ am + gear + carb, base, " +")
#'
#' # adding the position of the words & removing the message
#' str_split2df(carname ~ am + gear + carb, base, " +", id_unik = FALSE, add.pos = TRUE)
#'
#'
str_split2df = function(x, data = NULL, split = NULL, id = NULL, add.pos = FALSE,
                        id_unik = TRUE, fixed = FALSE, ignore.case = FALSE,
                        word = FALSE, dt = FALSE, ...){

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
  if("mc" %in% names(dots)){
    mc = dots$mc
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
               smagick("PROBLEM: the variable{$s, enum.bq, is ? var_pblm} not in the data set (`"),
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

        intro = smagick("The evaluation of the right side of `x` raised an error.\n",
                    "VALUE TO EVAL: {bq ? id_name}\n")

        if(!is.null(names(data))){
          var_pblm = setdiff(vars, names(data))
          if(length(var_pblm) > 0){
            stop(intro,
                 smagick("PROBLEM: the variable{$s, enum.bq, is ? var_pblm} not in the data set (`"),
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

    x_name = deparse_short(fml[[2]])
    id_names = names(id)

  } else {
    check_character(x)
    x_name = "x"
  }

  n = length(x)
  if(!missnull(id)){
    # id: a) a vector, b) a data frame. Same length as x

    if(is.list(id)){
      n_id = unique(lengths(id))
      if(length(n_id) != 1 || max(n_id) != n){
        extra = ""
        if(max(n_id) != n) extra = dsb("\nPROBELM: len x: .[#n ? n] len id: .[#n ? max(n_id)].")
        stop("The argument `id` must be either a vector of identifiers or a data.frame ",
             "of identifiers of the same length as `x`.",
             extra)
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
        stop("The argument `id` must be either a vector of identifiers or a data.frame ",
             "of identifiers of the same length as `x`.",
             dsb("\nPROBELM: len x: .[n ? n]; len id: .[n ? n_id]."))
      }
      if(!is.atomic(id)){
        stop("The argument `id` must be either a vector of identifiers or a data.frame ",
             "of identifiers of the same length as `x`.\nPROBLEM: `id` is not an atomic vector")
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
                                         fixed = fixed, word = word)
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
      setDT(res)
    } else {
      stop("To return a `data.table`, you need the `data.table` package to be installed. Currently this is not the case.",
           "\nUse install.packages('data.table')?")
    }
  }

  res
}

#' @describeIn str_split2df Splits a string vector and returns a `data.table`
str_split2dt = function(x, data = NULL, split = NULL, id = NULL, add.pos = FALSE,
                        id_unik = TRUE, fixed = FALSE){

  mc = match.call()
  str_split2df(x, data = data, split = split, id = id, add.pos = add.pos,
               id_unik = id_unik, fixed = fixed, mc = mc, dt = TRUE)
}


#' Cleans a character vector from multiple patterns
#'
#' Recursively cleans a character vector from several patterns. Quickly handle the 
#' tedious task of data cleaning by taking advantage of the syntax.
#' You can also apply all sorts of cleaning operations by summoning [[str_op]] operations.
#'
#' @param x A character vector.
#' @param ... Character scalars representing patterns. A pattern is of the form
#' "flags/pat1, pat2 => replacement". This means that patterns 'pat1' and 'pat2' will be replaced
#' with the string 'replacement'. By default patterns are comma separated and the replacement comes 
#' after a ' => ' (see args `sep` and `pipe` to change this). By default the replacement is the empty string 
#' (so "pat1, pat2" *removes* the patterns).
#' 
#' Available flags are: 'word' (add word boundaries), 'ignore' (the case), 'fixed' (no regex), and 'total'. 
#' The flag `total` leads to a *total replacement* of the string if the pattern is found. Use flags
#' with comma separation ("word, total/pat") or use only their initials ("wt/pat").
#' 
#' Starting with an '@' leads to operations in [str_op]. Ex: "@ascii, l, ws" turns
#' the string into ASCII, lowers the case and normalizes white spaces (see help of [str_ops]).
#' @param pipe Character scalar, default is `" => "`. If thevalue of `pipe` is found in a pattern,
#' then the string is split w.r.t. the pipe and anything after the pipe becomes the replacement.
#' 
#' For example in `str_clean(x, "e => a")` the default pipe is found in "e => a", so the pattern 
#' "e" will be replaced with "a". In other terms, this is equivalent to `str_clean(x, "e", replacement = "a")`.
#' Example changing the pipe: you can obtain the previous result with `str_clean(x, "e|>a", pipe = "|>")`.
#' @param sep Character scalar, default is `",[ \t\n]+"` (which means a comma followed with spaces 
#' and/or new lines). By default the patterns to be replaced are comma separated, that is 
#' the pattern is split w.r.t. the argument `sep` and a replacement is done for each sub-pattern.
#' 
#' Use `NULL` or the empty string to disable pattern separation.
#' 
#' For example: let's look at `str_clean(x, "w/one, two => three")`. First the flag "word" is extracted from
#' the pattern (see arg. `...`) as well as the replacement (see arg. `pipe`), leading to "one, two" the 
#' pattern to be replaced. Then the pattern is split w.r.t. `sep`, leading 
#' to two patterns "one" and "two". Hence the words (thanks to the flag "w") "one" and "two" from
#' the string `x` will be replaced with "three".
#' @param replacement Character scalar, default is the empty string. It represents the default 
#' value by which the patterns found in the character strings will be replaced. For example
#' `str_clean(x, "e", replacement = "a")` turn all letters "e" in `x` into "a".
#' @param total Logical scalar, default is `FALSE`. If `TRUE`, then when a pattern is found 
#' in a string, the full string is replaced (instead of just the pattern). Note, *importantly*, 
#' that when `total = TRUE` you can use logical operators in the patterns.
#' 
#' Example: `str_clean(x, "wi/ & two, three & !four => ", total = TRUE)`
#'
#' @return
#' The main usage returns a character vector of the same length as the vector in input.
#' Note, however, that since you can apply arbitrary [str_op] operations, the length and type
#' of the final vector may depend on those (if they are used).
#' 
#' @author 
#' Laurent R. Berge
#' 
#' @seealso 
#' A few basic operation: [str_is()], [str_get()], [str_clean()]. Chain basic operations with [str_ops()]. 
#' String interpolation combined with operation chaining: [smagick()].
#'
#' @examples
#'
#' x = c("hello world  ", "it's 5 am....")
#'
#' # we clean the o's and the points (we use 'fixed' to trigger fixed-search)
#' str_clean(x, c("o", "f/."))
#' # equivalently
#' str_clean(x, "fixed / o, .")
#' # equivalently
#' str_clean(x, "o, .", fixed = TRUE)
#' # equivalently
#' str_clean(x, "o", ".", fixed = TRUE)
#' 
#' #
#' # chaining operations: example using cars
#' #
#' 
#' cars = row.names(mtcars)
#' new = str_clean(cars, 
#'            # replace strings containing "Maz" with Mazda
#'            "total / Maz => Mazda", 
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
str_clean = function(x, ..., replacement = "", pipe = " => ", sep = ",[ \n\t]+", 
                     ignore.case = FALSE, fixed = FALSE, word = FALSE, total = FALSE){

  x = check_set_character(x, l0 = TRUE)
  if(length(x) == 0){
    return(x)
  }

  check_character(replacement, scalar = TRUE)
  check_character(pipe, scalar = TRUE)
  check_character(sep, scalar = TRUE, null = TRUE)

  is_sep = length(sep) > 0 && nchar(sep) > 0
  is_pipe = length(pipe) > 0 && nchar(pipe) > 0

  check_logical(ignore.case, scalar = TRUE)
  check_logical(fixed, scalar = TRUE)
  check_logical(word, scalar = TRUE)
  check_logical(total, scalar = TRUE)

  rep_main = replacement

  dots = list(...)
  warn_no_named_dots(dots)

  dots = unlist(dots)

  res = x
  for(i in seq_along(dots)){
    di = dots[[i]]
    
    first_char = substr(di, 1, 1)
    is_str_op = FALSE
    if(first_char == "\\" && substr(di, 2, 2) == "@"){
      di = substr(di, 2, nchar(di))
    } else if(first_char == "@" && nchar(di) > 1){
      is_str_op = TRUE
      di = substr(di, 2, nchar(di))
    }

    if(is_str_op){
      res = str_ops(res, di)
      next
    }

    if(is_pipe && grepl(pipe, di)){
      # application du pipe
      di_split = strsplit(di, pipe)[[1]]
      replacement = di_split[2]
      di = di_split[1]
    } else {
      replacement = rep_main
    }
    
    # we parse the special flags
    is_total = total
    di_parsed = parse_regex_pattern(di, c("ignore", "fixed", "word", "total"), 
                                     parse_logical = FALSE)
    flags = di_parsed$flags
    patterns = di_parsed$patterns

    is_total = total || "total" %in% flags
    is_fixed = fixed || "fixed" %in% flags
    is_ignore = ignore.case || "ignore" %in% flags
    is_word = word || "word" %in% flags

    if(is_sep){
      all_patterns = strsplit(patterns, split = sep)[[1]]
    } else {
      all_patterns = patterns
    }

    for(j in seq_along(all_patterns)){
      pat = all_patterns[j]

      if(is_total){
        # we allow logical operations when the replacement is in full
        pat_parsed = parse_regex_pattern(pat, c("ignore", "fixed", "word", "total"), 
                                          parse_logical = TRUE)
        pat = pat_parsed$patterns
        is_or = pat_parsed$is_or
      }

      who_current = NULL
      for(k in seq_along(pat)){
        p = pat[k]

        if(is_total){
          negate = FALSE
          first_char = substr(p, 1, 1)
          if(first_char == "\\" && substr(p, 2, 2) == "!"){
            p = substr(p, 2, nchar(p))
          } else if(first_char == "!" && nchar(p) > 1){
            negate = TRUE
            p = substr(p, 2, nchar(p))
          }
        }
        
        p = format_pattern(p, fixed = is_fixed, word = is_word, ignore = is_ignore)
        is_fixed = attr(p, "fixed")

        if(is_total){
          who_tmp = grepl(p, res, perl = !is_fixed, fixed = is_fixed)
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
          res = gsub(p, replacement, res, perl = !is_fixed, fixed = is_fixed)
        }
      }
      
      if(is_total){
        res[who_current] = replacement
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
#' depends on many things: encodings are a mess). `str_fill`
#' uses only base R functions to compensate this. It is slightly slower but, in general, safer. 
#' 
#' It also looks a bit like [base::format()], but slightly different (and a bit faster, but more restrictive).
#' 
#' @author 
#' Laurent R. Berge
#' 
#' @inherit str_clean seealso
#' 
#' @examples 
#' 
#' x = c("apple", "pineapple") 
#' 
#' # simple fill with blank
#' cat(paste0(str_fill(x), ":", c(3, 7), "€"), sep = "\n")
#' 
#' # center fill
#' cat(paste0(str_fill(x, center = TRUE), ":", c(3, 7), "€"), sep = "\n")
#' 
#' # changing the length of the fill and the symbol used for filling
#' cat(paste0(str_fill(x), ":", str_fill(c(3, 7), 3, "0", right = TRUE), "€"), sep = "\n")
#' 
#' # na behavior: default/NA/other
#' x = c("hello", NA) 
#' str_fill(x)
#' str_fill(x, na = NA)
#' str_fill(x, na = "(missing)")
#' 
#' 
str_fill = function(x = "", n = NULL, symbol = " ", right = FALSE, center = FALSE, na = "NA"){
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
  
  x_new = simple_str_fill(x[qui], n, symbol, right = right, center = center)

  res = x
  res[qui] = x_new
  res
}



####
#### dedicated utilities ####
####

parse_regex_pattern = function(pattern, authorized_flags, parse_logical = TRUE){
  # in: "fw/hey!, bonjour, a[i]"
  # common authorized_flags: c("fixed", "word", "ignore")

  info_pattern = cpp_parse_regex_pattern(pattern, parse_logical = parse_logical)
  
  if(!is.null(info_pattern$error)){
    main_msg = .sma("Problem found in the regex pattern {'50|..'k, Q ? pattern}.",
                    "\nPROBLEM: ")
    
    msg = switch(info_pattern$error,
                 "flag starting with space" = "flags cannot start with a space.", 
                 "flags list is too long" = 
                    .sma("the flag list is too long, ",
                         "most likely they are no flags.\nFIX? start with a slash."), 
                 "non valid character at position" = 
                    .sma("the flag list contains a non ",
                        "valid character in position {info_pattern$error_extra} ",
                        "({`info_pattern$error_extra`cfirst, clast, bq ? pattern}). ",
                        "Flags must only consist of lower case letters followed by a comma or a slash."), 
                 "flag empty" = 
                    .sma("The {Nth ? info_pattern$error_extra} flag is empty. ", 
                         "Flags must only consist of lower case letters followed by a comma or a slash."))
    
    note_rm = .sma("\n\nINFO: Regular expression flags must be of the form: 'flag1, flag2/pattern'",
                    " where a flag must consist of only lower case letters. Ex: 'ignore, fixed/dt[' leads",
                    " to the flags 'ignore' and 'fixed' and to the pattern '.['.", 
                    "\n      Alternatively, collate the first letters of the flags:",
                    " 'if/dt[' leads to the same results.", 
                    "\n      Start with a slash to remove the meaning of the slash and suppress the parsing",
                    " of flags. Ex.1: '/hey/': no flag, pattern is 'hey/'.",
                    " To have a slash as a pattern, double it: '//': no flag, pattern is '/'.")
    
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
              "\nNOTA: The syntax is \"flag1, flag2 / regex\". To remove the parsing of flags, start with a '/'.")
  }

  if("verbatim" %in% flags){
    info_pattern = cpp_parse_regex_pattern(pattern, FALSE)
  }

  res = list(flags = flags, patterns = info_pattern$patterns, is_or = info_pattern$is_or)

  res
}

format_simple_regex_flags = function(pattern, ignore = FALSE, word = FALSE, fixed = FALSE){
  
  new_regex = parse_regex_pattern(pattern, c("ignore", "word", "fixed"), FALSE)
  
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

to_integer = function(x){
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































































































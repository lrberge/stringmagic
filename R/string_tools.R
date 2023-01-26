#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Aug  8 16:26:42 2022
# ~: string tools
#----------------------------------------------#


#' Detects whether a pattern is in a character string
#'
#' Function that detects if patterns are in a string. The patterns can be chained, by default this is a regex search but fixed search can be triggered with a special syntax, supports negation.
#'
#' @param x A character vector.
#' @param ... Character scalars representing the patterns to be found. By default a (perl) regular-expression search is triggered. To use a fixed search instead, use an `#` first (ex: `"#.["` will search for `".["`). You can negate by adding a `!` first (ex: `"!sepal$"` will return `TRUE` for strings that do not end with `"sepal"`). If there are two or more patterns, the results are combined with a logical "and". To combine with an "or", you can start with a `|`. `|` has precedence over `!` which has precedence over `#`. You can escape the meaning of the first `!` or `#` with a double backslash `\\\\`.
#' @param or Logical, default is `FALSE`. In the presence of two or more patterns, whether to combine them with a logical "or" (the default is to combine them with a logical "and").
#' @param pattern (If provided, elements of `...` are ignored.) A character vector representing the patterns to be found. By default a (perl) regular-expression search is triggered. To use a fixed search instead, use an `#` first (ex: `"#.["` will search for `".["`). You can negate by adding a `!` first (ex: `"!sepal$"` will return `TRUE` for strings that do not end with `"sepal"`). If there are two or more patterns, the results are combined with a logical "and". To combine with an "or", you can start with a `|`. `|` has precedence over `!` which has precedence over `#`. You can escape the meaning of the first `!` or `#` with a double backslash `\\\\`.
#'
#' @details
#' Note that fixed search is much faster than regular expression search, so for large vectors it should be used whenever possible.
#'
#' @return
#' It returns a logical vector of the same length as `x`.
#'
#' @examples
#'
#' x = dsb("/one, two, one... two, microphone, check")
#'
#' # Default is regular expression search
#' str_is(x, "^...$")
#'
#' # to trigger fixed search use #
#' str_is(x, "#...")
#'
#' # to negate, use '!'
#' str_is(x, "!#...")
#'
#' # you can combine several patterns
#' # default combination is a logical "and"
#' str_is(x, "one", "c")
#'
#' # to combine with a logical "or"
#' str_is(x, "one", "|!o")
#'
#' # same using the argument `or`:
#' str_is(x, "one", "!o", or = TRUE)
#'
#' #
#' # str_which
#' #
#'
#' # it works exactly the same way as str_is
#' # Which are the items containing an 'e' and an 'o'?
#' str_which(x, "e", "o")
#'
#'
str_is = function(x, ..., or = FALSE, pattern = NULL){

  check_character(x, mbt = TRUE, l0 = TRUE)
  check_logical(or, scalar = TRUE)
  check_character(pattern, null = TRUE, no_na = TRUE)

  if(missnull(pattern)){
    dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)
    pattern = unlist(dots)
  }

  or_origin = or
  negate = FALSE
  do_negate = function(z) if(negate) !z else z
  logical_op = function(a, b) if(or) a | b else a & b

  for(i in seq_along(pattern)){
    pat = pattern[i]

    first_char = substr(pat, 1, 1)

    if(i > 1){
      if(first_char == "|"){
        # the | cannot be escaped because it makes no sense
        or = TRUE
        pat = substr(pat, 2, nchar(pat))
        first_char = substr(pat, 1, 1)
      } else {
        or = or_origin
      }
    }

    if(first_char == "\\"){
      # if either ! or # is escaped:
      # no negation and no fixed

      second_char = substr(pat, 2, 2)
      if(second_char %in% c("!", "#")){
        # we trim if needed
        pat = substr(pat, 2, nchar(pat))
      }

      negate = FALSE
      fixed = FALSE
    } else {
      # no escape

      negate = first_char == "!"
      if(negate){
        pat = substr(pat, 2, nchar(pat))
        first_char = substr(pat, 1, 1)
      }

      if(negate && substr(pat, 1, 2) == "\\#"){
        # => not fixed
        pat = substr(pat, 2, nchar(pat))
        fixed = FALSE
      } else {
        fixed = first_char == "#"
        if(fixed){
          pat = substr(pat, 2, nchar(pat))
        }
      }
    }

    value = do_negate(grepl(pat, x, perl = !fixed, fixed = fixed))

    if(i == 1){
      res = value
    } else {
      res = logical_op(res, value)
    }
  }

  res
}

#' @describeIn str_is
str_which = function(x, ..., or = FALSE, pattern = NULL){

  check_character(pattern, null = TRUE, no_na = TRUE)

  if(missnull(pattern)){
    dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)
    pattern = unlist(dots)
  }

  which(str_is(x, or = or, pattern = pattern))
}



#' Gets elements of a character vector
#'
#' Convenient way to get elements from a character vector
#'
#' @inheritParams str_is
#'
#' @param x A character vector.
#' @param seq Logical, default is `FALSE`. The argument `pattern` accepts a vector of patterns which are combined with an `and` by default. If `seq = TRUE`, then it is like if `str_get` was called sequentially with its results stacked. See examples.
#' @param seq.unik Logical, default is `FALSE`. The argument `pattern` accepts a vector of patterns which are combined with an `and` by default. If `seq.unik = TRUE`, then it is like if `str_get` was called sequentially with its results stacked, and `unique()` was applied in the end. See examples.
#'
#' @return
#' It always return a character vector.
#'
#' @examples
#'
#' x = rownames(mtcars)
#'
#' # Finds all Mazda cars
#' str_get(x, "Mazda")
#'
#' # Finds car names without numbers AND containing `u`
#' str_get(x, "!\\d", "u")
#'
#' # Stacks all Mazda and Volvo cars. Mazda first
#' str_get(x, "Mazda", "Volvo", seq = TRUE)
#'
#' # Stacks all Mazda and Volvo cars. Volvo first
#' str_get(x, "Volvo", "Mazda", seq = TRUE)
#'
#' # let's get the first word of each car name
#' car_first = str_op(x, "x")
#' # we select car brands ending with 'a', then ending with 'i'
#' str_get(car_first, "a$", "i$", seq = TRUE)
#' # seq.unik is similar to seq but applies unique()
#' str_get(car_first, "a$", "i$", seq.unik = TRUE)
#'
#'
#'
#'
str_get = function(x, ..., or = FALSE, seq = FALSE, seq.unik = FALSE, pattern = NULL){

  check_character(x, mbt = TRUE)
  check_character(pattern, null = TRUE, no_na = TRUE)
  check_logical(or, scalar = TRUE)
  check_logical(seq, scalar = TRUE)
  check_logical(seq.unik, scalar = TRUE)

  if(missnull(pattern)){
    dots = check_set_dots(..., mbt = TRUE, character = TRUE, scalar = TRUE, no_na = TRUE)
    pattern = unlist(dots)
  }

  if(seq.unik){
    seq = TRUE
  }

  if(seq){
    for(i in seq_along(pattern)){
      value = str_get(x, pattern = pattern[i], or = or, seq = FALSE)
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

  index = str_is(x, pattern = pattern, or)
  x[index]
}



#' Select values from a character vector or variables names from a data frame
#'
#' Multi-faceted pattern-based selection of variables
#'
#' @param x Either a character vector or a data frame. In both cases, only a character vector will be returned.
#' @param pattern A character scalar. The **comma separated** patterns to select the variables. Let `p` be an element of this pattern. By default, the variable named `p` is selected, and if not present an error is raised. Use `^` as the first character to enable partial matching. If `p = "^petal"`, all variables starting with `"petal"` will be selected. Use  `@` (resp. `#`) as first character to enable regular expression (resp. fixed pattern) matching. If `p = "@al$"`, all variables ending with `"al"` will be selected. Use `!` as first character to select all variables **not matching** the pattern. If `p = "!#petal"` then only variables **not** containing `"petal"` will be selected: and this works retroactively! Hence all variables previously selected that do not pass the negation are dropped. To *only include variables* through the negation, use `&` as first character: `p = "&!#petal"` will *add* all variables that do not contain `"petal"`. Finally, use a double pipe, `||`, to reorder the variables. After the pipes, use a comma separated list of values which work in the same way as the selection previously described. In case the argument `x` is a data set, you can use the special values `.num`, `.log`, `.lnum`, `.fact`, `.char`, `.fchar`, `.date` to select variables base on their types.
#' @param order Optional, default is `NULL`. A character scalar. A comma separated list of patterns to order the variables. Works in the same way as the argument `pattern`. Partial reminder: you can use special first characters. `!` to negate, `^` to partially match, `@` for regular expressions. See more details in the help of the argument `pattern`.
#'
#' @details
#' This function is tailored to select a list of variables from a data set. It also works for character strings.
#'
#' For character strings, this is a variation of [`str_get`].
#'
#' @return
#' Returns a character vector.
#'
#' @examples
#'
#' x = rownames(mtcars)
#'
#' # All variables starting with Maz
#' selvars(x, "^Maz")
#'
#' # All Mercedes but not 450
#' selvars(x, "^Mer, !#450")
#'
#' # All Mazda, and all cars that do not contain digits
#' selvars(x, "^Maz, &!@\\d")
#' # => note that without the `&` we would have dropped the Mazdas
#'
#' # All cars containing digits, but not Mercedes, we put the Fiat first and Mazda last
#' selvars(x, "@\\d, !^Merc || ^Fiat, !^Maz")
#'
#' #
#' # Using a data set as input, selecting via variables types
#' #
#'
#' # selecting factor variables and variables starting with Sepal
#' selvars(iris, ".fact, ^Sepal")
#'
selvars = function(x, pattern = NULL, order = NULL){


  mc = match.call()
  if("x" %in% names(mc) && mc$x == "."){
    # this is a data table call
    sc_all = sys.calls()
    if(length(sc_all) >= 4 && as.character(sc_all[[length(sc_all) - 3]][1])[1] == "[.data.table"){
      x = eval(quote(names_x), parent.frame(3))
    }
  }

  if(missing(x)){
    stop("Argument 'x' must be provided. PROBLEM: it is missing.")
  }

  is_data_set = is.data.frame(x)
  if(is_data_set){
    x_df = x
    x = names(x)
  } else {
    check_character(x)
  }

  check_character(pattern, no_na = TRUE, null = TRUE)
  check_character(order, null = TRUE, no_na = TRUE)

  x_origin = x

  if(length(pattern) > 1){
    if(any(grepl("||", pattern[1:(length(pattern) - 1)], fixed = TRUE))){
      # the order must always come last!!!!
      i = which(grepl("||", pattern[1:(length(pattern) - 1)], fixed = TRUE))[i]
      stop("You can use the double pipe `||` to reorder the variables, ",
           "but it must always come in the last element of argument `pattern`.",
           "\nPROBLEM: the double pipe appears in the ", n_th(i),
           " element (of ", length(pattern), ").")
    }
  }

  if(length(pattern) == 0){
    pattern = "@."
  }

  pattern = str_op(pattern, "'[, \t\n]+'S")

  n_pat = length(pattern)

  if(n_pat > 0 && grepl("||", pattern[n_pat], fixed = TRUE)){
    pat_split = str_op(pattern[n_pat], "'||'s, w")
    pattern[n_pat] = pat_split[1]
    order = str_op(pat_split[2], "'[, \t\n]+'S")
  }

  # default: full var
  qui_all = rep(FALSE, length(x))
  init = TRUE
  res = c()
  for(p in pattern){
    p_raw = p
    p_clean = NULL
    first_char = substr(p, 1, 1)

    is_and = first_char == "&"
    if(is_and){
      p = substr(p, 2, nchar(p))
      first_char = substr(p, 1, 1)
      all_vars = x_origin
    } else {
      all_vars = x
    }

    negate = first_char == "!"
    if(negate){
      p = substr(p, 2, nchar(p))
      first_char = substr(p, 1, 1)
    }

    if(p %in% c(".num", ".log", ".lnum", ".fact", ".char", ".fchar", ".date")){
      if(!is_data_set){
        stop(dsb("The special value .[bq?p] can only be used when the argument `x` is a data set. ",
                 "It is meaningless for character vectors."))
      }

      type_fun = switch(p,
                        .num = is.numeric,
                        .log = is.logical,
                        .lnum = function(x) is.numeric(x) || is.logical(x),
                        .fact = is.factor,
                        .char = is.character,
                        .fchar = function(x) is.factor(x) || is.character(x),
                        .date = function(x) any(grepl("date", class(x), ignore.case = TRUE)))

      is_selected = sapply(x_df, type_fun)

      if(!any(is_selected)){
        info = switch(p,
                      .num = "numeric",
                      .log = "logical",
                      .lnum = "numeric or logical",
                      .fact = "factor",
                      .char = "character",
                      .fchar = "factor or character",
                      .date = "date")

        stop(dsb("The special value .[bq?p] did not find any variable. ",
                 "Maybe you could check that there are .[info] variables in the data set?"))
      }

    } else if(first_char %in% c("#", "@", "^")){
      # fixed char search
      fixed = first_char == "#"
      p = substr(p, 2, nchar(p))

      if(first_char == "^"){
        p_clean = p
        p = paste0("^\\Q", p, "\\E")
      }

      is_selected = grepl(p, all_vars, perl = !fixed, fixed = fixed)

    } else {
      is_selected = all_vars == p
    }

    if(!any(is_selected) || (negate && all(is_selected))){
      info = switch(first_char,
                    "#" = "fixed pattern",
                    "@" = "regular expression",
                    "^" = "partial matching of",
                    "value")

      if(!is.null(p_clean)) p = p_clean

      p_info = if(p != p_raw) paste0("(raw is `", p_raw, "`) ") else ""
      consequence = "led to no variable being selected."
      if(info == "value"){
        consequence = paste0("is not ", ifelse(is_data_set, "a variable from the data set.", "present."))
      }
      extra = if(info == "value") " Note that you can use regex with `@` and partial matching with `^`." else ""

      stop("In the argument `pattern`, the ", info, " `", p, "` ", p_info, consequence, extra)
    }

    if(init){
      if(negate){
        res = all_vars[!is_selected]
        if(!is_and){
          # we restrict the values (unless asked explicitly not too)
          x = res
        }
      } else {
        res = all_vars[is_selected]
      }

    } else {
      if(negate){
        if(is_and){
          # we want the negated variables in
          wanted = setdiff(all_vars[!is_selected], res)
          res = c(res, wanted)
        } else {
          # we want the negated variables out
          unwanted = all_vars[is_selected]
          x = all_vars[!is_selected]
          res = setdiff(res, unwanted)
        }
      } else {
        # we respect the order of the user
        wanted = setdiff(all_vars[is_selected], res)
        res = c(res, wanted)
      }

    }

    init = FALSE
  }

  n_o = length(order)
  if(n_o == 0){
    return(res)
  }

  order = str_op(order, "'[, \t\n]+'S")

  for(i in n_o:1){
    or_raw = or = order[i]
    or_clean = NULL

    first_char = substr(or, 1, 1)
    negate = first_char == "!"
    if(negate){
      or = substr(or, 2, nchar(or))
      first_char = substr(or, 1, 1)
    }

    if(or %in% c(".num", ".log", ".lnum", ".fact", ".char", ".fchar", ".date")){
      if(!is_data_set){
        stop(dsb("The special value .[bq?or] can only be used when the argument `x` is a data set. ",
                 "It is meaningless for character vectors."))
      }

      type_fun = switch(or,
                        .num = is.numeric,
                        .log = is.logical,
                        .lnum = function(x) is.numeric(x) || is.logical(x),
                        .fact = is.factor,
                        .char = is.character,
                        .fchar = function(x) is.factor(x) || is.character(x),
                        .date = function(x) any(grepl("date", class(x), ignore.case = TRUE)))

      is_selected = sapply(x_df, type_fun)

      if(!any(is_selected)){
        info = switch(or,
                      .num = "numeric",
                      .log = "logical",
                      .lnum = "numeric or logical",
                      .fact = "factor",
                      .char = "character",
                      .fchar = "factor or character",
                      .date = "date")

        stop(dsb("The special value .[bq?or] did not find any variable. ",
                 "Maybe you could check that there are .[info] variables in the data set?"))
      }

    } else if(first_char %in% c("#", "@", "^")){
      fixed = first_char == "#"
      or = substr(or, 2, nchar(or))

      if(first_char == "^"){
        or_clean = or
        or = paste0("^\\Q", or, "\\E")
      }

      is_selected = grepl(or, res, perl = !fixed, fixed = fixed)

    } else {
      is_selected = res == or
    }

    if(!any(is_selected)){
      info = switch(first_char,
                    "#" = "fixed pattern",
                    "@" = "regular expression",
                    "^" = "partial matching of",
                    "value")

      if(!is.null(or_clean)) or = or_clean
      or_info = if(or != or_raw) paste0("(raw is `", or_raw, "`) ") else ""
      consequence = "led to no variable being selected."
      if(info == "value"){
        consequence = paste0("is not ", ifelse(is_data_set, "a variable from the data set.", " present."))
      }
      extra = if(info == "value") " Note that you can use regex with `@` and partial matching with `^`." else ""

      stop("In the argument `pattern`, the ", info, " `", or, "` ", or_info, consequence, extra)
    }

    if(negate){
      res = c(res[!is_selected], res[is_selected])
    } else {
      res = c(res[is_selected], res[!is_selected])
    }

  }

  res
}



#' Splits a character vector into a data frame
#'
#' Splits a character vector and format the resulting substrings into a data.frame
#'
#' @param x A character vector or a two-sided formula. If a two-sided formula, then the argument `data` must be provided since the variables will be fetched in there. A formula is of the form `char_var ~ id1 + id2` where `char_var` on the left is a character variable and on the right `id1` and `id2` are identifiers which will be included in the resulting table. Alternatively, you can provide identifiers via the argument `id`.
#' @param data Optional, only used if the argument `x` is a formula. It should contain the variables of the formula.
#' @param split A character scalar. Used to split the character vectors. By default this is a regular expression.
#' @param id Optional. A character vector or a list of vectors. If provided, the values of `id` are considered as identifiers that will be included in the resulting table.
#' @param add.pos Logical, default is `FALSE`. Whether to include the position of each split element.
#' @param id_unik Logical, default is `TRUE`. In the case identifiers are provided, whether to trigger a message if the identifiers are not unique. Indeed, if the identifiers are not unique, it is not possible to reconstruct the texts based only on them.
#' @param fixed Logical, default is `FALSE`. Whether to consider the argument `split` as fixed (and not as a regular expression).
#' @param dt Logical, default is `FALSE`. Whether to return a `data.table`. See also the function `str_split2dt`.
#' @param ... Not currently used.
#'
#' @return
#' It returns a `data.frame` or a `data.table` which will contain: i) `obs`: the observation index, ii) `pos`: the position of the text element in the initial string (optional, via add.pos), iii) the text element, iv) the identifier(s) (optional, only if `id` was provided).
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
#' #
#' str_split2df(carname ~ am + gear + carb, base, " +", id_unik = FALSE, add.pos = TRUE)
#'
#'
str_split2df = function(x, data = NULL, split = NULL, id = NULL, add.pos = FALSE,
                        id_unik = TRUE, fixed = FALSE, dt = FALSE, ...){

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

    x = try(eval(fml[[2]], data, enclos = parent.frame()))

    # error handling
    if(inherits(x, "try-error") || !is.atomic(x)){
      vars = all.vars(fml[[2]])

      if(!is.null(names(data))){
        var_pblm = setdiff(vars, names(data))
        if(length(var_pblm) > 0){
          stop("The evaluation of the left side of `x` raised an error:\n",
               cub("PROBLEM: the variable{$s, enum.bq, is ? var_pblm} not in the data set (`"),
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

      val = try(eval(term, data, enclos = parent.frame()))

      # error handling
      if(inherits(val, "try-error") || !is.atomic(val)){
        vars = all.vars(term)

        intro = cub("The evaluation of the right side of `x` raised an error.\n",
                    "VALUE TO EVAL: {bq ? id_name}\n")

        if(!is.null(names(data))){
          var_pblm = setdiff(vars, names(data))
          if(length(var_pblm) > 0){
            stop(intro,
                 cub("PROBLEM: the variable{$s, enum.bq, is ? var_pblm} not in the data set (`"),
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
        if(max(n_id) != n) extra = dsb("\nPROBELM: len x: .[$$n ? n] len id: .[$$n ? max(n_id)].")
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
             dsb("\nPROBELM: len x: .[$$n ? n] len id: .[$$n ? n_id]."))
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

  x_split = strsplit(x, split, fixed = fixed, perl = !fixed)

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
      stop("To return a `data.table`, you need the `data.table` package to be installed. This is not the case.",
           "\nUse install.packages('data.table')?")
    }
  }

  res
}

str_split2dt = function(x, data = NULL, split = NULL, id = NULL, add.pos = FALSE,
                        id_unik = TRUE, fixed = FALSE){

  mc = match.call()
  str_split2df(x, data = data, split = split, id = id, add.pos = add.pos,
               id_unik = id_unik, fixed = fixed, mc = mc, dt = TRUE)
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



#' Cleans a character vector from multiple patterns
#'
#' Recursively cleans a character vector from several patterns.
#' Has shortcuts for common cleaning cases (normalizing white spaces, punctuation, etc)
#'
#' @param x A character vector.
#' @param ... Character scalars representing patterns. The function `gsub` with `replace = ""` is applied recursively to each of the patterns. By default regular expressions are used. To used fixed-search instead, you can add `#` as the first element of your pattern. Ex: `#.` will clean all the points. You can include the special pattern `@w` to normalize white spaces (trims + concatenate multiple WS). To `@w` you can add any of the following codes in any order: `p` for punctuation, `d` for digits, and `i` for isolated. Ex: `@wpi` will clean punctuation, isolated letters and will normalize white spaces. It must always start with `@W`.
#'
#' @return
#' It returns a character vector of the same length as the vector in input.
#'
#' @examples
#'
#'
#' x = c("hello world  ", "it's 5 am....")
#'
#' # we clean the o's and the points (we use # to trigger fixed-search)
#' str_clean(x, c("o", "#."))
#'
#' # equivalent regex
#' str_clean(x, "o|\\.")
#'
#' #
#' # using the special values @w
#'
#' # trim
#' str_clean(x, "@w")
#'
#' # regex + WS + punct + isolated
#' str_clean(x, c("o(?= )", "@wpi"))
#'
#'
str_clean = function(x, pat){
  check_character(x, mbt = TRUE)
  check_character(pat, no_na = TRUE, mbt = TRUE)

  res = x
  for(v in pat){
    if(substr(v, 1, 2) == "@w"){
      # special keys
      v = str_trim(v, 1)
      res = str_op(res, v)

    } else {

      first_char = substr(v, 1, 1)
      if(first_char == "\\" && substr(v, 2, 2) == "#"){
        v = str_trim(v, 1)
      }

      fixed = first_char == "#"
      if(fixed){
        v = str_trim(v, 1)
      }

      res = gsub(v, "", res, fixed = fixed, perl = !fixed)
    }
  }

  res
}





































































































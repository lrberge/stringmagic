#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-04-13
# ~: implementation of the quickselect paradigm
#------------------------------------------------------------------------------#





#' select values from a character vector or variables names from a data frame
#'
#' multi-faceted pattern-based selection of variables
#'
#' @param x Either a character vector or a data frame. In both cases, only a character 
#' vector will be returned.
#' @param ... Character scalar. A **comma separated** list of patterns to select the variables. Ex: "var1, var2".
#' Let `var1` be an element of this pattern. By default, the variable exactly named `var1` is selected, and 
#' if not present an error is raised. Use `^` as the first character to enable partial matching. 
#' The value `"^petal"` selects all variables starting with `"petal"`. Use  `@` (resp. `#`) 
#' as first character to enable regular expression (resp. fixed pattern) matching. For example `"@al$"` selects 
#' all variables ending with `"al"`. Use `!` as first character to select all 
#' variables **not matching** the pattern. The value `"!#petal"` selects only the variables **not** containing 
#' `"petal": and this works retroactively! Hence *all variables previously selected 
#' that do not pass the negation are dropped*. To *only include variables* through the negation, use the tag
#' `+` as first character. For example : `"petal.length, +!#petal"` selects the variable `petal.length`
#' and *adds* all variables not containing the term "petal". Not using the "+" would have dropped the first variable.
#' To only apply a restriction when negating, use the tag `-` as first character.
#' Use a single pipe to sort all the previous variables: `"^petal | #length"` will select all the variables 
#' starting with "petal" place the ones containing the term length first. To sort only the variables in a single
#' pattern, use the double pipe `||`: `"^petal, ^sepal | #length"` selects the variables starting with 
#' "petal" and "sepal" and within the "sepal" variables places the ones containing the term length first. 
#' After the pipes, use a comma separated list of values which work in the same way as the selection previously described. 
#' In case the argument `x` is a data set, you can use the special values `.num`, `.log`, `.lnum` (logical or numeric), 
#' `.fact`, `.char`, `.fchar` (factor or character), `.date` to select variables base on their types.
#' @param .order Optional, default is `NULL`. A character scalar. A comma separated list of patterns to 
#' order the variables. Works in the same way as the argument `pattern`. Partial reminder: you can use 
#' special first characters. `!` to negate, `^` to partially match, `@` for regular expressions. 
#' See more details in the help of the argument `...`.
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
selvars = function(x, ..., .order = NULL, .in = NULL, .pattern = NULL, .frame = parent.frame()){

  mc = match.call(expand.dots = FALSE)
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
  
  # we don't eval directly since the user is allowed to use NSE 
  mcdots = mc[["..."]]
  
  # internal arguments
  is_internal = isTRUE(mcdots$.is_internal)
  only_names = FALSE
  is_selnames = is_selvalues = FALSE
  if(is_internal){
    # only two possible internal functions
    is_selnames = isTRUE(mcdots$.is_selnames)
    is_selvalues  = isTRUE(mcdots$.is_selvalues)
    only_names = TRUE

    dots = list(...)
    mc = dots[[".mc"]]
    mcdots = mc[["..."]]
  } else {
    set_pblm_hook()
  }

  n_dots = length(mcdots)
  if(n_dots == 0 && missnull(.order) && missnull(.in)){
    stop_hook("Please provide at least one variable to select (`...` is empty) or provide",
              " the argument `.order` or the argument `.in`.")
  }

  dot_names = names(mcdots)
  if(is.null(dot_names)){
    dot_names = character(n_dots)
  }

  # we can be much more general than just data.frames (anything with names: lists, vectors, etc)
  # 'x' can be:
  # - a data set (data.frame)
  # - a list
  # - a named vector
  # - a matrix

  is_data_set = FALSE
  data = NULL
  old_class = oldClass(x)
  is_df = FALSE
  if(is_selvalues){
    # x must be a character vector!!!!
    if(!is.atomic(x)){
      stop_hook("The argument `x` must be a character vector (or convertible to it). ", 
                "PROBLEM: it is not atomic, instead it is of class {enum.bq ? class(x)}.")
    }

    # if x is atomic, we convert it to character
    if(!is.character(x)){
      x = as.character(x)
    }

  } else {
    # here x must be a data set
    is_data_set = TRUE
    data = x
    if(is.matrix(x)){   
      x = colnames(x)
      if(is.null(x)){
        x = as.character(1:ncol(x))
      }

      # we convert matrix stuff to DF
      is_df = TRUE
      old_class = "data.frame"
    } else if(is.atomic(x)){
      x = names(x)
      if(is.null(names(x))){
        stop_hook("If the argument `x` is a vector, it must have names.",
                  "\nPROBLEM: it currently has no names.")
      }
    } else if(is.list(x)){
      is_df = is.data.frame(x)
      x = names(x)
      if(is.null(x) || any(nchar(x) == 0)){
        stop_hook("The argument `x` must be a data set with valid names.",
        "\nPROBLEM: {=is.null(x) ; ",
                     "it has no `names` attribute! ; ",
                     "the {which, nth, enum.3 ? nchar(x) == 0} name{#s, are ? sum(nchar(x) == 0)} empty.}")
      }
    } else {
      stop_hook("The argument `x` must be a data set.",
                "\nPROBLEM: `x` is not like a data set, instead it is of class {enum.bq?class(x)}.")
    }
  }

  x_origin = x

  check_character(.order, null = TRUE, no_na = TRUE)
  check_character(.in, null = TRUE)
  check_character(.pattern, null = TRUE)

  # "final_" variables represent the outcome of the selection algorithm
  final_names = character(0)
  final_vars = character(0)

  if(!missnull(.in)){
    x = intersect(x, .in)
  } 
  
  if(!missnull(.pattern)){
    if(n_dots != 0){
      stop_hook("If the argument `.pattern` is provided, then there should be no argument in `...`. ",
                "\nPROBLEM: there are currently {n_dots} arguments in `...`.",
                "\nFIX: either remove the argument `.pattern`, either remove the values in `...`.")
    }

    mcdots = list(paste(.pattern, collapse = ", "))
    n_dots = 1
  }

  if(n_dots == 0){
    final_vars = final_names = x
  }
  
  ####
  #### main loop ####
  ####
  

  # prefix for variables to be evaluated
  prefix_eval = "eval:::"

  for(i in seq_len(n_dots)){
    mcdi = mcdots[[i]]

    if(is.numeric(mcdi)){
      mcdi = as.character(mcdi)
    } else if(length(mcdi) == 2 && is_operator(mcdi, "!") && is.numeric(mcdi[[2]])){
      mcdi = deparse_long(mcdi)
    }

    if(is.character(mcdi)){
      # This is the standard variable selection
      new_vars = selvars_main_selection(x, data, mcdi, dot_names[i])
      new_names = names(new_vars)

      i_keep = !new_vars %in% final_vars
      final_vars = c(final_vars, new_vars[i_keep])
      final_names = c(final_names, new_names[i_keep])
    } else {
      # This is something that needs checking
      # this is an expression, we will work on its deparsed value
      expr_dp = deparse(mcdi, width.cutoff = 500)
      if(length(expr_dp) > 1){
        stop_hook("Multi-line expressions are not supported in `...` ", 
                  "(it concerns: {'\\n'c, '20|...'k, bq ? expr_dp}).")
      }

      is_char_string = grepl("\"", expr_dp, fixed = TRUE)
      if(!is_char_string){
        # case where a variable has been selected
        # We will apply evaluation the data table way:
        # - preference for variables in the data set, if not found: in the frame
        # => add an option to disable that?
        # - use ..varname to explicilty use variables in the frame

        new_vars = paste0(prefix_eval, expr_dp)

        if(nchar(dot_names[i]) == 0){
          new_names = expr_dp
        } else {
          new_names = dot_names[i]
        }

        final_vars = c(final_vars, new_vars)
        final_names = c(final_names, new_names)
      
      } else {
        expr_dp_split = strsplit(expr_dp, '"')[[1]]
        values_to_expand = expr_dp_split[seq_along(expr_dp_split) %% 2 == 0]

        all_list_equal = TRUE
        n_exp = length(values_to_expand)

        # we stop right away if inconsistencies in the new name wrt the `*` placeholder
        n_stars = str_op(dot_names[i], "* X.fixed, len")
        if(!nchar(dot_names[i]) == 0 && !n_stars %in% c(0, n_exp)){
          
          stop_hook("The expression {bq, '50|...'k ? expr_dp} evaluates {n_exp} variable{#s}. ",
                    "The new name given in the `...` argument-name should have either 0, or {n_exp} `*` placeholders.",
                    "\nPROBLEM: the new name contains {=n_exp > n_stars ; only }{n_stars} such placeholder.")
        }

        vars_expanded_list = vector("list", n_exp)
        names_expanded_list = vector("list", n_exp)
        for(k in 1:n_exp){
          vars_expanded_list[[k]] = selvars_main_selection(x, data, values_to_expand[k])
          names_expanded_list[[k]] = names(vars_expanded_list[[k]])
          if(all_list_equal && k > 1){
            if(length(vars_expanded_list[[k]]) != length(vars_expanded_list[[k - 1]]) || 
                  any(vars_expanded_list[[k]] != vars_expanded_list[[k - 1]])){
              all_list_equal = FALSE
            }
          }
        }

        if(all_list_equal){
          vars_expanded = vars_expanded_list
          names_expanded = names_expanded_list
        } else {
          vars_expanded = as.list(do.call(expand.grid, vars_expanded_list))
          names_expanded = as.list(do.call(expand.grid, names_expanded_list))
        }

        # we create the call 
        # this looks complicated because we are general and allow several expansions
        n_expr_elem = length(expr_dp_split)
        new_expr_dp_list = vector("list", n_expr_elem)
        for(k in 1:n_expr_elem){
          if(k %% 2 == 0){
            index = k / 2
            new_expr_dp_list[[k]] = vars_expanded[[index]]
          } else {
            new_expr_dp_list[[k]] = expr_dp_split[k]
          }
        }

        new_expr_dp = do.call(paste0, new_expr_dp_list)
        
        # now the names (it's a bit tricky: we allow flexibility so we pay the price)
        expr_names_construct = expr_dp_split
        names_expr = NULL
        if(nchar(dot_names[i]) != 0){
          if(n_stars == 0){
            n_vars = length(new_expr_dp) 
            if(n_vars > 1){
              stop_hook("The expression {bq, '50|...'k ? expr_dp} leads to {n_vars} variables. ",
                    "But the new name given in the `...` argument-name does not contain a `*` placeholder.",
                    "\nFIX: please provide a new name with {len ? vars_expanded} `*` placeholder{$s} in it.")
            }

            names_expr = dot_names[i]
          } else {
            expr_names_construct = strsplit(dot_names[i], "*", fixed = TRUE)[[1]]
            if(length(expr_names_construct) < n_stars + 1){
              expr_names_construct[n_stars + 1] = ""
            }
            # we need to add 1 bc:
            # "*_mean" => "" "_mean" (len 2)
            # "mean_*" => "mean_"    (len 1)
            # the behavior of splitting is kind of inconsistent
          }
        }

        if(is.null(names_expr)){
          n_expr_names = length(expr_names_construct)          
          names_expr_list = vector("list", n_expr_names)
          for(k in 1:n_expr_names){
            if(k %% 2 == 0){
              index = k / 2
              names_expr_list[[k]] = names_expanded[[index]]
            } else {
              names_expr_list[[k]] = expr_names_construct[k]
            }
          }

          names_expr = do.call(paste0, names_expr_list)
        }

        # we append 'eval:::' to diffenciate it from regular variables
        new_expr_dp = paste0(prefix_eval, new_expr_dp)
        
        final_vars = c(final_vars, new_expr_dp)
        final_names = c(final_names, names_expr)
      }
    }
  }

  # we may need to uniquify the names
  final_names = uniquify(final_names)

  #
  # final ordering ####
  #
  

  if(!missnull(.order)){
    if(length(.order) > 1){
      .order = paste(.order, collapse = ", ")
    }

    if(grepl("[|;=]", .order)){
      stop_hook("In argument `.order`, only regular variable selection is allowed ",
                "(i.e. a comma separated list of patterns).",
                "\n Current `.order`: {Q ? .order}",
                "\nPROBLEM: the character{$s} {'[|;=]'X, unik, enum.bq ? .order} are forbidden, please remove them.")
    }

    # note that the use of ".num" etc values can be problematic here
    # since "new" evaluated variables may not be in the data
    # => we need to evaluate the new data beforehand (but it's a bit costly)

    new_vars = selvars_internal(final_vars, all_vars, data, .order, TRUE)
    final_names = final_names[match(new_vars, final_vars)]
    final_vars = new_vars
  }

  if(only_names){
    res = setNames(final_vars, final_names)
    return(res)
  }

  ####
  #### variable creation ####
  ####
  

  # Final step: creation of the data set
  # evaluation of the variables
  n_vars = length(final_vars)
  new_values_list = vector("list", n_vars)
  is_eval = grepl(paste0("^", prefix_eval), final_vars)
  data_names = names(data)
  data_list = convert_to_list(data)
  n_obs = length(data_list[[1]])
  for(i in 1:n_vars){
    vi = final_vars[i]

    if(!is_eval[i]){
      new_values = data_list[[vi]]
    } else {
      expr_dp = substr(vi, nchar(prefix_eval) + 1, nchar(vi))
      expr = try(str2lang(expr_dp))
      if(inherits(expr, "try-error")){
        stop_hook("The values of the variables to be created must be valid R expressions.",
                  "\nPROBLEM: the value {bq?expr_dp} could not be parsed.")
      }

      all_vars = all.vars(expr)

      # tha vriables starting with '..' are fetched in the environment
      # in practice we add them to the list of evaluation
      if(any(grepl("^\\.\\.", all_vars))){
        vars2eval = str_op(all_vars, "'^\\.\\.'get, '^\\.\\.'r")
        for(v in vars2eval){
          if(exists(v, frame = .frame, inherits = TRUE)){
            data_list[[paste0("..", v)]] = eval(str2lang(v), .frame)
          } else {
            stop_hook("The variable {bq?v} (prefixed with '..' in the call) must exist ",
                      "in the environment (as all variables prefixed with '..').",
                      "\nPROBLEM: the variable {bq?v} cannot be found.")
          }
        }
      }
      
      # the remaining variables must be either in the data set or in the calling env
      all_vars_no_prefix = str_get("!^\\.\\.", all_vars)
      vars_doubt = setdiff(all_vars_no_prefix, data_names)
      for(v in vars_doubt){
        if(!exists(v, frame = .frame, inherits = TRUE)){
          sugg = suggest_item(v, data_names)
          sugg_txt = cub("{$(;;\nMaybe you meant {$enum.bq.or}?) ? sugg}")
          stop_hook("All variables must be either in the data set, either in the environment.",
                    "\nPROBLEM: the variable {bq?v} is not in the data set nor in the environment.", 
                    sugg_txt)
        }
      }

      new_values = eval(expr, data_list, enclos = .frame) 
    }

    new_values_list[[i]] = new_values
  }
  
  if(is_df){
    new_n_all = lengths(new_values_list)
    if(any(new_n_all != new_n_all[1])){
      # means there are at least two different numbers
      qui_pblm = which(new_n != n_obs & new_n != 1)

      if(length(qui_pblm) > 0){
        i_pblm = qui_pblm[1]
        vi = final_vars[i_pblm]
        var_pblm = str_clean(vi, cub("^{prefix_eval}"))
        new_n = new_n_all[i_pblm]
        stop_hook("The evaluation of {bq?var_pblm} led to a vector of {n?new_n} observations.",
                  "\nPROBLEM: there is a mismatch with the number of observations in the data set, equal to {n?n_obs}.")  
      }

      # we normalize the single unit values
      for(i in which(n_new == 1)){
        new_values_list[[i]] = rep(new_values_list[[i]], n_obs)
      }
    }
  }
  
  names(new_values_list) = final_names

  res = new_values_list

  if(is_df){
    # should we keep the row names? No at 80%.
    attr(res, "row.names") = .set_row_names(length(res[[1]]))
    oldClass(res) = old_class
    names(res) = final_names
  }

  res
}

selvars_main_selection = function(all_vars, data, pattern, dot_name = ""){
  # Returns the variables to keep
  # x: vector of variables
  # x_origin: original vector of variables
  # data: the data (optional)
  # pattern: the pattern, must be of length 1
  # dot_name: name given by the user in the ... argument

  current_vars = all_vars
  
  pat_parsed = cpp_parse_charselect(pattern)

  # parsed names: we use the list names as the default
  pnames = pat_parsed$names
  pnames[nchar(pnames) == 0] = dot_name

  new_names = pnames
  all_patterns = pat_parsed$patterns
  order_pat = pat_parsed$order
  order_type = pat_parsed$otype
  
  final_names = character(0)
  final_vars = character(0)
  for(i in seq_along(all_patterns)){
    p = all_patterns[i]

    selection = selvars_internal(current_vars, all_vars, data, p, FALSE)

    if(selection$negate){
      if(i == 1){
        final_vars = setdiff(current_vars, selection$vars)
        final_names[final_vars] = new_names[i]        

        if(order_type[i] == 2){
          final_vars = selvars_internal(final_vars, all_vars, data, order_pat[i], TRUE)
        }
      } else {
        final_vars = setdiff(final_vars, selection$vars)
        current_vars = final_vars
        # double pipe ordering is meaningless here
      }
    } else {
      i_keep = !selection$vars %in% final_vars
      new_vars = selection$vars[i_keep]
      final_names[new_vars] = new_names[i]

      if(order_type[i] == 2){
        new_vars = selvars_internal(new_vars, all_vars, data, order_pat[i], TRUE)
      }

      final_vars = c(final_vars, new_vars)
    }

    if(order_type[i] == 1){
      final_vars = selvars_internal(final_vars, all_vars, data, order_pat[i], TRUE)
    }
  }

  # taking care of the names
  final_names = final_names[final_vars]
  is_empty = nchar(final_names) == 0
  final_names[is_empty] = final_vars[is_empty]
  
  # taking care of placeholders
  i_star = grepl("*", final_names, fixed = TRUE)
  if(any(i_star)){
    names_star = final_names[i_star]
    before = gsub("\\*.*", "", names_star)
    after = gsub(".*\\*", "", names_star)
    final_names[i_star] = paste0(before, final_vars[i_star], after)
  }

  names(final_vars) = final_names
  return(final_vars)
}

selvars_internal = function(x, x_origin, data, pattern, is_order){
  # Returns the variables to keep
  # x: vector of variables
  # x_origin: original vector of variables
  # data: the data (optional)
  # pattern: the pattern, must be of length 1
  # is_order: whether it is about sorting or selecting
  #
  # if is_order == TRUE: a character vector is returned (vars)
  # if is_order == FALSE: a list of two elements, vars and negate, is returned

  if(!is_order && nchar(pattern) == 0){
    return(character(0))
  }

  if(is_order){
    pattern = str_op(pattern, "'[, \t\n]+'S")
  }

  data_list = NULL

  n_pat = length(pattern)
  n_x = length(x)
  all_vars = setNames(x, 1:n_x)

  for(i in n_pat:1){
    # we take the reverse order for order to work properly
    # no consequence for selection since it is of length 1

    p = p_raw = pattern[i]
    p_clean = NULL
    first_char = substr(p, 1, 1)

    is_addition = first_char == "+"

    if(is_order && (is_addition)){
      stop_hook("When ordering the variables, the tag `+` is invalid since it is only ", 
                "used to select the data.",
                "\nFIX: remove the {bq?first_char} in {bq?p_raw}.")
    }

    if(is_addition){
      p = substr(p, 2, nchar(p))
      first_char = substr(p, 1, 1)
      all_vars = x_origin
    }

    negate = first_char == "!"
    if(negate){
      p = substr(p, 2, nchar(p))
      first_char = substr(p, 1, 1)
    }

    if(p == "."){
      # => all the remaining variables
      if(negate){
        stop_hook("You cannot use the pattern {bq?p_raw}: it is equivalent to removing all variables!")
      }

      is_selected = rep(TRUE, length(all_vars))
    } else if(p %in% c(".num", ".log", ".lnum", ".fact", ".char", ".fchar", ".date")){
      if(is.null(data)){
        stop_hook("The special value {bq?p} can only be used when the argument `x` is a data set. ",
                  "It is meaningless when `x` is a character vector.")
      }

      type_fun = switch(p,
                        .num = is.numeric,
                        .log = is.logical,
                        .lnum = function(x) is.numeric(x) || is.logical(x),
                        .fact = is.factor,
                        .char = is.character,
                        .fchar = function(x) is.factor(x) || is.character(x),
                        .date = function(x) any(grepl("date", class(x), ignore.case = TRUE)))

      if(is.null(data_list)){
        data_list = as.list(data)
        data_list = data_list[all_vars]
      }

      is_selected = sapply(data_list, type_fun)

      if(!any(is_selected)){
        info = switch(p,
                      .num = "numeric",
                      .log = "logical",
                      .lnum = "numeric or logical",
                      .fact = "factor",
                      .char = "character",
                      .fchar = "factor or character",
                      .date = "date")

        stop_hook("The special value {bq?p} did not find any variable. ",
                  "Maybe you could check that there are {info} variables in the data set?")
      }

    } else if(is_numeric_in_char(p)){
      # this is an index
      # indexes ALWAYS refer to the original positionning

      p_num = as.numeric(p)
      if(p_num > length(x_origin)){
        stop_hook("Numeric indexes must be lower or equal to the number of variables in the data set.",
                  "\nPROBLEM: The value {bq?p} is larger than the total number of variables: {len?x_origin}.")
      }

      is_selected = logical(length(all_vars))
      is_selected[p_num] = TRUE

    } else if(first_char %in% c("#", "@", "^", "$")){
      # fixed char search
      fixed = first_char == "#"
      p = substr(p, 2, nchar(p))

      if(first_char == "^"){
        p_clean = p
        p = paste0("^\\Q", p, "\\E")
      } else if(first_char == "$"){
        p_clean = p
        p = paste0("\\Q", p, "\\E$")
      }

      is_selected = grepl(p, all_vars, perl = !fixed, fixed = fixed)

    } else {
      is_selected = all_vars == p
    }

    if(!any(is_selected) || (negate && all(is_selected))){
      info = switch(first_char,
                    "#" = "fixed pattern",
                    "@" = "regular expression",
                    "^" = "starts with pattern",
                    "$" = "ends with pattern",
                    "value")

      if(!is.null(p_clean)) p = p_clean

      p_info = if(p != p_raw) paste0("(raw is `", p_raw, "`) ") else ""
      
      is_restricted = length(x_origin) > length(x)
      consequence = cub("led to no variable being selected{=is_restricted ; in the current selection (after",
                        " the restrictions have been applied)}.")

      if(is_restricted){
        # The problem may come from the fact there was a restriction before, the variable exists
        # but not in the current set
        
        if(first_char %in% c("#", "@", "^", "$")){
          is_selection_all = grepl(p, x_origin, perl = !fixed, fixed = fixed)
        } else {
          is_selection_all = x_origin == p
        }

        if(any(is_selection_all)){
          stop_hook("The value {bq?p_raw} led to no value being selected. ",
                    "This comes from the restrictions that you previously applied (using `!`).",
                    "\nFIX: to add a variable despite the restrictions, either:",
                    "\n     a) write your pattern in a separate argument in `...` (restrictions ",
                    "are cancelled btw args.)",
                    "\n     b) add a `+` to specify that you want to fetch the variable from ",
                    "the original pool (like: `+{p_raw}`)")
        }
        
      }

      sugg = suggest_item(p, all_vars)

      extra = ""
      if(info == "value"){
        consequence = paste0("is not ", ifelse(!is.null(data), "a variable from the data set.", "present."))  
        sugg_txt = cub("{$(;;\nMaybe you meant {$enum.bq.or.6}?) ? sugg}")
        extra = paste0(sugg_txt, " Note that you can use regex with `@` and partial matching with `^`.")
      } else if(length(sugg) > 0){
        sugg_txt = cub("\nFYI here {$are ? sugg} a {$(;few )}variable{$s} that {$are} close: {$enum.bq.or.6}.")
        extra = sugg_txt
      }

      stop_hook("The ", info, " `", p, "` ", p_info, consequence, extra)
    }

    if(is_order){
      if(negate){
        all_vars = c(all_vars[!is_selected], all_vars[is_selected])
      } else {
        all_vars = c(all_vars[is_selected], all_vars[!is_selected])
      }
    } else {
      # simple selection: one single pattern
      # if(negate){
      #   vars_select = all_vars[!is_selected]
      # } else {
      #   vars_select = all_vars[is_selected]
      # }
      if(negate && is_addition){
        negate = FALSE
        vars_select = all_vars[!is_selected]
      } else {
        vars_select = all_vars[is_selected]
      }      

      res = list(vars = vars_select, negate = negate)

      return(res)
    }    
  }

  # if here: bc is_order = TRUE: we only return the vector
  res = all_vars

  return(res)
}







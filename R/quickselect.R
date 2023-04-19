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
selvars = function(.x, ..., .order = NULL, .in = NULL, .pattern = NULL, .frame = parent.frame(),
                   .ignore.case = TRUE){

  mc = match.call(expand.dots = FALSE)

  if(missing(.x)){
    stop("Argument '.x' must be provided. PROBLEM: it is missing.")
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
  if(n_dots == 0 && missnull(.order) && missnull(.in) && missnull(.pattern)){
    stop_hook("Please provide at least one variable to select (`...` is empty) or provide",
              " the argument `.pattern`, `.order` or the argument `.in`.")
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

  x = .x
  is_data_set = FALSE
  data = NULL
  old_class = oldClass(x)
  old_attr = attributes(x)
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
      data = convert_to_list(data)
      is_df = TRUE
      old_class = "data.frame"
    } else if(is.atomic(x)){
      x = names(x)
      if(is.null(names(x))){
        stop_hook("If the argument `x` is a vector, it must have names.",
                  "\nPROBLEM: it currently has no names.")
      }
      # conversion
      data = convert_to_list(data)
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
    dot_names = rep("", length(mcdots))
    n_dots = 1
  }

  if(n_dots == 0){
    final_vars = final_names = x
  }
  
  ####
  #### main loop ####
  ####
  

  is_eval = logical(0)
  previous_vars = previous_names = NULL
  index_iprev = NULL

  for(i in seq_len(n_dots)){
    expr = mcdots[[i]]

    if(is.character(expr)){
      # This is the standard variable selection
      new_vars = selvars_main_selection(x, data, expr, dot_names[i], .ignore.case)
      new_names = names(new_vars)

      i_keep = !new_vars %in% final_vars

      previous_vars = new_vars[i_keep]
      previous_names = new_names[i_keep]
      if(i < n_dots){
        index_iprev = seq_along(previous_vars) + length(final_vars)
      }

      final_vars = c(final_vars, previous_vars)
      final_names = c(final_names, previous_names)
      is_eval = c(is_eval, logical(length(previous_vars)))
    } else {
      # Here expressions end up always evaluated.
      # But the user can summon variable expansion by using backtick quoted
      # expressions. These will be expanded and we need to find out if there are some.
      
      expr_origin = expr
      expr = flag_expansion_patterns(expr)

      expr_dp = deparse(expr, width.cutoff = 500L)
      if(length(expr_dp) > 1){
        stop_hook("Multi-line expressions are not supported in `...` ", 
                  "(it concerns: {'\\n'c, '20|...'k, bq ? expr_dp}).")
      }

      is_expansion = grepl("_P_A_T_", expr_dp, fixed = TRUE)

      if(!is_expansion){
        # we move along after cleaning up & add it only if not already there
        if(!expr_dp %in% final_vars){

          if(.ignore.case){
            # we fix the case of regular variables if necessary
            vars_expr = all.vars(expr)
            var_pblm = setdiff(vars_expr, x)
            for(v in var_pblm){
              is_ok = cpp_equal_ignore_case(v, x)
              if(any(is_ok)){
                var_fixed = x[is_ok]
                if(length(expr) == 1){
                  expr_dp = var_fixed
                } else {
                  pat = paste0("(?<=^|[^[:alnum:]_])", v, "(?=$|[^[:alnum:]._])")
                  expr_dp = gsub(pat, var_fixed, expr_dp, perl = TRUE)
                }                
              }
            }
          }

          new_vars = expr_dp

          if(nchar(dot_names[i]) == 0){
            new_names = expr_dp
          } else {
            new_names = dot_names[i]
          }

          final_vars = c(final_vars, new_vars)
          final_names = c(final_names, new_names)
          # length(expr) == 1 means it's a regular variable name
          is_eval = c(is_eval, length(expr) != 1)

          # we do not accept .prev or .iprev following single variables
          previous_vars = previous_names = NULL
        }
      
      } else {

        # we first find out:
        # - expr_dp_split
        # - values_to_expand

        if(length(expr) == 1 && inherits(expr, "name")){
          # special case: avoids the costly perl expression stuff
          values_to_expand = as.character(expr)
          expr_dp_split = c("", values_to_expand, "")
        } else {
          expr_dp_split = strsplit(expr_dp, '"?_P_A_T_"?')[[1]]
          values_to_expand = expr_dp_split[seq_along(expr_dp_split) %% 2 == 0]
        }

        all_list_equal = TRUE
        n_exp = length(values_to_expand)

        # we stop right away if inconsistencies in the new name wrt the `*` placeholder
        find_stars = gregexpr("*", dot_names[i], fixed = TRUE)[[1]]
        n_stars = if(length(find_stars) == 1 && find_stars == -1) 0 else length(find_stars)
        if(!nchar(dot_names[i]) == 0 && !n_stars %in% c(0, n_exp)){
          
          expr_dp_origin = deparse_long(expr_origin)
          stop_hook("The expression {bq, '50|...'k ? expr_dp_origin} evaluates {n_exp} variable{#s}. ",
                    "The new name given in the `...` argument-name should have either 0 or {n_exp} `*` placeholders.",
                    "\nPROBLEM: the new name (here {bq?dot_names[i]}) contains {=n_exp > n_stars ; only }{n_stars} such placeholder{#s}.")
        }

        is_insert_previous = FALSE
        is_prev = FALSE
        vars_expanded_list = vector("list", n_exp)
        names_expanded_list = vector("list", n_exp)
        for(k in 1:n_exp){
          if(values_to_expand[k] %in% c(".prev", ".iprev")){
            is_prev = TRUE
            val = values_to_expand[k]
            if(is.null(previous_vars)){
              stop_hook("The special variable {bq?val} is not used in the right context.",
                        "\nIt should only be used in an expression that follows a previous expansion.",
                        "\nExample: selvars(head(iris), `^petal`, \"*_mean\" = mean({val}))")
            }
            
            if(val == ".iprev" && !all_list_equal){
              stop_hook("The special variable {bq?val} is not used in the right context.",
                        "\nIt should be used only in calls containing only one set of variables to be expanded.",
                        "\nExample: - good: selvars(head(iris), `^petal`, \"*_2\" = {val} ^2 )",
                        "\n         -  bad: selvars(head(iris), `^petal`, \"*_per_*\" = .iprev / `^sepal`))",
                        "\n In the last case, use `.prev` instead of `.prev` and it should work.")
            }

            is_insert_previous = val == ".iprev"
            vars_expanded_list[[k]] = previous_vars
            names_expanded_list[[k]] = previous_names
          } else if(k > 1 && values_to_expand[k] == "."){
            vars_expanded_list[[k]] = vars_expanded_list[[k - 1]]
            names_expanded_list[[k]] = names_expanded_list[[k - 1]]

          } else {
            vars_expanded_list[[k]] = selvars_main_selection(x, data, values_to_expand[k], "", .ignore.case)
            names_expanded_list[[k]] = names(vars_expanded_list[[k]])
          }
          
          if(all_list_equal && k > 1){
            if(length(vars_expanded_list[[k]]) != length(vars_expanded_list[[k - 1]]) || 
                  any(vars_expanded_list[[k]] != vars_expanded_list[[k - 1]])){
              all_list_equal = FALSE
            }
          }
        }

        # saving the expansions for later use
        if(!is_prev && i < n_dots){
          previous_vars = vars_expanded_list[[1]]
          previous_names = names_expanded_list[[1]]
        }        

        if(all_list_equal){
          vars_expanded = vars_expanded_list
          names_expanded = names_expanded_list
        } else {
          vars_expanded = as.list(do.call(expand.grid, vars_expanded_list))
          names_expanded = as.list(do.call(expand.grid, names_expanded_list))
        }

        #
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
        
        #
        # now the names (it's a bit tricky: we allow flexibility so we pay the price)
        expr_names_construct = expr_dp_split
        names_expr = NULL
        if(nchar(dot_names[i]) == 0){
          # we construct default names
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

        } else {
          # User-provided names
          if(n_stars == 0){
            n_vars = length(new_expr_dp) 
            if(n_vars > 1){
              expr_dp_origin = deparse_long(expr_origin)
              stop_hook("The expression {bq, '50|...'k ? expr_dp_origin} leads to {n_vars} variables. ",
                    "But the new name given in the `...` argument-name does not contain a `*` placeholder.",
                    "\nFIX: please provide a new name with {len ? vars_expanded} `*` placeholder{$s} in it.")
            }

            names_expr = dot_names[i]
          } else {
            names_expr = fill_the_placeholders(dot_names[i], names_expanded)            
          }
        }

        if(is_insert_previous){
          final_vars = insert_at(final_vars, new_expr_dp, index_iprev)
          final_names = insert_at(final_names, names_expr, index_iprev)
          is_eval = insert_at(is_eval, rep(TRUE, length(new_expr_dp)), index_iprev)
          index_iprev = index_iprev + 1:length(index_iprev)

        } else {

          if(i < n_dots){
            index_iprev = seq_along(new_expr_dp) + length(final_vars)
          }

          final_vars = c(final_vars, new_expr_dp)
          final_names = c(final_names, names_expr)
          is_eval = c(is_eval, rep(TRUE, length(new_expr_dp)))
        }
        
      }
    }
  }

  # we remove duplicated variables
  i_dup = duplicated_xy(final_vars, final_names)
  if(length(i_dup) > 0){
    final_vars = final_vars[-i_dup]
    final_names = final_names[-i_dup]
    is_eval = is_eval[-i_dup]
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

    if(grepl("[/=]", .order)){
      stop_hook("In argument `.order`, only regular variable selection is allowed ",
                "(i.e. a comma separated list of patterns).",
                "\n Current `.order`: {Q ? .order}",
                "\nPROBLEM: the character{$s} {'[/=]'X, unik, enum.bq ? .order} are forbidden, please remove them.")
    }

    # note that the use of ".num" etc values can be problematic here
    # since "new" evaluated variables may not be in the data
    # => we need to evaluate the new data beforehand (but it's a bit costly)

    new_vars = selvars_internal(final_vars, data, .order, TRUE, .ignore.case)
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
  
  # Groups... what do we do with groups?
  # - we first make the checking of the variables withtout evaluating
  # - in a second step we evaluate:
  #   a) first on a tiny subset to see if ALL the expressions are aggregations
  #   b) depending on this first check, either we loop on groups, either not

  n_vars = length(final_vars)

  is_group = "groups" %in% names(old_attr)
  all_expr = NULL
  if(is_group){
    all_expr = vector("list", n_vars)
  }
  new_values_list = vector("list", n_vars)
  data_names = names(data)
  data_list = convert_to_list(data)
  n_obs = length(data_list[[1]])
  for(i in 1:n_vars){
    vi = final_vars[i]

    if(!is_eval[i]){
      if(is_group){
        all_expr[[i]] = vi
      }
      
      new_values = data_list[[vi]]
    } else {
      expr_dp = vi
      expr = try(str2lang(expr_dp))
      if(inherits(expr, "try-error")){
        stop_hook("The values of the variables to be created must be valid R expressions.",
                  "\nPROBLEM: the value {bq?expr_dp} could not be parsed.")
      }

      if(is_group){
        all_expr[[i]] = expr
      }

      all_vars = all.vars(expr)

      # tha vriables starting with '..' are fetched in the environment
      # in practice we add them to the list of evaluation
      if(any(grepl("^\\.\\.", all_vars))){
        vars2eval = str_op(all_vars, "'^\\.\\.'get, '^\\.\\.'r")
        for(v in vars2eval){
          if(exists(v, envir = .frame, inherits = TRUE)){
            data_list[[paste0("..", v)]] = eval(str2lang(v), .frame)
          } else {
            stop_hook("The variable {bq?v} (prefixed with '..' in the call) must exist ",
                      "in the environment (as all variables prefixed with '..').",
                      "\nPROBLEM: the variable {bq?v} cannot be found.")
          }
        }
      }
      
      # the remaining variables must be either in the data set or in the calling env
      all_vars_no_prefix = str_get(all_vars, "!^\\.\\.")
      vars_doubt = setdiff(all_vars_no_prefix, data_names)
      for(v in vars_doubt){
        if(!exists(v, envir = .frame, inherits = TRUE)){
          sugg = suggest_item(v, data_names)
          sugg_txt = cub("{$(;;\nMaybe you meant {$enum.bq.or}?) ? sugg}")
          stop_hook("All variables must be either in the data set, either in the environment.",
                    "\nPROBLEM: the variable {bq?v} is not in the data set nor in the environment.", 
                    sugg_txt)
        }
      }

      if(!is_group){
        new_values = eval(expr, data_list, enclos = .frame) 
      }      
    }

    if(!is_group){
      new_values_list[[i]] = new_values 
    }
    
  }

  ####
  #### Grouping operations ####
  ####
  
  # see explanation above the previous loop
  if(is_group){
    vars_small = character(0)
    data_list_small = data_list
    is_agg = logical(n_vars)

    for(i in 1:n_vars){
      expr = all_expr[[i]]
      data_vars = intersect(data_names, all.vars(expr))

      for(v in setdiff(data_vars, vars_small)){
        vars_small = c(vars_small, v)
        data_list_small[[v]] = head(data_list[[v]])
      }

      new_values = eval(expr, data_list_small, enclos = .frame) 
      is_agg[i] = is.list(new_values) || length(new_values) == 1
    }

    # we always add the grouping variables if they're not there
    groups = old_attr$groups
    n_gvars = ncol(groups) - 1
    gnames = names(groups)[1:n_gvars]

    gvar_to_add = setdiff(gnames, final_vars)
    offset = length(gvar_to_add)
    final_names = c(gvar_to_add, final_names)
    final_vars = c(gvar_to_add, final_names)    

    new_values_list = vector("list", n_vars + offset)

    all_agg = all(is_agg)
    if(all_agg){
      for(i in seq_len(offset)){
        new_values_list[[i]] = groups[[gvar_to_add[i]]]
      }
    } else {
      var_pblm = setdiff(gvar_to_add, data_names)
      if(length(var_pblm) > 0){
        stop_hook("The grouping variable{$s, enum.bq ? var_pblm} are not in the data set. That should not be the case.",
                  "\nIf you think this is legitimate and this case should be handled: can you open a GH issue with an example?")
      }

      for(i in seq_len(offset)){
        new_values_list[[i]] = data_list[[gvar_to_add[i]]]
      }
    }
    
    i_no_agg = which(!is_agg)
    for(i in i_no_agg){
      expr = all_expr[[i]]
      if(is.character(expr)){
        new_values = data_list[[expr]]
      } else {
        new_values = eval(expr, data_list, enclos = .frame)
      }
      
      new_values_list[[i + offset]] = new_values
    }

    if(any(is_agg)){
      # All this is super slow..... sigh...

      # A) we find all the variables for which to subselect
      
      i_agg = which(is_agg)
      data_vars = character(0)
      for(i in i_agg){
        expr = all_expr[[i]]
        data_vars = c(data_vars, intersect(data_names, all.vars(expr)))
      }
      data_vars = unique(data_vars)

      # B) group wise operations
      
      obs_list = groups[[n_gvars + 1]]
      n_g = nrow(groups)
      n_total = if(all_agg) n_g else n_obs
      for(g in 1:n_g){
        # subselection
        obs_keep = obs_list[[g]]
        for(v in data_vars){
          data_list_small[[v]] = data_list[[v]][obs_keep]
        }

        for(i in i_agg){
          expr = all_expr[[i]]
          if(is.character(expr)){
            new_values = data_list_small[[expr]]
          } else {
            new_values = eval(expr, data_list_small, enclos = .frame)
          }

          if(g == 1){
            # not 100% sure it's worth it doing this initialization 
            # (may be error prone if evals can be of != types -- but so be it)
            vec = vector(mode(new_values), n_total)
            if(all_agg){
              vec[[1]] = new_values
            } else {
              vec[obs_keep] = new_values
            }
            
            new_values_list[[i + offset]] = vec
          } else {
            if(all_agg){
              new_values_list[[i + offset]][[g]] = new_values
            } else {
              if(is.list(new_values)){
                new_values = list(new_values)
              }
              new_values_list[[i + offset]][obs_keep] = new_values
            }
          }
        }
      }
    }

  }

  
  if(is_df){
    new_n_all = lengths(new_values_list)
    if(any(new_n_all != new_n_all[1])){
      # means there are at least two different numbers
      qui_pblm = which(new_n_all != n_obs & new_n_all != 1)

      if(length(qui_pblm) > 0){
        i_pblm = qui_pblm[1]
        vi = final_vars[i_pblm]
        new_n = new_n_all[i_pblm]
        stop_hook("The evaluation of {bq?vi} led to a vector of {n?new_n} observations.",
                  "\nPROBLEM: there is a mismatch with the number of observations in the data set, equal to {n?n_obs}.")  
      }

      # we normalize the single unit values
      n_obs = max(new_n_all)
      for(i in which(new_n_all == 1)){
        new_values_list[[i]] = rep(new_values_list[[i]], n_obs)
      }
    }
  }
  
  names(new_values_list) = final_names

  res = new_values_list

  if(is_df){
    # should we keep the row names? No at 80%.
    attr(res, "row.names") = .set_row_names(length(res[[1]]))

    if(is_group && all_agg){
      old_class = str_get(old_class, "!group")
      old_attr$groups = NULL
    }

    oldClass(res) = old_class
    # we copy the attributes
    for(a in setdiff(names(old_attr), c("class", "row.names", "names"))){
      attr(res, a) = old_attr[[a]]
    }

    names(res) = final_names
  }

  res
}

selvars_main_selection = function(all_vars, data, pattern, dot_name = "", .ignore.case = TRUE){
  # Returns the variables to keep
  # x: vector of variables
  # data: the data (optional)
  # pattern: the pattern, must be of length 1
  # dot_name: name given by the user in the ... argument

  current_vars = all_vars

  ####
  #### help ####
  ####

  first_char = substr(pattern, 1, 1)
  if(any(first_char == "?")){
    # The user requests help
    # - +/-: order ascending/descending
    # - #: the index of the variable
    # - regex: search for patterns in the variables

    p = pattern[first_char == "?"][1]

    p = substr(p, 2, nchar(p))
    first_char = substr(p, 1, 1)

    if(first_char == "?"){
      # generic quickselect help
      msg = paste0("Quickselect (QS) cheat sheet. A QS pattern is a comma separated list of patterns. Here are possible patterns (example from iris data set):",
                "\n - `petal.width`: selects the variable named 'Petal.Width' (case is ignored)",
                "\n - `!petal.width`: starting with a `!` negates the pattern (works with all patterns), here it selects all variables but `Petal.Width`",
                "\n - `^petal`: means starts with 'petal",
                "\n - `$width`: means ends with 'width'",
                "\n - `@\\d`: `@` is followed with a regular expression, here means 'contains a digit'",
                "\n - `#.`: `#` is followed with a fixed pattern, here means contains a dot ",
                "\n - `5`: variable in position 5",
                "\n - `-1`: variable in last position (negative number: start from the end)",
                "\n - `^petal & $width`: combine QS expressions with logical operators (here means starts with 'petal' and ends with 'width')",
                "\n - `.`: means all remaining variables",
                "\n - type-patterns: the following patterns select the variables based on their types: ",
                "`.num`, `.log`, `.lnum`, `.fact`, `.char`, `.fchar`, `.date`, `.list`",
                "\n - `2:species`: use ranges with the colon, here means from the second variable to the variable named Species. Each end of the range must point to only one element",
                "\n Examples: a) selvars(head(iris), \"^spe, $width\") ", 
                "\n           b) selvars(head(iris), \"-2:@pe.+len\")",
                "\n           c) selvars(head(iris), \"!.num, .num & #wid, .\")",
                "\n RENAMING:",
                "\n - you can rename a variable with the syntax new_name = old_name",
                "\n - when several variables are selected, you can use the placeholder `*`",
                "\n - you can apply replacement patterns to variables names using the syntax `{*:old => new}`.",
                " This transforms the `old` regular expression into `new`",
                "\n Example: selvars(head(iris), \"species = ^spe, {*:.Width => _w} = $width\")",
                "\n EXPRESSIONS:",
                "\n In `selvars` and in data.table quickselect, you can use expressions to create new variables.",
                "\n If you want to quickselect several variables, you can add a QS pattern in backticks, as in:",
                "\n Ex: selvars(head(iris), \"*_mean\" = mean(`^petal`))",
                "\n Note that special pattern variables don't require backticks:",
                "\n Ex: selvars(head(iris), \"*_mean\" = mean(.num))",
                "\n In this context, you have access to two special variables:",
                "\n - `.prev`: refers to the previously selected variables, only works in expressions",
                "\n - `.iprev`: like `.prev` but the new values are inserted in between the previous variables, only works in expressions",
                '\n Example: a) selvars(head(iris), "{*:\\. => _} = .num & ^petal", "*_r0" = round(.prev))',
                '\n          b) selvars(head(iris), "!.fchar", "*_r0" = round(.iprev))'
                )
      msg = fit_screen(msg, 0.8)
      on.exit(message(msg))

      stop("Quickselect help summoned. Here it is.")

    }
    
    var_index = seq_along(all_vars)
    if(first_char %in% c("+", "-")){
      var_order = order(all_vars, decreasing = first_char == "-")
      all_vars = all_vars[var_order]
      var_index = var_index[var_order]
      p = substr(p, 2, nchar(p))
      first_char = substr(p, 1, 1)
    }

    is_index = first_char == "#"
    if(is_index){
      p = substr(p, 2, nchar(p))
      first_char = substr(p, 1, 1)
    }

    # now the QS element
    is_QS = first_char == "("
    if(is_QS){
      # the QS is the pattern in paren
      pattern = gsub("\\).*", "", substr(p, 2, nchar(p)))
    } else if(!first_char %in% c(" ",",")){
      # the QS is the first item
      is_QS = TRUE
      pattern = gsub("[ ,].*", "", substr(p, 2, nchar(p)))
    }

    if(is_QS){
      vars_selected = try(selvalues(all_vars, .pattern = pattern))
      if("try-error" %in% class(vars_selected)){
        warning("When summoning help, the following pattern {bq?pattern} led to an error. All variables are displayed.")
        vars_selected = all_vars
      }
      i_keep = which(all_vars %in% vars_selected)
      all_vars = all_vars[i_keep]
      var_index = var_index[i_keep]
    }

    if(is_index){
      if(is.unsorted(var_index)){
        all_vars = cub("{all_vars}_=-=_({n.0 ? var_index})")
      } else {
        all_vars = cub("{n.0 ? var_index}:_=-=_{all_vars}")
      }
    }

    msg = str_op(all_vars, "', 'c, 100 swidth.#>, '\n'app, '_=-=_ =>  'r.fixed")
    extra = cub("\nNOTA: after `?`, you can add:", 
              " a) `+` (resp `-`) to reorder the vars alphabetically (`-`: decreasing),", 
              " b) `#` to add their index,",
              " c) a quickselect pattern to pick subsets of variables (use paren. to use svl patterns).",
              "\nEx: ?-@\\d or ?#(^pe & h$). Use `??` to trigger generic quickselect help.")
    extra = fit_screen(extra, width = 0.8)

    on.exit(message(msg, "\n", extra))

    stop("Help from quickselect summoned. Here is the list of variables:\n")

  }

  ####
  #### main loop ####
  ####
  
  pat_parsed = cpp_parse_charselect(pattern)

  # parsed names: we use the list names as the default
  pnames = pat_parsed$names
  pnames[nchar(pnames) == 0] = dot_name

  new_names = pnames
  all_patterns = pat_parsed$patterns
  order_pat = pat_parsed$order
  is_cond = pat_parsed$is_cond
  
  final_names = character(0)
  final_vars = character(0)
  for(i in seq_along(all_patterns)){
    main_pat = all_patterns[i]
    
    if(nchar(main_pat) == 0){
      # this is an ordering command that should reorder all the previous variables
      # it is always attached to the empty pattern
      # (orderings associated to non empty patterns are partial orderings)
      if(i == 1){
        final_vars = all_vars
      }

      final_vars = selvars_internal(final_vars, data, order_pat[i], TRUE, .ignore.case)
    } else {

      # preparation for handling conditions
      if(is_cond[i]){
        info = cpp_parse_conditions_in_pattern(main_pat)
        current_patterns = info$patterns
        current_operations = info$operations
      } else {
        current_patterns = main_pat
        current_operations = ""
      }

      new_vars = character(0)
      for(j in seq_along(current_patterns)){
        p = current_patterns[j]
        selection = selvars_internal(all_vars, data, p, FALSE, .ignore.case)
        if(j == 1){
          new_vars = selection
        } else {
          log_op = current_operations[j]
          if(log_op == "or"){
            new_vars = union(new_vars, selection)
          } else {
            new_vars = intersect(new_vars, selection)
          }
        }        
      }

      # adding the variables to the final stack + ordering if requested
      new_vars = setdiff(new_vars, final_vars)

      if(nchar(order_pat[i]) > 0){
        new_vars = selvars_internal(new_vars, data, order_pat[i], TRUE, .ignore.case)
      }

      final_names[new_vars] = new_names[i]
      final_vars = c(final_vars, new_vars)
    }
  }

  # taking care of the names
  final_names = final_names[final_vars]
  is_empty = nchar(final_names) == 0
  final_names[is_empty] = final_vars[is_empty]
  
  # taking care of placeholders
  if(nchar(dot_name) > 0){
    # fixed name provided by the user. Very likely contains a plaeholder. 
    if(grepl("*", dot_name, fixed = TRUE)){
        final_names = fill_the_placeholders(dot_name, final_vars)
    }
  } else {
    i_star = which(grepl("*", final_names, fixed = TRUE))
    for(i in i_star){
      final_names[i] = fill_the_placeholders(final_names[i], final_vars[i])
    }
  }  

  names(final_vars) = final_names
  return(final_vars)
}

selvars_internal = function(x, data, pattern, is_order, .ignore.case = TRUE){
  # Returns the variables to keep
  # x: vector of variable names
  # data: the data (optional)
  # pattern: the pattern, must be of length 1
  # is_order: whether it is about sorting or selecting
  #
  # returns a simple character vector

  if(!is_order && nchar(pattern) == 0){
    return(character(0))
  }

  if(is_order){
    pattern_origin = pattern
    pattern = str_op(pattern, "'[, \t\n]+'S")
  }

  data_list = NULL

  # having the special_data_types in the options instead of hard coded adds only 2us per call 
  # => worth it I think
  special_data_types = getSmagick_QS_data_types()

  n_pat = length(pattern)
  n_x = length(x)
  all_vars = setNames(x, 1:n_x)

  for(i in n_pat:1){
    # we take the reverse order for orderings to work properly
    # no consequence for selection since it is of length 1

    p = p_raw = pattern[i]
    p_clean = NULL
    first_char = substr(p, 1, 1)

    negate = first_char == "!"
    if(negate){
      p = substr(p, 2, nchar(p))
    }

    is_range = grepl(":", p, fixed = TRUE)
    my_range = integer(2)
    if(is_range){
      #  we need to better check => we allow escaping
      p = gsub("(?<!\\\\):", "___RANGE___", p, perl = TRUE)
      is_range = grepl("___RANGE___", p, fixed = TRUE)
      p = gsub("\\:", ":", p, fixed = TRUE)
    }

    if(is_range){
      current_patterns = trimws(strsplit(p, "___RANGE___", fixed = TRUE)[[1]])
    } else {
      current_patterns = p
    }

    for(j in seq_along(current_patterns)){
      p = current_patterns[j]
      first_char = substr(p, 1, 1)

      if(p == "."){
        # => all the remaining variables
        if(negate){
          stop_hook("You cannot use the pattern {bq?p_raw}: it is equivalent to removing all variables!")
        } else if(is_range){
          stop_hook("You cannot use the pattern {bq?p_raw}. The special value `.` cannot be used in ranges.",
                    "\nPossible fixes: - to escape the meaning of `:`, prepend it with a backslash",
                    "\n                - use direct variable names or number indexes",
                    "\n                - use patterns (`^value` for starts with 'value'; `$var` for ends with 'value'; `@regex`, etc)")
        }

        is_selected = rep(TRUE, length(all_vars))
      } else if(p %in% c(".prev", ".iprev")){
        stop_hook("The special variable {bq?p} is not used in the right context.",
                  "\nIt should only be used in an *expression* that follows a previous expansion. It cannot be used in a character vector.",
                  "\nExample: - good: selvars(head(iris), petal.., \"*_mean\" = mean({val}))", 
                  "\n         -  bad: selvars(head(iris), \"^petal, {val}\")")
      } else if(p %in% names(special_data_types)){

        if(is_range){
          stop_hook("You cannot use the pattern {bq?p_raw}. It is a range (bc of the `:`) and the special value",
                    " {bq?p} cannot be used in ranges.",
                    "\nPossible fixes: - to escape the meaning of `:`, prepend it with a backslash",
                    "\n                - use direct variable names or number indexes",
                    "\n                - use patterns (`^value` for starts with 'value'; `$var` for ends with 'value'; `@regex`, etc)")
        }

        if(is.null(data)){
          stop_hook("The special value {bq?p} can only be used when the argument `x` is a data set. ",
                    "It is meaningless when `x` is a character vector.")
        }

        my_type = special_data_types[[p]]

        if(is.null(data_list)){
          data_list = convert_to_list(data)
          data_list = data_list[all_vars]
        }

        is_selected = sapply(data_list, my_type$fun)

        if(!any(is_selected)){
          info = my_type$info

          stop_hook("The special value {bq?p} did not find any variable. ",
                    "Maybe you could check that there are {info} variables in the data set?")
        }

      } else if(cpp_is_int_in_char(p)){
        # this is an index
        # indexes ALWAYS refer to the original positionning

        if(is_order){
          stop_hook("In ordering queries (here in {bq?pattern_origin}), numeric indexes canno be used.",
                    "\nPROBLEM: the value {bq?p} is not valid. Please use othe means to select variables (",
                    "e.g. '^var' for start with, '$var' for end with, '@regex' for reg. expressions or ",
                    "the variable name directly).")
        }

        p_num = as.numeric(p)
        
        if(abs(p_num) > length(all_vars) || p_num == 0){
          extra = cub("{= p_num < 0 ; (in absolute value) }")
          pblm = cub("{= p_num == 0 ; The value of the index cannot be equal to 0!", 
                    "; The value {bq?p} is larger {extra}than the total number of variables: {len?all_vars}.}")
          stop_hook("Numeric indexes must be {extra}lower than, or equal to, the number of variables in the data set.",
                    "\nPROBLEM: {pblm}")
        }

        if(p_num < 0){
          p_num = length(all_vars) + p_num + 1
        }

        is_selected = logical(length(all_vars))
        is_selected[p_num] = TRUE

      } else if(first_char %in% c("#", "@", "^", "$") || cpp_is_trailing_dots(p)){
        # fixed char search
        fixed = first_char == "#"
        
        if(!fixed && cpp_is_trailing_dots(p)){
          first_char = "^"
          p = substr(p, 1, nchar(p) - 2)
        } else {
          p = substr(p, 2, nchar(p))
        }

        if(first_char == "^"){
          p_clean = p
          if(.ignore.case){
            p = paste0("(?i)^\\Q", p, "\\E")
          } else {
            p = paste0("^\\Q", p, "\\E")
          }
          
        } else if(first_char == "$"){
          p_clean = p
          if(.ignore.case){
            p = paste0("(?i)\\Q", p, "\\E$")
          } else {
            p = paste0("\\Q", p, "\\E$")
          }
          
        } else if(fixed){
          if(.ignore.case){
            fixed = FALSE
            perl = TRUE
            p = paste0("(?i)\\Q", p, "\\E")
          }
        } else if(.ignore.case){
          # => we're in the @ pattern
          p = paste0("(?i)", p)
        }        

        is_selected = grepl(p, all_vars, perl = !fixed, fixed = fixed)

      } else {

        if(is_range && first_char == "!"){
          stop("In ranges, you cannot use negations to select variables on the right side.",
               "\nPROBLEM: In the range {bq?p_raw} there is a negation in {bq?p}.",
               "\nFIX: remove the negation (i.e. drop the `!`) and select variables in a regular way (",
               "with variable names, indexes, or patterns).")
        }

        if(.ignore.case){
          is_selected = cpp_equal_ignore_case(p, all_vars)
        } else {
          is_selected = all_vars == p
        }        
      }

      if(!any(is_selected) || (negate && all(is_selected))){
        info = switch(first_char,
                      "#" = "fixed pattern",
                      "@" = "regular expression",
                      "^" = "starting pattern",
                      "$" = "ending pattern",
                      "value")

        if(!is.null(p_clean)) p = p_clean

        p_info = if(p != p_raw) paste0("(raw is `", p_raw, "`) ") else ""
        
        sugg = suggest_item(p, all_vars)

        consequence = "led to no variable being selected."
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

      } else if(is_range){
        n_matches = sum(is_selected)

        if(n_matches > 1){
          stop_hook("In ranges, the patterns from each side of the range must match only a single variable. ", 
                    "\nPROBLEM: in the range {bq?p_raw}, the pattern {bq?p} matched {n?n_matches} variables.",
                    "\n         These are: {enum ? all_vars[is_selected]}.")
        }

        my_range[j] = which(is_selected)

      }
    }

    if(is_range){
      is_selected = logical(length(all_vars))
      is_selected[my_range[1]:my_range[2]] = TRUE
    }    

    if(is_order){
      if(negate){
        all_vars = c(all_vars[!is_selected], all_vars[is_selected])
      } else {
        all_vars = c(all_vars[is_selected], all_vars[!is_selected])
      }
    } else {
 
      if(negate){
        vars_select = all_vars[!is_selected]
      } else {
        vars_select = all_vars[is_selected]
        if(is_range && my_range[1] > my_range[2]){
          vars_select = rev(vars_select)
        }
      }      

      res = vars_select

      return(res)
    }    
  }

  res = all_vars

  return(res)
}

####
#### Aliases ####
####

selnames = function(.x, ..., .order = NULL, .in = NULL, .pattern = NULL, .frame = parent.frame(),
                   .ignore.case = TRUE){

  mc = match.call(expand.dots = FALSE)
  selvars(.x = .x, .order = .order, .in = .in, .pattern = .pattern, .frame = .frame, 
          .ignore.case = .ignore.case, 
          .is_internal = TRUE, .is_selnames = TRUE, .mc = mc)

}

selvalues = function(.x, ..., .order = NULL, .in = NULL, .pattern = NULL, .frame = parent.frame(),
                   .ignore.case = TRUE){
  
  mc = match.call(expand.dots = FALSE)
  selvars(.x = .x, ..., .order = .order, .in = .in, .pattern = .pattern, .frame = .frame, 
          .ignore.case = .ignore.case, 
          .is_internal = TRUE, .is_selvalues = TRUE, .mc = mc)
}

####
#### data.table utilities ####
####

data_table_quickselect = function(enable = TRUE){
  if(isTRUE(enable)){
    assign("[.data.table", data_table_QS_internal, parent.frame())
  } else {
    rm(list = "[.data.table", envir = parent.env())
  }  
}

data_table_QS_internal = function(x, i, j, ...){
  
  mc = match.call()
  if("j" %in% names(mc)){
    
    if(is.character(mc$j) || (is_operator(mc$j, "c") && is.character(j))){
      vars = selnames(x, .pattern = j)

      if(all(names(vars) == vars)){
        mc$j = parse(text = capture.output(dput(j)))
      } else {
        nm_vars = names(vars)
        new_mc_j = str2lang("list()")
        for(i in seq_along(vars)){
          if(nm_vars[i] != vars[i]){
            new_mc_j[[nm_vars[i]]] = str2lang(vars[i])
          } else {
            new_mc_j[[length(new_mc_j) + 1]] = str2lang(vars[i])
          }
        }

        mc$j = new_mc_j
      }
      

    } else {
      is_valid = FALSE

      if(is_operator(mc$j, c(".", "list"))){
        is_valid = TRUE
        mc$j[[1]] = as.name("list")

      } else if(is_operator(mc$j, ":=")){

        nm = names(mc$j)
        if(all(nchar(nm) == 0)){
          # case left := right

          # we only take care of the case "varname" := .(stuff)
          # we convert it into ":="(new = fun(old)) format
          if(length(mc$j[[2]] == 1) && length(mc$j[[3]]) == 2 && is.character(mc$j[[2]])){
            is_valid = TRUE
            new_mc_j = mc$j[c(1, 3)]
            names(new_mc_j) = c("", mc$j[[2]])
            if(is_operator(new_mc_j[[2]], c(".", "list"))){
              new_mc_j[[2]] = mc$j[[3]][[1]]
            }             
            mc$j = new_mc_j
          }
        } else {
          # case ":="(new = fun(old))
          # => this is fine
          is_valid = TRUE
        }

      }

      if(is_valid){
        selnames_call = str2lang("selnames(.x = x)")

        mc_j = mc$j
        nm_mc_j = names_xpd(mc_j)
        is_nm = nchar(nm_mc_j) > 0
        for(i in 2:length(mc_j)){
          if(is_nm[i]){
            selnames_call[[nm_mc_j[i]]] = mc_j[[i]]
          } else {
            selnames_call[[length(selnames_call) + 1]] = mc_j[[i]]
          }
        }

        vars = eval(selnames_call)
        nm_vars = names(vars)

        # we reconstruct the call
        new_mc_j = str2lang("list()")
        new_mc_j[[1]] = mc$j[[1]]
        for(i in seq_along(vars)){
          if(nm_vars[i] != vars[i]){
            new_mc_j[[nm_vars[i]]] = str2lang(vars[i])
          } else {
            new_mc_j[[length(new_mc_j) + 1]] = str2lang(vars[i])
          }
        }

        mc$j = new_mc_j

      }

    }
    
  }
  
  mc[[1]] = as.name("sub_dt")
  my_list = list(sub_dt = asNamespace("data.table")[["[.data.table"]])
  
  eval(mc, my_list, enclos = parent.frame())
}



####
#### Utilities ####
####


setup_QS_data_types = function(){
  # Sets up the special data types in quickselect
  # Contains:
  # - the function to apply to identify the variable
  # - the information to pop in the error if such vars were not found

  res = list()

  res[[".num"]] = list(
    fun = is.numeric,
    info = "numeric"
  )

  res[[".log"]] = list(
    fun = is.logical,
    info = "logical"
  )

  res[[".lnum"]] = list(
    fun = function(x) is.numeric(x) || is.logical(x),
    info = "numeric or logical"
  )

  res[[".fact"]] = list(
    fun = is.factor,
    info = "factor"
  )

  res[[".char"]] = list(
    fun = is.character,
    info = "character"
  )

  res[[".fchar"]] = list(
    fun = function(x) is.factor(x) || is.character(x),
    info = "factor or character"
  )

  res[[".date"]] = list(
    fun = function(x) any(grepl("date", class(x), ignore.case = TRUE)),
    info = "date"
  )

  res[[".list"]] = list(
    fun = is.list,
    info = "list"
  )

  options(stringmagick_QS_data_types = res)

}

getSmagick_QS_data_types = function(){
  getOption("stringmagick_QS_data_types")
}



flag_expansion_patterns = function(call){
  # goes through a call and flags the variables to be expanded
  # they are flagged by adding "_P_A_T_" on both ends (for pattern)
  # example:
  #  in: substr(.char, 1, 1) %in% c('a', 'b')
  # out: substr("_P_A_T_.char_P_A_T_", 1, 1) %in% c('a', 'b')
  #
  #  in: `^petal`
  # out: "_P_A_T_^petal_P_A_T_"

  if(length(call) == 0) return(call)

  if(length(call) == 1){
    if(is.name(call)){
      special_data_types = names(getSmagick_QS_data_types())
      var = as.character(call)
      if(var %in% c(".", ".prev", ".iprev", special_data_types) || 
         !cpp_is_variable_name(var) ||
         cpp_is_trailing_dots(var)){
        call = paste0("_P_A_T_", var, "_P_A_T_")
      }
    }
    return(call)
  }

  for(i in 2:length(call)){
    call[[i]] = flag_expansion_patterns(call[[i]])
  }

  call
}





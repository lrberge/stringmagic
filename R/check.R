#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Jul 27 10:06:18 2022
# ~: checking functions
#----------------------------------------------#


# NOTE: it's unusual for me but there will be A LOT of code duplication
#       the objective is to limit the most possible the overheads, hence this decision
#       + the functions are simple and to the point (much less complex than what is possible with dreamerr), so that's manageable

####
#### checking ####
####


check_logical = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, no_na = FALSE, mbt = FALSE, up = 0){
  set_up(up + 1)

  if(missing(x)){
    if(mbt){
      x_dp = deparse_short(substitute(x))
      stop_up("The argument `", x_dp, "` must be provided. PROBLEM: it is missing.")
    }

    return()
  }

  if(null && is.null(x)){
    return()
  }

  if(!is.logical(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a logical ", type, ".",
            " PROBLEM: it is not logical, it is of class {enum?class(x)}.")
  }

  len_x = length(x)
  if(len_x == 0 &&  l0) return()

  if(scalar){
    if(missing(no_na)) no_na = TRUE

    if(len_x != 1){
      x_dp = deparse_short(substitute(x))
      nullable = if(null) "(nullable) " else ""
      stop_up("The ", nullable, "argument `", x_dp, "` must be a logical scalar.",
              " PROBLEM: it is not of length 1, it is of length ", length(x), ".")
    }
  }

  if(no_na && anyNA(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector without NA"
    problem = if(scalar) "it is equal to NA" else "it contains NA values"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a logical ", type, ". ",
            "PROBLEM: ", problem, ".")
  }

}

check_numeric = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, no_na = FALSE, 
                         mbt = FALSE, integer = FALSE, complex = FALSE, up = 0){
  set_up(up + 1)
  
  if(missing(x)){
    if(mbt){
      x_dp = deparse_short(substitute(x))
      stop_up("The argument `", x_dp, "` must be provided. PROBLEM: it is missing.")
    }

    return()
  }

  if(null && is.null(x)){
    return()
  }

  if((!complex && !is.numeric(x)) || (complex && !is_num_complex(x))){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    vector_type = if(scalar) "scalar" else "vector"
    type = if(integer) "an integer" else "a numeric"
    if(complex) paste0(type, ", or complex,")
    
    stop_up("The ", nullable, "argument `", x_dp, "` must be ", type, " ", vector_type, ".",
            " PROBLEM: it is not {'^.+ 'r ? type} it is of class {enum?class(x)}.")
  }

  len_x = length(x)
  if(len_x == 0 &&  l0) return()

  if(scalar){
    if(missing(no_na)) no_na = TRUE

    if(len_x != 1){
      x_dp = deparse_short(substitute(x))
      nullable = if(null) "(nullable) " else ""
      type = if(integer) "an integer" else "a numeric"
      
      stop_up("The ", nullable, "argument `", x_dp, "` must be ", type, " scalar.",
              " PROBLEM: it is not of length 1, it is of length ", length(x), ".")
    }
  }

  if(no_na && anyNA(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    vector_type = if(scalar) "scalar" else "vector without NA"
    type = if(integer) "an integer" else "a numeric"
    problem = if(scalar) "it is equal to NA" else "it contains NA values"
    
    stop_up("The ", nullable, "argument `", x_dp, "` must be ", type, " ", vector_type, ". ",
            "PROBLEM: ", problem, ".")
  }
  
  if(integer){
    if(complex){
      for(part in 1:2){
        z = if(part == 1) Re(x) else Im(x)
        if(any(z != round(z))){
          x_dp = deparse_short(substitute(x))
          i = which(z != round(z))[1]
          intro = ""
          if(part == 1 && all(Im(x) == 0)){
            msg = .sma("is equal to {z}")
          } else {
            msg = .sma("has {&i==1;a real;an imaginary} part equal to {z}")
          }
          stop_up("The argument {bq?x_dp} must be an integer {&scalar ; scalar ; vector}.",
                  "\nPROBLEM: {&len(x)==1 ; it ; the {nth ? i} value} ",
                  "{msg}, not an integer.")
        }
      }
    } else {
      if(any(x != round(x))){
        x_dp = deparse_short(substitute(x))
        i = which(x != round(x))[1]
        stop_up("The argument {bq?x_dp} must be an integer {&scalar ; scalar ; vector}.",
                "\nPROBLEM: {&length(x)==1 ; it is equal to {x} ; the {nth ? i} ",
                "value is equal to {x[i]}}, not an integer.")
      }
    }    
  }

}

check_character = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, 
                          no_na = FALSE, mbt = TRUE, up = 0){
                            
  set_up(up + 1)

  if(missing(x)){
    if(mbt){
      x_dp = deparse_short(substitute(x))
      stop_up("The argument `", x_dp, "` must be provided. PROBLEM: it is missing.")
    }

    return()
  }

  if(null && is.null(x)){
    return()
  }

  if(!is.character(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a character ", type, ".",
            " PROBLEM: it is not character, it is of class {enum?class(x)}.")
  }

  len_x = length(x)
  if(len_x == 0 && l0 && !scalar) return()

  if(scalar){
    if(missing(no_na)) no_na = TRUE

    if(len_x != 1){
      x_dp = deparse_short(substitute(x))
      nullable = if(null) "(nullable) " else ""
      stop_up("The ", nullable, "argument `", x_dp, "` must be a character scalar.",
              " PROBLEM: it is not of length 1, it is of length ", length(x), ".")
    }
  }

  if(no_na && anyNA(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector without NA"
    problem = if(scalar) "it is equal to NA" else "it contains NA values"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a character ", type, ". ",
            "PROBLEM: ", problem, ".")
  }

}

check_set_character = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, 
                               no_na = FALSE, mbt = TRUE){

  if(missing(x)){
    if(mbt){
      x_dp = deparse_short(substitute(x))
      stop_up("The argument `", x_dp, "` must be provided. PROBLEM: it is missing.")
    }

    return(NULL)
  }

  if(null && is.null(x)){
    return(NULL)
  }

  if(!is.atomic(x)){
    x_dp = deparse_short(substitute(x))
    stop_up("Argument {bq?x_dp} must be atomic. ",
            "\nPROBLEM: Currently it is of the non-atomic class {bq?class(x)[1]}.")
  }

  if(!is.character(x) || !identical(class(x), "character")){
    x = as.character(x)
  }

  len_x = length(x)
  if(len_x == 0 && l0 && !scalar){
    return(character(0))
  }

  if(scalar){
    if(missing(no_na)) no_na = TRUE

    if(len_x != 1){
      x_dp = deparse_short(substitute(x))
      nullable = if(null) "(nullable) " else ""
      stop_up("The ", nullable, "argument `", x_dp, "` must be a character scalar.\n",
              " PROBLEM: it is not of length 1, it is of length ", length(x), ".")
    }
  }

  if(no_na && anyNA(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector without NA"
    problem = if(scalar) "it is equal to NA" else "it contains NA values"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a character ", type, ". \n",
            " PROBLEM: ", problem, ".")
  }
  
  return(x)
}


check_envir = function(x){

  if(!inherits(x, "environment")){
    x_dp = deparse_short(substitute(x))
    if(is.list(x)){
      stop_up("The argument `", x_dp, "` must be an environment (ex: parent.frame()). ",
              "\nPROBLEM: it is not an environment, ",
              "it is a {&is.data.frame(x) ; data.frame ; list}.",
              "\nUse the argument `.data` instead.")
    } else {
      stop_up("The argument `", x_dp, "` must be an environment (ex: parent.frame()). ",
              "PROBLEM: it is not an environment, it is of class {enum?class(x)}.")
    }
  }

}

check_function = function(x, null = FALSE, up = 0, argname = NULL){
  set_up(up + 1)
  
  if(null && is.null(x)){
    return(NULL)
  }
  
  if(!is.function(x)){
    if(is.null(argname)){
      x_dp = deparse_short(substitute(x))
    }
    
    stop_up("The argument `", x_dp, "` must be a function. ",
            "PROBLEM: it is not a function, it is of class {enum?class(x)}.")
  }
}


check_set_dots = function(..., mc = NULL, mbt = FALSE, character = FALSE,
                          no_na = FALSE, scalar = FALSE, nofun = FALSE){
  # check the dots arguments

  n = ...length()
  if(n == 0){

    if(mbt){
      stop_up("At least one element in `...` must be provided. PROBLEM: `...` is empty.")
    } else {
      return(list())
    }

  }

  if(is.null(mc)){
    sysOrigin = sys.parent()
    mc = match.call(definition = sys.function(sysOrigin),
                    call = sys.call(sysOrigin), expand.dots = FALSE)
  }

  dots = vector("list", n)
  
  dots_nm = names(mc[["..."]]) 

  # We first catch evaluation problems
  for(i in 1:n){
    elem = try(...elt(i), silent = TRUE)

    if(isError(elem) || (nofun && is.function(elem))){
      nm = if(is.null(dots_nm)) "" else dots_nm[i]
      if(is.na(nm)) nm = ""

      mc_dots = mc[["..."]]
      value = deparse_short(mc_dots[[i]])

      nm = string_magic(" ({if(.C<4 ; erase) ! {nm} = }{value})")
      
      if(isError(elem)){
        if(grepl("try(...", elem, fixed = TRUE)){
          elem = gsub("^[^:]+:", "", elem)
        }

        stop_up("In the argument `...`, the {#nth ? i} element{nm} raises an error:\n",
                elem)
      } else {
        stop_up("In the argument `...`, the elements must not be functions.",
                "\nPROBLEM: the {#nth ? i} element{nm} is a function.")
      }
    }

    dots[[i]] = elem
  }
  
  # now we catch type problems
  if(scalar){
    if(any(lengths(dots) != 1)){
      len_dots = lengths(dots)
      i_pblm = which(len_dots != 1)
      len_pblm = len_dots[i_pblm]

      # I purposefully copy-paste in each block
      # otherwise the code becomes too complicated and especially more difficult to debug

      # We try to give as much info as possible
      n_pblm = length(i_pblm)
      nm_pblm = character(n_pblm)

      if(!is.null(dots_nm)){
        rep = dots_nm[i_pblm]
        rep[is.na(rep)] = ""
        nm_pblm = rep
      }

      # the value of the variables
      mc_dots = mc[["..."]]
      value_all = sapply(i_pblm, function(i) deparse_short(mc_dots[[i]]))

      info_call = .string_magic("`{if(.C<4 ; erase) ! {nm_pblm} = }{value_all}`")

      stop_up("In the argument `...`, all elements must be scalars (i.e. of length 1).\nPROBLEM: ",
              "{'\n'c ! The {nth ? i_pblm} element ({info_call}) is ",
              "of length {format, ws ? len_pblm}.}")
    }
  }

  if(character){
    # we convert to character => requirement is atomicity
    if(!all(sapply(dots, is.atomic))){      
      i_pblm = which(!sapply(dots, is.atomic))

      # We try to give as much info as possible
      n_pblm = length(i_pblm)
      nm_pblm = character(n_pblm)

      if(!is.null(dots_nm)){
        rep = dots_nm[i_pblm]
        rep[is.na(rep)] = ""
        nm_pblm = rep
      }

      # the value of the variables
      mc_dots = mc[["..."]]
      value_all = sapply(i_pblm, function(i) deparse_short(mc_dots[[i]]))

      info_call = string_magic("`{if(.C<4 ; erase) ! {nm_pblm} = }{value_all}`")

      cls_pblm = sapply(dots[i_pblm], function(x) string_magic("{bq ! {enum ? class(x)}}"))
      stop_up("In the argument `...`, all elements must be atomic (i.e. convertible to a character string).",
              "\nPROBLEM: {'\n         'c ! The {Nth ? i_pblm} element ({info_call}) is not character",
              " (instead it is of class: {cls_pblm}).}")
    }

    true_character = function(x) is.character(x) && identical(class(x), "character")
    i_no_char = which(!sapply(dots, true_character))
    for(i in i_no_char){
      dots[[i]] = as.character(dots[[i]])
    }
  }

  if(no_na){
    if(any(sapply(dots, anyNA))){
      i_pblm = which(sapply(dots, anyNA))

      # We try to give as much info as possible
      n_pblm = length(i_pblm)
      nm_pblm = character(n_pblm)

      if(!is.null(dots_nm)){
        rep = dots_nm[i_pblm]
        rep[is.na(rep)] = ""
        nm_pblm = rep
      }

      # the value of the variables
      mc_dots = mc[["..."]]
      value_all = sapply(i_pblm, function(i) deparse_short(mc_dots[[i]]))

      info_call = string_magic("`{if(.C<4 ; erase) ! {nm_pblm} = }{value_all}`")

      stop_up("In the argument `...`, all elements must be without NA.\nPROBLEM: ",
              "The {nth, enum ? i_pblm} element{$s} ({C ? info_call})",
              " {$contain} NA values.")
    }
  }

  names(dots) = dots_nm

  return(dots)
}


check_set_options = function(x, options, op = NULL, free = FALSE, case = FALSE){
  # x: always a character vector
  # options: the options to match
  if(length(x) == 0) return(x)

  n = length(x)
  res = x
  for(i in 1:n){
    v = x[i]

    pm = pmatch(v, options)
    if(is.na(pm) && !case){
      pm = pmatch(tolower(v), tolower(options))
    }
    
    if(is.na(pm) && !free){
      # absence of a match
      stop_hook("The option {bq?v} is not valid {&is.null(op) ; for the current operation ; for the operator {bq?op}}.\n",
                "FYI the option{$s ? options} available {$are, enum.bq}.")
    }

    if(!is.na(pm)){
      res[i] = options[pm]
    }
  }

  res
}

check_last = function(.last){
  if(is.null(.last) || is.function(.last)){
    # ok, no check
  } else if(!is.character(.last)){
    stop_hook("Argument `.last` mut be equal to an `string_magic` operations chain", 
              " or a function.",
              "\nPROBLEM: instead it is of class {enum.bq?class(.last)}.")
  } else {
    check_character(.last, null = TRUE, scalar = TRUE)
  }
}

check_set_delimiters = function(.delim){
  check_character(.delim, no_na = TRUE, up = 1)
  
  if(length(.delim) == 1){
    .delim = strsplit(.delim, " ", fixed = TRUE)[[1]]
  }
  
  check_delimiters(.delim)
  
  .delim
}

check_set_width = function(width_expr){
  sw = getOption("width") 
  data = list(.sw = sw)
  width = eval(width_expr, data, parent.frame(2))
  
  if(isFALSE(width)){
    width = Inf
  }
  
  if(is.null(width)){
    width = min(100, 0.9 * sw)
  }
  
  width
}

check_set_split = function(split){
  if(missing(split)){
    if(isTRUE(split)){
      split = ","
    }
    return(split)
  }
  
  if(is.logical(split)){
    check_logical(split, scalar = TRUE, up = 1)
    if(split){
      split = ","
    }
  } else if(is.character(split)){
    check_character(split, scalar = TRUE, up = 1)
    if(nchar(split) != 1){
      stop_hook("The argument `.split` must be a logical scalar or a single character symbol.",
              "\nPROBLEM: it is currently a character scalar ({10 Short, Q ? split}) but ",
              "contains {nchar(split)} characters.")
    }
  } else {
    stop_hook("The argument `.split` must be a logical scalar or a single character symbol.",
              "\nPROBLEM: instead it is of class {enum.bq ? class(split)}.")
  }
  
  split
}

is_num_complex = function(x){
  is.numeric(x) || is.complex(x)
}

check_set_mat = function(cmat, nmat, df){
  res = TRUE
  
  if(is.logical(cmat)){
    check_logical(cmat, scalar = TRUE)
    if(cmat){
      attr(res, "auto") = TRUE
      return(res)
    }    
  } else if(is_num_complex(cmat)){
    check_numeric(cmat, scalar = TRUE, complex = TRUE)
    attr(res, "row_col") = c(Re(cmat), Im(cmat))
    return(res)
  } else {
    stop_hook("The argument `.cmat` must be a logical scalar, an integer (giving the ",
              "nber of rows) or an imaginary (ex: `3i`, giving the number of columns).",
              "\nPROBLEM: instead it is of class {enum.bq ? class(split)}.")
  }
  
  if(is.logical(nmat)){
    check_logical(nmat, scalar = TRUE)
    if(nmat){
      attr(res, "auto") = TRUE
      attr(res, "num") = TRUE
      return(res)
    }    
  } else if(is_num_complex(nmat)){
    check_numeric(nmat, scalar = TRUE, complex = TRUE)
    attr(res, "num") = TRUE
    attr(res, "row_col") = c(Re(nmat), Im(nmat))
    return(res)
  } else {
    stop_hook("The argument `.cmat` must be a logical scalar, an integer (giving the ",
              "nber of rows) or an imaginary (ex: `3i`, giving the number of columns).",
              "\nPROBLEM: instead it is of class {enum.bq ? class(split)}.")
  }
  
  if(is.logical(df)){
    check_logical(df, scalar = TRUE)
    if(df){
      attr(res, "auto") = TRUE
      attr(res, "df") = TRUE
      return(res)
    }    
  } else if(is_num_complex(df)){
    check_numeric(df, scalar = TRUE, complex = TRUE)
    attr(res, "df") = TRUE
    attr(res, "row_col") = c(Re(df), Im(df))
    return(res)
  } else if(is.character(df)){
    if(length(df) == 1){
      colnames = strsplit(df, ",[ \t\n]*")[[1]]
    } else {
      colnames = df
    }
    
    attr(res, "df") = TRUE
    attr(res, "colnames") = colnames
    attr(res, "row_col") = c(0, length(colnames))
    return(res)
  } else {
    stop_hook("The argument `.df` must be a logical scalar, an integer (giving the ",
              "nber of rows), an imaginary (ex: `3i`, giving the number of columns), ",
              "or a character scalar giving the (comma separated) column names.",
              "\nPROBLEM: instead it is of class {enum.bq ? class(split)}.")
  }
  
  res = FALSE
  res
}

check_delimiters = function(.delim){
  
  if(!length(.delim) == 2){
    stop_hook("The argument .`delim` must lead to a character vector of length 2",
              " (after splitting the space if relevant).",
              "\nPROBLEM: it is of length {len?.delim}.")
  }
  
  if(any(nchar(.delim) == 0)){
    info = c("opening", "closing")[nchar(.delim) == 0]
    stop_hook("Argument `.delim` must be composed of non-empty delimiters.",
          "\nPROBLEM: the {C?info} delimiter{$s, are} equal to the empty string.")
  }
  
  forbid = "();?! "
  forbid_regex = paste0("[", forbid, "]")
  if(any(grepl(forbid_regex, .delim))){
    i = which(grepl(forbid_regex, .delim))[1]
    pblm = string_ops(.delim[i], paste0("'", forbid_regex, "'X"))
    info = string_magic("{&i == 1;opening;closing} delimiter (equal to {bq?.delim[i]})")
    stop_hook("Argument `.delim` cannot contain the following, reserved, ",
              "characters: {''s, bq, C?forbid}.",
              "\nPROBLEM: the {info} contains the forbidden character{$s ? pblm}: {$enum.bq}.")
  }
}

get_string_magic_context = function(){
  # this is specific to functions running within string_magic_internal
  
  up = 1
  while(!get0("is_string_magic_internal", parent.frame(up), inherits = FALSE, ifnotfound = FALSE)){
    up = up + 1
    if(identical(parent.frame(up), .GlobalEnv)){
      return("")
    }
  }
  
  x = get("x", parent.frame(up))
  .delim = get(".delim", parent.frame(up))
  
  i = get("i", parent.frame(up))
  n = get("n", parent.frame(up))
  
  # we reparse
  x_parsed = cpp_string_magic_parser(x, .delim, TRUE)
  context_specific = x_parsed[[i]]
  
  # normalizing newlines or very confusing
  x = escape_newline(x)
  context_specific = escape_newline(context_specific)
  
  # normalizing the if-else semi-colon
  x = gsub("_;;;_", ";", x)
  context_specific = gsub("_;;;_", ";", context_specific)
  
  same_context = gsub(" ", "", x) == gsub(" ", "", context_specific)
  
  if(same_context){
    res = .sma("CONTEXT: Problem found in {bq?context_specific}.")
  } else {
    res = .sma("CONTEXT: Problem found in {'80|..'k, Q?x},",
               "\n         when dealing with the interpolation {bq?context_specific}.")
  }
  
  res    
}

check_set_string_magic_parsing = function(x, check, .delim){
  # x is a character string to be turned into an R expression
  
  if(check){
    x_call = try(parse(text = x, keep.source = FALSE), silent = TRUE)
  } else {
    x_call = parse(text = x, keep.source = FALSE)
    return(x_call)
  }  
  
  if(inherits(x_call, "try-error")){
    
    open = .delim[1]
    close = .delim[2]
    
    context = get_string_magic_context()
    first_char = substr(x, 1, 1)
    
    if(first_char %in% c("#", "$")){
      msg = string_magic("PROBLEM: there is a syntax error in the pluralization (the ", 
                  "interpolation starting with {bq?first_char}).",
                  "\nFor more information on the syntax, type `string_magic(.help = TRUE)` and go to the section ",
                  "{Q!Interpolation and string operations: Principle}")
      
    } else if(first_char == "&"){
      msg = string_magic("PROBLEM: there is a syntax error in the if-else (the ", 
                  "interpolation starting with {'^&+'x, bq?x}).",
                  "\nFor more information on the syntax, type `string_magic(.help = TRUE)` and go to the section ",
                  "{Q!Special interpolation: if-else}")
    } else {
      if(grepl("[!?]", x)){
        type = paste0("2 (`", open, "ops ? expr", close, "`) or 3 (`", open, "ops ! verbatim", close, "`)")
        if(!grepl("!", x)){
          type = paste0("2 (`", open, "ops ? expr", close, "`)")
        } else if(!grepl("[?]", x)){
          type = paste0("3 (`", open, "ops ! verbatim", close, "`)")
        }
        
        extra = string_magic("\nCurrently the interpolation has been parsed as the first case: ",
                     "`{open}expr{close}`.",
                     "\nMaybe there is a syntax mistake preventing it to be interpreted as case {type}?")
      } else {
        extra = "\nCurrently it seems that the expression to be interpolated is not a valid R expression."
      }
      
      help_suggest = .sma("\nFor more information on the syntax, type `string_magic(.help = TRUE)` and go to the section ",
                          "{!Operations: General syntax}")
      
      x_call_clean = gsub("Error in parse[^:]+: ?", "Error when parsing: ", x_call)
      
      msg = .sma("PROBLEM: The expression {bq?x} could not be parsed, see error below:",
                 "\n{x_call_clean}",
                 "\nINFO: Interpolations can be of the form `{open}expr{close}`, ",
                 "`{open}op1, op2?expr{close}`, or", 
                 " `{open}op1, op2!verbatim{close}`. ",
                 "{extra}", 
                 "{help_suggest}")
    }
    
    .stop_hook(context, "\n", msg)
    
  }
  
  x_call
}

check_set_string_magic_eval = function(call, data, envir, check){
  # we need to eval the call
  
  is_dt = "dt_data" %in% names(attributes(envir))
  
  if(!check){
    if(is_dt){
      x = eval_dt(call, data, envir)
    } else {
      x = eval(call, data, envir)
    }
    
  } else {
    # now with check = TRUE
    
    if(is_dt){
      x = try(eval_dt(call, data, envir), silent = TRUE)
    } else {
      x = try(eval(call, data, envir), silent = TRUE)
    }
    
    if(inherits(x, "try-error")){
      context = get_string_magic_context()
      call_dp = deparse_short(call)
      if(grep("^expression\\(", call_dp)){
        call_dp = gsub("^expression\\(|\\)", "", call_dp)
      }
      x_clean = gsub("^Error in eva[^:]+: ?", "Error: ", x)
      
      msg = .sma("PROBLEM: The expression {bq ? call_dp} could not be evaluated, see error below:",
                "\n{x_clean}")
      
      stop_hook("{context}\n{msg}")
    }
  }  
  
  if(is.function(x)){
    context = get_string_magic_context()
    call_dp = deparse_short(call)
    msg = .sma("EXPECTATION: interpolated variables should be coercible to character strings.",
               "\nPROBLEM: the expression {bq?call_dp} is a **function**. Please provide a character string.")
    stop_hook("{context}\n{msg}")
  }
  
  x
}


# oparg: operator's argument
check_set_oparg_parse = function(argument, operator, check){
  
  if(check){
    call = try(str2lang(argument), silent = TRUE)
  } else {
    call = str2lang(argument)
    return(call)
  }
  
  
  if(inherits(call, "try-error")){
    context = get_string_magic_context()
    msg = .sma("EXPECTATION: In operation \"{bq?argument}{operator}\" the argument ",
               "in backticks is evaluated from the calling environment.",
               "\nPROBLEM: {bq?argument} is not a valid R-expression, see parsing error below:",
               "\n{call}")
    stop_hook("{context}\n{msg}")
  }
  
  call
}

check_set_oparg_eval = function(call, data, envir, operator, check){
  
  if(check){
    x = try(eval(call, data, envir), silent = TRUE)
  } else {
    x = eval(call, data, envir)
    return(x)
  }
  
  if(inherits(x, "try-error")){
    context = get_string_magic_context()
    msg = .sma("EXPECTATION: In operation {bq?operator} the argument ",
               "in backticks is evaluated from the calling environment.",
               "\nPROBLEM: {bq?deparse_short(call)} could not be evaluated, see error below:",
               "\n{x}")
    stop_hook("{context}\n{msg}")
  }
  
  x
}

report_string_magic_parsing_error = function(x, x_parsed, .delim, error = TRUE){
  
  x_parsed = x_parsed[[1]]
  error_msg = x_parsed[1]
  xi = x_parsed[2]
  
  #
  # getting the context
  #
  
  open = .delim[1]
  close = .delim[2]
  
  x = escape_newline(x)
  xi = escape_newline(xi)
  
  same_context = gsub(" ", "", x) == gsub(" ", "", xi)
  if(same_context){
    context = .sma("CONTEXT: Problem found in {bq?xi}.")
  } else {
    context = .sma("CONTEXT: Problem found in {'80|..'k, Q?x},",
                   "\n         when dealing with the interpolation {bq?xi}.")
  }
  
  #
  # the error message
  #
  
  suggest = ""
  if(error_msg == "slash operator not ending correctly"){
    if(is_box_open(xi, .delim) && !is_box_close(xi, .delim)){
      msg = .sma("PROBLEM: the opening delimiter ({bq?.delim[1]}) is not ", 
                 "matched with a closing delimiter ({bq?.delim[2]}).",
                 "\nNOTE: to escape the meaning of the delimiter, use a ",
                 "double backslash: \\\\{.delim[1]}")
      
      suggest= "INFO: see string_magic(.help = TRUE) and go to the section 'Escaping and special cases'"
    } else {
      # we diagnose the substring
      if(is_box_open(xi, .delim)){
        xi_small = string_trim(xi, 1)
      } else {
        xi_small = string_trim(xi, nchar(open), nchar(close))
      }
      
      xi_small_parsed = cpp_string_magic_parser(xi_small, .delim)
      
      if(length(xi_small_parsed) == 1 && isTRUE(attr(xi_small_parsed, "error"))){
        msg = report_string_magic_parsing_error(xi_small_parsed, xi_small, .delim, FALSE)
        msg = gsub("^CONTEXT:", "         ", msg)
      } else {
        # here I don't knwo what the error can be
        # in theory, we should not be here
        stop("Internal error: uncaught parsing error, could you report your string_magic() call?")
      }
      
      # We don't add a suggestion since it should be in the 'msg'
    }
    
  } else if(error_msg == "no closing bracket"){
    
    # {unik, sort ? iris[['Species']}
    # we first try on iris[['Species'] only
    
    done = FALSE
    expr_msg = ""
    if(grepl("?", xi, fixed = TRUE)){
      xi_expr = gsub(".+[?]", "", xi)
      pblm = cpp_find_closing_problem(xi_expr, .delim)
      if(pblm %in% c("", "delim")){
        # no problem found or delim prblm (we check with the full xi)
      } else {
        done = TRUE
        close = .delim[2]
        xi_expr_clean = string_ops(xi_expr, "'mf/{close}'s, -1 last, `close`collapse, tws")
        expr_msg = .sma("in the expression {bq?xi_expr_clean}, ")
      }
    }
    
    if(!done){
      pblm = cpp_find_closing_problem(xi, .delim)
    }    
    
    if(pblm == "delim"){
      suggest = "INFO: see string_magic(.help = TRUE) and go to the section 'Escaping and special cases'"
    }
    
    delim_msg = if(all(.delim == c("{", "}"))) "bracket" else "delimiter"
    
    pblm = switch(pblm, 
                  "delim" = .sma("the opening {delim_msg} ({bq?.delim[1]}) is not ", 
                                  "matched with a closing {delim_msg} ({bq?.delim[2]}).",
                                  "\n\nNOTE: to escape the meaning of the {delim_msg}, use a ",
                                  "double backslash: \\\\{.delim[1]}."),
                  "'" = "a single quote (') open is not closed.",
                  "\"" = "a double quote (\") open is not closed.",
                  "`" = "a backtick (`) open is not closed.",
                  "(" = "a parenthesis (`(`) open is not closed.",
                  "{" = "a bracket (`{`) open is not closed.",
                  "[" = "a bracket (`[`) open is not closed.",
                  "it seems that something open is not closed."
                  )
    msg = .sma("PROBLEM: {expr_msg}{pblm}")  
    
  } else if(string_x(error_msg, 7) == "if-else"){
    type = if(string_x(xi, 2, 1 + nchar(open)) == "&&") "&&" else "&"
    syntax = if(type == "&&") "{&&cond ; verb_true}" else "{&cond ; verb_true ; verb_false}"
    intro = .sma("The syntax of the if-else operation is {syntax}, with ", 
                 "`verb_.` a verbatim expression (possibly containing interpolations).")
    
    if(error_msg == "if-else: error extracting condition"){
      
      msg = .sma("{intro}\nPROBLEM: no semi-colon (used to delimit the condition) ",
                 "was found so the condition could not be parsed.")
                 
    } else if(error_msg == "if-else: error extracting the first part"){
      msg = .sma("{intro}\nPROBLEM: no semi-colon {&type == '&' ; or closing bracket }was ",
                 "found to delimit `verb_true`.")
    } else if(error_msg == "if-else: error extracting the second part"){
      msg = .sma("{intro}\nPROBLEM: no closing bracket was found to delimit `verb_false`.")
    }
    
    suggest = "INFO: see string_magic(.help = TRUE) and go to the section 'Special interpolation: if-else'"
    
  } else {
    # pluralization
    
    suggest = "INFO: see string_magic(.help = TRUE) and go to the section 'Special interpolation: Pluralization'"
    type = string_x(xi, 1, 1 + nchar(open))
    syntax = .sma("{open}{type}op1, op2{close} or {open}{type}op1, op2 ? variable{close}")
    
    if(error_msg == "pluralization: operators cannot contain parentheses"){
      msg = .sma("In pluralization, the parenthesis is a forbidden character *except* ",
                 "for the special operation `(v1;v2)` (without any letter before the paren!).",
                 "\nPROBLEM: a parenthesis was found attached to an operator.")
      
    } else if(error_msg == "pluralization: the interpolation ends incorrectly"){
      msg = .sma("The syntax of the pluralization is {syntax}, with `op` pluralization operations.", 
                 "\nPROBLEM: The pluralization did not end correctly.")
    } else {
      # (v1;v2) case
      type = string_ops(error_msg, "(first|second|third) x")
      pblm = switch(type, 
                    "first" = "The semi-colon to delimit the first part is missing.",
                    "second" = "The semi-colon or closing parenthesis to delimit the second part is missing.",
                    "third" = "The closing parenthesis to delimit the third part is missing.")
      
      msg = .sma("In pluralizations, the special verbatim operation must be of the ",
                 "form (v1;v2) or (v1;v2;v3), with `v.` verbatim expressions ", 
                 "(possibly containing interpolations).",
                 "\nPROBLEM: the {type} verbatim expression could not be extracted. {pblm}")
    }
    
  }
  
  full_msg = .sma("{context}\n{msg}{if(.C>0;'\n'paste) ? suggest}")
  
  if(error){
    .stop_hook(full_msg)
  } else {
    return(full_msg)
  }
  
}

check_set_eval_fun = function(fun__, x__, ...){
  # we postfix weirdly to avoid common argument names
  # the .x is pretty common
  
  check_function(fun__, up = 1, argname = deparse(substitute(fun__)))
  
  # NOTA: formalArgs does not work for primitives, like sum
  arg_names = names(formals(args(fun__)))
  dots = list(...)
  args = list()
  args[[1]] = x__
  if(!is.null(arg_names)){
    arg_ok = intersect(names(dots), arg_names)
    args[arg_ok] = dots[arg_ok]
  }
  
  res = try(do.call(fun__, args), silent = TRUE)
  if(isError(res)){
    fun_arg = deparse(substitute(fun__))
    stop_hook("The evaluation of the function in argument {bq?fun_arg} ", 
              "led to an error, see below:",
              "\n{'^.+do\\.call[^\n]+\n'r?res}")
  }
  
  res
}

####
#### utilities ####
####


suggest_item = function(x, items, write_msg = TRUE, newline = TRUE, info = "variable"){
  # typical use: x is not in items
  #              we want to suggest possible values
  # returns vector of length 0 if no suggestion
  
  items_origin = items

  # 1: with case
  nx = nchar(x)
  items_nx = substr(items, 1, nx)

  qui = items_nx == x
  if(length(x) == 0){
    res = character(0)
    
  } else if(any(qui)){
    res = items[qui]

  } else {
    # 2: without case

    x = tolower(x)
    items_nx = tolower(items_nx)

    qui = items_nx == x
    if(any(qui)){
      res = items[qui]
    } else {
      # 3: with spelling mistakes, keeping the order
      # only if x > 3 characters

      if(nx < 3) return(character(0))

      # lazy algorithm
      score = numeric(length(items))
      for(i in 1:nx){
        score = score + (substr(items_nx, i, i) == substr(x, i, i))
      }

      if(any(score > (nx / 2))){
        s_order = order(score, decreasing = TRUE)
        score = score[s_order]
        items = items[s_order]
        
        res = items[score > nx / 2]
        
      } else {
        # 4: with spelling mistakes, ignoring the order
        x_letters = strsplit(x, "")[[1]]
        score = numeric(length(items))
        for(i in 1:nx){
          score = score + (substr(items_nx, i, i) %in% x_letters)
        }

        s_order = order(score, decreasing = TRUE)
        score = score[s_order]
        items = items[s_order]
        
        res = items[score > (nx * 0.65)]
      }
    }
  }

  if(write_msg){
    if(length(res) == 0){
      if(length(items_origin) <= 5){
        res = string_magic("FYI the {info}{$s, are, enum.bq ? items_origin}.")
      } else {
        res = string_magic("\nFYI the {info}s are: {sort, ', 'c ? items_origin}.")
      }
    } else {
      res = string_magic("Maybe you meant {enum.bq.or ? res}?")
    }

    if(newline){
      res = paste0("\n", res)
    }
  } else {
    if(length(res) == 0){
      res = head(items_origin, 5)
    }
  }

  res
}

opt_equal = function(x, option){
  any(!is.na(pmatch(x, option)))
}

deparse_short = function(x){
  x_dp = deparse(x)
  if(length(x_dp) > 1){
    x_dp = paste0(x_dp, "...")
  }

  x_dp
}

deparse_long = function(x){
  x_dp = deparse(x, width.cutoff = 500L)
  if(length(x_dp) > 1){
    x_dp = paste0(x_dp, collapse = "\n")
  }

  x_dp
}


####
#### dreamerr's copies ####
####


setDreamerr_show_stack = function(show_full_stack = FALSE){

  check_logical(show_full_stack, scalar = TRUE, mbt = TRUE)

  options("dreamerr_show_full_stack" = show_full_stack)

}

set_up = function(.up = 1){
  if(length(.up) == 1 && is.numeric(.up) && !is.na(.up) && .up == floor(.up) && .up >= 0){
    assign("DREAMERR__UP", .up, parent.frame())
  } else {
    stop("Argument '.up' must be an integer scalar greater or equal to 1. ",
         "This is currently not the case.")
  }
}

set_pblm_hook = function(){
  assign("string_magic_HOOK", 1, parent.frame())
}

get_up_hook = function(){
  # up with set_up
  f = parent.frame()
  up = 1
  while(!identical(f, .GlobalEnv)){
    if(exists("string_magic_HOOK", f)){
      break
    }
    up = up + 1
    f = parent.frame(up + 1)
  }
  
  # we accept direct nestedness
  f_up = parent.frame(up + 2)
  while(!identical(f_up, .GlobalEnv) && exists("string_magic_HOOK", f_up)){
    up = up + 1
    f = f_up
    f_up = parent.frame(up + 2)
  }

  if(identical(f, .GlobalEnv)){
    up = 1
  }

  up
}

.stop_hook = function(..., msg = NULL, envir = parent.frame()){
  # verbatim version
  up = get_up_hook()

  stop_up(..., up = up, msg = msg, envir = envir, verbatim = TRUE)
}

stopi = function(..., envir = parent.frame()){
  stop_up(..., up = 1, envir = envir, verbatim = FALSE)
}

stop_hook = function(..., msg = NULL, envir = parent.frame(), verbatim = FALSE){
  up = get_up_hook()

  stop_up(..., up = up, msg = msg, envir = envir, verbatim = verbatim)
}

stop_up = function(..., up = 1, msg = NULL, envir = parent.frame(), verbatim = FALSE){

  if(verbatim){
    main_msg = paste0(...)
  } else {
    main_msg = .string_magic(..., .envir = envir)
  }

  # up with set_up
  mc = match.call()
  if(!"up" %in% names(mc)){
    up_value = mget("DREAMERR__UP", parent.frame(), ifnotfound = 1)
    up = up_value[[1]]
  }
  
  up = up + 1

  show_full_stack = isTRUE(getOption("dreamerr_show_full_stack"))

  sc = sys.calls()
  
  if(show_full_stack){
    # The user requests the full stack
    my_call = sapply(sc, function(x) deparse(x, width.cutoff = 200L, nlines = 1))
    my_call = string_magic("{'\n'c ! [{format.0 ? 1:length(my_call)}] {'100|...'k ? my_call}}")

    intro = paste0("the full stack is shown (set this off with setDreamerr_show_stack(FALSE))\n", my_call)

  } else {
    # only the original call
    my_call = sys.call(sys.parent(up))
    my_call = deparse(my_call)[1]
    nmax = 50
    if(nchar(my_call) > nmax) my_call = paste0(substr(my_call, 1, nmax - 1), "...")

    intro = paste0("in ", my_call)
  }  

  main_msg = fit_screen(main_msg)

  if(!is.null(msg)){
    if(length(msg) > 1){
      msg = paste(msg, collapse = "")
    }
    msg = fit_screen(msg)
    on.exit(message(msg))
  }

  stop(intro, ": \n", main_msg, call. = FALSE)

}

warn_no_named_dots = function(dots, extra_args = NULL, extra_funName = ""){
  if(!is.null(names(dots))){
    is_extra = !is.null(extra_args)
    args_fun = names(formals(sys.function(sys.parent())))
    args_fun = unique(c(args_fun, extra_args))
    args_fun = setdiff(args_fun, "...")

    dot_names = names(dots)
    dot_names = dot_names[nchar(dot_names) > 0]
    sugg_txt = suggest_item(dot_names[1], args_fun, info = "argument")

    warn_up("The arguments in `...` shall not have names{&is_extra; or may refer to the ",
                      "arguments of the function in {bq?extra_funName}}. ",
            "The name{$s, enum.bq ? dot_names}",
            " may refer to {$(an;some)} argument{$s}?", sugg_txt)

  }
}

.warn_hook = function(..., envir = parent.frame(), immediate. = FALSE){
  up = get_up_hook()

  warn_up(..., up = up, envir = envir, verbatim = TRUE, immediate. = immediate.)
}

warn_hook = function(..., envir = parent.frame(), immediate. = FALSE, verbatim = FALSE){
  up = get_up_hook()

  warn_up(..., up = up, envir = envir, verbatim = verbatim, immediate. = immediate.)
}

warni = function(..., envir = parent.frame(), immediate. = FALSE){
  warn_up(..., up = 1, envir = envir, verbatim = FALSE, immediate. = immediate.)
}

warn_up = function (..., up = 1, immediate. = FALSE, envir = parent.frame(), verbatim = FALSE){

  if(verbatim){
    message = paste0(...)
  } else {
    message = .string_magic(..., .envir = envir)
  }
  
  mc = match.call()

  if (!"up" %in% names(mc)) {
      up_value = mget("DREAMERR__UP", parent.frame(), ifnotfound = 1)
      up = up_value[[1]]
  }
  
  up = up + 1
  
  my_call = sys.call(sys.parent(up))
  my_call = deparse(my_call)[1]
  
  nmax = 50
  if (nchar(my_call) > nmax){
    my_call = paste0(substr(my_call, 1, nmax - 1), "...")
  }
      
  warning("In ", my_call, ":\n ", fit_screen(message),
          call. = FALSE, immediate. = immediate.)
}


fit_screen = function(msg, width = NULL, leading_ws = TRUE, leader = ""){
  # makes a message fit the current screen, by cutting the text at the appropriate location
  # msg must be a character string of length 1
  
  if(length(msg) == 0) return(msg)

  # Note that \t are NOT handled
  
  # eval
  width = check_set_width(substitute(width))

  N_LEAD = nchar(leader)

  if(width > 1){
    MAX_WIDTH = width
  } else {
    MAX_WIDTH = getOption("width") * width
  }

  MAX_WIDTH = max(MAX_WIDTH, 15)

  res = c()

  msg_split = strsplit(msg, "\n", fixed = TRUE)[[1]]

  for(m in msg_split){
    if(nchar(m) <= MAX_WIDTH){
      res = c(res, paste0(leader, m))
    } else {
      # we apply a splitting algorithm

      lead_ws = gsub("^([ \t]*).*", "\\1", m, perl = TRUE)
      m = trimws(m)
      N_LEAD_WS = nchar(lead_ws)
      add_lead = TRUE
      first = TRUE

      m_split = strsplit(m, "(?<=[^ ]) ", perl = TRUE)[[1]]

      while(TRUE){

        if(add_lead){
          width = MAX_WIDTH - N_LEAD_WS - N_LEAD
          prefix = paste0(leader, lead_ws)
        } else {
          width = MAX_WIDTH - N_LEAD
          prefix = leader
        }

        if(sum(nchar(m_split) + 1) - 1 <= width){
          res = c(res, paste0(prefix, paste(m_split, collapse = " ")))
          break
        }

        where2split = which.max(cumsum(nchar(m_split) + 1) - 1 > width) - 1
        res = c(res, paste0(prefix, paste(m_split[1:where2split], collapse = " ")))
        m_split = m_split[-(1:where2split)]

        if(!leading_ws && first){
          add_lead = FALSE
          first = FALSE
        }

      }
    }
  }

  paste(res, collapse = "\n")
}

isError = function(x){
  inherits(x, "try-error")
}

####
#### fixest copies ####
####


check_expr = function(expr, ..., clean, up = 0, arg_name, verbatim = FALSE){

  res = tryCatch(expr, error = function(e) structure(list(conditionCall(e),
                                                          conditionMessage(e)), class = "try-error"))
  if(inherits(res, "try-error")){

    if(missing(up)){
      # up with set_up
      f = parent.frame()
      up = 1
      while(!identical(f, .GlobalEnv)){
        if(exists("string_magic_HOOK", f)){
          break
        }
        up = up + 1
        f = parent.frame(up + 1)
      }

      if(identical(f, .GlobalEnv)){
        up = 1
      }
    }
    
    up = up + 1 
    
    if(verbatim){
      msg = paste0(..., collapse = "")
    } else {
      msg = .string_magic(..., .envir = parent.frame())
    }
    
    if(nchar(msg) == 0){
      if(missing(arg_name)){
        arg_name = deparse(substitute(expr))
      }
      msg = paste0("Argument '", arg_name, "' could not be evaluated: ")
      stop_up(msg, res[[2]], verbatim = verbatim, envir = parent.frame(), up = up)
      
    } else {
      call_non_informative = deparse(substitute(expr),100)[1]

      call_error = deparse(res[[1]], 100)[1]

      if(call_error == call_non_informative || call_error == "NULL" || grepl("^(doTry|eval)", call_error)){
        call_error = ""
      } else {
        call_error = paste0("In ", call_error, ": ")
      }

      err = res[[2]]
      if(grepl("^in eval\\(str[^:]+:\n", err)){
        err = sub("^in eval\\(str[^:]+:\n", "", err)
      }
            
      # cleaning ugly call repetition
      current_call = deparse(sys.call(sys.parent(up)))[1]
      first_err = gsub("\n.+", "", err)
      if(grepl(substr(current_call, 1, 20), first_err, fixed = TRUE)){
        err = gsub("^[^\n]+\n", "", err)
      }
      
      # cleaning extra artifacts
      i_clean = 0
      msg_split = strsplit(msg, "\n")[[1]]
      err_split = strsplit(err, "\n")[[1]]
      while(i_clean < min(length(msg_split), length(err_split))){
        if(string_x(msg_split[i_clean + 1], 15) == string_x(err_split[i_clean + 1], 15)){
          i_clean = i_clean + 1
        } else {
          break
        }
      }
      
      for(i in seq_len(i_clean)){
        err = gsub("^[^\n]+\n", "", err)
      }
      
      if(!missing(clean)){
        err = string_clean(err, clean)
      }
      stop_up(msg, "\n", call_error, err, verbatim = TRUE, up = up)
    }
  }
  res
}





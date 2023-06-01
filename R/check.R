#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Jul 27 10:06:18 2022
# ~: checking functions
#----------------------------------------------#

# string.ops will be an import of dreamerr, so I cannot use dreamerr here

# NOTE: it's unusual for me but there will be A LOT of code duplication
#       the objective is to limit the most possible the overheads, hence this decision
#       + the functions are simple and to the point (much less complex than what is possible with dreamerr), so that's manageable

####
#### checking ####
####


check_logical = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, no_na = FALSE, mbt = FALSE){

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
            " PROBLEM: it is not logical, it is of class ", enum(class(x)), ".")
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

check_numeric = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, no_na = FALSE, mbt = FALSE, integer = FALSE){

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

  if(!is.numeric(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    vector_type = if(scalar) "scalar" else "vector"
    type = if(integer) "an integer" else "a numeric"
    
    stop_up("The ", nullable, "argument `", x_dp, "` must be ", type, " ", vector_type, ".",
            " PROBLEM: it is not {'^.+ 'r ? type} it is of class ", enum(class(x)), ".")
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
    if(any(x != round(x))){
      x_dp = deparse_short(substitute(x))
      i = which(x != round(x))[1]
      stop_up("The argument {bq?x_dp} must be an integer {&scalar ; scalar ; vector}.",
              "\nPROBLEM: {&length(x)==1 ; it is equal to {x} ; the {nth ? i} value is equal to {x[i]}}, not an integer.")
    }
  }

}

check_character = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, no_na = FALSE, mbt = TRUE){

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
            " PROBLEM: it is not character, it is of class ", enum(class(x)), ".")
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
    stop_up(dsb("Argument `.[x_dp]` must be atomic. \n",
                "PROBLEM: Currently it is of the non-atomic class .[bq?class(x)[1]]."))
  }

  if(!is.character(x)){
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
      # .[length(x)]
      # .[len, n ? x]
      # .[Len ? x]
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
    stop_up("The argument `", x_dp, "` must be an environment (ex: parent.frame()). ",
            "PROBLEM: it is not an environment, it is of class ", enum(class(x)), ".")
  }

}


check_set_dots = function(..., mc = NULL, mbt = FALSE, character = FALSE,
                          no_na = FALSE, scalar = FALSE){
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
  dots_nm = ...names()

  # We first catch evaluation problems
  for(i in 1:n){
    elem = try(...elt(i), silent = TRUE)

    if(isError(elem)){
      nm = if(is.null(dots_nm)) "" else dots_nm[i]
      if(is.na(nm)) nm = ""

      mc_dots = mc[["..."]]
      value = deparse_short(mc_dots[[i]])

      # really, this is gibberish: who can understand the code?
      nm = cub(" ({if(.C<4 ; erase) ! {nm} = }{value})")

      if(grepl("try(...", elem, fixed = TRUE)){
        elem = gsub("^[^:]+:", "", elem)
      }

      stop_up("In the argument `...`, the {#nth ? i} element{#s} raises an error:\n",
              elem)
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

      # really, this is gibberish: who can understand the code?
      info_call = .cub("`{if(.C<4 ; erase) ! {nm_pblm} = }{value_all}`")

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

      # really, this is gibberish: who can understand the code?
      info_call = cub("`{if(.C<4 ; erase) ! {nm_pblm} = }{value_all}`")

      cls_pblm = sapply(dots[i_pblm], function(x) dsb(".[bq ! .[enum ? class(x)]]"))
      stop_up(dsb("In the argument `...`, all elements must be atomic (i.e. convertible to a character string).\nPROBLEM: ",
                  ".['\n'c ! The .[Nth ? i_pblm] element (.[info_call]) is not character",
                  " (instead it is of class: .[cls_pblm]).]"))
    }

    i_no_char = which(!sapply(dots, is.character))
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

      # really, this is gibberish: who can understand the code?
      info_call = cub("`{if(.C<4 ; erase) ! {nm_pblm} = }{value_all}`")

      stop_up(dsb("In the argument `...`, all elements must be without NA.\nPROBLEM: ",
                  "The .[nth, enum ? i_pblm] element.[$s] (.[C ? info_call])",
                  " .[$contain] NA values."))
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

    if(case){
      pm = pmatch(v, options)
    } else {
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

warn_no_named_dots = function(dots){
  if(!is.null(names(dots))){
    args_fun = names(formals(sys.function(sys.parent())))
    args_fun = setdiff(args_fun, "...")

    dot_names = names(dots)
    dot_names = dot_names[nchar(dot_names) > 0]
    sugg_txt = suggest_item(dot_names[1], args_fun, info = "argument")

    warn_up("The arguments in `...` shall not have names. The name{$s, enum.bq ? dot_names}",
            " may refer to {$(an;some)} argument{$s}?", sugg_txt)

  }
}

warn_up = function (..., up = 1, immediate. = FALSE, verbatim = FALSE){

  if(verbatim){
    message = paste0(...)
  } else {
    message = cub(..., frame = parent.frame())
  }
  
  mc = match.call()

  if (!"up" %in% names(mc)) {
      up_value = mget("DREAMERR__UP", parent.frame(), ifnotfound = 1)

      up = up_value[[1]]
  }

  my_call = deparse(sys.calls()[[max(1, sys.nframe() - (1 + up))]])[1]
  
  nmax = 50
  if (nchar(my_call) > nmax){
    my_call = paste0(substr(my_call, 1, nmax - 1), "...")
  }
      
  warning("In ", my_call, ":\n ", fit_screen(message),
          call. = FALSE, immediate. = immediate.)
}



setDreamerr_show_stack = function(show_full_stack = FALSE){

  check_logical(show_full_stack, scalar = TRUE, mbt = TRUE)

  options("dreamerr_show_full_stack" = show_full_stack)

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
  if(any(qui)){
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
        
        res = items[score > (nx * 3 / 4)]
      }
    }
  }  

  if(write_msg){
    if(length(res) == 0){
      if(length(items) <= 5){
        res = cub("FYI the {info}{$s, are, enum.bq ? items_origin}.")
      } else {
        res = cub("FYI the first 5 {info}s are {enum.bq ? items_origin}.")
      }
    } else {
      res = cub("Maybe you meant {enum.bq.or ? res}?")
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

set_pblm_hook = function(){
  assign("STRINGOPS_HOOK", 1, parent.frame())
}

.stop_hook = function(..., msg = NULL, frame = parent.frame()){
  # verbatim version
  up = get_up_hook()

  stop_up(..., up = up + 1, msg = msg, frame = frame, verbatim = TRUE)
}

stop_hook = function(..., msg = NULL, frame = parent.frame(), verbatim = FALSE){
  up = get_up_hook()

  stop_up(..., up = up + 1, msg = msg, frame = frame, verbatim = verbatim)
}

get_up_hook = function(){
  # up with set_up
  f = parent.frame()
  up = 1
  while(!identical(f, .GlobalEnv)){
    if(exists("STRINGOPS_HOOK", f)){
      break
    }
    up = up + 1
    f = parent.frame(up + 1)
  }

  if(identical(f, .GlobalEnv)){
    up = 1
  }

  up
}


####
#### dreamerr's copies ####
####

# DO NOT MAKE ANY SUBSTANTIAL CHANGE TO THESE FUNCTIONS!!!
# They come from dreamerr. If I want to improve them, I should change the ones of dreamerr first


set_up = function(.up = 1){
  if(length(.up) == 1 && is.numeric(.up) && !is.na(.up) && .up == floor(.up) && .up >= 0){
    assign("DREAMERR__UP", .up, parent.frame())
  } else {
    stop("Argument '.up' must be an integer scalar greater or equal to 1. This is currently not the case.")
  }
}

stop_up = function(..., up = 1, msg = NULL, frame = parent.frame(), verbatim = FALSE){

  if(verbatim){
    main_msg = paste0(...)
  } else {
    main_msg = .cub(..., frame = frame)
  }
  

  # up with set_up
  mc = match.call()
  if(!"up" %in% names(mc)){
    up_value = mget("DREAMERR__UP", parent.frame(), ifnotfound = 1)
    up = up_value[[1]]
  }

  show_full_stack = isTRUE(getOption("dreamerr_show_full_stack"))

  sc = sys.calls()
  
  if(show_full_stack){
    # The user requests the full stack
    my_call = sapply(sc, function(x) deparse(x, width.cutoff = 200L, nlines = 1))
    my_call = cub("{'\n'c ! [{format.0 ? 1:length(my_call)}] {'100|...'k ? my_call}}")

    intro = paste0("the full stack is shown (set this off with setDreamerr_show_stack(FALSE))\n", my_call)

  } else {
    # only the original call
    my_call = deparse(sc[[max(1, sys.parent(1 + up))]])[1] # call can have svl lines
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


fit_screen = function(msg, width = 0.9, leading_ws = TRUE, leader = ""){
  # makes a message fit the current screen, by cutting the text at the appropriate location
  # msg must be a character string of length 1
  
  if(length(msg) == 0) return(msg)

  # Note that \t are NOT handled

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


enum = function (x, type, verb = FALSE, s = FALSE, past = FALSE, or = FALSE,
                 start_verb = FALSE, quote = FALSE, enum = FALSE, other = "", nmax = 7){
  # function that enumerates items and add verbs
  # in argument type, you can have a mix of the different arguments, all separated with a "."

  if(length(x) == 0) return("");

  if(!missing(type)){
    args = strsplit(type, "\\.")[[1]]
    s = "s" %in% args
    past = "past" %in% args
    or = "or" %in% args
    start_verb = "start" %in% args
    quote = "quote" %in% args
    enum = any(grepl("^enum", args))
    if(enum){
      arg_enum = args[grepl("^enum", args)][1]
      if(grepl("i", arg_enum)){
        enum = "i"
      } else if(grepl("I", arg_enum)){
        enum = "I"
      } else if(grepl("a", arg_enum)){
        enum = "a"
      } else if(grepl("A", arg_enum)){
        enum = "A"
      } else if(grepl("1", arg_enum)){
        enum = "1"
      } else {
        enum = "i"
      }

      args = args[!grepl("^enum", args)]
    }

    is_other = any(grepl("^other\\(", args))
    if(is_other){
      arg_other = args[grepl("^other\\(", args)][1]
      other = gsub("^.+\\(|\\).*", "", arg_other)

      args = args[!grepl("^other", args)]
    }

    # Now the verb
    verb = setdiff(args, c("s", "past", "or", "start", "quote"))
    if(length(verb) == 0){
      verb = "no"
    } else {
      verb = verb[1]
    }

  } else {
    verb = as.character(verb)

    enum = match.arg(as.character(enum), c("i", "a", "1", "I", "A", "FALSE", "TRUE"))
    if(enum == "TRUE") enum = "i"
  }

  n = length(x)

  # Ensuring it's not too long
  if(n > nmax){

    nmax = nmax - 1

    other = trimws(other)
    if(nchar(other) > 0){
      other = paste0(other, " ", n - nmax, " others")
    } else {
      other = paste0(n - nmax, " others")
    }

    if(quote){
      x = c(paste0("'", x[1:nmax], "'"), other)
    } else {
      x = c(x[1:nmax], other)
    }

    n = length(x)
  } else if(quote){
    x = paste0("'", x, "'")
  }

  # The verb
  if(verb == "FALSE"){
    verb = "no"
  } else if(verb %in% c("be", "are")){
    verb = "is"
  } else if(verb == "have"){
    verb = "has"
  } else if(verb == "does"){
    verb = "do"
  } else if(verb %in% c("do not", "does not", "don't", "doesn't")){
    verb = "do not"
  } else if(verb %in% c("is not", "are not", "isn't", "aren't")){
    verb = "is not"
  } else if(verb %in% c("was", "were")){
    verb = "is"
    past = TRUE
  }

  if(past){
    if(verb %in% c("no", "is", "is not", "has", "do", "do not")){
      verb_format = switch(verb, is = ifunit(n, " was", " were"), "is not" = ifunit(n, " wasn't", " weren't"), no = "", has=" had", do = "did", "do not" = " didn't")
    } else {
      verb_format = paste0(" ", verb, "ed")
    }
  } else {
    if(verb %in% c("no", "is", "is not", "has", "do", "do not")){
      verb_format = switch(verb, is = ifunit(n, " is", " are"), "is not" = ifunit(n, " isn't", " aren't"), no = "", has = ifunit(n, " has", " have"), do = ifunit(n, " does", " do"), "do not" = ifunit(n, " doesn't", " don't"))
    } else {
      verb_format = ifelse(n == 1, paste0(" ", verb, "s"), paste0(" ", verb))
    }

  }

  if (s) {
    startWord = ifelse(n == 1, " ", "s ")
  } else {
    startWord = ""
  }

  if(enum != "FALSE"){
    enum_comma = ","

    if(enum == "i"){
      enum_all = strsplit("i.ii.iii.iv.v.vi.vii.viii.ix.x.xi.xii.xiii.xiv.xv", "\\.")[[1]][1:n]
    } else if(enum == "I"){
      enum_all = toupper(strsplit("i.ii.iii.iv.v.vi.vii.viii.ix.x.xi.xii.xiii.xiv.xv", "\\.")[[1]][1:n])
    } else if(enum == "a"){
      enum_all = base::letters[1:n]
    } else if(enum == "A"){
      enum_all = base::LETTERS[1:n]
    } else if(enum == "1"){
      enum_all = 1:n
    }

    enum_all = paste0(enum_all, ") ")
  } else {
    enum_comma = ""
    enum_all = rep("", n)
  }

  if (n == 1) {
    if(!start_verb){
      res = paste0(startWord, x, verb_format)
    } else {
      res = paste0(startWord, gsub(" ", "", verb_format), " ", x)
    }

  } else {
    and_or = ifelse(or, " or ", " and ")
    if(!start_verb){
      res = paste0(startWord, paste0(enum_all[-n], x[-n], collapse = ", "), enum_comma, and_or, enum_all[n], x[n], verb_format)
    } else {
      res = paste0(startWord, gsub(" ", "", verb_format), " ", paste0(x[-n], collapse = ", "), and_or, x[n])
    }

  }

  res
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
  if ("try-error" %in% class(res)) {

    if(missing(up)){
      # up with set_up
      f = parent.frame()
      up = 1
      while(!identical(f, .GlobalEnv)){
        if(exists("STRINGOPS_HOOK", f)){
          break
        }
        up = up + 1
        f = parent.frame(up + 1)
      }

      if(identical(f, .GlobalEnv)){
        up = 1
      }
    }
    
    set_up(1 + up)
    
    if(verbatim){
      msg = paste0(..., collapse = "")  
    } else {
      msg = .cub(..., frame = parent.frame())
    }
    
    if (nchar(msg) == 0) {
      if (missing(arg_name)) {
        arg_name = deparse(substitute(expr))
      }
      msg = paste0("Argument '", arg_name, "' could not be evaluated: ")
      stop_up(msg, res[[2]], verbatim = verbatim, frame = parent.frame())
    }
    else {
      call_non_informative = deparse(substitute(expr),100)[1]

      call_error = deparse(res[[1]], 100)[1]

      if (call_error == call_non_informative || call_error ==
          "NULL" || grepl("^(doTry|eval)", call_error)) {
        call_error = ""
      } else {
        call_error = paste0("In ", call_error, ": ")
      }

      err = res[[2]]
      if (grepl("^in eval\\(str[^:]+:\n", err)) {
        err = sub("^in eval\\(str[^:]+:\n", "", err)
      }
      
      if (!missing(clean)) {
        if (grepl(" => ", clean)) {
          clean_split = strsplit(clean, " => ")[[1]]
          from = clean_split[1]
          to = clean_split[2]
        }
        else {
          from = clean
          to = ""
        }
        stop_up(msg, "\n  ", call_error, gsub(from, to, err), verbatim = TRUE)
      }
      else {
        stop_up(msg, "\n  ", call_error, err, verbatim = TRUE)
      }
    }
  }
  res
}





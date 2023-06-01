#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Jul 27 09:42:17 2022
# ~: Misc funs for string.ops, mostly internal
#----------------------------------------------#



####
#### string manipulation ####
####

str_to_ascii = function(x, options){
  # force to ASCII using iconv. Keeps track of encoding error

  opts = c("silent", "utf8")
  options = check_set_options(options, opts, "ascii", free = TRUE)

  is_silent = "silent" %in% options

  if("utf8" %in% options){
    from = "UTF-8"
  } else {
    from = setdiff(options, opts)
    if(length(from) == 0){
      from = ""
    }
  }

  old_char = try(nchar(x), silent = TRUE)

  if("try-error" %in% class(old_char)){
    # super nasty encoding => nchar can't be used
    x_conv = iconv(x, to = "ASCII//TRANSLIT//IGNORE", from = from)
    nbfail = 0

    if(!is_silent){
      warning(fit_screen("The encoding was a bit nasty (nchar couldn't be used!): please check the conversion to ASCII.",
              "\n(Ignore this message with the `silent` option [ex: `ascii.silent`]. Pass the native encoding as an option?)"))
    }

  } else {
    x_conv = iconv(x, to = "ASCII//TRANSLIT//IGNORE", from = from)
    which_failed = which(old_char != nchar(x_conv) | grepl("?", x_conv, fixed = TRUE))
    nbfail = length(which_failed)
  }

  if(nbfail > 0){
    # We manually remove the problems
    x_manual = ascii_convert_manual(x[which_failed])

    x_conv[which_failed] = x_manual

    # if necessary, we update the isFailed attribute
    if("isFailed" %in% names(attributes(x_manual))){
      ANY_FAILED = TRUE
      attr(x_conv, "isFailed") = which_failed[attr(x_manual, "isFailed")]

      if(!is_silent){
        warning(fit_screen(cub("When transforming to ASCII, there {#was, n ? nbfail} failed encoding{#s}. ",
                    "Attribute `isFailed` is created (note that you can only access it if the variable is not transformed).",
                    "\n(Ignore this message with the `silent` option [ex: `ascii.silent`]. Pass the native encoding as an option?)")))
      }
    }
  }

  x_conv
}


ascii_convert_manual = function(x, type = c("all", "upper", "lower")){
  # This is an internal function that translates some characters into ascii
  # such characters could not be transformed using iconv
  # You can find country specific characters in http://german.typeit.org/

  type = match.arg(type)

  #
  # Lower characters
  #

  from_lower = c("á|à|å|æ|ä|â|ã|ą",
                 "é|è|ë|ê",
                 "í|ï|î",
                 "ó|ò|ô|ø|œ|õ",
                 "ú|ü|û|ù",
                 "ç",
                 "ñ",
                 "ß|ś")

  to_lower = c("a", "e", "i", "o", "u", "c", "n", "s")

  #
  # Upper characters
  #

  # toupper(from_lower)
  from_upper = c("Á|À|Å|Æ|Ä|Â|Ã",
                 "É|È|Ë|Ê",
                 "Í|Ï|Î",
                 "Ó|Ò|Ô|Ø|Œ|Õ",
                 "Ú|Ü|Û|Ù",
                 "Ÿ",
                 "Ç",
                 "Ñ",
                 "ß")

  to_upper = c("A", "E", "I", "O", "U", "Y", "C", "N", "S")

  #
  # Go!
  #

  from = c(from_upper, from_lower)
  to = c(to_upper, to_lower)

  for(i in 1:length(from)){
    x = gsub(from[i], to[i], x, perl = TRUE)
  }

  # We check for the remaining problems
  x_conv = iconv(x, to = "ASCII")
  which_failed = which(is.na(x_conv))
  nb_failed = length(which_failed)

  if(nb_failed > 0){

    x_new = iconv(x[which_failed], to = "ASCII//TRANSLIT//IGNORE")

    # We check AGAIN for the remaining problems
    which_failed_special = which(nchar(x[which_failed]) != nchar(x_new))
    nb_failed = length(which_failed_special)

    # For the failed encodings, we just replace the failed values by underscores
    # We don't let the value of "ASCII//TRANSLIT//IGNORE" because it can be very problematic
    if(nb_failed > 0) x_new[which_failed_special] = iconv(x[which_failed][which_failed_special], to = "ASCII", sub = "_")

    x[which_failed] = x_new

    if(nb_failed > 0){
      # warning("There has been ", nb_failed, " failed encoding. Attribute isFailed is created to track the problems.")
      attr(x, "isFailed") = which_failed
    }

  }

  return(x)
}



####
#### internal ####
####

format_help = function(x, pattern = NULL){
  # x: help message

  do_select = !is.null(pattern) && !isTRUE(pattern)

  text = x
  select = logical(length(x))

  for(i in seq_along(pattern)){
    p = pattern[i]

    p_raw = p
    first_char = substr(p, 1, 1)

    if(first_char == "#"){
      # fixed char search
      p = substr(p, 2, nchar(p))
      qui = grepl(p, x, fixed = TRUE)

    } else if(first_char %in% c("@", "^")){
      # regex or pmatch
      p = substr(p, 2, nchar(p))
      if(first_char == "^"){
        p = paste0("(?i)(^| )\\Q", p, "\\E")
      }
      qui = grepl(p, x, perl = TRUE)

    } else {
      p = paste0("(?i)(^| )\\Q", p, "\\Es?( |$)")
      qui = grepl(p, x, perl = TRUE)
    }

    if(!any(qui)){
      stop_up("In argument `help`, the pattern `", p_raw, "` did not match any element of the help.")
    }

    select = select | qui
  }

  if(do_select){
    # We select the section
    # of each selected line

    line_id = which(qui)
    section_id = which(substr(x, 1, 3) != "   " & x != "")
    empty_id = which(x == "")

    start = end = c()
    for(i in seq_along(line_id)){
      line = line_id[i]
      start[i] = max(section_id[section_id <= line])
      if(line == length(x)){
        end[i] = line
      } else {
        end[i] = min(empty_id[empty_id > line])
      }

    }

    info = cbind(start, end)
    info = info[!duplicated(info[, 1]), ]

    text = c()
    for(i in 1:nrow(info)){
      text = c(text, x[info[i, 1]:info[i, 2]])
    }

    arrow_id = which(text %in% x[line_id])
    arrows = rep("   ", length(text))
    arrows[arrow_id] = "-> "
  }

  # we format the tabs
  if(any(str_is(text, "#_TAB_"))){

    qui_tabs = str_is(text, "#_TAB_")
    tabs_split = strsplit(text[qui_tabs], "_TAB_", fixed = TRUE)

    tabs_mat = do.call(rbind, tabs_split)
    tabs_mat = apply(tabs_mat, 2, format)

    # we correct the backslashes not "properly" (in our context) formatted by format
    col = tabs_mat[, 2]
    nmax = max(nchar(col))

    if(any(nchar(col) < nmax)){
      qui = nchar(col) < nmax
      str_short = col[qui]
      tabs_mat[qui, 2] = substr(paste0(str_short, "           "), 1, nmax)
    }

    tabs_mat[, 3] = gsub(" +$", "", tabs_mat[, 3])

    tabs_fmt = paste(tabs_mat[, 1], tabs_mat[, 2], tabs_mat[, 3])

    text[qui_tabs] = tabs_fmt
  }

  if(any(str_is(text, "#---|"))){
    # we format the sections
    qui_sec = str_is(text, "#---|")

    text[qui_sec] = .dsb(".[55k ! .['|'r ? text[qui_sec]].[55*c ! -]]|")
  }

  # we add the indicator of match
  if(do_select){
    text = paste0(arrows, text)
  }

  message(fit_screen(paste(text, collapse = "\n")))
}

bespoke_msg = function(x, fun_name = NULL){
  # function to write an error message curated for the current function
  # cat(bespoke_msg('x = 1:5; dsb("test.[3K ? x[-1]]")', ".cub", FALSE))
  # the original error msg must always be written in the dsb way

  if(length(x) > 1){
    x = paste(x, collapse = "")
  }

  # We find the original functions, the one just before string_ops_internal
  if(is.null(fun_name)){
    all_funs = sapply(sys.calls(), function(x) deparse(x)[1])
    qui = which(grepl("string_ops_internal", all_funs)) - 1
    if(length(qui) > 0 && qui[1] > 0){
      fun_name = gsub("\\(.+", "", all_funs[qui[1]])
    } else {
      fun_name = "dsb"
    }
  }

  # we double the backslashes for printing
  if(grepl("\\", x, fixed = TRUE)){
    x = gsub("\\", "\\\\", x, fixed = TRUE)
  }

  if(fun_name == "dsb") return(x)

  is_dsb = grepl("dsb", fun_name, fixed = TRUE)

  res = gsub("dsb(", paste0(fun_name, "("), x, fixed = TRUE)
  res = gsub("dsb`", paste0(fun_name, "`"), res, fixed = TRUE)

  if(is_dsb) return(res)

  res = gsub(".[", "{", res, fixed = TRUE)

  if(grepl("[", res, fixed = TRUE)){
    # means we have indexing, which is valid
    res = gsub("\\[([^]]+)\\]", "[\\1]_", res)
    res = gsub("](?!_)", "}", res, perl = TRUE)
    res = gsub("]_", "]", res, fixed = TRUE)
  } else {
    res = gsub("]", "}", res, fixed = TRUE)
  }

  res
}

is_numeric_in_char = function(x){
  res = tryCatch(as.numeric(x), warning = function(x) "not numeric")
  !identical(res, "not numeric")
}


str_x = function(x, n, from){
  # string extract

  if(is.character(n)){
    n = nchar(n)
  }

  if(n < 0){
    if(!missing(from)) stop("Internal error: you should not provide arg. `from` when `n` is negative.")
    from = nchar(x) + n + 1
    to = nchar(x)
  } else {
    from = 1
    to = n
  }

  substr(x, from, to)
}

str_trim = function(x, n_first = 0, n_last = 0){
  # positive values: first
  # negative values: last

  if(is.character(n_first)){
    n_first = nchar(n_first)
  } else if(n_first < 0){
    n_last = -n_first
    n_first = 0
  }

  substr(x, 1 + n_first, nchar(x) - n_last)
}


n_th = function(x, letters = TRUE, compact = FALSE){
  # The main purpose of this function is for smallish 'x'
  # only to print info, not performance oriented.

  if(is.character(x)) return(x)

  is_compact = FALSE
  if(length(x) > 1 && all(diff(x) == 1)){
    is_compact = TRUE
    x = x[c(1, length(x))]
  }

  res = character(length(x))

  if(letters){
    # We don't go all the way, it makes no sense

    res[x == 0] = "zeroth"

    dict = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh",
             "eighth", "nineth", "tenth", "eleventh", "twelfth", "thirteenth",
             "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth",
             "nineteenth", "twentyth")

    qui = x >= 1 & x <= 20
    res[qui] = dict[x[qui]]

    if(!any(res == "")){
      if(is_compact){
        res = paste0(res[1], " to ", res[2])
      }
      return(res)
    }
  }

  qui = res == ""
  rest = x[qui] %% 10
  rest[rest == 0 | rest >= 4] = 4
  rest[x[qui] %in% 11:13] = 4
  postfix = c("st", "nd", "rd", "th")

  res[qui] = paste0(x[qui], postfix[rest])

  if(is_compact){
    res = paste0(res[1], " to ", res[2])
  }

  res
}

# Internal fun, n is ALWAYS len 1, positive and not missing
n_letter = function(x){

  res = character(length(x))

  for(i in seq_along(x)){

    xi = x[i]
    
    if(xi >= 100){
      val = format(xi, big.mark = ",")
    } else {

      is_minus = xi < 0
      if(is_minus) xi = abs(xi)

      num2char = c("zero", "one", "two", "three", "four", "five", "six", "seven",
                  "eight", "nine", "ten", "eleven", "twelve", "thirteen",
                  "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
                  "nineteen")
      if(xi < 20){
        val = num2char[xi + 1]
      } else if(xi < 100){
        tens = xi %/% 10
        digit = xi %% 10
        tens_letter = c("twenty", "thirty", "forty", "fifty",
                        "sixty", "seventy", "eighty", "ninety")

        num2char = paste0("-", num2char)
        num2char[1] = ""

        val = paste0(tens_letter[tens - 1], num2char[digit + 1])
      }

      if(is_minus){
        val = paste0("minus ", val)
      }
    }
    
    res[i] = val
  }

  res
}


enum_letter = function(n){
  # returns only lowercase
  # oddity: there is no powers of z
  # => because otherwise the algorithm would have been too complex
  # and we don't care tbh


  if(n < 27){
    return(letters[1:n])
  }

  n_26 = log(n, 26)
  n_26 = floor(n_26) + (n_26 %% 1 == 0)

  rest = (1:n) - 1
  res = vector("list", n_26 + 1)
  i = 1
  for(p in n_26:0){
    num = 26**p

    if(p == 0){
      res[[i]] = letters[(rest %/% num) + 1]
    } else {
      res[[i]] = c("", letters)[(rest %/% num) + 1]
    }

    rest = rest %% num
    i = i + 1
  }

  res = do.call(base::paste0, res)

  res
}



# Internal fun, n is ALWAYS len 1, positive and not missing
n_times = function(x, letters = TRUE){
  # 1 and 2 are always once and twice

  res = character(length(x))

  for(i in seq_along(x)){
    xi = x[i]

    if(xi == 0) {
      if(letters){
        res[i] = "zero times"
      } else {
        res[i] = "0 times"
      }
    } else if(xi < 4 && letters){
      dict = c("once", "twice", "three times", "four times")
      res[i] = dict[xi]
    } else if(!letters && xi <= 2){
      res[i] = if(xi == 1) "once" else "twice"
    } else {
      if(letters){
        xi = n_letter(xi)  
      }      
      res[i] = paste0(xi, " times")
    }    
  }

  res
}

enum_main = function(x, options){

  # we construct the call to enum
  # default values
  quote = NULL
  enum = FALSE
  or = FALSE
  nmax = 7

  # Quote
  q_map = c("q" = "'", "Q" = '"', "bq" = "`")
  qui = which(options %in% names(q_map))
  if(length(qui) > 0){
    if(length(qui) > 1){
      stop_hook("In `enum`, you cannot use more than one quote keyword ",
                "(q, Q, or bq) in the enumeration. PROBLEM: `", op, "` is not valid.")
    }

    q = options[qui]
    options = options[-qui]
    quote = q_map[q]
  }

  # or
  if("or" %in% options || "nor" %in% options){
    or = intersect(c("or", "nor"), options)
    options = setdiff(options, c("or", "nor"))
  }

  # i, a, etc
  qui = which(options %in% c("i", "I", "a", "A", "1"))
  if(length(qui) > 0){
    enum = options[qui[1]]
    options = options[-qui]
  }

  # digit
  qui = which(grepl("^[[:digit:]]+$", options) & options != "1")
  if(length(qui) > 0){
    nmax = as.numeric(options[qui[1]])
  }
  
  oxford = opt_equal(options, "oxford")
  
  enumerate_items(x, quote = quote, or = or, enum = enum, nmax = nmax, oxford = oxford)
}

format_difftime = function(x, options){
  # x: number of seconds or difftime or time

  if(is.character(x)){
    if(is_numeric_in_char(x)){
      # x: number of seconds
      x = as.numeric(x)
    } else {
      # When the data is not conform:
      # - should there be an error?
      # - should I return NA?

      if(!opt_equal(options, "silent")){
        warning(fit_screen(dsb("Operation `dtime` could not be applied since the data",
                                " was not numeric, nor a POSIX time, nor a time-difference.",
                                "\n (To avoid this warning, use the option `silent`: `dtime.silent`.)")))
      }

      return(rep("(difftime: NA)", length(x)))
    }
  }

  res = character(length(x))

  for(i in seq_along(x)){
    xi = x[i]
    
    if(inherits(xi, "POSIXt")){
      xi = Sys.time() - xi
    }
    
    if(inherits(xi, "difftime")){
      xi = as.double(xi, units = "secs")
    }
    
    if(xi > 3600){
      n_hour = xi %/% 3600
      rest_s = floor(xi %% 3600)
      n_min = rest_s %/% 60
      res[i] = cub("{n ? n_hour} hour{#s} {%02i ? n_min} min")
    } else if(xi > 60){
      n_min = xi %/% 60
      n_sec = floor(xi %% 60)
      res[i] = cub("{n ? n_min} min {%02i ? n_sec} sec")
    } else if(xi > 0.9){
      res[i] = paste0(fsignif(xi, 2, 1), "s")
    } else if(xi > 1e-3){
      res[i] = paste0(fsignif(xi * 1000, 2, 0), "ms")
    } else {
      res[i] = "<1 ms"
    }
  }
  
  res
}

fsignif = function (x, s = 2, r = 0, commas = TRUE){
  # This is not intended to be applied to large vectors (not efficient)
  # Only for the in-print formatting of some numbers

  # It would supa dupa simple in c++...

  if(is.character(x)){
    return(x)
  }

  if(!is.numeric(x)){
    stop("The argumnent 'x' must be numeric.")
  }

  commas_single = function(x, s, r){

    if(!is.finite(x) || abs(x) < 1) return(as.character(x))

    if((p <- ceiling(log10(abs(x)))) < s){
      r = max(r, s - p)
    }

    x_sign = sign(x)
    x = abs(x)

    x_str = deparse(x)
    dec_string = ""

    if(grepl(".", x_str, fixed = TRUE)){
      decimal = gsub(".*\\.", ".", x_str)
      dec_string = substr(decimal, 1, 1 + r)

      if(dec_string == ".") dec_string = ""
    }

    entier = sprintf("%.0f", floor(x))
    quoi = rev(strsplit(entier, "")[[1]])
    n = length(quoi)
    sol = c()
    for (i in 1:n) {
      sol = c(sol, quoi[i])
      if (i%%3 == 0 && i != n) sol = c(sol, ",")
    }

    res = paste0(ifelse(x_sign == -1, "-", ""), paste0(rev(sol), collapse = ""), dec_string)

    res
  }

  signif_single = function(x, s, r) {
    if(is.na(x)) {
      return(NA)
    }

    if(abs(x) >= 10^(s - 1)){
      return(round(x, r))
    } else {
      return(signif(x, s))
    }
  }

  res = sapply(x, signif_single, s = s, r = r)

  if(commas) res = sapply(res, commas_single, s = s, r = r)

  qui0 = grepl("^0.", res, perl = TRUE)
  if(any(qui0)){
    qui_short = nchar(res) < s + 2
    if(any(qui_short)){
      for(i in which(qui0)[qui_short]){
        res[i] = as.vector(sprintf("%s%.*s", res[i], s + 2 - nchar(res[i]), "0000000000000000"))
      }
    }
  }

  res
}



enumerate_items = function (x, or = FALSE, quote = NULL,
                            enum = FALSE, nmax = 7, oxford = FALSE){
  # function that enumerates items
  # in argument type, you can have a mix of the different arguments, all separated with a "."

  if(length(x) == 0) return("");

  is_quote = !is.null(quote)
  n = length(x)

  # Ensuring it's not too long
  is_other = n > nmax
  if(is_other){

    nmax = nmax - 1

    other = paste0(n - nmax, " others")

    if(is_quote){
      x = c(paste0(quote, x[1:nmax], quote), other)
    } else {
      x = c(x[1:nmax], other)
    }

    n = length(x)
  } else if(is_quote){
    x = paste0(quote, x, quote)
  }

  is_enum = enum %in% c("i", "I", "a", "A", "1")
  if(is_enum){
    enum_comma = ","

    if(enum == "i"){
      enum_all = tolower(as.roman(1:n))
    } else if(enum == "I"){
      enum_all = as.roman(1:n)
    } else if(enum == "a"){
      enum_all = enum_letter(n)
    } else if(enum == "A"){
      enum_all = toupper(enum_letter(n))
    } else if(enum == "1"){
      enum_all = 1:n
    }

    enum_all = paste0(enum_all, ") ")

    if(is_other){
      enum_all[length(enum_all)] = ""
    }

  } else {
    enum_comma = ""
    enum_all = rep("", n)
  }

  if (n == 1) {
    res = x
  } else {
    
    and_or = " and "
    if(identical(or, "or") || isTRUE(or)){
      and_or = " or "
    } else if(identical(or, "nor")){
      and_or = " nor "
    }

    if(is_enum){
      res = paste0(paste0(enum_all[-n], x[-n], collapse = ", "), enum_comma, and_or, enum_all[n], x[n])
    } else {
      if(oxford){
        and_or = paste0(",", and_or)
      }
      res = paste0(paste0(x[-n], collapse = ", "), and_or, x[n])
    }

  }

  res
}


conjugate = function(verb, plural){

  # note that the parser squashes the spaces
  info_case = gsub("[[:upper:]]", "_", verb)
  verb = tolower(verb)

  if(verb %in% c("is", "are")){
    verb = "is"
  } else if(verb %in% c("has", "have")){
    verb = "has"
  } else if(verb %in% c("has not", "have not")){
    verb = "has not"
  } else if(verb %in% c("hasn't", "haven't")){
    verb = "hasn't"
  } else if(verb %in% c("donot", "doesnot")){
    verb = "do not"
  } else if(verb %in% c("don't", "doesn't")){
    verb = "don't"
  } else if(verb %in% c("isnot", "arenot")){
    verb = "is not"
  } else if(verb %in% c("isn't", "aren't")){
    verb = "isn't"
  } else if(verb %in% c("wasnot", "werenot")){
    ver = "was not"
  } else if(verb %in% c("wasn't", "weren't")){
    ver = "wasn't"
  } else if(verb %in% c("was", "were")){
    verb = "was"
  } else if(grepl("ies$", verb)){
    verb = gsub("ies$", "y", verb)

  } else if(grepl("(o|h)es$", verb)){
    verb = gsub("es$", "", verb)

  } else if(grepl("s$", verb)){
    verb = gsub("s$", "", verb)
  }

  pick = function(a, b) if(plural) b else a

  if(verb %in% c("is", "is not", "isn't", "has", "has not", "hasn't", 
                 "do", "do not", "don't", "was", "was not", "wasn't")){

    res = switch(verb,
                 is = pick("is", "are"),
                 "is not" = pick("is not", "are not"),
                 "isn't" = pick("isn't", "aren't"),
                 has = pick("has", "have"),
                 "has not" = pick("has not", "have not"),
                 "hasn't" = pick("hasn't", "haven't"),
                 do = pick("does", "do"),
                 "do not" = pick("does not", "do not"),
                 "don't" = pick("doesn't", "don't"),
                 "was" = pick("was", "were"),
                 "was not" = pick("was not", "were not"),
                 "wasn't" = pick("wasn't", "weren't"))

  } else {
    # by default, the rest is in plural form

    if(plural){
      res = verb

    } else {
      if(grepl("(h|o)$", verb)){
        res = paste0(verb, "es")

      } else if(grepl("[^aeo]y$", verb)){
        res = paste0(str_trim(verb, -1), "ies")

      } else {
        res = paste0(verb, "s")
      }
    }
  }

  if(substr(info_case, 1, 2) == "__"){
    res = toupper(res)
  } else if(substr(info_case, 1, 1) == "_"){
    substr(res, 1, 1) = toupper(substr(res, 1, 1))
  }

  res
}


missnull = function(x){
  missing(x) || is.null(x)
}

names_xpd = function(x){
  nm = names(x)
  if(is.null(nm)){
    nm = character(length(x))
  }
  nm
}


is_operator = function(x, op){
  # operators in formulas

  if(length(x) <= 1){
    res = FALSE
  } else {
    res = as.character(x[[1]]) %in% op
  }

  res
}

# fml = x ~ a + b + c*d + I(e + f)
fml_extract_elements = function(fml){

  stopifnot(inherits(fml, "formula"))

  rhs = fml[[3]]

  main_elements = list()
  positive = logical(0)

  is_plus = is_operator(rhs, "+")
  while(is_plus || is_operator(rhs, "-")){
    main_elements = append(main_elements, rhs[[3]])
    positive = append(positive, is_plus)
    rhs = rhs[[2]]
    is_plus = is_operator(rhs, "+")
  }
  main_elements = append(main_elements, rhs)

  # we clean the I()
  for(i in seq_along(main_elements)){
    elem = main_elements[[i]]
    if(is_operator(elem, "I")){
      main_elements[[i]] = elem[[2]]
    }
  }

  res = rev(main_elements)

  res
}




# x = "x = 1:5; dsb(Hi .[' 'c ! .[letters[x]].[x[]]])"
dsb2curb = function(x){
  # transforms help written for dsb into help for cub

  # function name
  res = gsub("dsb", "cub", x)

  # opening
  res = gsub(".[", "{", res, fixed = TRUE)

  # closing (more difficult)
  res = gsub("\\[([^]]*)\\]", "_OPEN_\\1_CLOSE_", res)

  res = gsub("]", "}", res, fixed = TRUE)

  res = gsub("_OPEN_", "[", res, fixed = TRUE)
  res = gsub("_CLOSE_", "]", res, fixed = TRUE)

  res
}

insert = function(x, y, i, replace = FALSE){
  # x = list(a = 1, b = 2, c = 3) ; y = list(u = 33, v = 55) ; i = 2 ; insert(x, y, i)
  # we insert y into x in location i
  # we don't lose the names!
  # replace: whether to erase the value at the index

  mode = mode(x)
  if(!mode %in% c("list", "numeric", "logical", "character", "integer")){
      stop("Internal error: the current mode (", mode, ") is node supported in insert().")
  }

  n_x = length(x)

  if(n_x == 0){
      return(y)
  }

  n_y = length(y)
  res = vector(mode, n_x + n_y - replace)

  names_x = names(x)
  if(is.null(names_x)) names_x = character(n_x)
  names_y = names(y)
  if(is.null(names_y)) names_y = character(n_y)

  if(replace && i <= n_x && length(x) == 1){
    # edge case of replacement
    res = setNames(y, names_y)
    return(res)
  }

  if(i > n_x){
      res[1:n_x] = x
      res[n_x + 1:n_y] = y
      names(res) = c(names_x, names_y)

  } else if(i == 1){
      res[1:n_y] = y
      if(replace){
        res[n_y + 2:n_x] = x
        names(res) = c(names_y, names_x[-1])
      } else {
        res[n_y + 1:n_x] = x
        names(res) = c(names_y, names_x)
      }      

  } else {
      res[1:(i-1)] = x[1:(i-1)]
      res[(i-1) + 1:n_y] = y
      if(replace){
        if(i == n_x){
          names(res) = c(names_x[1:(i-1)], names_y)
        } else {
          res[(i+n_y):(n_x + n_y - 1)] = x[(i + 1):n_x]
          names(res) = c(names_x[1:(i-1)], names_y, names_x[(i + 1):n_x])
        }
      } else {
        res[(i+n_y):(n_x + n_y)] = x[i:n_x]
        names(res) = c(names_x[1:(i-1)], names_y, names_x[i:n_x])
      }
  }

  return(res)
}

insert_at = function(x, y, index){
  # x = list(a = 1, b = 2, c = 3) ; y = list(u = 33, v = 55) ; i = 2 ; insert(x, y, i)
  # we insert y into x in location i
  # we don't lose the names!

  if(length(y) != length(index)){
    stop("Internal error: the length of the object to be inserted must match the length of the indexes.")
  }

  if(max(index) > length(x)){
    stop("Internal error: the value of the index cannot be greater than the length of the object in which ot insert.")
  }

  mode = mode(x)
  if(!mode %in% c("list", "numeric", "logical", "character", "integer")){
      stop("Internal error: the current mode (", mode, ") is node supported in insert().")
  }

  n_x = length(x)
  n_y = length(y)
  res = vector(mode, n_x + n_y)
  names_res = character(n_x + n_y)

  names_x = names(x)
  if(is.null(names_x)) names_x = character(n_x)
  names_y = names(y)
  if(is.null(names_y)) names_y = character(n_y)

  x_index = 1:n_x
  is_insert = +(x_index %in% index)
  x_index = x_index + c(0, cumsum(is_insert)[seq_len(n_x - 1)])

  res[x_index] = x
  names_res[x_index] = names_x

  y_index = index + cumsum(rep(1, n_y))
  res[y_index] = y
  names_res[y_index] = names_y
  
  names(res) = names_res

  return(res)
}


convert_to_list = function(x){
  if(is.list(x)){
    res = unclass(x)
  } else if(is.matrix(x)){
    res = vector("list", ncol(x))
    for(i in 1:ncol(x)){
      res[[i]] = x[, i, drop = TRUE]
    }
    names(res) = colnames(x)
  } else if(is.vector(x)){
    res = as.list(x)
  } else {
    stop_up("Internal error: the current format ({bq, enum ? class(x)}) could not be converted to a list.")
  }

  res
}


uniquify = function(x){
  x_int = to_integer(x)
  n = length(x)

  if(n == 0){
    return(x)
  }

  if(max(x_int) == n){
    return(x)
  }

  suffix = character(n)
  x_tab = tabulate(x_int)
  i_multiple = which(x_tab > 1)
  for(i in i_multiple){
    n_digits = ceiling(log10(x_tab[i] + 0.1))
    suffix[x_int == i] = sprintf("%0*i", n_digits, 1:x_tab[i])
  }

  res = paste0(x, suffix)
  res
}


apply_star_operation = function(x, all_operations){
  # only a few operations can be applied

  all_op = check_set_options(all_operations, c("fix", "lower"), free = TRUE)
  txt = x

  for(op in all_op){
    if(op == "fix"){
      txt = gsub("[^[:alnum:]._]", "_", txt)
      txt = gsub("_+", "_", txt)
      txt = gsub("^[_.]+", ".", txt)
      txt = gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", txt)
      txt = tolower(txt)
    } else if(op == "lower"){
      txt = tolower(txt)
    } else {
      
      op = gsub("^'|'$", "", op)
      op = gsub("\\\\", "\\", op, fixed = TRUE)
      if(grepl(" => ", op, fixed = TRUE)){
        op_split = strsplit(op, " => ", fixed = TRUE)[[1]]
      } else {
        op_split = c(op, "")
      }

      txt = gsub(op_split[1], op_split[2], txt)
    }
  }

  txt
}

fill_the_placeholders = function(pattern, varnames){
  # the pattern contains placeholders
  # there are filled with 'names'
  # varnames can be either a list, either a vector

  if(!is.list(varnames)){
    varnames = list(varnames)
  }

  pattern_parsed_stars = cpp_parse_name_stars(pattern)

  is_star = pattern_parsed_stars$is_star
  text = pattern_parsed_stars$info

  new_names_list = vector("list", length(is_star))
  index_name = 0
  n_elements = length(is_star)
  for(j in 1:n_elements){
    if(!is_star[j]){
      new_names_list[[j]] = text[[j]]
    } else {
      index_name = index_name + 1
      new_names_list[[j]] = apply_star_operation(varnames[[index_name]], text[[j]])
    }
  }

  res = do.call(paste0, new_names_list)            
  
  res  
}


duplicated_xy = function(x, y){
  x_dup = duplicated(x)
  y_dup = duplicated(y)
  which(x_dup & y_dup)
}

eval_dt = function(call, data = list(), frame){
  
  is_data = TRUE
  if(missing(frame)){
    frame = data
    is_data = FALSE
  }

  dt_data = attr(frame, "dt_data")
  
  if(is_data){
    for(v in names(data)){
      dt_data[[v]] = data[[v]]
    }
  }

  if(".BY" %in% names(dt_data)){
    # we subset the data
    all_vars = all.vars(call)
    for(v in intersect(all_vars, names(dt_data))){
      value = dt_data[[v]]
      if(length(value) > 1){
        dt_data[[v]] = value[dt_data[[".I"]]]
      }
    }
  }

  eval(call, dt_data, frame)
}



# substitute to sprintf which does not handle char length properly
# slower but safer
simple_str_fill = function(x, n = NULL, symbol = " ", right = FALSE, center = FALSE){
  
  x_nc = nchar(x)
  if(is.null(n)){
    n = max(x_nc)
  }

  pattern = sprintf("% *s", n, "  ")
  if(symbol != " "){
    pattern = gsub(" ", symbol, pattern, fixed = TRUE)
  }
  
  i_add = which(x_nc < n)
  if(length(i_add)){
    x_add = x[i_add]
    extra = substr(rep(pattern, length(x_add)), 1, n - x_nc[i_add])
    if(center){
      nc_extra = nchar(extra)
      extra_left = substr(extra, 1, ceiling(nc_extra / 2))
      extra_right = substr(extra, ceiling(nc_extra / 2) + 1, nc_extra)
      x[i_add] = paste0(extra_left, x_add, extra_right)
      
    } else if(right){
      x[i_add] = paste0(extra, x_add)
    } else {
      x[i_add] = paste0(x_add, extra)
    }
    
  }
  
  x
}

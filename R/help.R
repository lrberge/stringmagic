#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-06-02
# ~: smagic help setup
#------------------------------------------------------------------------------#





setup_help_compact = function(){

  msg = c(
    "# Welcome to smagick compact help (also try smagick(help = TRUE) for more extensive help)",
    "Usage: smagick(s) with 's' a character string",
    " ",
    "# BASIC usage -------------------------|",
    "  smagick evaluates anything in '{}' and inserts it in 's'.",
    '  Ex: if x = "John", then smagick("Hi {x}!") -> "Hi John!"',
    " ",
    "# STRING OPERATIONS -------------------|",
    "  Each {} instance supports one or more string operations.",
    "  The syntax is {'arg'op.option ? x} or {'arg'op.option ! x}, with:",
    "    - 'arg' a quoted string used as argument (not all operators need arguments),",
    "    - op an operator code,",
    "    - option an option (not all operators have options),",
    "    - ? or !:",
    "      + ?: evaluates the expression x",
    "      + !: takes x as verbatim",
    "    - x an expression to be evaluated or some verbatim text (no quote needed).",
    '  Ex: smagick("{\' + \'c?1:3} = 6") -> "1 + 2 + 3 = 6". 1:3 is collapsed (c) with \' + \'.',
    "",
    "  Using ! instead of ? applies the operation to the *verbatim* of the expression.",
    '  Ex: smagick("{\': => 2\'r ! 1:3} = 6") -> "123 = 6".',
    "      In the string '1:3', ':' is replaced (r) with '2'.",
    "",
    "  Operations can be chained using comma-separation. The syntax is: {'s1'op1, 's2'op2?x}",
    "  Evaluations are from left to right.",
    '  Ex: smagick("{\': => 2\'r, \'\'s, \' + \'c!1:3} = 6") -> "1 + 2 + 3 = 6',
    "      1) '1:3'            -> ':' is replaced (r) with '2'  -> '123',",
    "      2) '123'            -> is split (s) with ''          -> c('1', '2', '3')",
    "      3) c('1', '2', '3') -> is collapsed (c) with ' + '   -> '1 + 2 + 3'",
    "",
    "  Nesting works, but only in verbatim components.",
    "  Ex: x = c(\"Doe\", \"Smith\")",
    "      smagick(\"Hi {' and 'c ! John {x}}\") -> \"Hi John Doe and John Smith\"",
    "",
    "  Operators have default values, so the quoted argument is optional.",
    '  Ex: smagick("{c ? 1:3}") -> "1 2 3". 1:3 is collapsed (c) with \' \', its default.',
    "",
    "# OPERATORS ---------------------------|",
    "  Below is a compact list of operators; their default arg. is in quotes, ",
    "  their options are in brackets",
    "",
    "  %, ascii[silent, utf8], bq, ' 'c, ', | and 'C, cfirst, clast, dsort, ",
    "  dtime[silent], each[c], enum[bq, q, Q, or, nor, 1, i, I, a, A, oxford], ", 
    "  erase, '[[:alnum:]]+'extract[first], fill[right, center], first, ",
    "  format[letter, upper, right, center], get, insert[right], is, ",
    "  k, K, last, len[letter, upper, format], lower, n[letter, upper, 0], ",
    "  nth[letter, upper, compact], ntimes[letter, upper], nuke, ",
    "  num[warn, soft, rm, clean], paste[right, front, back], q, Q, r, R, rev,", 
    "  rm[empty, blank, noalpha, noalnum, all], ' 's, ',[ \\t\\n]+'S, sort, stop, times[c], ", 
    "  title[force, ignore], trim[right, both], tws, unik, upper[first, sentence],",
    "  which, width, ws[punct, digit, isolated], x, X",
    "",
    "# CONDITIONS---------------------------|",
    "  Two condition operators: `if` and `vif`",
    "    - if(cond ; ops_true ; ops_false): ops_true = operations applied if TRUE",
    "    - vif(cond ; verb_true ; verb_false): verb_true = replacement text if TRUE ",
    "  The condition cond accepts the special values '.' (=the variable), ",
    "  '.len' (alias '.N') and `.nchar` (alias `.C`).",
    "",
    "# PLURALIZATION -----------------------|",
    "  There are two pluralization tags: `$` and `#`.",
    "  - use `$` to pluralize on the *length* of the variable",
    "  - use `#` to pluralize on the *value* of the variable",
    "  ex, length: x = c(\"Mark\", \"Francis\"); smagick(\"{$enum, is?x} here.\")",
    "  ex, value: n = 1; smagick(\"{n} file{#s, were} found.\")",
    "",
    "  When pluralizing you can perform the following operations:",
    "    - s, es: adds an 's' (or 'es') if it is plural",
    "    - y or ies: adds an 'y' if singular and 'ies' if plural",
    "    - enum: enumerates the elements (see help for the regular enum)",
    "    - (s1;s2): adds verbatim 's1' if singular and 's2' if plural",
    "    - (s1;s2;s3): adds verbatim 's1' if zero, 's2' if singular and 's3' if plural",
    "    - (s1;;s3): adds verbatim 's1' if zero, 's3' if singular or plural",
    "    - is, or any verb: conjugates the verb appropriately",
    "    - n, N: add the number of elements as a number (n) or in letters (N)",
    "  You can chain operations, in that case a whitespace is automatically added between them.",
    "  ex: x = sample(20, 5); smagick(\"The winning number{$s, is, enum ? sort(x)}.\")",
    "",
    "  You need not provide the value over which to pluralize if it has been used previously or will be used afterwards:",
    "  ex: x = \"Mara\"; smagick(\"I like {C ? x}, {$(she;they), is} my best friend{$s}.\")",
    "",
    "# SPECIALS ----------------------------|",
    "  Use '/' first to split the character with commas:",
    '  Ex: smagick("/x1, x2")            -> c("x1", "x2")',
    '      smagick("Hi {/David, Dora}!") -> c("Hi David!", "Hi Dora!")',
    "",
    "  In quoted arguments, use backticks to evaluate them from the frame.",
    '  Ex: n = 3 ; smagick("{`n`times.c!$}") -> "$$$". The \'$\' is replicated n times, then collapsed.'
  )

  options("smagick_help_compact" = msg)
}


generate_help_extensive = function(){
  
  if(!is_smagick_root()) return(NULL)
  
  
  mtime_origin = floor(as.numeric(file.info("R/smagick_main.R")["mtime"]))
  mtime_destination = readLines("R/AUTO_help.R", n = 1)
  mtime_destination = str_ops(mtime_destination, "x, num")
    
  if(mtime_origin <= mtime_destination){
    return(NULL)
  }  
  
  message("Help rewritten.")
  
  smagick_txt = readLines("R/smagick_main.R")
  
  i_smagick = str_which(smagick_txt, "^sma = smagick = f")
  doc = smagick_txt[1:(i_smagick - 1)]
  i_start_doc = max(str_which(doc, "!^#'")) + 1
  
  doc = doc[i_start_doc:length(doc)]
  
  # We select only the sections
  i_at = str_which(doc, "#' @")
  is_sec = str_is(doc[i_at], "#' @section")
  i_end = c(i_at[-1], length(doc)) - 1
  
  # we extract + format the section
  text = c()
  for(id_sec in which(is_sec)){
    my_sec = doc[i_at[id_sec]:i_end[id_sec]]
        
    title = str_ops(my_sec[1], "'.+@section'r, tws, ':$'r, '# 'paste, ' ----|'paste.right")
    content = str_ops(my_sec[-1], "'^#.'r, tws, '_ENDLINE_'paste.right, ''c")
    
    content = str_clean(content, 
                        "^(_ENDLINE_)+|(_ENDLINE_)+$",
                        "(_ENDLINE_){2,} => _NEWLINE__NEWLINE_",
                        "_ENDLINE_([+-]) => _NEWLINE_  \\1",
                        " *_ENDLINE_ * =>  ")
                        
    content = str_ops(content, "'_NEWLINE_'s")
    
    text = c(text, "", title, "", content)
  }
  
  # modifying the text to that it can be written and be interpreted as code
  text_dp = capture.output(dput(as.character(text)))
  text_dp[1] = paste0("txt = ", text_dp[1])
  
  fun_txt = c(paste0("# ", mtime_origin), 
              "# DO NOT EDIT BY HAND: generated with generate_help_extensive() in help.R",
              "",
              "setup_help_extensive = function(){",
              text_dp,
              "",
              "  options(smagick_help_extensive = txt)",
              "}")
  
  writeLines(fun_txt, "./R/AUTO_help.R")
}


format_help = function(pattern = NULL, x = NULL){
  # pattern = "replace"
  
  if(is.null(x)){
    x = getOption("smagick_help_extensive")
  }
  
  select = logical(length(x))

  for(i in seq_along(pattern)){
    p = pattern[i]

    qui = str_is(x, p, ignore.case = TRUE)

    if(!any(qui)){
      stop_up("In argument `help`, the pattern `", p, "` did not match any element of the documentation.")
    }

    select = select | qui
  }

  # We select the section
  # of each selected line

  line_id = which(select)
  section_id = str_which(x, "^# ")
  empty_id = which(x == "")

  start = end = c()
  for(i in seq_along(line_id)){
    line = line_id[i]
    start[i] = max(section_id[section_id <= line])
    if(line == length(x)){
      end[i] = line
    } else if(line %in% section_id){
      if(line == max(section_id)){
        end[i] = length(x)
      } else {
        next_section = min(section_id[section_id > line])
        end[i] = next_section - 1
      }
    } else {
      end[i] = min(empty_id[empty_id > line])
    }
  }

  info = cbind(start, end)
  info = info[order(start, -end), , drop = FALSE]
  info = info[!duplicated(info[, 1]), , drop = FALSE]

  text = c()
  for(i in 1:nrow(info)){
    text = c(text, x[info[i, 1]:info[i, 2]])
  }
  
  # we format the text
  text = fit_screen(paste(text, collapse = "\n"))
  text = strsplit(text, "\n")[[1]]
  
  # we format the sections
  if(any(str_is(text, "fixed/---|"))){
    qui_sec = str_is(text, "fixed/---|")
    text[qui_sec] = .sma("{80k ! {'f/|'r ? text[qui_sec]}{80 times.c ! -}}|")
  }
  
  # we highlight the selections
  highlights = list(line = NULL, value = NULL)
  for(i in seq_along(pattern)){
    p = pattern[i]
    qui = str_is(text, p, ignore.case = TRUE)
    
    pat_parsed = format_simple_regex_flags(p, ignore = TRUE)
    p = pat_parsed$pattern
    is_fixed = pat_parsed$fixed
    
    for(j in which(qui)){
      line = text[j]
      
      pos = which(highlights$line == j)
      line_exists = length(pos) > 0
      if(line_exists){
        line_new = highlights$value[pos]
      } else {
        line_new = gsub(".", " ", line)
      }
      
      info = gregexec(p, line, fixed = is_fixed, perl = !is_fixed)[[1]]
      
      start = as.numeric(info)
      len = attr(info, "match.length")
      
      replace = str_fill("", max(len), symbol = "^")
      
      for(k in seq_along(start)){
        substr(line_new, start[k], start[k] + len[k] - 1) = replace
      }
      
      if(line_exists){
        highlights$value[pos] = line_new
      } else {
        highlights$line = c(highlights$line, j)
        highlights$value = c(highlights$value, line_new)
      }      
    }
  }
  
  # now adding the highlights to the text
  hlines = highlights$line
  hvalues = highlights$value
  
  text[hlines] = paste0(text[hlines], "\n", hvalues)
  
  n = length(text)
  
  section_id = str_which(text, "^# ")
  empty_id = which(text == "")
  match_id = str_which(text, "(^| +)\\^+( +|$)")
  bullet_id = str_which(text, "^ *[+-]")
  
  msg = function(x, start = 1, len = 1){
    my_range = start:min(start + len - 1, length(text))
    cat(paste(text[my_range], collapse = "\n"))
  }
  
  # if small: we print the full text
  if(length(text) < 10){
    msg(text, 1, 500)
    return(invsible(NULL))
  }
  
  message(.sma("Welcome to smagick dynamic help:\n", 
               "enter = continue ; p = next paragraph ; n = next match ; N = next section ; q or Q = quit"))
  
  msg(text, 1, 5)
  i = 6
  
  while(i <= n){
    choice = readline(text[i])
    i = i + 1
    if(i >= n) break
    
    if(choice == ""){
      msg(text, i, 1)
      i = i + 1
    } else if(choice == "n"){
      # next match
      
      i_next = match_id[match_id > i]
      
      if(length(i_next) == 0){
        message("---> no additional match found")
        return(invisible(NULL))
      }
      
      message("---> jumping to the next match\n")
      
      # we add context
      i_min = i
      i = min(i_next)
      
      while(i >= i_min){
        if(text[i] == ""){
          # end
          i = i + 1
          break
        } else if(i %in% bullet_id){
          # end 
          break
        } else {
          i = i - 1
        }
      }
            
      msg(text, i, 1)
      i = i + 1
    } else if(choice %in% c("p", "N")){
      
      if(choice == "p"){
        # next paragraph
        i_next = min(empty_id[empty_id > i] + 1, bullet_id[bullet_id > i])
      } else {
        # next section
        i_next = section_id[section_id > i]
      }
      
      what = if(choice == "p") "paragraph" else "section"
      
      if(length(i_next) == 0){
        message("---> this was the last ", what, "\n")
        return(invisible(NULL))
      }
      i_next = min(i_next)
      
      message("---> jumping to next ", what, "\n")
      i = i_next
      
      msg(text, i, 1)
      i = i + 1      
    } else if(choice %in% c("Q", "q")){
      return(invisible(NULL))
    } else {
      message(.sma("Command {bq?choice} is not recognized, please whoose among: {S, bq, C ! enter, p, n, N, Q}"))
    }
  }
}

general_help = function(){
  
  message("Welcome to smagick help. Please choose which sections to read.")
  
  x = getOption("smagick_help_extensive")
  
  #
  # showing the titles
  #
  
  section_titles = str_ops(x, "'^#'get, '^#|-+\\|'r, tws")
  
  max_w = getOption("width") / 2 - 5
  
  section_titles = str_ops(section_titles, "`max_w`width, ':01:: 'paste, '\n => \n    'r")
  
  if(length(section_titles) %% 2 == 1){
    section_titles = c(section_titles, " ")
  }
    
  if(any(str_is(section_titles, "\n"))){
    no_nl = str_which(section_titles, "!\n")
    section_titles[no_nl] = paste0(section_titles[no_nl], "\n ")
    
    section_titles_split = unlist(strsplit(section_titles, "\n"))
    index = seq_along(section_titles_split)
    main_part = section_titles_split[index %% 2 == 1]
    extra_part = section_titles_split[index %% 2 == 0]
    index_extra = seq_along(extra_part)
    extra_part_1 = extra_part[index_extra %% 2 == 1]
    extra_part_2 = extra_part[index_extra %% 2 == 0]
    
    insert_index = index_extra[index_extra %% 2 == 0]
    tmp = insert_at(main_part, extra_part_1, insert_index)
    section_titles = insert_at(tmp, extra_part_2, insert_index + seq_along(extra_part_1))
  } 
  
  index = seq_along(section_titles)
  titles_odd = section_titles[index %% 2 == 1] 
  titles_even = section_titles[index %% 2 == 0]
  
  mat_titles = cbind(str_fill(titles_odd), str_fill(titles_even))
  is_empty = str_is(titles_odd, "^\\s*$") & str_is(titles_even, "^\\s*$")
  
  mat_titles = mat_titles[!is_empty, , drop = FALSE]
  
  mat_titles = apply(mat_titles, 1, paste, collapse = "  ")
  cat(mat_titles, sep = "\n")
  
  while(TRUE){
    choice = readline("Which to select (Q = quit)? (ex: 5, 11): ")
    if(choice %in% c("q", "Q")){
      return(invisible(NULL))
    }  
    
    choice_fmt = try(eval(str2lang(.sma("c({choice})"))), silent = TRUE)
    if(inherits(choice_fmt, "try-error")){
      message(fit_screen(.sma("Sorry the parsing failed, please provide a comma separated ", 
                              "list of numbers, like '1, 2, 6' for example.")))
    } else {
      break
    }
  }
  
  #
  # The help
  #
  
  section_choice = choice_fmt
  
  section_id = str_which(x, "^#")
  
  text = c()
  for(id in section_choice){
    i_start = section_id[id]
    i_end = if(id == max(section_id)) length(x) else min(section_id[section_id > i_start]) - 1
    text = c(text, x[i_start:i_end])
  }

  msg = function(x, start = 1, len = 1){
    my_range = start:min(start + len - 1, length(text))
    cat(paste(text[my_range], collapse = "\n"))
  }
  
  # we format the sections
  if(any(str_is(text, "fixed/---|"))){
    qui_sec = str_is(text, "fixed/---|")
    text[qui_sec] = .sma("{80k ! {'f/|'r ? text[qui_sec]}{80 times.c ! -}}|")
  }
  
  text_origin = text
  text = strsplit(fit_screen(paste0(text, collapse = "\n")), "\n")[[1]]
  
  # if small: we print the full text
  if(length(text) < 10){
    msg(text, 1, 500)
    return(invisible(NULL))
  }
  
  message("Available controls:")
  message("enter = continue ; p = next paragraph ; N = next section ; s = search a regex ; Q = quit")
  
  empty_id = which(text == "")
  section_id = str_which(text, "^#")
  bullet_id = str_which(text, "^ *[+-]")
  
  msg(text, 1, 5)
  
  n = length(text)
  i = 6 
  while(i < n){
    choice = readline(text[i])
    i = i + 1
    if(i == n) break
    
    if(choice == ""){
      msg(text, i, 1)
      i = i + 1
    } else if(choice %in% c("p", "N")){
      
      if(choice == "p"){
        # next paragraph
        i_next = min(empty_id[empty_id > i] + 1, bullet_id[bullet_id > i])
      } else {
        # next section
        i_next = section_id[section_id > i]
      }
      
      if(length(i_next) == 0){
        message("---> this was the last ", if(choice == "p") "paragraph" else "section", "\n")
        return(invisible(NULL))
      }
      i_next = min(i_next)
      
      message("---> jumping to next ", if(choice == "p") "paragraph" else "section", "\n")
      i = i_next
      
      msg(text, i, 1)
      i = i + 1      
    } else if(choice == "s"){
      message(fit_screen(.sma("Please write a regex pattern (flags `fixed` and `word` accepted,",
                              " ignore case is always on; ex: 'w/width'):")))
                   
      pattern = readline("")
      if(pattern %in% c("q", "Q")){
        return(invisible(NULL))
      }
      
      while(!any(str_is(text, pattern))){
        message(.sma("The pattern {bq?pattern} does not match any item. Please provide a new one:"))
        pattern = readline("")
        if(pattern %in% c("q", "Q")){
          return(invisible(NULL))
        }
      }
      
      format_help(pattern, text)
      return(invisible(NULL))
      
    } else if(choice %in% c("Q", "q")){
      return(invisible(NULL))
    } else {
      message(.sma("Command {bq?choice} is not recognized, please whoose among: {S, bq, C ! enter, p, s, N, Q}"))
    }
    
  }
  
}

smagick_dynamic_help = function(help){
  if(isTRUE(help)){
    general_help()
  } else {
    format_help(help)
  }
}

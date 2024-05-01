#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-06-02
# ~: string_magic help setup
#------------------------------------------------------------------------------#
  



generate_help_extensive = function(){
  
  if(!is_string_magic_root()) return(NULL)
  
  if(!identical(parent.frame(), .GlobalEnv) && 
         !identical(deparse(sys.calls()[[1]]), "devtools::load_all()")){
    return(NULL)
  }
  
  mtime_origin = floor(as.numeric(file.info("R/string_magic_doc.R")["mtime"]))
  mtime_destination = readLines("R/AUTO_help.R", n = 1)
  mtime_destination = as.numeric(string_trim(mtime_destination, 2))
    
  if(mtime_origin <= mtime_destination){
    return(NULL)
  }
  
  # we check if browser is used
  for(f in list.files("R", full.names = TRUE)){
    if(grepl("help.R", f, fixed = TRUE)){
      next
    }
    txt = readLines(f)
    if(any(grepl("browser()", txt, fixed = TRUE))){
      return(NULL)
    }
  }
  
  message("Help rewritten.")
  
  string_magic_txt = readLines("R/string_magic_doc.R")
  
  i_string_magic = string_which(string_magic_txt, "^NULL")
  doc = string_magic_txt[1:(i_string_magic - 1)]
  i_start_doc = max(string_which(doc, "!^#'")) + 1
  
  doc = doc[i_start_doc:length(doc)]
  
  # We select only the sections
  i_at = string_which(doc, "#' @")
  is_sec = string_is(doc[i_at], "#' @section")
  i_end = c(i_at[-1], length(doc)) - 1
  
  # we extract + format the section
  text = c()
  for(id_sec in which(is_sec)){
    my_sec = doc[i_at[id_sec]:i_end[id_sec]]
        
    title = string_ops(my_sec[1], "'.+@section'r, tws, ':$'r, '# 'paste, ' ----|'paste.right")
    content = string_ops(my_sec[-1], "'^#.'r, tws, '_ENDLINE_'paste.right, ''c")
    
    content = string_clean(content, 
                        "^(_ENDLINE_)+|(_ENDLINE_)+$",
                        "(_ENDLINE_){2,} => _NEWLINE__NEWLINE_",
                        "_ENDLINE_([+-]) => _NEWLINE_  \\1",
                        " *_ENDLINE_ * =>  ")
                        
    content = string_ops(content, "'_NEWLINE_'s")
    
    text = c(text, "", title, "", content)
  }
  
  # modifying the text so that it can be written and be interpreted as code
  text_dp = capture.output(dput(as.character(text)))
  text_dp[1] = paste0("txt = ", text_dp[1])
  
  fun_txt = c(paste0("# ", mtime_origin), 
              "# DO NOT EDIT BY HAND: generated with generate_help_extensive() in help.R",
              "",
              "setup_help_extensive = function(){",
              text_dp,
              "",
              "  options(string_magic_help_extensive = txt)",
              "}")
  
  writeLines(fun_txt, "./R/AUTO_help.R")
}


format_help = function(pattern = NULL, x = NULL){
  # pattern = "replace"
  
  if(is.null(x)){
    x = getOption("string_magic_help_extensive")
  }
  
  select = logical(length(x))

  for(i in seq_along(pattern)){
    p = pattern[i]

    qui = string_is(x, p, ignore.case = TRUE)

    if(!any(qui)){
      stop_up("In argument `.help`, the pattern `", p, "` did not match any element of the documentation.")
    }

    select = select | qui
  }

  # We select the section
  # of each selected line

  line_id = which(select)
  section_id = string_which(x, "^# ")
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
  if(any(string_is(text, "fixed/---|"))){
    qui_sec = string_is(text, "fixed/---|")
    text[qui_sec] = .sma("{80k ! {'f/|'r ? text[qui_sec]}{80 times.c ! -}}|")
  }
  
  # we highlight the selections
  highlights = list(line = NULL, value = NULL)
  for(i in seq_along(pattern)){
    p = pattern[i]
    qui = string_is(text, p, ignore.case = TRUE)
    
    pat_parsed = format_simple_regex_flags(p, ignore = TRUE)
    p = pat_parsed$pattern
    is_fixed  = pat_parsed$fixed
    
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
      
      replace = string_fill("", max(len), symbol = "^")
      
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
  
  section_id = string_which(text, "^# ")
  empty_id = which(text == "")
  match_id = string_which(text, "(^| +)\\^+( +|$)")
  bullet_id = string_which(text, "^ *[+-]")
  
  msg = function(x, start = 1, len = 1){
    my_range = start:min(start + len - 1, length(text))
    cat(paste(text[my_range], collapse = "\n"))
  }
  
  # if small: we print the full text
  if(length(text) < 10){
    msg(text, 1, 500)
    return(invisible(NULL))
  }
  
  message(.sma("Welcome to string_magic dynamic help:\n", 
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
  
  message("Welcome to string_magic help. Please choose which sections to read.")
  
  x = getOption("string_magic_help_extensive")
  
  #
  # showing the titles
  #
  
  section_titles = string_ops(x, "'^#'get, '^#|-+\\|'r, tws")
  
  max_w = getOption("width") / 2 - 5
  
  section_titles = string_ops(section_titles, "`max_w`width, ':01:: 'paste, '\n => \n    'r")
  n_sec = length(section_titles)
  
  if(length(section_titles) %% 2 == 1){
    section_titles = c(section_titles, " ")
  }
    
  if(any(string_is(section_titles, "\n"))){
    no_nl = string_which(section_titles, "!\n")
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
  
  mat_titles = cbind(string_fill(titles_odd), string_fill(titles_even))
  is_empty = string_is(titles_odd, "^\\s*$") & string_is(titles_even, "^\\s*$")
  
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
    } else if(any(!choice_fmt %in% 1:n_sec)){
      message(fit_screen(.sma("All numbers must be in between 1 and {n_sec}.")))
    } else {
      break
    }
  }
  
  #
  # The help
  #
  
  section_choice = choice_fmt
  
  section_id = string_which(x, "^#")
  text = c()
  for(id in section_choice){
    i_start = section_id[id]
    i_end = if(i_start == max(section_id)) length(x) else min(section_id[section_id > i_start]) - 1
    text = c(text, x[i_start:i_end])
  }

  msg = function(x, start = 1, len = 1){
    my_range = start:min(start + len - 1, length(text))
    cat(paste(text[my_range], collapse = "\n"))
  }
  
  # we format the sections
  if(any(string_is(text, "fixed/---|"))){
    qui_sec = string_is(text, "fixed/---|")
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
  section_id = string_which(text, "^#")
  bullet_id = string_which(text, "^ *[+-]")
  
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
      
      while(!any(string_is(text, pattern))){
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

string_magic_dynamic_help = function(help){
  if(isTRUE(help)){
    general_help()
  } else {
    format_help(help)
  }
}

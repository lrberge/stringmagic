#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-05-17
# ~: test for auxilliary tools
#------------------------------------------------------------------------------#

chunk("tools")


####
#### string_ops ####
####

cars = row.names(mtcars)
val = string_ops(cars, "'\\d+'x, rm, unik, num, dsort, 3 first")
test(val, c(914, 710, 450))

val = string_ops(cars, "'\\d+'x, rm", "unik, num", "dsort, 3 first")
test(val, c(914, 710, 450))

all_months = month.name[airquality$Month]
val = string_ops(all_months, "lower", "5 firstchar", pre_unik = TRUE)
val_bis = tolower(substr(all_months, 1, 5))
test(val, val_bis)

####
#### string_is ####
####


x = string_vec("one, two, one... two, microphone, check")

val = string_is(x, "^...$")
test(val, c(TRUE, TRUE, FALSE, FALSE, FALSE))


val = string_is(x, "f/...")
test(val, c(FALSE, FALSE, TRUE, FALSE, FALSE))

val = string_is(x, "...", fixed = TRUE)
test(val, c(FALSE, FALSE, TRUE, FALSE, FALSE))


val = string_is(x, "f/!...")
test(val, c(TRUE, TRUE, FALSE, TRUE, TRUE))

val = string_is(x, "!...", fixed = TRUE)
test(val, c(TRUE, TRUE, FALSE, TRUE, TRUE))


val = string_is(x, "iw/one")
test(val, c(TRUE, FALSE, TRUE, FALSE, FALSE))

val = string_is(x, "one", ignore.case = TRUE, word = TRUE)
test(val, c(TRUE, FALSE, TRUE, FALSE, FALSE))


val = string_is(x, "iw/!one")
test(val, c(FALSE, TRUE, FALSE, TRUE, TRUE))

val = string_is(x, "!one", ignore.case = TRUE, word = TRUE)
test(val, c(FALSE, TRUE, FALSE, TRUE, TRUE))


val = string_is(x, "i/one & .{5,}")
test(val, c(FALSE, FALSE, TRUE, TRUE, FALSE))


val = string_is(x, "one", "c", fixed = TRUE)
test(val, c(FALSE, FALSE, FALSE, TRUE, FALSE))

val = string_is(x, "one & c", fixed = TRUE)
test(val, c(FALSE, FALSE, FALSE, TRUE, FALSE))

val = string_is(x, "one | c", fixed = TRUE)
test(val, c(TRUE, FALSE, TRUE, TRUE, TRUE))

# magic
my_words = "one, two"
val = string_is(x, "wm/{my_words}", fixed = TRUE)
test(val, c(TRUE, TRUE, TRUE, FALSE, FALSE))

# escaping
x = string_vec("hey!, /twitter, !##!, a & b, c | d")

val = string_which(x, "\\/t")
test(val, 2)

val = string_which(x, "!\\/")
test(val, c(1, 3:5))

val = string_which(x, "\\!")
test(val, c(1, 3))

val = string_which(x, "a \\& b")
test(val, 4)

val = string_which(x, "f/twi |  \\|  | \\!")
test(val, c(1:3, 5))

# other
test(string_is("bonjour", "fixed/---|"), FALSE)



####
#### string_get ####
####


x = rownames(mtcars)

val = string_get(x, "Mazda")
test(val, c("Mazda RX4", "Mazda RX4 Wag"))

val = string_get(x, "!\\d & u")
test(val, c("Hornet Sportabout", "Lotus Europa"))

val = string_get(x, "Mazda", "Volvo", seq = TRUE)
test(val, c("Mazda RX4", "Mazda RX4 Wag", "Volvo 142E"))

val = string_get(x, "volvo", "mazda", seq = TRUE, ignore.case = TRUE)
test(val, c("Volvo 142E", "Mazda RX4", "Mazda RX4 Wag"))

cars = string_clean(rownames(mtcars)," .+")
val = string_get(cars, "i/^h", "i/^m", seq = TRUE)
test(val, c("Hornet", "Hornet", "Honda", "Mazda", 
            "Mazda", "Merc", "Merc", 
            "Merc", "Merc", "Merc", "Merc", "Merc", "Maserati"))

val = string_get(cars, "i/^h", "i/^m", seq.unik = TRUE)
test(val, c("Hornet", "Honda", "Mazda", "Merc", "Maserati"))

####
#### string_clean ####
####


x = c("hello world  ", "it's 5 am....")

val = string_clean(x, "o", "f/.")
test(val, c("hell wrld  ", "it's 5 am"))

val = string_clean(x, "f/o, .")
test(val, c("hell wrld  ", "it's 5 am"))

val = string_clean(x, "@ws")
test(val, c("hello world", "it's 5 am...."))

val = string_clean(x, "o(?= )", "@ws.p.i")
test(val, c("hell world", "it am"))

# replacement
val = string_clean(x, "o => a", "f/. => !")
test(val, c("hella warld  ", "it's 5 am!!!!"))

val = string_clean(x, "w/hello, it => _")
test(val, c("_ world  ", "_'s 5 am...."))

# total + string_ops
val = string_clean(x, "it/Hell => Heaven", "@'f/...'rm")
test(val, "Heaven")

# single
val = string_clean(x, "single/[aeiou] => _")
test(val, c("h_llo world  ", "_t's 5 am...."))

# magic
vowels = "aeiou"
val = string_clean(x, "m/[{vowels}] => _")
test(val, c("h_ll_ w_rld  ", "_t's 5 _m...."))

val = string_clean("bonjour les gens", "m/[{vowels}]{2,} => _")
test(val, "bonj_r les gens")

vowels_split = strsplit(vowels, "")[[1]]
val = string_clean("bonjour les gens", "m/{'|'c?vowels_split} => _")
test(val, "b_nj__r l_s g_ns")

####
#### string_split2df ####
####


x = c("Nor rain, wind", "When my information")
id = c("ws", "jmk")

val = string_split2df(x, "[[:punct:] ]+")
test(val$x, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))

val = string_split2df(x, "[[:punct:] ]+", id = id)
test(val$x, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))
test(val$id, c("ws", "ws", "ws", "jmk", "jmk", "jmk"))

base = data.frame(text = x, my_id = id)
val = string_split2df(text ~ my_id, base, "[[:punct:] ]+")
test(val$text, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))
test(val$my_id, c("ws", "ws", "ws", "jmk", "jmk", "jmk"))

base = data.frame(text = x, my_id = id)
val = string_split2df(text ~ my_id, base, "[[:punct:] ]+", add.pos = TRUE)
test(val$pos, c(1L, 2L, 3L, 1L, 2L, 3L))

x = c("One two, one two", "Microphone check")
val = string_split2df(x, "w/one")
test(val$x, c("One two, ", " two", "Microphone check"))

####
#### string_fill ####
####

x = c("bon", "bonjour les gens")
txt = string_fill(x)
test(txt, c("bon             ", "bonjour les gens"))

txt = string_fill(x, right = TRUE)
test(txt, c("             bon", "bonjour les gens"))

txt = string_fill(x, center = TRUE)
test(txt, "       bon      ", "bonjour les gens")

x = c(5, 15)
txt = string_fill(x, n = 3, right = TRUE)
test(txt, c("  5", " 15"))

txt = string_fill(x, n = 1, right = TRUE)
test(txt, c("5", "15"))

txt = string_fill(x, n = 3, right = TRUE, symbol = "0")
test(txt, c("005", "015"))

x = c("  hey  ", "de bas en haut")
txt = string_fill(x)

#
# string_vec ####
#

# regular
txt = string_vec("bon, jour, les gens, 
          je suis, la bas")
test(txt, c("bon", "jour", "les gens", "je suis", "la bas"))

# with nesting
y = c("Ana", "Charles")
z = c("Romeo Montaigu", "Juliette Capulet")
txt = string_vec("Jules, {y}, Francis, {'\\s+'S, ~(firstchar, ''c) ? z}")
test(txt, c("Jules", "Ana", "Charles", "Francis", "RM", "JC"))

# with/without variable protection
x = "x{{1:2}}, xx"
txt = string_vec(x, "y{{1:3}}", .delim = "{{ }}")
test(txt, c("x{{1:2}}, xx", "y1", "y2", "y3"))

txt = string_vec(x, "y{{1:3}}", .delim = "{{ }}", .protect.vars = FALSE)
test(txt, c("x1", "x2", "xx", "y1", "y2", "y3"))

txt = string_vec(x, "y{{1:3}}", .delim = "{{ }}", .protect.vars = FALSE, .split = FALSE)
test(txt, c("x1, xx", "x2, xx", "y1", "y2", "y3"))

# split
txt = string_vec("x1,x2\\,x3,   x4")
test(txt, c("x1", "x2,x3", "x4"))

txt = string_vec("x1;x2\\;x3;   x4", .split = ";")
test(txt, c("x1", "x2;x3", "x4"))

# matrix
mat = string_vec("1, 2, 3",
                 "4, 9, 9", 
                 "1, 2, 3", 
                 "7, 7, 9", .nmat = TRUE)
test(mat, structure(c(1, 4, 1, 7, 2, 9, 2, 7, 3, 9, 3, 9), dim = 4:3))

mat = string_vec("1, 2, 3, \n 4, 9, 9, \n 1, 2, 3", .nmat = TRUE)
test(mat, structure(c(1, 4, 1, 2, 9, 2, 3, 9, 3), dim = c(3L, 3L)))

mat = string_vec("1, 2, 3, 4, 9, 9, 1, 2, 3", .nmat = 3)
test(mat, structure(c(1, 4, 1, 2, 9, 2, 3, 9, 3), dim = c(3L, 3L)))

mat = string_vec("1, 2, 3, 4, 9, 9, 1, 2", .nmat = 2i)
test(mat, structure(c(1, 3, 9, 1, 2, 4, 9, 2), dim = c(4L, 2L)))

mat = string_vec("1, 2, 3 \n 4, 9, 9 \n 1, 2, 3", .nmat = TRUE, .last = "'\n'S")
test(mat, structure(c(1, 4, 1, 2, 9, 2, 3, 9, 3), dim = c(3L, 3L)))

mat = string_vec("1, 2, 3", .nmat = 2 + 3i, .last = "'\n'S")
test(mat, structure(c(1, 1, 2, 2, 3, 3), dim = 2:3))

mat = string_vec("1, john,\n 3, marie,\n 5, harry", .cmat = TRUE)
test(mat, structure(c("1", "3", "5", "john", "marie", "harry"), dim = 3:2))

df = string_vec("1, john,\n 3, marie,\n 5, harry", .df = TRUE)
test(df, data.frame(V1 = c(1, 3, 5), V2 = c("john", "marie", "harry")))

df = string_vec("1, john,\n 3, marie,\n 5, harry", .df = "id, name")
test(df, data.frame(id = c(1, 3, 5), name = c("john", "marie", "harry")))
test(names(df), c("id", "name"))

df = string_vec("1, john,\n 3, marie,\n 5, harry", .df = c("id", "name"))
test(names(df), c("id", "name"))

#
# paste_conditional ####
#

id = rep(1:2, each = 13)
x = paste_conditional(letters, id, "", names = FALSE)
test(x, c("abcdefghijklm", "nopqrstuvwxyz"))

x = paste_conditional(letters, 3 - id, "", names = FALSE)
test(x, c("nopqrstuvwxyz", "abcdefghijklm"))

base_cars = within(mtcars, carname <- row.names(mtcars))
base_cars = head(base_cars)
cars = paste_conditional(carname ~ gear + carb, base_cars, sep = ", ")
test(cars, c("Hornet 4 Drive, Valiant", "Hornet Sportabout", "Datsun 710", "Mazda RX4, Mazda RX4 Wag"))

test(names(cars), c("gear: 3, carb: 1", "gear: 3, carb: 2", "gear: 4, carb: 1", "gear: 4, carb: 4"))


####
#### string_extract ####
####

cars = row.names(mtcars)
val = string_extract(cars, "s/^\\w+")
val_bis = gsub(" .+", "", cars)
test(val, val_bis)

val = unlist(string_extract(cars, "^\\w+"))
test(val, val_bis)

val = string_extract(cars, "^\\w+", unlist = TRUE)
test(val, val_bis)

pat = "\\w+"
val = string_extract(cars, "m/^{pat}", unlist = TRUE)
test(val, val_bis)


####
#### string_split ####
####

time = "This is the year 2024."

val = string_split(time, " ")
test(val, c("This", "is", "the", "year", "2024."))

val = string_split(time, " ", simplify = FALSE)
test(val, list(c("This", "is", "the", "year", "2024.")))

val = string_split(time, "is")
test(val, c("Th", " ", " the year 2024."))

val = string_split(time, "w/is")
test(val, c("This ", " the year 2024."))

pat = "is"
val = string_split(time, "wm/{pat}")
test(val, c("This ", " the year 2024."))


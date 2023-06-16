#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-05-17
# ~: test for auxilliary tools
#------------------------------------------------------------------------------#




####
#### str_is ####
####


x = dsb("/one, two, one... two, microphone, check")

val = str_is(x, "^...$")
test(val, c(TRUE, TRUE, FALSE, FALSE, FALSE))


val = str_is(x, "f/...")
test(val, c(FALSE, FALSE, TRUE, FALSE, FALSE))

val = str_is(x, "...", fixed = TRUE)
test(val, c(FALSE, FALSE, TRUE, FALSE, FALSE))


val = str_is(x, "!f/...")
test(val, c(TRUE, TRUE, FALSE, TRUE, TRUE))

val = str_is(x, "!...", fixed = TRUE)
test(val, c(TRUE, TRUE, FALSE, TRUE, TRUE))


val = str_is(x, "iw/one")
test(val, c(TRUE, FALSE, TRUE, FALSE, FALSE))

val = str_is(x, "one", ignore.case = TRUE, word = TRUE)
test(val, c(TRUE, FALSE, TRUE, FALSE, FALSE))


val = str_is(x, "!iw/one")
test(val, c(FALSE, TRUE, FALSE, TRUE, TRUE))

val = str_is(x, "!one", ignore.case = TRUE, word = TRUE)
test(val, c(FALSE, TRUE, FALSE, TRUE, TRUE))


val = str_is(x, "i/one & .{5,}")
test(val, c(FALSE, FALSE, TRUE, TRUE, FALSE))


val = str_is(x, "one", "c", fixed = TRUE)
test(val, c(FALSE, FALSE, FALSE, TRUE, FALSE))

val = str_is(x, "one & c", fixed = TRUE)
test(val, c(FALSE, FALSE, FALSE, TRUE, FALSE))

val = str_is(x, "one | c", fixed = TRUE)
test(val, c(TRUE, FALSE, TRUE, TRUE, TRUE))

# escaping
x = dsb("/hey!, /twitter, !##!, a & b, c | d")

val = str_which(x, "\\/t")
test(val, 2)

val = str_which(x, "//t")
test(val, 2)

val = str_which(x, "!\\/")
test(val, c(1, 3:5))

val = str_which(x, "\\!")
test(val, c(1, 3))

val = str_which(x, "a \\& b")
test(val, 4)

val = str_which(x, "f/twi |  \\|  | \\!")
test(val, c(1:3, 5))

val = str_which(x, "!f/!! & e")
test(val, c(1, 3:5))

# other
test(str_is("bonjour", "fixed/---|"), FALSE)



####
#### str_get ####
####


x = rownames(mtcars)

val = str_get(x, "Mazda")
test(val, c("Mazda RX4", "Mazda RX4 Wag"))

val = str_get(x, "!\\d & u")
test(val, c("Hornet Sportabout", "Lotus Europa"))

val = str_get(x, "Mazda", "Volvo", seq = TRUE)
test(val, c("Mazda RX4", "Mazda RX4 Wag", "Volvo 142E"))

val = str_get(x, "volvo", "mazda", seq = TRUE, ignore.case = TRUE)
test(val, c("Volvo 142E", "Mazda RX4", "Mazda RX4 Wag"))

cars = str_clean(rownames(mtcars)," .+")
val = str_get(cars, "i/^h", "i/^m", seq = TRUE)
test(val, c("Hornet", "Hornet", "Honda", "Mazda", 
            "Mazda", "Merc", "Merc", 
            "Merc", "Merc", "Merc", "Merc", "Merc", "Maserati"))

val = str_get(cars, "i/^h", "i/^m", seq.unik = TRUE)
test(val, c("Hornet", "Honda", "Mazda", "Merc", "Maserati"))

####
#### str_clean ####
####

x = c("hello world  ", "it's 5 am....")

val = str_clean(x, "o", "f/.")
test(val, c("hell wrld  ", "it's 5 am"))

val = str_clean(x, "f/o, .")
test(val, c("hell wrld  ", "it's 5 am"))

val = str_clean(x, "@ws")
test(val, c("hello world", "it's 5 am...."))

val = str_clean(x, "o(?= )", "@ws.p.i")
test(val, c("hell world", "it am"))

# replacement
val = str_clean(x, "o => a", "f/. => !")
test(val, c("hella warld  ", "it's 5 am!!!!"))

val = str_clean(x, "w/hello, it => _")
test(val, c("_ world  ", "_'s 5 am...."))

# total + str_op
val = str_clean(x, "it/Hell => Heaven", "@'f/...'rm")
test(val, "Heaven")


####
#### str_split2df ####
####


x = c("Nor rain, wind", "When my information")
id = c("ws", "jmk")

val = str_split2df(x, "[[:punct:] ]+")
test(val$x, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))

val = str_split2df(x, "[[:punct:] ]+", id = id)
test(val$x, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))
test(val$id, c("ws", "ws", "ws", "jmk", "jmk", "jmk"))

base = data.frame(text = x, my_id = id)
val = str_split2df(text ~ my_id, base, "[[:punct:] ]+")
test(val$text, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))
test(val$my_id, c("ws", "ws", "ws", "jmk", "jmk", "jmk"))

base = data.frame(text = x, my_id = id)
val = str_split2df(text ~ my_id, base, "[[:punct:] ]+", add.pos = TRUE)
test(val$pos, c(1L, 2L, 3L, 1L, 2L, 3L))

x = c("One two, one two", "Microphone check")
val = str_split2df(x, "w/one")
test(val$x, c("One two, ", " two", "Microphone check"))

####
#### str_fill ####
####

x = c("bon", "bonjour les gens")
txt = str_fill(x)
test(txt, c("bon             ", "bonjour les gens"))

txt = str_fill(x, right = TRUE)
test(txt, c("             bon", "bonjour les gens"))

txt = str_fill(x, center = TRUE)
test(txt, "       bon      ", "bonjour les gens")

x = c(5, 15)
txt = str_fill(x, n = 3, right = TRUE)
test(txt, c("  5", " 15"))

txt = str_fill(x, n = 1, right = TRUE)
test(txt, c("5", "15"))

txt = str_fill(x, n = 3, right = TRUE, symbol = "0")
test(txt, c("005", "015"))

x = c("  hey  ", "de bas en haut")
txt = str_fill(x)

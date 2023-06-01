#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-05-17
# ~: test for auxilliary tools
#------------------------------------------------------------------------------#




####
#### sma_is ####
####


x = dsb("/one, two, one... two, microphone, check")

val = sma_is(x, "^...$")
test(val, c(TRUE, TRUE, FALSE, FALSE, FALSE))


val = sma_is(x, "f/...")
test(val, c(FALSE, FALSE, TRUE, FALSE, FALSE))

val = sma_is(x, "...", fixed = TRUE)
test(val, c(FALSE, FALSE, TRUE, FALSE, FALSE))


val = sma_is(x, "!f/...")
test(val, c(TRUE, TRUE, FALSE, TRUE, TRUE))

val = sma_is(x, "!...", fixed = TRUE)
test(val, c(TRUE, TRUE, FALSE, TRUE, TRUE))


val = sma_is(x, "iw/one")
test(val, c(TRUE, FALSE, TRUE, FALSE, FALSE))

val = sma_is(x, "one", ignore.case = TRUE, word = TRUE)
test(val, c(TRUE, FALSE, TRUE, FALSE, FALSE))


val = sma_is(x, "!iw/one")
test(val, c(FALSE, TRUE, FALSE, TRUE, TRUE))

val = sma_is(x, "!one", ignore.case = TRUE, word = TRUE)
test(val, c(FALSE, TRUE, FALSE, TRUE, TRUE))


val = sma_is(x, "i/one & .{5,}")
test(val, c(FALSE, FALSE, TRUE, TRUE, FALSE))


val = sma_is(x, "one", "c", fixed = TRUE)
test(val, c(FALSE, FALSE, FALSE, TRUE, FALSE))

val = sma_is(x, "one & c", fixed = TRUE)
test(val, c(FALSE, FALSE, FALSE, TRUE, FALSE))

val = sma_is(x, "one | c", fixed = TRUE)
test(val, c(TRUE, FALSE, TRUE, TRUE, TRUE))

# escaping
x = dsb("/hey!, /twitter, !##!")

val = sma_is(x, "\\/t")
test(val, c(FALSE, TRUE, FALSE))

val = sma_is(x, "!\\/")
test(val, c(TRUE, FALSE, TRUE))

val = sma_is(x, "\\!")
test(val, c(TRUE, FALSE, TRUE))


####
#### sma_get ####
####


x = rownames(mtcars)

val = sma_get(x, "Mazda")
test(val, c("Mazda RX4", "Mazda RX4 Wag"))

val = sma_get(x, "!\\d & u")
test(val, c("Hornet Sportabout", "Lotus Europa"))

val = sma_get(x, "Mazda", "Volvo", seq = TRUE)
test(val, c("Mazda RX4", "Mazda RX4 Wag", "Volvo 142E"))

val = sma_get(x, "volvo", "mazda", seq = TRUE, ignore.case = TRUE)
test(val, c("Volvo 142E", "Mazda RX4", "Mazda RX4 Wag"))

cars = sma_clean(rownames(mtcars)," .+")
val = sma_get(cars, "i/^h", "i/^m", seq = TRUE)
test(val, c("Hornet", "Hornet", "Honda", "Mazda", 
            "Mazda", "Merc", "Merc", 
            "Merc", "Merc", "Merc", "Merc", "Merc", "Maserati"))

val = sma_get(cars, "i/^h", "i/^m", seq.unik = TRUE)
test(val, c("Hornet", "Honda", "Mazda", "Merc", "Maserati"))

####
#### sma_clean ####
####

x = c("hello world  ", "it's 5 am....")

val = sma_clean(x, "o", "f/.")
test(val, c("hell wrld  ", "it's 5 am"))

val = sma_clean(x, "f/o, .")
test(val, c("hell wrld  ", "it's 5 am"))

val = sma_clean(x, "@ws")
test(val, c("hello world", "it's 5 am...."))

val = sma_clean(x, "o(?= )", "@ws.p.i")
test(val, c("hell world", "it am"))

# replacement
val = sma_clean(x, "o => a", "f/. => !")
test(val, c("hella warld  ", "it's 5 am!!!!"))

val = sma_clean(x, "w/hello, it => _")
test(val, c("_ world  ", "_'s 5 am...."))

# total + sma_op
val = sma_clean(x, "it/Hell => Heaven", "@'f/...'rm")
test(val, "Heaven")


####
#### sma_split2df ####
####


x = c("Nor rain, wind", "When my information")
id = c("ws", "jmk")

val = sma_split2df(x, "[[:punct:] ]+")
test(val$x, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))

val = sma_split2df(x, "[[:punct:] ]+", id = id)
test(val$x, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))
test(val$id, c("ws", "ws", "ws", "jmk", "jmk", "jmk"))

base = data.frame(text = x, my_id = id)
val = sma_split2df(text ~ my_id, base, "[[:punct:] ]+")
test(val$text, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))
test(val$my_id, c("ws", "ws", "ws", "jmk", "jmk", "jmk"))

base = data.frame(text = x, my_id = id)
val = sma_split2df(text ~ my_id, base, "[[:punct:] ]+", add.pos = TRUE)
test(val$pos, c(1L, 2L, 3L, 1L, 2L, 3L))

x = c("One two, one two", "Microphone check")
val = sma_split2df(x, "w/one")
test(val$x, c("One two, ", " two", "Microphone check"))

####
#### sma_fill ####
####

x = c("bon", "bonjour les gens")
txt = sma_fill(x)
test(txt, c("bon             ", "bonjour les gens"))

txt = sma_fill(x, right = TRUE)
test(txt, c("             bon", "bonjour les gens"))

txt = sma_fill(x, center = TRUE)
test(txt, "       bon      ", "bonjour les gens")

x = c(5, 15)
txt = sma_fill(x, n = 3, right = TRUE)
test(txt, c("  5", " 15"))

txt = sma_fill(x, n = 1, right = TRUE)
test(txt, c("5", "15"))

txt = sma_fill(x, n = 3, right = TRUE, symbol = "0")
test(txt, c("005", "015"))

x = c("  hey  ", "de bas en haut")
txt = sma_fill(x)

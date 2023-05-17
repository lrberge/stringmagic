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

val = str_is(x, "#...")
test(val, c(FALSE, FALSE, TRUE, FALSE, FALSE))

val = str_is(x, "!#...")
test(val, c(TRUE, TRUE, FALSE, TRUE, TRUE))

val = str_is(x, c("#one", "#c"))
test(val, c(FALSE, FALSE, FALSE, TRUE, FALSE))

val = str_is(x, c("#one", "|#c"))
test(val, c(TRUE, FALSE, TRUE, TRUE, TRUE))


# escaping

x = dsb("/hey!, #twitter, !##!")

val = str_is(x, "\\#")
test(val, c(FALSE, TRUE, TRUE))

val = str_is(x, "!\\#")
test(val, c(TRUE, FALSE, FALSE))

val = str_is(x, "\\!")
test(val, c(TRUE, FALSE, TRUE))

val = str_is(x, "\\!#")
test(val, c(FALSE, FALSE, TRUE))


####
#### str_get ####
####


x = rownames(mtcars)

val = str_get(x, "Mazda")
test(val, c("Mazda RX4", "Mazda RX4 Wag"))

val = str_get(x, c("!\\d", "u"))
test(val, c("Hornet Sportabout", "Lotus Europa"))

val = str_get(x, c("Mazda", "Volvo"), seq = TRUE)
test(val, c("Mazda RX4", "Mazda RX4 Wag", "Volvo 142E"))

val = str_get(x, c("Volvo", "Mazda"), seq = TRUE)
test(val, c("Volvo 142E", "Mazda RX4", "Mazda RX4 Wag"))


####
#### selvars ####
####


x = rownames(mtcars)

val = selvars(x, "^Maz")
test(val, c("Mazda RX4", "Mazda RX4 Wag")

val = selvars(x, "^Mer, !#450")
test(val, c("Merc 240D", "Merc 230", "Merc 280", "Merc 280C")

val = selvars(x, "^Maz, &!@\\d")
test(val, c("Mazda RX4", "Mazda RX4 Wag", "Hornet Sportabout", "Valiant",
            "Cadillac Fleetwood", "Lincoln Continental", "Chrysler Imperial",
            "Honda Civic", "Toyota Corolla", "Toyota Corona", "Dodge Challenger",
            "AMC Javelin", "Pontiac Firebird", "Lotus Europa", "Ford Pantera L",
            "Ferrari Dino", "Maserati Bora"))

val = selvars(x, "@\\d, !^Merc || ^Fiat, !^Maz")
test(val, c("Fiat 128", "Fiat X1-9", "Datsun 710", "Hornet 4 Drive", "Duster 360",
            "Camaro Z28", "Porsche 914-2", "Volvo 142E", "Mazda RX4", "Mazda RX4 Wag"))

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

####
#### str_clean ####
####

x = c("hello world  ", "it's 5 am....")

val = str_clean(x, c("o", "#."))
test(val, c("hell wrld  ", "it's 5 am"))

val = str_clean(x, "o|\\.")
test(val, c("hell wrld  ", "it's 5 am"))

val = str_clean(x, "@w")
test(val, c("hello world", "it's 5 am...."))

val = str_clean(x, c("o(?= )", "@wpi"))
test(val, c("hell world", "it am"))










################################################################################
# Author: Laurent R. Bergé
# Created: 2023-04-11
# ~: tests for the functions relating to quickselect
################################################################################


base = head(as.data.frame(t(mtcars)))

selvars(base, "^Volvo")

###
### simple selection ####
###


selvars(base, "^Toy")

selvars(base, "Mazda RX4, ^Toy")

selvars(base, "maz = Mazda RX4, car_* = ^Toy")

selvars(iris, "mean_*" = mean("^Petal"))

selvars(iris, mean("^Petal"))

selvars(base, volvo = "^Volvo")

selvars(head(iris), ".num")

selvars(iris, mean(".num"))

selvars(iris, "*" = head(".num"), species = head("^Spe"))

####
#### ranges ####
####

selvars(base, "5:7")

selvars(base, "7:5")

selvars(base, "-1:12")

selvars(base, "-1:-3")

selvars(base, "-1:-3")

selvars(base, "Mazda RX4:^Datsun")

selvars(base, "$450SE : 15")

selvars(base, "$450SE : 15 / #Fleet")

# errors
try(selvars(base, "$450SE : ."))

try(selvars(base, "$450SE : 150"))

try(selvars(base, "@mercedez"))






x = names(as.data.frame(base))
data = base
mcdi = "^Volvo"
dot_names = ""

selvars_main_selection(x, data, mcdi, dot_names)

all_vars = x
data = data
pattern = mcdi
dot_name = ""








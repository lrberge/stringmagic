#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Aug  3 09:40:02 2022
# ~: string.ops tests
#----------------------------------------------#

# Tests all string operations
# - should go through all the branches
# - I test the conditional operations inside the operators when relevant

chunk("smagic")

dsb = function(...) smagic(..., .delim = ".[ ]")

#
# collapse ####
#

# regular
x = 1:3
test(smagic("win = {'-'c ? x}"), "win = 1-2-3")

test(smagic("win = {c ? x}"), "win = 1 2 3")

# with last element
test(smagic("win = {C ? x}"), "win = 1, 2 and 3")

test(smagic("win = {'|/'c ? x}"), "win = 12/3")

# default behavior
test(smagic("win = {c ? x}"), "win = 1 2 3")

# empty collapse
test(smagic("win = {''c ? x}"), "win = 123")

# conditional collapse
x = string_vec("bonjour les gens., comment ca va?, bien?, bien.")
txt = smagic("{' 'S, ~(firstchar.3, '.'c) ? x}")
test(txt, c("bon.les.gen", "com.ca.va?", "bie", "bie"))

#
# split ####
#

x = "Cogito...Ergo...Sum"
txt = smagic("He said: {'f/...'s ? x}")
test(txt, c("He said: Cogito", "He said: Ergo", "He said: Sum"))

txt = smagic("He said: {'i/[cs]'s ? x}")
test(txt, c("He said: ", "He said: ogito...Ergo...", "He said: um"))



#
# replace ####
#

x = c("Blanche dit 'Bla bla blanc'.")
txt = smagic("She said: {'bla'r, ws ? x}")
test(txt, "She said: Blanche dit 'Bla nc'.")

txt = smagic("She said: {'bla'r.ignore, ws ? x}")
test(txt, "She said: nche dit ' nc'.")

txt = smagic("She said: {'i/bla'r, ws ? x}")
test(txt, "She said: nche dit ' nc'.")

txt = smagic("She said: {'bla'r.word, ws ? x}")
test(txt, "She said: Blanche dit 'Bla blanc'.")

txt = smagic("She said: {'w/bla'r, ws ? x}")
test(txt, "She said: Blanche dit 'Bla blanc'.")

txt = smagic("She said: {'bla'r.word.ignore, ws ? x}")
test(txt, "She said: Blanche dit ' blanc'.")

txt = smagic("She said: {'wi/bla'r, ws ? x}")
test(txt, "She said: Blanche dit ' blanc'.")

txt = smagic("She said: {'.'r.fixed, ws ? x}")
test(txt, "She said: Blanche dit 'Bla bla blanc'")

txt = smagic("She said: {'fixed/.'r, ws ? x}")
test(txt, "She said: Blanche dit 'Bla bla blanc'")

txt = smagic("She said: {'Bla, blanc => ??'r.word, ws ? x}")
test(txt, "She said: Blanche dit '?? bla ??'.")

txt = smagic("She said: {'word/Bla, blanc => ??'r, ws ? x}")
test(txt, "She said: Blanche dit '?? bla ??'.")

txt = smagic("She said: {'Bla, blanc => \\1\\1'r.w, ws ? x}")
test(txt, "She said: Blanche dit 'BlaBla bla blancblanc'.")

txt = smagic("She said: {'w/Bla, blanc => \\1\\1'r, ws ? x}")
test(txt, "She said: Blanche dit 'BlaBla bla blancblanc'.")

# total replacement
x = string_vec("jingle bells, jingle bells, jingle, all the way")
txt = smagic("{', 'c ! {'t/jing => [sound]'r ? x}}")
test(txt, "[sound], [sound], [sound], all the way")

txt = smagic("{', 'c ! {'jing => [sound]'r.total ? x}}")
test(txt, "[sound], [sound], [sound], all the way")

txt = smagic("{'(?<!\\b)e => a'replace.single ! Where is the letter e?}")
test(txt, "Whare is the letter e?")

# default
test(smagic("a = {r ? 1:5}"), "err")
test(smagic("a = {R ? 1:5}"), "err")

# other with replacement

x = c("[]", "Moon", "Sun")

txt = dsb("Stare at the .['f/[]'r ? x]")
test(txt, c("Stare at the ", "Stare at the Moon", "Stare at the Sun"))

txt = dsb("Stare at the .['f/[] => ...'r ? x]")
test(txt, c("Stare at the ...", "Stare at the Moon", "Stare at the Sun"))

txt = dsb("Stare at the .['[[:upper:]]'R ? x]")
test(txt, c("Stare at the []", "Stare at the oon", "Stare at the un"))

txt = dsb("Stare at the .['[[:upper:]] => P'R ? x]")
test(txt, c("Stare at the []", "Stare at the Poon", "Stare at the Pun"))


#
# x: single extraction ####
#

x = c("Blanche dit", " 'Bla bla blanc'.")
txt = smagic("mots: {'bla'x ? x}")
test(txt, c("mots: ", "mots: bla"))

test(smagic("mots: {'bla'x.ignore ? x}"), 
     c("mots: Bla", "mots: Bla"))
     
test(smagic("mots: {'i/bla'x ? x}"), 
     c("mots: Bla", "mots: Bla"))

test(smagic("mots: {'bla'x.i.w ? x}"), 
     c("mots: ", "mots: Bla"))

test(smagic("mots: {'iw/bla'x ? x}"), 
     c("mots: ", "mots: Bla"))

# default
txt = smagic("mots: {x ? x}")
test(txt, c("mots: Blanche", "mots: Bla"))

# alias
test(smagic("mots: {'iw/bla'extract.first ? x}"), 
     c("mots: ", "mots: Bla"))


#
# X: multiple extractions ####
#

x = c("Blanche dit", " 'Bla bla blanc'.")
txt = smagic("{'bla'X ? x}")
test(txt, c("bla", "bla"))

txt = smagic("{'bla'X.i ? x}")
test(txt, c("Bla", "Bla", "bla", "bla"))

txt = smagic("{'i/bla'X ? x}")
test(txt, c("Bla", "Bla", "bla", "bla"))

txt = smagic("{'bla'X.i.w ? x}")
test(txt, c("Bla", "bla"))

txt = smagic("{'ignore, word/bla'X ? x}")
test(txt, c("Bla", "bla"))

# conditional
x = string_vec("laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = smagic("info: {'\\d+'X, ~('-'c), C ? x}")
test(txt, "info: 57-26, 32-7, 29-8 and 55")

#
# is, get, which ####
#

x = c("Hi Mary.", "Hi Charles!", "Are you OK?", "I think so Charles.", "Great to hear, Mary!")
txt = smagic("A few quotes:\n{Q, 'mary | charles & \\!'get.ignore, '  'paste, '\n'c ? x}")
test(txt, "A few quotes:\n  \"Hi Charles!\"\n  \"Great to hear, Mary!\"")

txt = smagic("A few quotes:\n{Q, 'i/mary | charles & \\!'get, '  'paste, '\n'c ? x}")
test(txt, "A few quotes:\n  \"Hi Charles!\"\n  \"Great to hear, Mary!\"")

x = c("It's me again.", "What do you mean a gain?", "Yes, again!", "Ah AGAIN, I thought A GAIN!")
index = string_ops(x, "'gain'which.w.ig")
test(index, c(2, 4))

index = string_ops(x, "'gain & .'is.fixed")
test(index, c(TRUE, FALSE, FALSE, FALSE))

# conditional
x = string_vec("laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = smagic("info: {' 'S, '!\\d'get, ~('-'c), C ? x}")
test(txt, "info: laura-and, charles-and, charly-and and june")


#
# each/times ####
#

test(smagic("I like {5 times.c ! ?} marks!"), "I like ????? marks!")

x = c("mary", "richard")
y = c("yes", "no")
txt = smagic("The discussion: {', 'c ! {upper.first, 2 times ? x}: '{2 times ? y}'}...")
test(txt, "The discussion: Mary: 'yes', Richard: 'no', Mary: 'yes', Richard: 'no'...")

test(smagic("values: {2 each.c ? c('a', 'b')}"), "values: aabb")

#
# case ####
#

x = "where is bryan? Bryan is in the KITCHEN."

txt = dsb(".[lower ? x]")
test(txt, "where is bryan? bryan is in the kitchen.")

txt = dsb(".[upper.first ? x]")
test(txt, "Where is bryan? Bryan is in the KITCHEN.")

txt = dsb(".[upper ? x]")
test(txt, "WHERE IS BRYAN? BRYAN IS IN THE KITCHEN.")

txt = dsb(".[lower, upper.sentence ? x]")
test(txt, "Where is bryan? Bryan is in the kitchen.")

txt = dsb(".[title ? x]")
test(txt, "Where Is Bryan? Bryan Is In The KITCHEN.")

txt = dsb(".[title.force ? x]")
test(txt, "Where Is Bryan? Bryan Is In The Kitchen.")

txt = dsb(".[title.force.ignore ? x]")
test(txt, "Where Is Bryan? Bryan Is in the Kitchen.")

x = "results from a new estimator: a new hope"
txt = dsb(".[title.i ? x]")
test(txt, "Results From a New Estimator: A New Hope")

#
# quotes ####
#

x = "siren"
txt = smagic("Is it the song of the {q ? x}, of the {Q ? x} or of the {bq ? x}?")
test(txt, "Is it the song of the 'siren', of the \"siren\" or of the `siren`?")

#
# format ####
#

x = c(1, 123, 123456)
txt = smagic("The numbers are:\n{'\n'c ! - {format ? x} | {rev, Format ? x}}")
test(txt, "The numbers are:\n- 1       | 123,456\n- 123     |     123\n- 123,456 |       1")

txt = smagic("The numbers are:\n{'\n'c ! - {format.0 ? x} | {rev, Format.zero ? x}}")
test(txt, "The numbers are:\n- 0000001 | 123,456\n- 0000123 | 0000123\n- 123,456 | 0000001")

today = structure(19531, class = "Date")
txt = smagic("Today is {'%d, %m, %Y'for ? today}")
test(txt, "Today is 23, 06, 2023")

txt = smagic("Today is {'%d, %m, %Y'for ? .date}")
test_contains(txt, "\\d\\d, \\d\\d, \\d{4}")

txt = smagic("Today is {.now}")
test_contains(txt, "\\d{4} & \\d:\\d")

txt = smagic("We're {.now('%H:%M %Y')}")
test_contains(txt, "\\d:\\d & \\d{4}")

#
# sprintf ####
#

txt = smagic("pi = {%.3f ? pi}")
test(txt, "pi = 3.142")

txt = smagic("pi = {% 8.3f ? pi}")
test(txt, "pi =    3.142")

x = c("michael", "ana")
txt = smagic("The winners are:\n{'\n'c ! - {% 10s ? x}}")
test(txt, "The winners are:\n-    michael\n-        ana")

txt = smagic("The winners are:\n{'\n'c ! - {%- 10s ? x}!}")
test(txt, "The winners are:\n- michael   !\n- ana       !")

#
# ws ####
#

x = " &Hej! Welcome!! A\t\n 6-feet wide bed awaits!\t."

txt = dsb(".[ws ? x]")
test(txt, "&Hej! Welcome!! A 6-feet wide bed awaits! .")

txt = dsb(".[ws.p ? x]")
test(txt, "Hej Welcome A 6 feet wide bed awaits")

txt = dsb(".[ws.d ? x]")
test(txt, "&Hej! Welcome!! A -feet wide bed awaits! .")

txt = dsb(".[ws.i ? x]")
test(txt, "&Hej! Welcome!! 6-feet wide bed awaits!")

txt = dsb(".[ws.p.d ? x]")
test(txt, "Hej Welcome A feet wide bed awaits")

txt = dsb(".[ws.punct.isolated ? x]")
test(txt, "Hej Welcome feet wide bed awaits")

txt = dsb(".[ws.d.i ? x]")
test(txt, "&Hej! Welcome!! -feet wide bed awaits!")

txt = dsb(".[ws.p.d.i ? x]")
test(txt, "Hej Welcome feet wide bed awaits")

#
# tws ####
#

x = c("  bonjour les gens", "ahem \n  ", NA)
txt = dsb(".[tws ? x]")
test(txt, c("bonjour les gens", "ahem", NA))

#
# trim ####
#

x = c("bonjour", "ahem")
txt = dsb(".[4 trim ? x]")
test(txt, c("our", ""))

txt = dsb(".[-2 trim ? x]")
test(txt, c("bonjo", "ah"))

txt = dsb(".[5 trim.r ? x]")
test(txt, c("bo", ""))

txt = dsb(".[1 trim.b ? x]")
test(txt, c("onjou", "he"))

#
# k, keep char ####
#

x = c("this is a long sentence", "a short one")
txt = smagic("{11k, ' and 'c ? x}")
test(txt, "this is a l and a short one")

txt = smagic("{11 k, ' and 'c ? x}")
test(txt, "this is a l and a short one")

txt = smagic("{'11|..'k, ' and 'c ? x}")
test(txt, "this is a l.. and a short one")

txt = smagic("{11|.. shorten, ' and 'c ? x}")
test(txt, "this is a l.. and a short one")

txt = smagic("{'11||..'k, ' and 'c ? x}")
test(txt, "this is a.. and a short one")

txt = smagic("{11|.. shorten.inclu, ' and 'c ? x}")
test(txt, "this is a.. and a short one")

txt = smagic("{11k.d, ' and 'c ? x}")
test(txt, "this is a .. and a short one")

txt = smagic("{11 Shorten, ' and 'c ? x}")
test(txt, "this is a .. and a short one")

#
# K, keep elements ####
#

x = 1:5

txt = smagic("{3K ? x}")
test(txt, 1:3)

txt = smagic("{3KO ? x}")
test(txt, c("1", "2", "three others"))

txt = smagic("{3Ko ? x}")
test(txt, c("1", "2", "3 others"))

txt = smagic("{'3|le reste'K ? x}")
test(txt, c("1", "2", "3", "le reste"))

txt = smagic("{'3||le reste'K ? x}")
test(txt, c("1", "2", "le reste"))

txt = smagic("{'3||et :rest:/:n:'K ? x}")
test(txt, c("1", "2", "et 3/5"))

txt = smagic("{'3||et :REST:/:N:'K ? x}")
test(txt, c("1", "2", "et three/five"))

# conditional
x = string_vec("laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
test(smagic("info: {' 'S, ~(2Ko), C ? x}"), "err")

#
# enum ####
#

x = 1:5
txt = smagic("{enum.q.3.oxf ? x}")
test(txt, "'1', '2', and 3 others")

txt = smagic("{enum.bq.or ! x{1:3}}")
test(txt, "`x1`, `x2` or `x3`")

txt = smagic("{enum.Q.nor.1 ! x{1:3}}")
test(txt, '1) "x1", 2) "x2", nor 3) "x3"')

txt = smagic("{enum.a ! x{1:3}}")
test(txt, "a) x1, b) x2, and c) x3")

txt = smagic("{enum.i ! x{1:3}}")
test(txt, "i) x1, ii) x2, and iii) x3")

# conditional
x = c("123", "abc", "a1b2")
txt = smagic("{''S, ~(enum), ' ; 'c ? x}")
test(txt, "1, 2 and 3 ; a, b and c ; a, 1, b and 2")

#
# first and last ###
#

x = 1:5
txt = smagic("{2 first, ''c ? x}")
test(txt, "12")

txt = smagic("{last.2, ''c ? x}")
test(txt, "45")

a = 2
txt = smagic("{'3'first, `a`last, ''c ? x}")
test(txt, "23")

txt = smagic("{'1|1'first, ''c ? x}")
test(txt, "15")

txt = smagic("{'-3'first, ''c ? x}")
test(txt, "45")

# conditional
x = c("123", "abc", "a1b2")
txt = smagic("{''S, ~(first), ' ; 'c ? x}")
test(txt, "1 ; a ; a")

txt = smagic("{''S, ~(last), ' ; 'c ? x}")
test(txt, "3 ; c ; 2")

#
# firstchar, lastchar ####
#

x = c("bonjour", "les", "gens")
a = 2
txt = smagic("{3 firstchar, `a`lastchar ? x}")
test(txt, c("on", "es", "en"))

#
# rev ####
#

test(smagic("{rev ? 1:3}"), 3:1)

# conditional
x = c("123", "abc", "a1b2")
txt = smagic("{''S, ~(rev, ''c), ' ; 'c ? x}")
test(txt, "321 ; cba ; 2b1a")

#
# sort, dsort ####
#

x = c(5, 3, 8, 1)
test(smagic("{sort ? x}"), c(1, 3, 5, 8))

test(smagic("{dsort ? x}"), c(8, 5, 3, 1))

# with preprocessing
x = "Mark is 34, Bianca is 55, Odette is 101, Julie is 21 and Frank is 5"
txt = smagic("{', | and 's, '\\D'sort, C ? x}")
test(txt, "Odette is 101, Julie is 21, Mark is 34, Frank is 5 and Bianca is 55")

txt = smagic("{', | and 's, '\\D'sort.num, C ? x}")
test(txt, "Frank is 5, Julie is 21, Mark is 34, Bianca is 55 and Odette is 101")

# conditional
x = c("521", "aebc")
txt = smagic("{''S, ~(sort, ''c), ' ; 'c ? x}")
test(txt, "125 ; abce")

txt = smagic("{''S, ~(dsort, ''c), ' ; 'c ? x}")
test(txt, "521 ; ecba")

#
# paste ####
#

x = 1:2
txt = smagic("{'x'paste, ', 'c ? x}")
test(txt, "x1, x2")

txt = smagic("{'*'paste.both, ', 'c ? x}")
test(txt, "*1*, *2*")

txt = smagic("{_ paste.right, ', 'c ? x}")
test(txt, "1_, 2_")

x = 1:3
txt = smagic("The number{if(.N>1 ; 's are 'paste.front ; ' is'paste.front), C ? x}.")
test(txt, "The numbers are 1, 2 and 3.")

x = 5
txt = smagic("The number{if(.N>1 ; 's are 'paste.front ; ' is 'paste.front), C ? x}.")
test(txt, "The number is 5.")

#
# insert ####
#

x = 1:2
txt = smagic("{'0'insert, ''c ? x}")
test(txt, "012")

txt = smagic("{'0'insert.both, ''c ? x}")
test(txt, "0120")

txt = smagic("{'0'insert.right, ''c ? x}")
test(txt, "120")

# conditional
x = c("521", "aebc")
test(smagic("{''S, ~('0'insert, ''c), ' ; 'c ? x}"), "err")

#
# fill ####
#

x = c("bon", "bonjour les gens")
txt = smagic("{fill, q, C ? x}")
test(txt, "'bon             ' and 'bonjour les gens'")

txt = smagic("{fill.right, q, C ? x}")
test(txt, "'             bon' and 'bonjour les gens'")

txt = smagic("{fill.center, q, C ? x}")
test(txt, "'      bon       ' and 'bonjour les gens'")

x = c(5, 15)
txt = smagic("{3 fill.right, q, C ? x}")
test(txt, "'  5' and ' 15'")

txt = smagic("{1 fill.right, q, C ? x}")
test(txt, "'5' and '15'")

txt = smagic("{'3|0'fill.right, q, C ? x}")
test(txt, "'005' and '015'")

#
# join ####
#

x = "\n    bonjour \\\n   les gens\n est-ce que ca va?"
txt = smagic("Text: {tws, join ? x}")
test(txt, "Text: bonjour les gens\n est-ce que ca va?")

#
# escape ####
#

x = "bon\njour \t les \t gens"
txt = smagic(x, .last = "escape")
test(txt, "bon\njour \\t les \\t gens")

#
# unik ####
#

x = c(1, 1, 2, 3, 3, 3) 
test(smagic("{unik ? x}"), 1:3)

# conditional
x = c("11211125564454", "aggafsgaffasg")
txt = smagic("{''S, ~(unik, ''c), ' ; 'c ? x}")
test(txt, "12564 ; agfs")

#
# nth ####
#

x = c(1, 6)
txt = smagic("They arrived {nth, C ? x}.")
test(txt, "They arrived 1st and 6th.")

txt = smagic("They arrived {Nth, C ? x}.")
test(txt, "They arrived first and sixth.")

#
# ntimes ####
#

x = c(1, 6)
txt = smagic("They won {ntimes, C ? x}.")
test(txt, "They won once and 6 times.")

txt = smagic("They won {Ntimes, C ? x}.")
test(txt, "They won once and six times.")

#
# n ####
#

x = c(45546, "bonjour")
txt = smagic("A = {n ? x} ; B = {n.l ? 55}")
test(txt, c("A = 45,546 ; B = fifty-five", "A = bonjour ; B = fifty-five"))

txt = smagic("{N.up ? 8} times he won!")
test(txt, "Eight times he won!")

year = 2023
txt = smagic("This test was written in {n.R?year}.")
test(txt, "This test was written in MMXXIII.")

txt = smagic("This test was written in {n.r?year}.")
test(txt, "This test was written in mmxxiii.")

#
# len ####
#

x = 1:5
txt = smagic("{Len.up ? x} numbers.")
test(txt, "Five numbers.")

x = 1:2
txt = smagic("x is of length {len ? x}, or {Len ? x}.")
test(txt, "x is of length 2, or two.")

# conditionaly
x = c("bonjour les gens", "la pluie", "est drue, je rentre")
txt = smagic("Number of words: {' 'S, ~(len), C ? x}.")
test(txt, "Number of words: 3, 2 and 4.")

num = string_ops(x, "' 'S, ~(len), num")
test(num, c(3, 2, 4))

#
# width ####
#

x = "Rome, l'unique objet de mon ressentiment, Rome a qui vient ton bras d'immoler mon amant"
txt = smagic("Voici le texte a apprendre:\n{'40|>'width ? x}.")
test(txt, c("Voici le texte a apprendre:\n> Rome, l'unique objet de mon\n> ressentiment, Rome a qui vient ton\n> bras d'immoler mon amant."))

#
# difftime ####
#

x = 3654
txt = smagic("Time since last check: {dtime ? x}.")
test(txt, "Time since last check: 1 hour 00 min.")

x = structure(1680294984.14505, class = c("POSIXct", "POSIXt")) - structure(1680292481.19258, class = c("POSIXct", "POSIXt"))
txt = smagic("Time since last check: {dtime ? x}.")
test(txt, "Time since last check: 41 min 42 sec.")

#
# erase ####
#

x = 1:5
txt = smagic("{if(. <= 3 ; erase), '.'c ? x}")
test(txt, "...4.5")

#
# nuke ####
#

x = 1:5
txt = smagic("nothing = {nuke ? x}")
test(txt, "nothing = ")

#
# rm ####
#

x = c("", "    ", "556", ":!", "pour qui sont ces", "serpents qui sifflent sur nos tetes?")
txt = smagic("{5 firstchar, rm, ', 'c ? x}")
test(txt, "    , 556, :!, pour , serpe")

txt = smagic("{5 firstchar, rm.blank, ', 'c ? x}")
test(txt, "556, :!, pour , serpe")

txt = smagic("{5 firstchar, rm.noalpha, ', 'c ? x}")
test(txt, "pour , serpe")

txt = smagic("{5 firstchar, rm.noalnum, ', 'c ? x}")
test(txt, "556, pour , serpe")

txt = smagic("{5 firstchar, rm.all, ', 'c ? x}")
test(txt, "")

txt = smagic("{5 firstchar, 'pour'rm.blank, ', 'c ? x}")
test(txt, "556, :!, serpe")

# conditional
x = string_vec("laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = smagic("{' 'S, rm.noalpha, ~('-'c), C ? x}")
test(txt, "laura-and, charles-and, charly-and and june")

#
# num ####
#

x = "parse55this"
txt = string_ops(x, "'\\d+'x, num")
test(txt, 55)

x = c("55", "abc")
txt = smagic("{num ? x}")
test(txt, c(55, NA))

txt = smagic("{num.soft ? x}")
test(txt, c(55, "abc"))

txt = smagic("{num.clear ? x}")
test(txt, c(55, ""))

txt = smagic("{num.rm ? x}")
test(txt, 55)

# conditional
x = string_vec("laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = smagic("{' 'S, num.rm, ~('-'c), C ? x}")
test(txt, "57-26, 32-7, 29-8 and 55")

#
# ascii ####
#

test(dsb("laurent .[ascii.utf8 ! bergé]"), "laurent berge")

#
# stop ####
#

x = "Hi I'm Laurent and I'm trying to remove stop-words."
txt = smagic("Before: {x}\nAfter: {stop, ws ? x}")
test(txt, "Before: Hi I'm Laurent and I'm trying to remove stop-words.\nAfter: Hi Laurent trying remove stop-words.")

#
# if-else ####
#

# elementwise
x = c(15, 550)
txt = dsb("The value is .[& x > 50 ; > 50 ; <= 50]")
test(txt, c("The value is <= 50", "The value is > 50"))

txt = dsb("The value is .[& x > 50 ; > 50]")
test(txt, c("The value is ", "The value is > 50"))

# testing the quote
txt = dsb("The value is .[& x > 50 ; > 50 ; '   .']")
test(txt, c("The value is    .", "The value is > 50"))

# whole string
x = c(15, 550)
txt = dsb("The max value is .[& max(x) > 50 ; large ; small].")
test(txt, "The max value is large.")

x = c(-5, 15)
txt = dsb("The max value is .[& max(x) > 50 ; large ; small].")
test(txt, "The max value is small.")

x = c(-5, 15)
txt = dsb("The max value is .[& max(x) > 50 ; large].")
test(txt, "The max value is .")

# nesting
x = c(15, 550)
txt = smagic("The values are{& length(x) < 5 ; ': {C ? x}' ; 'too many'}.")
test(txt, "The values are: 15 and 550.")

# special if else
x = c(15, 550)
txt = dsb("The value is .[&& x > 50 ; > 50]")
test(txt, c("The value is 15", "The value is > 50"))

# long interpolated true and false
x = 1:3
y = letters[1:3]
txt = smagic("{C ! {&&x %% 2 == 1;{y}}}")
test(txt, "a, 2 and c")

z = tail(letters, 3)
txt = smagic("{C ! {&x %% 2 == 1; {y} ; {bq?z}}}")
test(txt, "`x`,  b and `z`")

#
# if ####
#

# special values: ., .len (or .N), .nchar (or .C)

x = c(5, 25, 30, 7)
txt = smagic("The large numbers are {if(.<10 ; nuke), C ? x}.")
test(txt, "The large numbers are 25 and 30.")

x = c(5, 25, 30, 7)
txt = smagic("The numbers are {if(.C < 2 ; 'x'pa ; 1k), C ? x}.")
test(txt, "The numbers are x5, 2, 3 and x7.")

txt = smagic("The numbers are {if(.nchar >= 2 ; 'x'pa ; 1k), C ? x}.")
test(txt, "The numbers are 5, x25, x30 and 7.")

x = c(5, 25, 30, 7)
txt = smagic("The hash is {if(.N < 4 ; ':'c ; '-'c), C ? x}.")
test(txt, "The hash is 5-25-30-7.")

x = c(5, 25, 30)
txt = smagic("The hash is {if(.N < 4 ; ':'c ; '-'c), C ? x}.")
test(txt, "The hash is 5:25:30.")

# conditional
x = c(5, 25, 30)
txt = smagic("Example: {if(.C < 2 ; 'x'pa ; ''s, ~('/'c)), C ? x}.")
test(txt, "Example: x5, 2/5 and 3/0.")


#
# vif ####
#

x = c(5, 25, 30, 7)
txt = smagic("There are {vif(.len<3 ; not many ; many), C ? x} numbers.")
test(txt, "There are many numbers.")

x = c(5, 25)
txt = smagic("There are {vif(.N<3 ; not many ; many), C ? x} numbers.")
test(txt, "There are not many numbers.")

# elementwise
x = c(5, 25, 30, 7)
txt = smagic("Numbers: {vif(.<10 ; small ; large), C ? x}.")
test(txt, "Numbers: small, large, large and small.")

# nesting
x = c(5, 25, 30, 7)
txt = smagic("Numbers: {vif(.N > 5 ; sum = {n?sum(x)} ; prod = {n?prod(x)}), C ? x}.")
test(txt, "Numbers: prod = 26,250.")

x = c(5, 25, 30, 7, 5, 4)
txt = smagic("Numbers: {vif(.N > 5 ; sum = {n?sum(x)} ; prod = {n?prod(x)}), C ? x}.")
test(txt, "Numbers: sum = 76.")

# '.' as a placeholder
x = c(5, 12, 20, 35)
txt = smagic("y = {3 first, vif(.N<2 ; short ; {' + 'c ? .}) ? x}")
test(txt, "y = 5 + 12 + 20")

#
# escaping ####
#

txt = smagic("\\{} it's some braces")
test(txt, "{} it's some braces")

txt = dsb("\\.[] it's some dsb")
test(txt, ".[] it's some dsb")

txt = smagic("I'm saying {q!ha \\} ha}!")
test(txt, "I'm saying 'ha } ha'!")

txt = dsb("I'm saying .[q!ha \\] ha]!")
test(txt, "I'm saying 'ha ] ha'!")

#
# user-defined ops ####
#

# without scope
xplode = function(x, argument, options, ...){
  unlist(strsplit(as.character(x), ""))
}

smagic_register_fun(xplode, "xplode")

txt = smagic("bon{xplode!jour}")
test(txt, c("bonj", "bono", "bonu", "bonr"))

smagic_register_ops("'50|-'fill", "h2")
txt = smagic("my header ", .last = "h2")


# with scope
smagic_register_ops("''s, x paste, ' + 'c", "equick", namespace = "test_ns")

sma_test = smagic_alias(.namespace = "test_ns")
txt = sma_test("y = {equick!173ab}")
test(txt, "y = x1 + x7 + x3 + xa + xb")

test_err_contains(smagic("y = {equick!173ab}"), "valid operator")
test_err_contains(sma_test("y = {aezgfaffs!173ab}"), "!xplode")

#
# errors ####
#

test_err_contains(smagic("hi {there"), "bracket")

test_err_contains(smagic("hi {upper ! there"), "bracket")

test_err_contains(smagic("hi {S, ''S, ~(first ! bon, jour}"), "parenthesis")

test_err_contains(smagic("hi {$(bon) ! jour}"), "if/plurali & (v1")

test_err_contains(smagic("hi {&bonjour}"), "semi-colon")

test_err_contains(smagic("hi {&bonjour ; jour}"), "bonjour & evaluated")

test_err_contains(smagic("hi {&TRUE ; {upper ! jour ; voir}"), "closing bracket")

test_err_contains(smagic("hi {&TRUE ; {upperest ! jour} ; voir}"), "w/upper")

test_err_contains(smagic("hi {&TRUE ; {upperest ? jour} ; voir}"), "jour & evaluated")

test_err_contains(smagic("hi {&bonjour ; jour}"), "bonjour & not found")

test_err_contains(smagic("error operation: {if(1:5 ; upper ; lower) ! ohohoh}"), "w/if & length")

test_err_contains(smagic("error operation: {if(fakfak; upper ; lower) ! ohohoh}"), "w/if & evaluated & not found")

test_err_contains(smagic("error operation: {if(fakfak%m; upper ; lower) ! ohohoh}"), "w/if & parsed")

test_err_contains(smagic("error operation: {S, ~(sekg) ! ohohoh, hihihi}"), "not a valid op")

test_err_contains(string_vec("hi, {there"), "bracket & matched & escape")

#
# multi line expressions ####
#

txt = smagic("The solution to x + 32 = 5 is {
     y = 32
     z = 5
     z - y
}")
test(txt, "The solution to x + 32 = 5 is -27")

txt = smagic("First letters: {
     x = head(letters)
     fun = function(z){
          paste(z, collapse = '')
     }
     fun(x)
}")
test(txt, "First letters: abcdef")

#
# variables as arguments ####
#

txt = smagic("Hi {you}!", you = "Omer")
test(txt, "Hi Omer!")

txt = smagic("m * n = {m * n}", m = 5, n = 4)
test(txt, "m * n = 20")

# with nesting
txt = smagic("vars = {' + 'c ! {v1}_{suffix}}", v1 = c("x1", "z5"), suffix = "post")
test(txt, "vars = x1_post + z5_post")

# with ifelse
txt = smagic("value = {&v1 > 0 ; {.} ; {v2}}", v1 = 55, v2 = "error")
test(txt, "value = 55")

txt = smagic("value = {&v1 > 0 ; {.} ; {v2}}", v1 = -55, v2 = "error")
test(txt, "value = error")

# with plural
txt = smagic("The number{$s, is, ({v};{$enum}) ? x}", x = 1:3, v = "error")
test(txt, "The numbers are 1, 2 and 3")

txt = smagic("The number{$s, is, ({v};{$enum}) ? x}", x = 3, v = "error")
test(txt, "The number is error")

#
# default options ####
#

sma2 = smagic_alias(.delim = "$[ ]")
txt = sma2("x$[1:2]", .last = "C")
test(txt, "x1 and x2")





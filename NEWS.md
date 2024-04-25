
# stringmagic 1.1.1

## Hot fix

- make `stringmagic` compatible with `R` < 4.1.0 by removing calls to `...names()`.

# stringmagic 1.1.0

## New functions

- add `string_extract` to extract patterns

- add `string_split` to split character strings

## Improvements

- `string_ops` now uses `...` to pass operations. This is backward compatible.

- `string_clean`: now the magic flag also expands the replacements:
```R
x = "Hi Mary, how's John doing?"
from = "John"
to = "Kate"
string_clean(x, "m/{from} => {to}")
#> [1] "Hi Mary, how's Kate doing?"
```

- `string_magic`: add the `comma` flag to the `enum` operation. In that case, the enumeration ends with ", " instead of ", and ".

- `string_magic`: the if-else operation `&` now keeps memory of variables accessed within data sets:
```R
data = list(x = c(15, 25, 550), y = rnorm(1000))
string_magic("The values are{& length(data$x) < 5 ; : {enum ? .} ;  too many}.")
# [1] "The values are: 15, 25 and 550."
string_magic("The values are{& length(data$y) < 5 ; : {enum ? .} ;  too many}.")
# [1] "The values are too many."
```

- `string_magic`: new operation `deparse` (alias: `dp`) to deparse an object and keep only the first characters of the deparsed string.

- improve error messages.

## Aliases

- new battery of short aliases: `sma` for `string_magic`, `catma` for catmagic, `mema` for `message_magic`, etc.. (`st_ops`, `st_is`, `st_any`, `st_all`, `stextract`, `stwhich`, `stget`, `stclean`, `stvec`, `streplace`, `stsplit` -- short names with vowels after `st` have an underscore.)

# stringmagic 1.0.0

First public release. The syntax should be stable.  

This package is a spinoff from [fixest's formula syntax interpolation](https://lrberge.github.io/fixest/reference/xpd.html).

Many thanks to Achim Zeileis, Vincent Arel-Bundock and Kyle Butts who provided insightful comments
during the development.

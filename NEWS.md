
# stringmagic 1.2.0

## Compatibility with old R versions

- `stringmagic` is now fully compatible with R v3.5.0 (at least)

- in C code: use `STRING_PTR_RO` instead of `STRING_PTR` to comply with new CRAN policy

## Bugs

- fix major bug in the if-else operator (`&`) leading to opposite operations

- in `string_vec`, fix bug leading to the removal of empty strings

- in `string_vec`, fix bugs with the arguments `.sep` and `.collapse` 

## New operators

- `string_magic`: new operator `table` to flexibly attach elements with their frequencies:
```R
dna = string_split("atagggagctacctgcgcgtcgcccaaaagcaggg", "")
cat_magic("Letters in the DNA seq. {''c, Q ? dna}: ",
          # by default: inverse frequency sorting
          "  -      default: {table, enum ? dna}",
          "  - value sorted: {table.sort, enum ? dna}",
          # argument in single quotes to customize the display, it's a 
          #   `string_magic` interpolation
          "  -       shares: {'{x} [{round(s * 100)}%]' table, enum ? dna}",
          # `fsort` sorts by **increasing** frequency
          "  - freq. sorted: {'{q ? x}' table.fsort, enum ? dna}",
          .sep = "\n")
#> Letters in the DNA seq. "atagggagctacctgcgcgtcgcccaaaagcaggg": 
#>   -      default: g (12), c (10), a (9) and t (4)
#>   - value sorted: a (9), c (10), g (12) and t (4)
#>   -       shares: g [34%], c [29%], a [26%] and t [11%]
#>   - freq. sorted: 't', 'a', 'c' and 'g'
```

- `string_magic`: new operators `round` and `signif`  (wtih shorthands `r0` to `r6` and `s0` to `s6`) to flexibly format numbers:
```R
x = c(153, 207.256, 0.00254, 15231312.2)
# keeping one significant digit
cat_magic("v1: {s1, align ? x} ",
          # removing the comma for large numbers and preserving ints
          "v2: {s1.int.nocomma ? x}", .collapse = "\n")
#> v1: 153.0        v2: 153
#> v1: 207.2        v2: 207.2
#> v1: 0.002        v2: 0.002
#> v1: 15,231,312.2 v2: 15231312.2

string_magic("pi = {r3 ? pi}")
#> [1] "pi = 3.142"
```

## New features

- add the argument `.data` to `string_magic()`, used to evaluate variables in the interpolations

- new functions `get_interpolated_expr()` and `get_interpolated_vars()`. This function recovers all the expressions to be interpolated in a call to `string_magic()` (oriented for developers).

- new argument `center.right` in the function `string_fill` to resolve situations in which the characters are not perfectly centered

- make `cat_magic` and `message_magic` more in line with their base R counterparts (they work properly with vectors now)

## User visible change: Functions renaming

- the functions `st_ops`, `st_is`, `st_any`, `st_all` have been renamed into `stops`, `stis`, `stany`, `stall` to align with the convention of all other aliases. Although the names aren't great, at least they are consistent.

## Minor changes

- the new operator `swidth` (screen width) replaces the operator `width`. The operator width becomes an alias for `fill`.

- the default screen width for `message_magic` becomes the minimum between 100 characters and 90% of the current screen size (actually the console size).

- rework the argument `.width` in `message_magic` and `cat_magic`: now the special variable `.sw` can only be used in a one-sided formula (non-standard evaluation is no longer supported)

- improve error handling

- add `left` option to operators when relevant. Thanks to @kylebutts, #3

- in `string_vec`, change the default of argument `.protect.vars` to `FALSE`, which is much more aligned to common sense

- in `string_magic`'s argument `.post`: removal of argument catching, which could lead, occasionnally, to bugs very hard to understand

- in `string_vec`: add the arguments `.check` and `.help`.

# stringmagic 1.1.2

## Hot fix

- make `stringmagic` compatible with `R` in [4.1.0; 4.1.2].

## New features

- add argument `.trigger` to `cat/message_magic_alias`


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

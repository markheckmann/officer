# generates unique identifiers

generates unique identifiers based on
[`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html).

## Usage

``` r
uuid_generate(n = 1, ...)
```

## Arguments

- n:

  integer, number of unique identifiers to generate.

- ...:

  arguments sent to
  [`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)

## Examples

``` r
uuid_generate(n = 5)
#> [1] "50bd838a-6389-4aa5-b7c4-e575244ec281"
#> [2] "3877caa1-653a-4d2f-88d4-a45bcd2a343b"
#> [3] "64d59bf0-0a56-437e-9ce3-99ae450bc8d8"
#> [4] "5d3ff089-0cc5-4bd3-b22b-2109fc814341"
#> [5] "703b3ba8-c27e-4232-ae72-ca43df5762f1"
```

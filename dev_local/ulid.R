library(ulid)

unmarshal()

# Monotonic sort order (correctly detects and handles the same millisecond)
# SO post in ULID sorting
library(ulid)
library(stringi)

gen_ulid <- \(sleep) replicate(5, {Sys.sleep(sleep); generate()})

u <- gen_ulid(1)
stri_sort(u) == u

u <- gen_ulid(.1)
stri_sort(u) == u

df <- unmarshal(u)
format(df$ts, "%Y-%m-%d %H:%M:%OS3")




data.frame(ulid = generate(), ets = as.character(Sys.time()))
u <- replicate(5, {Sys.sleep(.1); gen()}, simplify = FALSE)
df <- bind_rows(u) |> mutate(ts_extracted = unmarshal(ulid)) |> arrange(ulid)
df

# ording based on seconds

ts_str <- paste0("2024-05-30 10:00:00.", seq(0, 1000, len = 5))
ts <- as.POSIXct(ts_str, format = "%Y-%m-%d %H:%M:%OS")
ts_str
format(ts, "%Y-%m-%d %H:%M:%OS3")

us <- ts_generate(ts)
us
stringr::str_sort(us, numeric = T)
us

x <- c("1A", "11")
stringi::stri_sort(x)

format(y, "%Y-%m-%d %H:%M:%OS6")

x <- c("2014-02-24 11:30:00.123", "2014-02-24 11:30:00.456")
x <- paste0("2024-105-30 10:00:00.", seq(0, 1000, by = 200))
y <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS")
format(y, "%Y-%m-%d %H:%M:%OS3")


ts <- replicate(5, {Sys.sleep(.01); Sys.time()}, simplify = FALSE)
d <- lapply(ts, \(x) print(as.numeric(x)*1000, digits=15))
d <- lapply(ts, \(x) print(as.numeric(x)*1000))

gen <- \() data.frame(ulid = generate(), ets = as.character(Sys.time()))
u <- replicate(5, {Sys.sleep(.1); gen()}, simplify = FALSE)
df <- bind_rows(u) |> mutate(ts_extracted = unmarshal(ulid)) |> arrange(ulid)
df

gen <- \() data.frame(ulid = generate(), ets = as.character(Sys.time()))
u <- replicate(5, {Sys.sleep(1); gen()}, simplify = FALSE)
df <- bind_rows(u) |> mutate(ts_extracted = unmarshal(ulid)) |> arrange(ulid)
df

u <- replicate(5, {Sys.sleep(.1); generate()})
identical(u, str_sort(u, numeric = TRUE))
unmarshal(u) |> mutate(ms = as.numeric(format(ts, "%OS3")) * 1000)

u <- replicate(5, {Sys.sleep(1); generate()})
identical(u, str_sort(u, numeric = TRUE)

library(stringr)

sort(u)
str_sort(u, numeric = TRUE)

mixedsort(n)

str_sort(seq.names, numeric = TRUE)
# stri_sort(seq.names, numeric = TRUE)






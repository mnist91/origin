library(data.table)

dt <- as.data.table(mtcars)
dt[, mpg := 3]
dt[, `:=`(gear = 4)]
dt[, `:=`(gear = mean(gear)), by = .(vs, am)]

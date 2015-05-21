## Analyse the Ohraz data Case study 5 of Leps & Smilauer

## load vegan
library("vegan")

## load the data
spp <- read.csv("ohraz-spp.csv", header = TRUE, row.names = 1)
env <- read.csv("ohraz-env.csv", header = TRUE, row.names = 1)
molinia <- spp[, 1]
spp <- spp[, -1]

## Year as numeric
env <- transform(env,
                 year = as.numeric(as.character(year)),
                 plotid = factor(plotid))

## hypothesis 1
c1 <- rda(spp ~ year + year:mowing + year:fertilizer +
          year:removal + Condition(plotid), data = env)

h <- how(within = Within(type = "none"),
         plots = Plots(strata = env$plotid, type = "free"),
         nperm = 999)

set.seed(42)
a1 <- anova(c1, permutations = h, model = "reduced", parallel = 4)
a1 <- permutest(c1, permutations = h, model = "reduced", parallel = 3)

## hypothesis 2
c2 <- rda(spp ~ year:mowing + year:fertilizer + year:removal +
          Condition(year + plotid), data = env)
set.seed(32)
a2 <- anova(c2, permutations = h, model = "reduced", parallel = 3)

## hypothesis 3
c3 <- rda(spp ~ year:fertilizer +
          Condition(year + plotid + year:mowing + year:removal), data = env)
set.seed(21)
a3 <- anova(c3, permutations = h, model = "reduced", parallel = 3)

## hypothesis 4
c4 <- rda(spp ~ year:mowing +
          Condition(year + plotid + year:fertilizer + year:removal), data = env)
set.seed(11)
a4 <- anova(c4, permutations = h, model = "reduced", parallel = 3)

## hypothesis 5
c5 <- rda(spp ~ year:removal +
          Condition(year + plotid + year:fertilizer + year:mowing), data = env)
set.seed(1)
a5 <- anova(c5, permutations = h, model = "reduced", parallel = 3)

## Analyse the Ohraz data Case study 5 of Leps & Smilauer

## load vegan
library("vegan")
library("gdata")

## load the data
### Download the data zip
furl <- "http://regent.prf.jcu.cz/maed2/chap15.zip"
td <- tempdir()
tf <- tempfile(tmpdir = td, fileext = ".zip")
download.file(furl, tf)

### list the files in the zip, we want the xls version (file 3)
fname <- unzip(tf, list = TRUE)$Name[3]
unzip(tf, files = fname, exdir = td, overwrite = TRUE) # unzip
datpath <- file.path(td, fname)                        # path to xls

### read the xls file, sheet 2 contains species data, sheet 3 the env
spp <- read.xls(datpath, sheet = 2, skip = 1, row.names = 1)
env <- read.xls(datpath, sheet = 3, row.names = 1)

## load the data
## spp <- read.csv("seedling-spp.csv", header = TRUE, row.names = 1, skip = 1)
## env <- read.csv("seedling-env.csv", header = TRUE, row.names = 1, skip = 1)

## block as factor
env <- transform(env, block = factor(block))

## Gradient lengths
seed.dca <- decorana(spp)
seed.dca

## Design based analysis
mod1 <- rda(spp ~ treatment + Condition(block), data = env)
mod1
mod1a <- rda(spp ~ treatment, data = env)
mod1a

plot(mod1, display = c("species", "cn"), scaling = 1, type = "n",
     xlim = c(-10.5, 1.5))
text(mod1, display = "species", scaling = 1, cex = 0.8)
text(mod1, display = "cn", scaling = 1, col = "blue", cex = 1.2,
     labels = c("Control", "Litter+Moss", "Litter", "Removal"))

h <- how(blocks = env$block, nperm = 999)

set.seed(42)
p1 <- anova(mod1, permutations = h, parallel = 3)
p1
## set.seed(42)
## p1a <- anova(mod1a, permutations = h, parallel = 3)
## p1a

set.seed(24)
p1axis <- anova(mod1, permutations = h, parallel = 3, by = "axis")
p1axis

## set.seed(42)
## pp1 <- permutest(mod1, permutations = h, parallel = 3)
## set.seed(42)
## pp1a <- permutest(mod1a, permutations = h, parallel = 3)

## Model-based analysis
setBlocks(h) <- NULL                    # remove blocking
getBlocks(h)                            # confirm

## mod2 <- rda(spp ~ treatment + Condition(block), data = env)
set.seed(51)
p2 <- anova(mod1, permutations = h, parallel = 3)
p2

set.seed(83)
p2axis <- anova(mod1, permutations = h, parallel = 3, by = "axis")
p2axis

## Normalized
spp.norm <- decostand(spp, method = "normalize", MARGIN = 1)

mod2 <- rda(spp.norm ~ treatment + Condition(block), data = env)
mod2
eigenvals(mod2) / mod2$tot.chi

set.seed(76)
anova(mod2, permutations = h, parallel = 3)

## ----setup-options, echo = FALSE, results = "hide", message = FALSE------
knitr::opts_chunk$set(comment=NA, fig.align = "center",
                      out.width = "0.7\\linewidth",
                      echo = TRUE, message = TRUE, warning = TRUE,
                      cache = TRUE)
knitr::knit_hooks$set(crop.plot = knitr::hook_pdfcrop)

## ----packages, echo = FALSE, results = "hide", message = FALSE-----------
library("vegan")
data(varespec)
data(varechem)

## ----housekeeping, eval=FALSE--------------------------------------------
## setwd("your/working/dir")

## ----loading-1, results='hide', message = FALSE--------------------------
library("vegan")
data(dune)
data(dune.env)

## ----loading-2-----------------------------------------------------------
dim(dune)                               # number of samples, species
head(dune[,1:6])

## ----loading-3-----------------------------------------------------------
head(dune.env, n=3)
summary(dune.env)

## ----pca1----------------------------------------------------------------
(pca <- rda(dune))

## ----NMDS1, results='hide'-----------------------------------------------
dune.bray.ord <- metaMDS(dune, distance = "bray", k = 2, trymax = 50)

## ----NMDS2, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord)

## ----NMDS3, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites")

## ----NMDS4, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "species")

## ----NMDS5, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites", type = "text")

## ----NMDS5-2, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites")
set.seed(314) ## make reproducible
ordipointlabel(dune.bray.ord, display = "sites", scaling = 3, add = TRUE)

## ----NMDS5-5, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "species")
set.seed(314) ## make reproducible
ordipointlabel(dune.bray.ord, display = "species", scaling = 3, add = TRUE)

## ----NMDS5-6, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord)
set.seed(314) ## make reproducible
ordipointlabel(dune.bray.ord, scaling = 3, add = TRUE)

## ----NMDS6, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites", cex=2)

## ----NMDS7, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
colors.vec <- c("red", "blue", "orange", "grey")
plot(dune.bray.ord, display = "sites", type = "n")
points(dune.bray.ord, display = "sites", cex=2, pch = 21, 
       col = colors.vec[dune.env$Management], 
       bg = colors.vec[dune.env$Management])
legend("topright", legend = levels(dune.env$Management), bty = "n",
                      col = colors.vec, pch = 21, pt.bg = colors.vec)

## ----NMDS11, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites", cex=2) # just site points

## ----NMDS12, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites", cex=2)
ordihull(dune.bray.ord,groups = dune.env$Management, label = TRUE) # convex hulls

## ----NMDS14, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites", cex=2)
ordihull(dune.bray.ord,groups = dune.env$Management, label = TRUE, col = "blue")
ordispider(dune.bray.ord,groups = dune.env$Management, label = TRUE)

## ----NMDS17, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, type = "n")

## ----NMDS18, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, type = "n")
points(dune.bray.ord, display = "sites", cex = 2)

## ----NMDS19, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites", type = "n")

## ----NMDS20, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites", type = "n")
points(dune.bray.ord, display = "sites", cex = 2)

## ----NMDS21, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites", type = "n")
points(dune.bray.ord,display = "sites", cex = 2)
ordispider(dune.bray.ord,groups = dune.env$Management, label = TRUE)

## ----NMDS22, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth"----
plot(dune.bray.ord, display = "sites", type = "n")
points(dune.bray.ord, display = "sites", cex = 2)
ordiellipse(dune.bray.ord,groups = dune.env$Management, label = TRUE)

## ----NMDS23, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth", message=FALSE, results='hide'----
plot(dune.bray.ord, display = "sites", type = "n")
points(dune.bray.ord,display = "sites", cex = 2)
ordisurf(dune.bray.ord,dune.env$A1, add = TRUE)

## ----NMDS24--------------------------------------------------------------
dune.bray.ord.A1.fit <- envfit(dune.bray.ord,dune.env$A1, permutations = 1000)
dune.bray.ord.A1.fit

## ----NMDS25, fig.height = 5, crop.plot = TRUE, out.width = "0.5\\linewidth", message=FALSE, results='hide'----
plot(dune.bray.ord, display = "sites", type = "n")
points(dune.bray.ord,display = "sites", cex = 2)
plot(dune.bray.ord.A1.fit, add = TRUE)
ordisurf(dune.bray.ord,dune.env$A1, add = TRUE)


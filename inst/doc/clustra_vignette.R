## ---- setup, echo = FALSE-----------------------------------------------------
knitr::opts_knit$set(
  collapse = TRUE,
  comment = "#>"
)
start_knit = proc.time()

## -----------------------------------------------------------------------------
library(clustra)
mc = 1 # If running on a unix or a Mac platform, increase up to 2x # cores
set.seed(12345)
data = gen_traj_data(n_id = c(400, 800, 1600), m_obs = 25, 
                     s_range = c(-365, -14), e_range = c(0.5*365, 2*365),
                     noise = c(0, 5))
head(data)

## ----fig.width = 7, fig.height = 9--------------------------------------------
library(ggplot2)
ggplot(data[id %in% sample(unique(data[, id]), 9)],
       aes(x = time, y = response)) + facet_wrap(~ id) + geom_point()

## -----------------------------------------------------------------------------
set.seed(1234737)
cl = clustra(data, k = 3, maxdf = 30, conv = c(10, 0), mccores = mc,
             verbose = TRUE)

## ----fig.width = 7, fig.height = 7--------------------------------------------
sdt = data
if(nrow(data) > 10000)
  sdt = data[, group:=factor(..cl$data_group)][sample(nrow(data), 10000)]
ggplot(sdt, aes(x = time, y = response)) + geom_point(pch = ".")

np = 100
k = length(cl$tps)
ntime = seq(data[, min(time)], data[, max(time)], length.out = np)
pdata = expand.grid(time = ntime, group = factor(1:k))
pdata = subset(pdata, group %in% which(lengths(cl$tps) > 0))
pred = vector("list", k)
for(i in 1:k) 
  if(is.null(cl$tps[[i]])) {
    pred[[i]] = NULL
  } else {
    pred[[i]] = mgcv::predict.bam(cl$tps[[i]], newdata = list(time = ntime),
                        type = "response")
  }
pdata$pred = do.call(c, pred)
ggplot(pdata, aes(x = time, y = pred, color = group)) + 
  geom_point(data = sdt, aes(y = response), pch = ".") + geom_line()

## -----------------------------------------------------------------------------
MixSim::RandIndex(cl$data_group, data[, true_group])

## ----fig.width = 7, fig.height = 9--------------------------------------------
set.seed(1234567)
data2 = gen_traj_data(n_id = c(500, 1000, 2000), m_obs = 25, s_range = c(-365, -14),
                     e_range = c(60, 2*365), noise = c(0, 20))
iplot = sample(unique(data2$id), 9)
sampobs = match(data2$id, iplot, nomatch = 0) > 0
ggplot(data2[sampobs], aes(x = time, y = response)) + 
  facet_wrap(~ id) + geom_point()
cl = clustra(data2, k = 3, maxdf = 30, conv = c(10, 0), mccores = mc, verbose = TRUE)
MixSim::RandIndex(cl$data_group, data2[, true_group])

## ----fig.width = 7, fig.height = 7--------------------------------------------
sdt = data2
if(nrow(data) > 10000)
  sdt = data2[, group:=factor(..cl$data_group)][sample(nrow(data), 10000)]
ggplot(sdt, aes(x = time, y = response)) + geom_point(pch = ".")

np = 100
k = length(cl$tps)
ntime = seq(data[, min(time)], data[, max(time)], length.out = np)
pdata = expand.grid(time = ntime, group = factor(1:k))
pdata = subset(pdata, group %in% which(lengths(cl$tps) > 0))
pred = vector("list", k)
for(i in 1:k) 
  if(is.null(cl$tps[[i]])) {
    pred[[i]] = NULL
  } else {
    pred[[i]] = mgcv::predict.bam(cl$tps[[i]], newdata = list(time = ntime),
                        type = "response")
  }
pdata$pred = do.call(c, pred)
ggplot(pdata, aes(x = time, y = pred, color = group)) + 
  geom_point(data = sdt, aes(y = response), pch = ".") + geom_line()

## ----fig.width = 7------------------------------------------------------------
set.seed(1234737)
sil = clustra_sil(data, k = c(2, 3, 4), mccores = mc, conv = c(7, 1),
                  verbose = TRUE)
plot_sil = function(x) {
  msil = round(mean(x$silhouette), 2)
  ggplot(x, aes(id, silhouette, color = cluster, fill = cluster)) + geom_col() +
    ggtitle(paste("Average Width:", msil)) +
    scale_x_discrete(breaks = NULL) + scale_y_continuous("Silhouette Width") +
    geom_hline(yintercept = msil, linetype = "dashed", color = "red")
}
lapply(sil, plot_sil)

## ----fig.width = 7------------------------------------------------------------
sil = clustra_sil(cl)
lapply(sil, plot_sil)

## ----fig.width = 7, fig.height=7----------------------------------------------
set.seed(1234737)
ran = clustra_rand(data, k = c(2, 3, 4), mccores = mc, replicates = 10,
                   conv = c(7, 1), verbose = TRUE)
rand_plot(ran)

## ----fig.width = 7, fig.height = 7--------------------------------------------
set.seed(12347)
cl = clustra(data2, k = 40, maxdf = 30, conv = c(10, 0), mccores = mc,
             verbose = TRUE)
gpred = function(tps, newdata) 
  as.numeric(mgcv::predict.bam(tps, newdata, type = "response",
                               newdata.guaranteed = TRUE))
resp = do.call(rbind, lapply(cl$tps, gpred, newdata = data.frame(
  time = seq(min(data2$time), max(data2$time), length.out = 100))))
plot(hclust(dist(resp)))

## -----------------------------------------------------------------------------
cat("clustra vignette run time:\n")
print(proc.time() - start_knit)


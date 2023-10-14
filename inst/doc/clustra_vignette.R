## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_knit$set(
  collapse = TRUE,
  comment = "#>"
)
start_knit = proc.time()
data.table::setDTthreads(1) # manage data.table threads

## ----gendata------------------------------------------------------------------
library(clustra)
mc = 2 # If running on a unix or a Mac platform, increase up to # cores
if (.Platform$OS.type == "windows") mc = 1
init = "random"
set.seed(12345)
data = gen_traj_data(n_id = c(500, 1000, 1500, 2000), types = c(2, 1, 3, 2), 
                     intercepts = c(70, 130, 120, 130), m_obs = 25, 
                     s_range = c(-365, -14), e_range = c(0.5*365, 2*365),
                     noise = c(0, 15))
head(data)

## ----plotdata, fig.width = 7, fig.height = 9----------------------------------
plot_sample(data[id %in% sample(unique(data[, id]), 9)], group = "true_group")

## ----clustra4-----------------------------------------------------------------
set.seed(12345)
cl4 = clustra(data, k = 4, maxdf = 10, conv = c(10, 0), mccores = mc,
             verbose = TRUE)

## ----smooths, fig.width = 7, fig.height = 7-----------------------------------
plot_smooths(data, group = NULL)
plot_smooths(data, cl4$tps)

## ----rand4--------------------------------------------------------------------
MixSim::RandIndex(cl4$data_group, data[, true_group])

## ----gen2_clustra, fig.width = 7, fig.height = 9------------------------------
set.seed(12345)
data2 = gen_traj_data(n_id = c(500, 1000, 1500, 2000), types = c(2, 1, 3, 2), 
                     intercepts = c(70, 130, 120, 130), m_obs = 25,
                     s_range = c(-365, -14), e_range = c(60, 2*365), 
                     noise = c(0, 30))
plot_sample(data2[id %in% sample(unique(data2[, id]), 9)], group = "true_group")

cl4a = clustra(data2, k = 4, maxdf = 10, conv = c(10, 0), mccores = mc,
             verbose = TRUE)
MixSim::RandIndex(cl4a$data_group, data2[, true_group])

## ----smooths2, fig.width = 7, fig.height = 7----------------------------------
plot_smooths(data2, group = NULL)
plot_smooths(data2, cl4a$tps)

## ----sil, fig.width = 7-------------------------------------------------------
set.seed(12345)
sil = clustra_sil(data, kv = c(2, 3, 4, 5), mccores = mc, maxdf = 10,
                  conv = c(7, 1), verbose = TRUE)
lapply(sil, plot_silhouette)

## ----sil2, fig.width = 7------------------------------------------------------
sil = clustra_sil(cl4)
lapply(sil, plot_silhouette)

## ----rand_plot, fig.width = 7, fig.height=7, eval = FALSE---------------------
#  set.seed(12345)
#  ran = clustra_rand(data, k = c(2, 3, 4, 5), mccores = mc,
#                     replicates = 10, maxdf = 10, conv = c(7, 1), verbose = TRUE)
#  rand_plot(ran)

## ----rand_plot_d, fig.width = 7, fig.height=7, eval = FALSE-------------------
#  set.seed(12345)
#  ran = clustra_rand(data, k = c(2, 3, 4, 5), starts = "distant", mccores = mc,
#                     replicates = 10, maxdf = 10, conv = c(7, 1), verbose = TRUE)
#  rand_plot(ran)

## ----hclust, fig.width = 7, fig.height = 7------------------------------------
set.seed(12345)
cl30 = clustra(data, k = 40, maxdf = 10, conv = c(10, 0), mccores = mc,
             verbose = TRUE)
gpred = function(tps, newdata) 
  as.numeric(mgcv::predict.bam(tps, newdata, type = "response",
                               newdata.guaranteed = TRUE))
resp = do.call(rbind, lapply(cl30$tps, gpred, newdata = data.frame(
  time = seq(min(data2$time), max(data2$time), length.out = 100))))
plot(hclust(dist(resp)))

## ----hclust_d, fig.width = 7, fig.height = 7----------------------------------
set.seed(12345)
cl30 = clustra(data, k = 40, starts = "distant", maxdf = 10, conv = c(10, 0), mccores = mc,
             verbose = TRUE)
gpred = function(tps, newdata) 
  as.numeric(mgcv::predict.bam(tps, newdata, type = "response",
                               newdata.guaranteed = TRUE))
resp = do.call(rbind, lapply(cl30$tps, gpred, newdata = data.frame(
  time = seq(min(data2$time), max(data2$time), length.out = 100))))
plot(hclust(dist(resp)))

## ----finish-------------------------------------------------------------------
cat("clustra vignette run time:\n")
print(proc.time() - start_knit)


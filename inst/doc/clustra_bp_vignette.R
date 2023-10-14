## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_knit$set(
  collapse = TRUE,
  comment = "#>"
)
cran = list(LibsData = TRUE, plot_true = TRUE, plot_raw = TRUE, run5 = TRUE,
            run5_v2 = FALSE, iter_plots = TRUE, plot5_side_by_side = FALSE, 
            fig1 = FALSE, comp_true = FALSE, run10 = FALSE, plot10 = FALSE, 
            run2 = FALSE, rand2 = FALSE, silplot = FALSE, silplot2 = FALSE, 
            rand_plot = FALSE, hclust40 = FALSE, SAS_R_data = FALSE, 
            Rand_SAS_R = FALSE, true_group = FALSE)
figs = list(LibsData = TRUE, plot_true = FALSE, plot_raw = FALSE, run5 = TRUE,
            run5_v2 = TRUE, iter_plots = FALSE, plot5_side_by_side = FALSE, 
            fig1 = TRUE, comp_true = TRUE, run10 = TRUE, plot10 = TRUE, 
            run2 = TRUE, rand2 = TRUE, silplot = TRUE, silplot2 = FALSE,
            rand_plot = TRUE, hclust40 = FALSE, SAS_R_data = TRUE, 
            Rand_SAS_R = TRUE, true_group = TRUE)
all = list(LibsData = TRUE, plot_true = TRUE, plot_raw = TRUE, run5 = TRUE,
           run5_v2 = TRUE, iter_plots = TRUE, plot5_side_by_side = TRUE,
           fig1 = TRUE, comp_true = TRUE, run10 = TRUE, plot10 = TRUE,
           run2 = TRUE, rand2 = TRUE, silplot = TRUE, silplot2 = TRUE, 
           rand_plot = TRUE, hclust40 = TRUE, SAS_R_data = TRUE, 
           Rand_SAS_R = TRUE, true_group = TRUE)
chunk = cran  # chose which version of the vignette to run
full_data = FALSE # choose if full 80,000 id data or sample of 10,000 ids
mc = 2
# If running on a Unix or a Mac platform, set to number of available cores.
# This vignette will experience notable speedup up to about 10 cores.
# On Intel chips with hyperthreading, up to 2x available cores can be used.
if (.Platform$OS.type == "windows") mc = 1

data.table::setDTthreads(1) # manage data.table threads (increase with full_data)

## ----LibsData, warning=FALSE, eval = chunk$LibsData---------------------------
library(clustra)
start_time = deltime()
print(paste("Number of cores being used: mc parameter =", mc))

repo = "https://github.com/MVP-CHAMPION/"

repo_sas = paste0(repo, "clustra-SAS/raw/main/")
if(full_data) {
  url = paste0(repo_sas, "bp_data/simulated_data_27June2023.csv.gz")
  data = data.table::fread(url)
} else {
  data = bp10k
}
data$true_group = data$group
head(data)
plot_path = paste0(tempdir(), "/") # output dir for plots
cat("Resulting plots are saved in", plot_path, "directory\n")

## ----plot_true, fig.width = 7, fig.height = 9, eval = chunk$plot_true---------
set.seed(12345)
plot_sample(data, layout = c(3, 3), group = "true_group")

## ----plot_raw, fig.width = 7, fig.height = 7, eval = chunk$plot_raw-----------
plot_smooths(data, fits = NULL, max.data = 20000, group = "true_group")

## ----run5, eval = chunk$run5--------------------------------------------------
set.seed(12345)
cl5 = clustra(data, k = 5, maxdf = 30, conv = c(20, 0.5), mccores = mc, verbose = TRUE)

## ----run5_v2, eval = chunk$run5_v2--------------------------------------------
#  # What happens if we cut the iterations at 5 maximum?
#  set.seed(12345)
#  cl5.v2 = clustra(data, k = 5, maxdf = 30, conv = c(5, 0.5), mccores = mc)

## ----iter_plots, fig.width=7, fig.height=7, eval = chunk$iter_plots-----------
set.seed(12345)
clustra(data, k = 5, maxdf = 30, conv = c(10, 0.5), mccores = mc, verbose = 2, ylim = c(110, 170))

## ----plot5_side_by_side, fig.width = 7, fig.height = 5, warnings=FALSE, eval = chunk$plot5_side_by_side----
#  plot_smooths(data, fits = cl5$tps, max.data = 30000)
#  m = matrix(c(0.1, .95, 0.1, .95), nrow = 1)
#  sc = split.screen(m)
#  screen(1)
#    sc = split.screen(c(1, 2))
#    screen(2)
#      plot_smooths(data, fits = cl5$tps, max.data = 0, ylim = c(110, 180))
#    screen(3)
#      plot_smooths(data, fits = cl5.v2$tps, max.data = 0, ylim = c(110, 180))
#  close.screen(all.screens = TRUE)

## ----fig1, fig.width = 7, fig.height = 5, eval = chunk$fig1-------------------
#  png(paste0(plot_path, "R_cl5_plot.png"), width = 480, height = 360)
#      plot_smooths(data, fits = cl5$tps, max.data = 0, ylim = c(110, 180))
#  dev.off()
#  knitr::include_graphics(paste0(plot_path, "R_cl5_plot.png"))

## ----comp_true, eval = chunk$comp_true----------------------------------------
#  MixSim::RandIndex(cl5$data_group, data[, true_group])$AR
#  MixSim::RandIndex(cl5.v2$data_group, data[, true_group])$AR
#  MixSim::RandIndex(cl5$data_group, cl5.v2$data_group)$AR

## ----run10, eval = chunk$run10------------------------------------------------
#  set.seed(12345)
#  t = deltime()
#  cl10 = clustra(data, k = 10, maxdf = 30, conv = c(50, 0.5), mccores = mc)
#  deltimeT(t, paste("run time on", mc, "cores "))

## ----plot10, fig.width = 7, fig.height = 5, eval = chunk$plot10---------------
#  png(paste0(plot_path, "R_cl10_plot.png"), width = 480, height = 360)
#    plot_smooths(data, cl10$tps, max.data = 0, ylim = c(110, 180))
#  dev.off()
#  knitr::include_graphics(paste0(plot_path, "R_cl10_plot.png"))
#  MixSim::RandIndex(cl10$data_group, data[, true_group])$AR

## ----run2, fig.width = 7, fig.height = 5, eval = chunk$run2-------------------
#  set.seed(12345)
#  t = deltime()
#  cl2 = clustra(data, k = 2, maxdf = 30, conv = c(20,0.5), mccores=mc)
#  deltimeT(t, paste("Seconds run time on", mc, "cores "))
#  cat(paste("Number of iterations completed:",cl2$iterations))
#  cat(paste("Number of individuals changing groups in last iteration:",cl2$changes))
#  png(paste0(plot_path, "R_cl2_plot.png"), width = 480, height = 360)
#    plot_smooths(data, cl2$tps, max.data = 0, ylim = c(110, 180))
#  dev.off()
#  knitr::include_graphics(paste0(plot_path, "R_cl2_plot.png"))
#  MixSim::RandIndex(cl2$data_group, data[, true_group])$AR

## ----rand2, comp.within, eval = chunk$rand2-----------------------------------
#  MixSim::RandIndex(cl5$data_group,cl2$data_group)$AR
#  MixSim::RandIndex(cl5$data_group,cl10$data_group)$AR
#  MixSim::RandIndex(cl10$data_group,cl2$data_group)$AR

## ----silplot, fig.width = 7, fig.height = 5, eval = chunk$silplot-------------
#  t = deltime()
#  
#  sil = clustra_sil(cl5, mccores = mc, conv=c(20,0.5))
#  png(paste0(plot_path, "R_cl5_silplot.png"), width = 480, height = 360)
#    lapply(sil, plot_silhouette)
#  dev.off()
#  set.seed(12345)
#  sil2 = clustra_sil(cl2, mccores = mc, conv=c(20,0.5))
#  png(paste0(plot_path, "R_cl2_silplot.png"), width = 480, height = 360)
#    lapply(sil2, plot_silhouette)
#  dev.off()
#  set.seed(12345)
#  sil10 = clustra_sil(cl10, mccores = mc, conv=c(20,0.5))
#  png(paste0(plot_path, "R_cl10_silplot.png"), width = 480, height = 360)
#    lapply(sil10, plot_silhouette)
#  dev.off()
#  knitr::include_graphics(paste0(plot_path, "R_cl5_silplot.png"))
#  knitr::include_graphics(paste0(plot_path, "R_cl2_silplot.png"))
#  knitr::include_graphics(paste0(plot_path, "R_cl10_silplot.png"))
#  deltimeT(t, paste("Seconds silhouettes on", mc, "cores "))

## ----silplot2, fig.width = 7, fig.height = 5, eval = chunk$silplot2-----------
#  # Example running from the data step
#  t = deltime()
#  set.seed(12345)
#  sil = clustra_sil(data, k = c(2, 5, 10), mccores = mc, conv = c(50, 0.5))
#  lapply(sil, plot_silhouette)
#  deltimeT(t, paste("Silhouettes and clustra on", mc, "cores "))

## ----rand_plot, fig.width = 6.5, fig.height=6, eval = chunk$rand_plot---------
#  t = deltime()
#  set.seed(12345)
#  ran = clustra_rand(data, k = seq(2, 10, 1), starts = "random", mccores = mc,
#                     replicates = 10, conv=c(40,0.5), verbose = TRUE)
#  deltimeT(t, paste("Seconds clustra_rand on", mc, "cores "))
#  rand_plot(ran, name = paste0(plot_path, "R_randplot.pdf")) # save pdf version
#  rand_plot(ran) # render png version

## -----------------------------------------------------------------------------
gpred = function(tps, newdata) {
  as.numeric(mgcv::predict.bam(tps, newdata, type = "response",
                               newdata.guaranteed = TRUE))
}

## ----hclust40, fig.width = 7, fig.height = 5, eval = chunk$hclust40-----------
#  set.seed(12345)
#  t = deltime()
#  cl40 = clustra(data, k = 40, maxdf = 30, conv = c(100,0.5), mccores = mc, verbose = TRUE)
#  timep = data.frame(time = seq(min(data$time), max(data$time), length.out = 100))
#  resp = do.call(rbind, lapply(cl40$tps, gpred, newdata = timep))
#  png(paste0(plot_path, "R_hclust_plot.png"), width = 720, height = 480)
#    plot(hclust(dist(resp)))
#  dev.off()
#  knitr::include_graphics(paste0(plot_path, "R_hclust_plot.png"))
#  deltimeT(t, paste("clustra k = 40, conv = c(100, 0.5) on", mc, "cores + hclust + dist "))

## ----SAS_R_data, fig.width = 7, fig.height = 5, eval = chunk$SAS_R_data-------
#  sas.file = function(file, rp = repo_sas) paste0(rp, "sas_results/", file)
#  
#  sas_r_cplot = function(file, clustra_out) {
#    sas = cbind(haven::read_sas(sas.file(file)), source = 1)
#    r = data.frame(time = min(sas$time):max(sas$time))
#    n = nrow(r)
#    k = nrow(sas)/n
#    ## below assumes same time in each group and groups are contiguous.
#    rpred = do.call(c, parallel::mclapply(clustra_out$tps, gpred, newdata = r,
#                                          mc.cores = mc))
#  
#    ## compute mapping from SAS to R using manhattan distance over time
#    rmap = vector("integer", k)
#    m = as.matrix(
#      dist(rbind(t(matrix(sas$pred, n)),
#                 t(matrix(rpred, nrow = n))), "manhattan"))[1:k, (k + 1):(2*k)]
#    for(i in 1:k) { # assign starting with closest pair
#      ind = arrayInd(which.min(m), dim(m))
#      rmap[ind[1]] = ind[2]
#      m[ind[1], ] = Inf
#      m[, ind[2]] = Inf
#    }
#  
#    r = cbind(r, group = sas$group, pred = rpred, source = 2)
#    plot(pred ~ time, data = sas, type = "n")
#    for(i in 1:k) {
#      sr = (1 + (i - 1)*n):(i*n)
#      rr = (1 + (rmap[i] - 1)*n):(rmap[i]*n)
#      lines(sas[sr, ]$time, sas[sr, ]$pred, lty = sas[sr, ]$source, col = i)
#      lines(r[rr, ]$time, r[rr, ]$pred, lty = r[rr, ]$source, col = i)
#    }
#    rmap # output the mapping from SAS clusters to R: R = rmap[SAS]
#  }
#  png(paste0(plot_path, "R_SAS_cl2_compare.png"), width = 480, height = 360)
#    map2 = sas_r_cplot("graphout_cl2.sas7bdat", cl2)
#  dev.off()
#  png(paste0(plot_path, "R_SAS_cl5_compare.png"), width = 480, height = 360)
#    map5 = sas_r_cplot("graphout_cl5.sas7bdat", cl5)
#  dev.off()
#  png(paste0(plot_path, "R_SAS_cl10_compare.png"), width = 480, height = 360)
#    map10 = sas_r_cplot("graphout_cl10.sas7bdat", cl10)
#  dev.off()
#  knitr::include_graphics(paste0(plot_path, "R_SAS_cl2_compare.png"))
#  knitr::include_graphics(paste0(plot_path, "R_SAS_cl5_compare.png"))
#  knitr::include_graphics(paste0(plot_path, "R_SAS_cl10_compare.png"))

## ----Rand_SAS_R, eval = chunk$Rand_SAS_R--------------------------------------
#  MixSim::RandIndex(haven::read_sas(sas.file("rms_cl2.sas7bdat"))$newgroup, cl2$group)
#  MixSim::RandIndex(haven::read_sas(sas.file("rms_cl5.sas7bdat"))$newgroup, cl5$group)
#  MixSim::RandIndex(haven::read_sas(sas.file("rms_cl10.sas7bdat"))$newgroup, cl10$group)

## ----true_group, eval = chunk$true_group--------------------------------------
#  true_group = data[, mean(true_group),by=id]
#  MixSim::RandIndex(haven::read_sas(sas.file("rms_cl5.sas7bdat"))$newgroup, true_group$V1)
#  MixSim::RandIndex(true_group$V1, cl5$group)

## ----echo=FALSE---------------------------------------------------------------
deltimeT(start_time, "clustra vignette run time ")


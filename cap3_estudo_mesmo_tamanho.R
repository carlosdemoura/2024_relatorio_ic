#devtools::load_all("D:/carlos/01_pesquisa/fastan")
devtools::load_all("/home/moura/fastan")
source("utils.R")

############################
#####   Exploratório   #####
############################

cap3_simdata_expl =
  readRDS("light/cap4_tmax_expl.rds") |>
  set_data(simdata = "posterior", seed = 12345) |>
  delete("info", "fit", "diagnostic", "summary") |>
  set_info("simdata expl mesma dim dados tmax")

cmdstanr::write_stan_json(interface(cap3_simdata_expl), "cmd/cap3_simdata_expl.json")
#./examples/test/model sample num_warmup=5000 num_samples=5000 save_warmup=false data file=examples/cap4/cap3_simdata_expl.json output file=examples/cap4/cap3_simdata_expl.csv init=1 random seed=12345

cap3_simdata_expl = 
  cap3_simdata_expl |>
  set_summary(samp = extract_cmd(cmdstanr::read_cmdstan_csv("cmd/cap3_simdata_expl.csv")))
saveRDS(cap3_simdata_expl, "light/cap3_simdata_expl.rds")
cap3_simdata_expl = readRDS("light/cap3_simdata_expl.rds")

accuracy(cap3_simdata_expl)

plot_hpd(cap3_simdata_expl, par = "alpha", col = 1, stat = c("mean", "real")) + text_size(20)
ggsave(img("cap3_expl_alpha"), width = 15, height = 5, dpi = 300, bg = "white")

plot_hpd(cap3_simdata_expl, par = "lambda", row = 1, stat = c("mean", "real")) + text_size(15)
ggsave(img("cap3_expl_lambda"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(cap3_simdata_expl, par = "sigma2", col = 1, stat = c("mean", "real")) + text_size(20)
ggsave(img("cap3_expl_sigma2"), width = 15, height = 5, dpi = 300, bg = "white")


#############################
#####   Confirmatório   #####
#############################

cap3_simdata_conf =
  readRDS("light/cap4_tmax_conf.rds") |>
  set_data(simdata = "posterior", seed = 12345) |>
  delete("info", "fit", "diagnostic", "summary") |>
  set_info("simdata conf mesma dim dados tmax")

cmdstanr::write_stan_json(interface(cap3_simdata_conf), "cmd/cap3_simdata_conf.json")
#./examples/test/model sample num_warmup=5000 num_samples=5000 save_warmup=false data file=examples/cap4/cap3_simdata_conf.json output file=examples/cap4/cap3_simdata_conf.csv init=1 random seed=12345

cap3_simdata_conf = 
  cap3_simdata_conf |>
  set_summary(samp = extract_cmd(cmdstanr::read_cmdstan_csv("cmd/cap3_simdata_conf.csv")))
saveRDS(cap3_simdata_conf, "light/cap3_simdata_conf.rds")

accuracy(cap3_simdata_conf)

plot_contrast(cap3_simdata_conf) + text_size(20)
ggsave(img("cap3_conf_contrast1"), width = 8, height = 8, dpi = 300, bg = "white")

plot_contrast(cap3_simdata_conf, stat = "real") + text_size(20)
ggsave(img("cap3_conf_contrast2"), width = 8, height = 8, dpi = 300, bg = "white")

plot_hpd(cap3_simdata_conf, par = "alpha", col = 1, stat = c("mean", "real"))  + text_size(20)
ggsave(img("cap3_conf_alpha1"), width = 15, height = 5, dpi = 300, bg = "white")

plot_lambda(cap3_simdata_conf, stat = "real") + text_size(15)
ggsave(img("cap3_conf_lambda"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(cap3_simdata_conf, par = "sigma2", col = 1, stat = c("mean", "real"))  + text_size(20)
ggsave(img("cap3_conf_sigma2"), width = 15, height = 5, dpi = 300, bg = "white")


##################################
#####   Semi-confirmatório   #####
##################################

cap3_simdata_sc =
  readRDS("light/cap4_tmax_sc.rds") |>
  invert_signal(2) |>
  set_data(simdata = "posterior", seed = 12345) |>
  delete("info", "fit", "diagnostic", "summary") |>
  set_info("simdata semi.conf mesma dim dados tmax")

cmdstanr::write_stan_json(interface(cap3_simdata_sc), "cmd/cap3_simdata_sc.json")
#./examples/test/model sample num_warmup=5000 num_samples=5000 save_warmup=false data file=examples/cap4/cap3_simdata_sc.json output file=examples/cap4/cap3_simdata_sc.csv init=1 random seed=12345

cap3_simdata_sc = 
  cap3_simdata_sc |>
  set_summary(samp = extract_cmd(cmdstanr::read_cmdstan_csv("cmd/cap3_simdata_sc.csv")))
saveRDS(cap3_simdata_sc, "light/cap3_simdata_sc.rds")

accuracy(cap3_simdata_sc)

plot_hpd(cap3_simdata_sc, par = "alpha", col = 1, stat = c("mean", "real")) + text_size(20)
ggsave(img("cap3_sc_alpha1"), width = 15, height = 5, dpi = 300, bg = "white")

plot_hpd(cap3_simdata_sc, par = "alpha", col = 2, stat = c("mean", "real")) + text_size(20)
ggsave(img("cap3_sc_alpha2"), width = 15, height = 5, dpi = 300, bg = "white")

plot_contrast(cap3_simdata_sc, stat = "mean") + text_size(20)
ggsave(img("cap3_sc_contrast1"), width = 8, height = 8, dpi = 300, bg = "white")

plot_contrast(cap3_simdata_sc, stat = "real") + text_size(20)
ggsave(img("cap3_sc_contrast2"), width = 8, height = 8, dpi = 300, bg = "white")

plot_lambda(cap3_simdata_sc, stat = "real") + text_size(15)
ggsave(img("cap3_sc_lambda"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(cap3_simdata_sc, par = "sigma2", col = 1, stat = c("mean", "real")) + text_size(20)
ggsave(img("cap3_sc_sigma2"), width = 15, height = 5, dpi = 300, bg = "white")


#####################
#####   Geral   #####
#####################

# Associação entre s2 e sigma2 real e sigma2 post

devtools::load_all("D:/carlos/01_pesquisa/meteobr")
devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
library(maps)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")


############################
#####   Exploratório   #####
############################

simdata_expl = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_exploratorio.rds")

plot_hpd(simdata_expl$summary, par = "alpha", col = 1, stat = c("mean", "real")) + theme(text=element_text(size=20))
ggsave(img("cap3_expl_alpha.png"), width = 15, height = 5, dpi = 300, bg = "white")

plot_hpd(simdata_expl$summary, par = "lambda", row = 1, stat = c("mean", "real")) + theme(text=element_text(size=15))
ggsave(img("cap3_expl_lambda.png"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(simdata_expl$summary, par = "sigma2", col = 1, stat = c("mean", "real"))  + theme(text=element_text(size=20))
ggsave(img("cap3_expl_sigma2.png"), width = 15, height = 5, dpi = 300, bg = "white")


#############################
#####   Confirmatório   #####
#############################

simdata_conf = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_confirmatorio.rds")

plot_contrast(simdata_conf$summary) + theme(text=element_text(size=20))
ggsave(img("cap3_conf_contrast1.png"), width = 8, height = 8, dpi = 300, bg = "white")

plot_hpd(simdata_conf$summary, par = "alpha", col = 1, stat = c("mean", "real"))  + theme(text=element_text(size=20))
ggsave(img("cap3_conf_alpha1.png"), width = 15, height = 5, dpi = 300, bg = "white")

plot_lambda(simdata_conf$summary, stat = "real") + theme(text=element_text(size=15))
ggsave(img("cap3_conf_lambda.png"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(simdata_conf$summary, par = "sigma2", col = 1, stat = c("mean", "real"))  + theme(text=element_text(size=20))
ggsave(img("cap3_conf_sigma2.png"), width = 15, height = 5, dpi = 300, bg = "white")

xxx = simdata_conf$model$real$alpha
dim(xxx) = c(dim(xxx), 1)
dimnames(xxx) = list(NULL, NULL, "real")
plot_contrast(list(alpha = xxx), par = "alpha", stat = "real") + labs(title = "Alpha real values") + theme(text=element_text(size=20))
ggsave(img("cap3_conf_contrast2.png"), width = 8, height = 8, dpi = 300, bg = "white")


##################################
#####   Semi-confirmatório   #####
##################################

simdata_sc = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_semi_confirmatorio.rds")

plot_hpd(simdata_sc$summary, par = "alpha", col = 1, stat = c("mean", "real")) + theme(text=element_text(size=20))
ggsave(img("cap3_sc_alpha1.png"), width = 15, height = 5, dpi = 300, bg = "white")

plot_hpd(simdata_sc$summary, par = "alpha", col = 2, stat = c("mean", "real")) + theme(text=element_text(size=20))
ggsave(img("cap3_sc_alpha2.png"), width = 15, height = 5, dpi = 300, bg = "white")

plot_contrast(simdata_sc$summary, stat = "mean") + theme(text=element_text(size=20))
ggsave(img("cap3_sc_contrast1.png"), width = 8, height = 8, dpi = 300, bg = "white")

plot_lambda(simdata_sc$summary, stat = "real") + theme(text=element_text(size=15))
ggsave(img("cap3_sc_lambda.png"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(simdata_sc$summary, par = "sigma2", col = 1, stat = c("mean", "real")) + theme(text=element_text(size=20))
ggsave(img("cap3_sc_sigma2.png"), width = 15, height = 5, dpi = 300, bg = "white")

xxx = simdata_sc$model$real$alpha
dim(xxx) = c(dim(xxx), 1)
dimnames(xxx) = list(NULL, NULL, "real")
plot_contrast(list(alpha = xxx), par = "alpha", stat = "real") + labs(title = "Alpha real values") + theme(text=element_text(size=20))
ggsave(img("cap3_sc_contrast2.png"), width = 8, height = 8, dpi = 300, bg = "white")

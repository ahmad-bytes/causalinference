rm(list = ls())


sesame <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/sesame.txt")
# View(sesame)


tau_sate.treated = mean((sesame$y - sesame$pretest)[sesame$encouraged == 1])

tau_sate.nottreated = mean((sesame$y - sesame$pretest)[sesame$encouraged == 0])

tau_sate.mean = mean((sesame$y - sesame$pretest)[sesame$encouraged == 1]) - mean((sesame$y - sesame$pretest)[sesame$encouraged == 0])
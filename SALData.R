## ---------------------------
##
## Script name: SALData.R
##
## Purpose of script: Averaging of grain-size distribution data obtained using
##                    Shimadzu SALD 2300 laser diffraction analyzer
##
## Author: PhD Dmitrii Borisov
##
## Date Created: 2022-04-19
##
## Email: borisov.ocean@gmail.com
## 


# Подключение библиотек || LIBRARIES-----------------------------
library(readr)
library(stringi)
library(magrittr)
library(SciViews)

# Выбор папки с файлами *.dat || CHOOSE A FOLDER WITH *.DAT FILES-----
if (interactive() && .Platform$OS.type == "windows")
  path <- choose.dir()

#ПОДГОТОВКА ДАННЫХ-----------------------------------------------
# Вектор с полными именами всех dat-файлов в указанной папке || FULL NAMES OF DAT-FILES IN THE FOLDER

fn_full <- dir(path, pattern = "*.dat", full.names = T)

# Вектор с краткими именами всех dat-файлов в указанной папке || SHORT NAMES OF DAT-FILES IN THE FOLDER
fn <- basename(fn_full)

# Импорт данных из файлов в единый список || DATA IMPORT FROM FILES TO A LIST
dat <- lapply(fn_full, 
              readr::read_delim, 
              delim = ":", 
              skip = 15, 
              col_names = F, 
              trim_ws = T)

# вектор с границами гранулометрических классов || BOUNDARIES BETWEEN GRAIN-SIZE CLASSES 
gs <- as.vector(t(dat[[1]][,1]))

# Подсчет количества повторных анализов для каждого образца 
# || ESTIMATION OF ANALYSIS ITERATIONS FOR EACH SAMPLE

  # паттерны и значения для замены в кратких именах фалов для приведения их к единому виду ||
  # PATTERNS & REPLACEMENTS FOR THE FORMATING OF 
patterns <- c(paste("_", c(1:6), ".dat", sep = ""),
              "АНС-", "АНС", "_bulk", "_rem")
replacements <- c(rep("", 6), "ANS", "ANS", "", "")
  # вектор с форматированными краткими именами файлов (ID образов) ||
  # VECTOR WITH FORMATTED SHORT NAMES OF THE DAT-FILES (SAMPLE IDs)
s_id <- stri_replace_all_regex(fn,
                               pattern = patterns,
                               replacement = replacements,
                               vectorize=F)
  # вектор со значениями повторений аналиов для каждого образца в порядке, соотв. вектору fn
  # VECTOR WITH MEASUREMENT ITERATIONS FOR EACH SAMPLE IN ORDER SIMILAR TO fn VECTOR
s <- table(s_id)[match(unique(s_id), names(table(s_id)))]
  # имена значений вектора <- ID образцов || NAMES OF VECTOR VALUES <- SAMPLE IDs
names(s) <- unique(s_id)

# дата фрейм с данными НЕ кумулятивных распределений || DATAFRAME WITH ALL THE NON-CUMULATIVE DISTRIBUTIONS
gs_dist_all <- data.frame(gs)

for(i in 1:length(dat)){
  gs_dist_all <- cbind(gs_dist_all, dat[[i]][, 3])
}
colnames(gs_dist_all) <- c('size um', fn)

# дата фрейм с данными кумулятивных распределений || DATAFRAME WITH ALL THE CUMULATIVE DISTRIBUTIONS
gs_dist_cumm_all <- data.frame(gs)
for(i in 1:length(dat)){
  gs_dist_cumm_all <- cbind(gs_dist_cumm_all, dat[[i]][, 2])
}
colnames(gs_dist_cumm_all) <- c('size um', fn)

#ОСРЕДНЕНИЕ ДАННЫХ || DATA AVERAGING--------------------------------------
  # номер начального остолбца с гран.данными для выборок из датафрейма ||
  # THE NUMBER OF THE FIRST COLUMN WITH GS-DATA
start_col <- 2
  #номера начальных столбцов для остальных выборок из датафрейма ||
  # COLUMN NUMBERS RELATED TO THE BEGINNING OF EACH MEASUREMENT SERIES
subset_from <- as.integer(c(start_col, cumsum(s)+start_col))

#датафрейм для осредненных не кумулятивных распределений
gs_mean_dist <- data.frame(gs)
for(i in 1:length(s)){
  from <- subset_from[i]
  to <- subset_from[i]+s[i]-1
  if(from != to){
    gs_mean_dist <- cbind(gs_mean_dist, rowMeans(gs_dist_all[,from:to]))
  } else{
    gs_mean_dist <- cbind(gs_mean_dist, gs_dist_all[,from])
  }
  
}
colnames(gs_mean_dist) <- c('size um', names(s))

gs_mean_dist_cumm <- data.frame(gs)
for(i in 1:length(s)){
  from <- subset_from[i]
  to <- subset_from[i]+s[i]-1
  if(from != to){
    gs_mean_dist_cumm <- cbind(gs_mean_dist_cumm, rowMeans(gs_dist_cumm_all[,from:to]))
  }else{
    gs_mean_dist_cumm <- cbind(gs_mean_dist_cumm, gs_dist_cumm_all[,from])
  }
}
colnames(gs_mean_dist_cumm) <- c('size um', names(s))

# ЗАПИСЬ ОСРЕДНЕННЫХ ДАННЫХ В ФАЙЛЫ || WRITTING THE AVERAGED DATA TO FILES--------

write.table(gs_mean_dist, 
            file = paste(path,"//ANS45-35_av_bulk.dat", 
            sep = "", 
            collapse = ""), 
            append = FALSE, sep = "\t", 
            row.names = F)

write.table(gs_mean_dist_cumm, 
            file = paste(path,"//ANS45-35_av_cumm_bulk.dat", 
            sep = "", collapse = ""), 
            append = FALSE, 
            sep = "\t", 
            row.names = F)


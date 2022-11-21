#' data preparation for word cloud
#' @author  Wenbo Chen
#' @date    Oct., 2022
#' @progress done


# loading pkgs and data ---------------------------------------------------
## library(devtools)
# install_github("qinwf/jiebaR") #install jieba developing version
library(tidytext)
library(tidyverse)
library(wordcloud2)
library(RColorBrewer)
library(jiebaR)
library(readtext)
library(quanteda)

load("/Users/jameschen/Documents/02_Teaching/06_r4ds/slides/data/lj_sh_2019.RData")

wk <- worker()
property_dic <- c("村",
                   "花园",
                  "家园",
                  "景园",
                   "小区",
                   "城",
                   "街坊",
                   "苑",
                  "邸",
                   "公馆",
                   "华庭",
                   "豪园",
                   "公寓",
                  "花苑",
                  "佳苑",
                  "弄",
                  "里",
                  "廷",
                  "坊",
                  "庭",
                  "广场",
                  "湾",
                  "大楼",
                  "名邸",
                  "邨",
                  "居")

new_user_word(wk,property_dic)

wordcloud2(freq(segment(lj$property_name,wk)),shape = "star")

?wordcloud2()




fre <- vector()
for(i in 1:length(property_dic)){
   fre[i] <- sum(str_detect(lj$property_name,property_dic[i] ))
}

df <- tibble(
  word = property_dic,
  fre
)

wordcloud(df,
           size = 1,
           minSize = 1)

segment(lj$property_name,wk)


## code to prepare `DATASET` dataset goes here
# 这段代码的目的是创建一个名为"DATASET"的数据集，并将其准备好以供后续使用。
usethis::use_data(DATASET, overwrite = TRUE)
names = c("White Li","Great Meng","Pure-lake Tao","Easy-life Bai")
born = c(701,NA,365,772)
DATASET = data.frame(name = names, born = born)

usethis::use_data(DATASET, overwrite = TRUE)

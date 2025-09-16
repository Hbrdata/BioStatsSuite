#' crosstable
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#######实验室交叉表#########

# 用途：实验室交叉表

#基本参数：
# data_cond=                 要分析的数据集|<条件>
# group_c=                   组别变量|组别1/组别2/……；不能为缺失
# missing=                  将缺失缺失值填补为XX；填写内容为format参数中等号左边的内容
# row_colvar=                横向分析的变量（即首行显示的变量）/横向变量标签|纵向分析的变量（即首列显示的变量）/纵向变量标签；一般横向分析的变量（row_var）使用“治疗前”
# format=                    分析变量的内容和标签；格式为：内容1=标签1|内容2=标签2|……；若内容为char,则需要带''
# table_title=                表格的标题
# footnote=                   表格的footnote

#示例

# adcrslb <-read_excel("E:/Rlanguage/2 - system/函数/7.crosstable/SAS程序与数据/adcrslb.xlsx")

# crosstable(
# data_cond="adcrslb|RANDYN=='是' & SS=='是' & visitnum=='2' & lbtest=='白细胞数'"
# group_c="arm3|试验组/阳性药组/安慰剂组"
# missing=4
# row_colvar="LBCLSIG_1/治疗前|LBCLSIG/治疗后"
# format="1=正常|2=异常无临床意义|3=异常有临床意义|4=未查"
# table_title="用药后6周±3天血常规指标（白细胞计数）的临床意义变化情况（SS）"
# footnote="(％)表示以治疗前每种情况分别作为分母计算治疗后每种情况变化的百分数"
# )





c_crosstable <- function(data_cond,group_c, missing, row_colvar, format,table_title, footnote)
{

  library(readxl)
  library(dplyr)
  library(table1)
  library(tibble)
  library(kableExtra)
  library(officer)
  library(flextable)
  library(rlang)
  library(gmodels)
  library(tidyr)
  library(purrr)


  ################## 拆分组别、分析变量 ################################

  #拆分组别变量，组别名称；计算组别个数
  grp_part <-  unlist(strsplit(group_c, "|", fixed = TRUE))

  grpvar_ <-  grp_part[1]
  grpnames_ <- grp_part[2]

  s_ <- 1
  grpnames_ <- unlist(strsplit(grpnames_, "/", fixed = TRUE))
  #建立list来存储连续生成的组别名称
  cat_grpname <- list()

  while (s_ <= length(grpnames_) && grpnames_[s_] != "") {
    cat_grpname[[s_]] <- grpnames_[s_]
    s_ <- s_ + 1
    grp_num=s_ - 1
  }

  #拆分分析的变量和标签
  rowcolvarlabel_ <- unlist(strsplit(row_colvar, "|", fixed = TRUE))
  rowvar_ <- strsplit(rowcolvarlabel_[1],"/",fixed = TRUE)[[1]][1]
  rowlabel_ <- strsplit(rowcolvarlabel_[1],"/",fixed = TRUE)[[1]][2]

  colvar_ <- strsplit(rowcolvarlabel_[2],"/",fixed = TRUE)[[1]][1]
  collabel_ <- strsplit(rowcolvarlabel_[2],"/",fixed = TRUE)[[1]][2]

  #拆分分析变量中内容的具体分类，和标签
  # 使用strsplit分割字符串

  s_ <- 1
  catlist_ <- unlist(strsplit(format, "|", fixed = TRUE))
  #建立list来存储连续生成的变量名称
  catcont_  <- list()
  catlabel_ <- list()

  while (s_ <= length(catlist_) && catlist_[s_] != "") {
    # 假设每个部分都是"内容=标签"的形式
    # 使用strsplit再次分割当前部分

    part_split <- unlist(strsplit(catlist_[s_], "=", fixed = TRUE))
    # var_parts_3 <- unlist(strsplit(catlist_[s_], "=", fixed = TRUE))

    catcont_[[s_]] <- unlist(strsplit(catlist_[s_], "=", fixed = TRUE))[1]
    catlabel_[[s_]] <- unlist(strsplit(catlist_[s_], "=", fixed = TRUE))[2]

    # 索引递增
    s_ <- s_ + 1
    catnum_=s_-1
  }



  anavarorder_ <- data.frame(
    colanavar_ = NA,
    colanavarlabel_ = NA,
    colvarcd_ = NA,
    colanavarorder_ = NA,
    grpcd_ = NA,
    stringsAsFactors = FALSE
  )

  row_index <- 1

  for (s_ in 1:grp_num) {
    for (x_ in 1:catnum_) {
      anavarorder_[row_index,"colanavar_"] <- catcont_[[x_]]
      anavarorder_[row_index,"colanavarlabel_"] <- catlabel_[[x_]]
      anavarorder_[row_index,"colvarcd_"] <- x_
      anavarorder_[row_index,"colanavarorder_"] <- x_
      anavarorder_[row_index,"grpcd_"] <- s_
      row_index <- row_index+1
    }
  }

  data_cond_part <- unlist(strsplit(data_cond,"|",fixed = TRUE))

  data_n_ <- data_cond_part[1]
  cond_n_ <- data_cond_part[2]

  data_0 <- get(data_n_)
  cond_n_ <- parse_expr(cond_n_)
  data_0 <- data_0  %>%
    filter(!!cond_n_) #根据条件筛选出数据框

  group_cond <- c(grpnames_)

  data_0 <- data_0 %>%
    filter(.data[[grpvar_]] %in% group_cond )

  row_var_expr <- rlang::ensym(rowvar_)
  col_var_expr <- rlang::ensym(colvar_)
  group_expr <- rlang::ensym(grpvar_)

  d_0 <- data_0 %>%
    select({{row_var_expr}},{{col_var_expr}},{{group_expr}})
  d_0 <- setNames(d_0,c("row_0","col_0","group_0"))
  d_0$grpcd_ <- NA
  d_0$colvarcd_ <- NA
  d_0$rowvarcd_ <- NA


  for (i in 1:nrow(d_0)){
    for (s_ in 1:grp_num) {
      if (d_0$group_0[i] == cat_grpname[[s_]]){
        d_0$grpcd_[i] <-s_
        break
      }

    }

  }


  for (i in 1:nrow(d_0)) {
    if (is.na(d_0$row_0[i])){
      d_0$row_0[i] <- missing
    }
    if (is.na(d_0$col_0[i])){
      d_0$col_0[i] <- missing
    }
  }

  for (i in 1:nrow(d_0)){
    for (s_ in 1:catnum_) {
      if (d_0$row_0[i]==catcont_[[s_]]) {
        d_0$rowvarcd_[i] <- s_
      }

      if (d_0$col_0[i]==catcont_[[s_]]) {
        d_0$colvarcd_[i] <- s_
      }
    }
  }



  #生成频数表
  n1_ <- data.frame(ftable(d_0,row.vars = c("colvarcd_","rowvarcd_"),col.vars = "grpcd_"))
  n1_ <- n1_ %>%
    pivot_wider(names_from = rowvarcd_, values_from = Freq, values_fill = 0)
  char_vector <- unlist(lapply(catcont_, as.character))
  n1_ <- n1_ %>%
    select(grpcd_,colvarcd_,char_vector)
  n1_$total <- rowSums(n1_[,!names(n1_) %in% c("grpcd_","colvarcd_")])
  n1_$grpcd_ <- as.character(n1_$grpcd_)
  n1_$colvarcd_ <- as.character(n1_$colvarcd_)


  n2_<-list()
  for (i in 1:grp_num){
    na_row <- rep(NA, ncol(n1_))
    na_row <- as.data.frame(t(na_row))
    names(na_row) <- names(n1_)
    na_row[1,1] <- "row_total"
    na_row[1,2] <- "row_total"
    p_name <- paste0("p_", 1:catnum_)
    np_name <- paste0("np_", 1:(catnum_))
    denom_cols <- c(p_name,"p_total",np_name,"np_total")
    n2_[[i]] <- n1_ %>% filter(grpcd_ %in% i)
    inter_frame <- as.data.frame(n2_[[i]])
    n2_[[i]] <- rbind(inter_frame,na_row)
    n2_col_total <- data.frame(n2_[[i]]) %>%
      select(-c(grpcd_, colvarcd_)) %>% # 选择除了grpcd_和colvarcd_以外的所有列
      summarise_all(list(~sum(., na.rm = TRUE)))
    for (s_ in 1:(catnum_+1)){
      n2_[[i]][catnum_+1,2+s_] <- n2_col_total[1,s_]
      # n2_[[i]]$row_total[s_] <- n2_col_total[1,s_]
      n2_[[i]][denom_cols] <- NA

    }

  }

  for (i in 1:grp_num) {
    for (s_ in 1:(catnum_+1)) {
      for (z_ in 1:(catnum_+1)){
        n2_[[i]][z_,(3+catnum_+s_)] <-  sprintf("%.2f",((n2_[[i]][z_,(2+s_)])/(n2_[[i]][(catnum_+1),(2+s_)]))*100)
      }
    }
  }

  for (i in 1:grp_num) {
    for (s_ in 1:(catnum_+1)) {
      if(n2_[[i]][(catnum_+1),(2+s_)] == 0){
        n2_[[i]][,(3+catnum_+s_)] <- "0.00"
      }

    }
  }

  for (i in 1:grp_num) {
    for (s_ in 1:(catnum_+1)) {
      for (z_ in 1:(catnum_+1)){
        n2_[[i]][z_,(4+catnum_+catnum_+s_)] <- paste((n2_[[i]][z_,(2+s_)]),"(",n2_[[i]][z_,(3+catnum_+s_)],"%",")")
        n2_[[i]][catnum_+1,(4+catnum_+catnum_+s_)] <-(n2_[[i]][(catnum_+1),(2+s_)])
      }
    }
  }







  n3_ <- list()
  for (i in 1:grp_num){
    np_name <- paste0("np_", 1:(catnum_))
    n3_[[i]] <- n2_[[i]] %>% select(colvarcd_,np_name,np_total)
    for (s_ in 1:catnum_){
      n3_[[i]]$colvarcd_[1+catnum_]<-"合计"
    }
    na_row <- rep(NA, ncol(n3_[[i]]))
    names(na_row) <- names(n3_[[i]])
    n3_[[i]] <- rbind(na_row,n3_[[i]])
    n3_[[i]][1,1] <- cat_grpname[[i]]
    for (z_ in 1:catnum_) {
      n3_[[i]][z_+1,1] <- catlabel_[[z_]]
    }
    col_name <- unlist(catlabel_)
    row_col_name <- paste(collabel_,"/",rowlabel_ )
    names(n3_[[i]]) <- c(row_col_name,col_name,"合计")
  }


  table_out <-  bind_rows(n3_)
  ft <- flextable(table_out)
  ft <- color(ft,part = 'footer', color = 'black')
  ft <- set_caption(ft,caption = table_title)
  # ft<-font(ft,fontname="SimSun",part="all")
  ft<-font(ft,fontname="Times New Roman",part="all")
  ft<-hline_top(ft,border = fp_border_default(color="black",width=1.5),part="header")
  ft<-hline_bottom(ft,border = fp_border_default(color="black",width=1.5),part="body")
  ft<-hline(ft,i=1,border = fp_border_default(color="black",width=1),part="header")
  ft <- add_footer_lines(ft,footnote)
  rm(table_out,envir = .GlobalEnv)
  ft <- autofit(ft)
  ft
}

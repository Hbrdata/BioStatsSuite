#' c_describe
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
#######分类变量描述性统计函数#########

# 用途：用于对数据框中数据进行分组统计学描述

#基本参数：
# data_cond   =        要进行统计描述的数据集|<筛选条件（即当做分子的n）>
# denominator_cond =   当做分母的数据集的名称|条件
# varlist =            分析的变量|变量标签|分类1=分类1标签/分类2=分类2标签/……
# coltotal =           是否输出列合计（最后一行的“合计”）；默认为输出，如果coltotal=0,则不输出
# rowtotal =           是否输出行合计（最后一列的“合计”）；默认为输出，如果rowtotal=0,则不输出
# outyn =              是否输出表格；默认为outyn=1(输出表格)；outyn=0为不输出表格，后续表格会进行累加
# title =              表格的title
# footnote =           表格的footnote


#示例

# adsl<-read_excel("E:/Rlanguage/2 - system/R/R/rfile/数据/adsl.xlsx")
#
# c_describe(data_cond ="adsl|FAS!=''"
#            ,denominator_cond="adsl|FAS!=''"
#            ,varlist= "sex|性别|'男'=男/'女'=女"
#            ,group_c= 'arm3|试验组/对照组/安慰剂组'
#            ,coltotal=1
#            ,rowtotal=1
#            ,outyn=1
#            ,table_title="标题"
#            ,ftnote="底注")





c_describe <- function(data_cond,denominator_cond,varlist,group_c,coltotal,rowtotal,outyn=1,table_title,ftnote)
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
  ##########################拆分分析变量及标签
  varlist_parts_1 <- unlist(strsplit(varlist, "|", fixed = TRUE))
  anavar_ <-  varlist_parts_1[1]
  avalabel_ <- varlist_parts_1[2]
  catlist_ <- varlist_parts_1[3]

  # 使用strsplit分割字符串

  s_ <- 1
  var_parts_2 <- unlist(strsplit(catlist_, "/", fixed = TRUE))
  #建立list来存储连续生成的变量名称
  catcont_  <- list()
  catlabel_ <- list()

  while (s_ <= length(var_parts_2) && var_parts_2[s_] != "") {
    # 假设每个部分都是"内容=标签"的形式
    # 使用strsplit再次分割当前部分

    part_split <- unlist(strsplit(var_parts_2[s_], "=", fixed = TRUE))
    var_parts_3 <- unlist(strsplit(var_parts_2[s_], "=", fixed = TRUE))

    catcont_[[s_]] <- var_parts_3[1]
    catlabel_[[s_]] <- var_parts_3[2]

    # 索引递增
    s_ <- s_ + 1
    catnum_=s_-1
  }

  #制作输出数据集的全部cat，并进行排序

  cat_ <- data.frame(
    catlabel_ = character(catnum_),
    catorder_ = numeric(catnum_),
    stringsAsFactors = FALSE
  )

  for (i in 1:catnum_) {
    cat_$catlabel_[i] <- catlabel_[[i]]
    cat_$catorder_[i] <- i
  }

  cat_999 <- list(catlabel_ = '合计',catorder_=999)
  cat_ <- rbind(cat_,cat_999)

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

  var_expr <- rlang::ensym(anavar_)
  group_expr <- rlang::ensym(grpvar_)

  d_0 <- data_0 %>%
    select({{var_expr}},{{group_expr}})
  d_0 <- setNames(d_0,c("var_0","group_0"))
  d_0$grpcd_ <- NA
  d_0$catorder_ <- NA

  #拆分数据集及条件（分子numerator），并且给组别排序

  for (i in 1:nrow(d_0)){
    for (s_ in 1:grp_num) {
      if (d_0$group_0[i] == cat_grpname[[s_]]){
        d_0$grpcd_[i] <-s_
        break
      }

    }
    for (i in 1:nrow(d_0)){
      for (s_ in 1:catnum_) {
        if (!is.na(d_0$var_0[i]) && d_0$var_0[i] == parse_expr(catcont_[[s_]])) {
          d_0$catorder_[i] <- s_
          break
        }
      }
    }
  }
  #为每个组别增加一条观测444，防止有的组别没有观测，会错乱
  d_0_newrow <- data.frame(
    group_0 = character(grp_num),
    grpcd_ = integer(grp_num),
    catorder_ = 444
  )

  for (s_ in 1:grp_num) {
    d_0_newrow$group_0[s_] <- grpnames_[s_]
    d_0_newrow$grpcd_[s_] <- s_
  }

  d_0 <- bind_rows(d_0, d_0_newrow)
  ######计算分子
  #计算频数
  n1_ <- data.frame(CrossTable(d_0$catorder_,d_0$grpcd_))
  n1_ <- n1_ %>% select(t.x,t.y,t.Freq)
  #跨栏操作重塑数据框
  n1_ <- n1_ %>%
    pivot_wider(names_from = t.y, values_from = t.Freq, values_fill = 0)
  #添加总计行
  total_c_0 <- colSums(n1_[, -c(1)], na.rm = TRUE)  # 计算除了第一列和第二列之外的总和
  total_c_0 <- data.frame(total_c_0)
  total_c <- as.data.frame(t(total_c_0))
  n1_ <- bind_rows(n1_,total_c)

  #添加总计列
  n1_$n999_ <- rowSums(n1_[, -c(1)])

  n1_ <- n1_ %>%
    rename(
      catorder_ =  t.x
    )
  # 由于前面每个组别插入了一行444，所以在这个地方要将合计都删除1个，找到合计的数值
  n1_$BREAK_ <- NA
  n1_$catorder_ <- as.integer(as.character(n1_$catorder_))
  n1_$BREAK_[is.na(n1_$catorder_)] <- 'RBREAK_'
  n1_$catorder_[is.na(n1_$catorder_)] <- 999

  for (s_ in 1:grp_num) {
    n1_[n1_$catorder_ == 999,s_+1] <- (n1_[n1_$catorder_ == 999,s_+1] )-1

  }

  n1_ <- n1_ %>%
    mutate(
      n999_ = if_else(!is.na(BREAK_) , n999_-grp_num, n999_),
    )
  #拆分分母数据集及条件（分母denominator），并且给组别排序
  denominator_cond_part <- unlist(strsplit(data_cond,"|",fixed = TRUE))

  data_d_ <- denominator_cond_part[1]
  cond_d_ <- denominator_cond_part[2]

  data_1 <- get(data_d_)
  cond_d_ <- parse_expr(cond_d_)
  data_1 <- data_1  %>%
    filter(!!cond_d_) #根据条件筛选出数据框

  group_cond <- c(grpnames_)

  data_1 <- data_1 %>%
    filter(.data[[grpvar_]] %in% group_cond )

  var_expr <- rlang::ensym(anavar_)
  group_expr <- rlang::ensym(grpvar_)

  d_1 <- data_1 %>%
    select({{var_expr}},{{group_expr}})
  d_1 <- setNames(d_1,c("var_0","group_0"))
  d_1$grpcd_ <- NA

  #拆分数据集及条件（分子numerator），并且给组别排序

  for (i in 1:nrow(d_1)){
    for (s_ in 1:grp_num) {
      if (d_1$group_0[i] == cat_grpname[[s_]]){
        d_1$grpcd_[i] <-s_
        break
      }

    }
  }

  n2_ <- d_1 %>%
    group_by(grpcd_) %>%
    summarise(
      n = n(),  # 计数每个组的观测值数量
    )

  n2_ <- as.data.frame(t(n2_))
  col_names <- as.character(n2_[1, ])  # 提取第一行作为列名，转换为字符型
  names(n2_) <- col_names  # 设置新的列名

  n2_ <- n2_[-1, ]  # 删除第一行
  names(n2_) <- col_names  # 设置新的列名

  n2_$d999_ <-0

  n2_$d999_ <- rowSums(n2_)

  n2_$BREAK_ <- NA

  ##################将连续命名的变量提取###########
  need_col <- paste0('', 1:grp_num )

  n3_ <- left_join(cat_, n1_, by = "catorder_") %>%
    select(catorder_,catlabel_,BREAK_,all_of(need_col))

  for (i in 1:grp_num) {
    for (s_ in 1:catnum_) {
      if(is.na(n3_[s_,3+i]) ){
        n3_[s_,3+i] = 0
      }
    }
  }

  #########################整合结果
  n_name <- paste0("n_", 1:grp_num)
  d_name <- paste0("d_", 1:(grp_num))
  p_name <- paste0("p_", 1:grp_num)
  denom_cols <- c(d_name, p_name)
  n3_[denom_cols] <- NA

  n3_$n999_ <- NA
  n3_$d999_ <- NA
  n3_$p999_ <- NA

  n999_index <- which(names(n3_) == "n999_")
  d999_index <- which(names(n3_) == "d999_")
  p999_index <- which(names(n3_) == "p999_")
  catorder_999_r <- which(n3_$catorder_ == 999)

  for (i in 1:grp_num) {
    for (s_ in 1:catnum_) {
      n3_[s_,3+grp_num+i] <- n3_[n3_$catorder_ == 999,3+i]
      n3_[s_,3+grp_num+grp_num+i] <-sprintf("%.2f",( n3_[s_,3+i]/n3_[n3_$catorder_ == 999,3+i])*100)
      start_col <- 4  # 第四列的索引
      end_col <- 3 + grp_num  # 结束列的索引
      n3_$n999_ <- rowSums(n3_[,start_col:end_col ])
    }
  }

  for (i in 1:catnum_) {
    n3_[i,d999_index] <- n3_[catorder_999_r,n999_index]
    n3_[catorder_999_r,d999_index] <- n2_[1,grp_num+1]
    n3_[i,p999_index] <- sprintf("%.2f",( n3_[i,n999_index]/n3_[i,d999_index])*100)
  }

  n4_ <- n3_
  np_name <- paste0("np_", 1:grp_num)
  np_cols <- c(np_name)
  n4_[np_cols] <- NA
  n4_$np999_ <- NA
  np999_index <- which(names(n4_) == "np999_")

  for (i in 1:grp_num) {
    for (s_ in 1:catnum_) {
      n4_[s_,3+3+(grp_num*3)+i] <-  paste( n4_[s_,3+i] ,'(',n4_[s_,3+(grp_num*2)+i],')')
      n4_[s_,np999_index] <- paste( n4_[s_,which(names(n4_) == "n999_")] ,'(',n4_[s_,which(names(n4_) == "p999_")] ,')')
    }

    n4_[which(n4_$catorder_ == 999) ,3+3+(grp_num*3)+i] <- paste( n4_[which(n4_$catorder_ == 999) ,3+i],'(',n2_[1,i]-n4_[which(n4_$catorder_ == 999) ,3+i] ,')')

    n4_[which(n4_$catorder_ == 999) ,which(names(n4_) == "np999_")] <- paste(
      n4_[which(n4_$catorder_ == 999) ,which(names(n4_) == "n999_")],
      '(',
      n4_[which(n4_$catorder_ == 999) ,which(names(n4_) == "d999_")]-n4_[which(n4_$catorder_ == 999) ,which(names(n4_) == "n999_")],
      ')'
    )
  }

  ###########最终数据列提取

  t_0 <- n4_%>%
    select(catlabel_,all_of(np_name),np999_)
  #创建与t_0变量名称与变量数量相同且只有一行空行的数据框
  t_1 <- data.frame(matrix(NA, nrow = 1, ncol = ncol(t_0), dimnames = list(NULL, names(t_0))))

  #为数据添加分析变量label
  t_2 <- bind_rows(t_1,t_0)
  t_2[1,1] <- avalabel_
  t_2<<-t_2

  ####制作表头

  title_0 <- d_1 %>%
    group_by(grpcd_) %>%
    summarise(
      n = n(),  # 计数每个组的观测值数量
    )

  title_0_1 <- d_1 %>%
    summarise(
      grpcd_ = 999,
      n = n(),  # 计数每个组的观测值数量
    )

  title_0 <- bind_rows(title_0,title_0_1)
  title_0$grp_name <- c(grpnames_,'合计')
  title_0$grp_n <- paste(title_0$grp_name,'\n(n = ',title_0$n, ')')
  title_0 <- title_0 %>% select( grp_n )
  title_0 <- data.frame(t(title_0))

  NA_column <- rep(NA, nrow(title_0))  # 使用rep()函数创建一个长度为nrow(df)的NA向量

  # 使用cbind()函数将新列添加到df的最左侧
  title_0 <- cbind(NA_column, title_0)

  #数据框重命名
  names(title_0) <- c('catlabel_',all_of(np_name),'np999_')

  if (coltotal==0){
    t_2 <- subset(t_2, catlabel_ != '合计')
  }

  if (rowtotal==0){
    title_0 <- title_0[, -which(names(title_0) == "np999_")]
    t_2 <- t_2[, -which(names(t_2) == "np999_")]
  }

  if (exists('table_out')==FALSE){
    table_out<-title_0
  }

  table_out <- bind_rows(table_out,t_2)
  table_out <<-table_out

  col_names <- table_out[1, ]
  col_names[1,1] <- '  '
  table_out <- slice(table_out, -1)# 移除第一行
  names(table_out) <- col_names # 将第一行的值设置为列名


  ft<-if(outyn==1){
    #绘制表格

    ft <- flextable(table_out)
    ft <- color(ft,part = 'footer', color = 'black')
    ft <- set_caption(ft,caption = table_title)
    ft<-font(ft,fontname="SimSun",part="all")
    ft<-font(ft,fontname="Times New Roman",part="all")
    ft<-hline_top(ft,border = fp_border_default(color="black",width=1.5),part="header")
    ft<-hline_bottom(ft,border = fp_border_default(color="black",width=1.5),part="body")
    ft<-hline(ft,i=1,border = fp_border_default(color="black",width=1),part="header")
    ft <- add_footer_lines(ft,ftnote)
    rm(table_out,t_2,envir = .GlobalEnv)
    ft <- autofit(ft)
    # ft <- set_table_properties(layout = "autofit", width = 1)
    ft

  }
  ft
}



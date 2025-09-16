#' q_describe
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
#'
#######连续型变量描述性统计函数#########

# 用途：用于对数据框中数据进行分组统计学描述

#基本参数：
# data_name   = 用于分析的数据框
# data_cond   = 数据筛选条件
# var_name    = 用于分析的变量名称
# var_label   = 变量标签
# group_name  = 分组变量名称
# group_cond  = 需要分析的组别
# table_title = 表名
# ftnote      = 底注
# totalyn     = 是否输出合计列，0：否；其他值：是
# outyn       = 是否输出表格，其他值：否；1：是

#示例

# adsl<-read_excel("E:/Rlanguage/2-system/R/R/rfile/数据/adsl.xlsx")
#
# q_describe(data_name = "adsl"
#            ,data_cond = "FAS!=''"
#            ,var_name="HEIGHT"
#            ,var_label="我是标签"
#            ,group_name="arm3"
#            ,group_cond=c('对照组','试验组','安慰剂组')
#            ,table_title="我是表格名称"
#            ,ftnote="我是底注"
#            ,totalyn=1
#            ,outyn=1)



q_describe<-function(data_name,data_cond,var_name,var_label,group_name,group_cond,table_title,ftnote,totalyn,outyn=1)
{
  library(readxl)
  library(dplyr)
  library(table1)
  library(tibble)
  library(kableExtra)
  library(officer)
  library(flextable)
  library(rlang)

  ##############根据条件创建数据框###########
  ############## 处理 group_cond 参数 ##############
  # 如果 group_cond 是字符串，按逗号分割并处理
  if (is.character(group_cond) && length(group_cond) == 1) {
    # 分割字符串并去除前后空格
    group_cond <- unlist(strsplit(group_cond, ",\\s*"))
    group_cond <- trimws(group_cond)

    # 处理可能的中文引号或其他特殊字符
    group_cond <- gsub("['\"`]", "", group_cond)  # 移除引号
  }

  # 检查 group_cond 是否有效
  if (length(group_cond) == 0 || all(group_cond == "")) {
    stop("分组条件 group_cond 无效或为空")
  }

  ##############根据条件创建数据框###########
  data_0 <- get(data_name)


    data_cond_0 <- parse_expr(data_cond)


    data_0 <- data_0  %>%
      filter(!!data_cond_0) #根据条件筛选出数据框


  # # 检查分组变量是否存在
  # if (!group_name %in% names(data_0)) {
  #   stop(paste("分组变量", group_name, "在数据集中不存在"))
  # }
  #
  # # 检查分析变量是否存在
  # if (!var_name %in% names(data_0)) {
  #   stop(paste("分析变量", var_name, "在数据集中不存在"))
  # }

  data_0 <- data_0 %>%
    filter(.data[[group_name]] %in% group_cond )

  # data_0_out <<- data_0

  # var_expr <- rlang::ensym(var_name)
  # group_expr <- rlang::ensym(group_name)
  #
  var_expr <- var_name
  group_expr <- group_name


  d_0 <- data_0 %>%
    select(all_of(c(var_expr, group_expr)))

  # d_0_out <<- d_0


  d_1 <- setNames(d_0,c("var_0","group_0"))

  d_1$group_0 <- factor(d_1$group_0, levels = group_cond)   #将分组变量因子化，并按照输入顺序等级赋值，保证
  #输入顺序即为显示顺序
  label(d_1$var_0)<-var_label
  # d_1_out <<- d_1


  #制作表头
  title_0 <- d_1 %>%
    group_by(group_0) %>%
    filter(group_0 %in% group_cond) %>%  # 使用filter挑选分组
    summarise(
      n = n() # 计数每个组的观测值数量
    )

  title_0_1 <- d_1 %>%
    summarise(
      group_0 = '合计',
      n = n(),  # 计数每个组的观测值数量
    )

  title_0 <- bind_rows(title_0,title_0_1)

  title_0$grp_n = paste(title_0$group_0, '\n(n = ', title_0$n, ')')

  title_0 <- title_0 %>% select(grp_n)

  title_0_0 <- rep(list(NA), ncol(title_0))
  names(title_0_0) <- names(title_0)  # 确保新行的列名与df相同

  # 创建一个新的数据框，先添加新行，再添加原始数据框
  title_0 <- rbind(title_0_0, title_0)

  title_0[1,1]<- '  '

  title_0 <- as.data.frame(t(as.matrix(title_0)))

  col_names <- as.character(title_0[1, ])  # 提取第一行作为列名，转换为字符型
  title_name <- as.character(title_0[1, ])
  title_0 <- title_0[-1, ]  # 删除第一行
  names(title_0) <- col_names  # 设置新的列名


  # title_0_out <<- title_0

  #进行描述性统计

  s_0 <- d_1 %>%
    group_by(group_0) %>%
    filter(group_0 %in% group_cond) %>%  # 使用filter挑选分组
    summarise(
      mean = sprintf('%.2f',mean(var_0, na.rm = TRUE)),  #na.rm=TRUE 表示对于数据中的NA值，确保函数在计算时值考虑非缺失值，得到一个
      #基于有效数据的统计结果
      median = sprintf('%.2f',median(var_0, na.rm = TRUE)),
      Q1 = sprintf('%.2f',quantile(var_0,probs = 0.25, type = 2,na.rm = TRUE)),
      Q2 = sprintf('%.2f',quantile(var_0,probs = 0.50, type = 2,na.rm = TRUE)),
      Q3 = sprintf('%.2f',quantile(var_0,probs = 0.75, type = 2,na.rm = TRUE)),
      sd = sprintf('%.2f',sd(var_0, na.rm = TRUE)),
      min = sprintf('%.2f',min(var_0, na.rm = TRUE)),
      max = sprintf('%.2f',max(var_0, na.rm = TRUE)),
      n = sum(!is.na(var_0)),  # 计数每个组的观测值数量
      missing = sum(is.na(var_0)),
      N_Missing = paste(n,'(',missing,')',sep = ''),
      Mean_SD = paste(mean,'(',sd,')',sep=''),
      Median_Q1_Q3 = paste(median,'(',Q1,',',Q3,')',sep = ''),
      Min_Max = paste(min,',',max)
    )

  #总计列

  s_1 <- d_1 %>%
    summarise(
      group_0 = '合计',
      mean = sprintf('%.2f',mean(var_0, na.rm = TRUE)),  #na.rm=TRUE 表示对于数据中的NA值，确保函数在计算时值考虑非缺失值，得到一个
      #基于有效数据的统计结果
      median = sprintf('%.2f',median(var_0, na.rm = TRUE)),
      Q1 = sprintf('%.2f',quantile(var_0,probs = 0.25,type = 2, na.rm = TRUE)),
      Q2 = sprintf('%.2f',quantile(var_0,probs = 0.50, type = 2,na.rm = TRUE)),
      Q3 = sprintf('%.2f',quantile(var_0,probs = 0.75,type = 2,na.rm = TRUE)),
      sd = sprintf('%.2f',sd(var_0, na.rm = TRUE)),
      min = sprintf('%.2f',min(var_0, na.rm = TRUE)),
      max = sprintf('%.2f',max(var_0, na.rm = TRUE)),
      n = sum(!is.na(var_0)),  # 计数每个组的观测值数量
      missing = sum(is.na(var_0)),
      N_Missing = paste(n,'(',missing,')',sep = ''),
      Mean_SD = paste(mean,'(',sd,')',sep=''),
      Median_Q1_Q3 = paste(median,'(',Q1,',',Q3,')',sep = ''),
      Min_Max = paste(min,',',max)
    )

  s_2 <- bind_rows(s_0,s_1)

  s_2$grp_n = paste(s_2$group_0, '\n(n = ', s_2$n, ')')

  s_2 <- s_2 %>% select(grp_n,N_Missing,Mean_SD,Median_Q1_Q3,Min_Max)


  s_3 <- data.frame(matrix(NA,nrow = nrow(s_2) + 1, ncol = ncol(s_2)), stringsAsFactors = FALSE)

  s_3[1, ] <- names(s_2)

  for (i in 1:nrow(s_2)){
    s_3[i + 1, ] <- as.character(unlist(s_2[i, ]))
  }


  # 使用as.matrix进行转置
  t_0 <- as.data.frame(t(as.matrix(s_3)))


  t_1 <- data.frame(matrix(NA,nrow = nrow(t_0) + 1, ncol = ncol(t_0)),stringsAsFactors = FALSE)

  t_1[2,1] <- label(d_1$var_0)

  t_1[1, ] <- t_0[1, ]

  t_1[1,1] <- NA

  for (i in 1:4) {
    t_1[i + 2, ] <- as.character(unlist(t_0[i+1, ]))
  }


  col_names <- t_1[1, ]
  col_names[1,1] <- '  '
  t_2 <- slice(t_1, -1)# 移除第一行
  names(t_2) <- col_names # 将第一行的值设置为列名
  # t_2[is.na(t_2)] <- ""   #将数据框中NA显示为空值
  t_2[2,1] <- 'N(Missing)'
  t_2[3,1] <- 'Mean(SD)'
  t_2[4,1] <- 'Median(Q1,Q3)'
  t_2[5,1] <- 'Min,Max'

  names(t_2) <- title_name

  t_2 <<- t_2

  #判断table_out是否存在，如果不存在，则创建一个table_out，并将其设置为title_0的值
  if (exists('table_out')==FALSE){
    table_out<-title_0
  }

  table_out <- bind_rows(table_out,t_2)

  if (totalyn==0){
    table_out <- table_out[, -ncol(table_out)]

  }

  table_out <<-table_out

  ft<-if(outyn==1){
    #绘制表格
    ft <- flextable(table_out)
    # ft <- theme_vanilla(ft)


    ft <- color(ft,part = 'footer', color = 'black')
    ft <- set_caption(ft,caption = table_title)
    # ft<-font(ft,fontname="SimSun",part="all")
    ft<-font(ft,fontname="Times New Roman",part="all")
    ft<-hline_top(ft,border = fp_border_default(color="black",width=1.5),part="header")
    ft<-hline_bottom(ft,border = fp_border_default(color="black",width=1.5),part="body")
    ft<-hline(ft,i=1,border = fp_border_default(color="black",width=1),part="header")
    ft <- add_footer_lines(ft,ftnote)
    rm(table_out,t_2,envir = .GlobalEnv)
    ft

  }
  ft
}

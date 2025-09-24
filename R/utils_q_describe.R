#' q_describe
#'
#' @description A utils function for descriptive statistics of continuous variables
#'
#' @param data Object of the data frame
#' @param data_cond Data filtering condition
#' @param var_name Variable name for analysis
#' @param var_label Variable label
#' @param group_name Grouping variable name
#' @param group_cond Group conditions to analyze
#' @param table_title Table title
#' @param ftnote Footnote
#' @param totalyn Whether to output total column (0: no, other: yes)
#' @param outyn Whether to output table (1: yes, other: no)
#'
#' @return A flextable object with descriptive statistics
#'
#' @importFrom dplyr filter select group_by summarise bind_rows n all_of
#' @importFrom rlang parse_expr .data
#' @importFrom stats median quantile sd setNames
#' @importFrom flextable flextable set_caption font hline_top hline_bottom hline add_footer_lines
#' @importFrom magrittr %>%
#' @noRd
#示例

# adsl<-read_excel("E:/Rlanguage/2-system/R/R/rfile/数据/adsl.xlsx")
#
# q_describe(data = "adsl"
#            ,data_cond = "FAS!=''"
#            ,var_name="HEIGHT"
#            ,var_label="我是标签"
#            ,group_name="arm3"
#            ,group_cond=c('对照组','试验组','安慰剂组')
#            ,table_title="我是表格名称"
#            ,ftnote="我是底注"
#            ,totalyn=1
#            ,outyn=1)



q_describe<-function(data,data_cond,var_name,var_label,group_name,group_cond,table_title,ftnote,totalyn,outyn=1)
{

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
  data_0 <- data  # 直接使用传入的数据框


    data_cond_0 <- rlang::parse_expr(data_cond)


    data_0 <- data_0  %>%
      dplyr::filter(!!data_cond_0) #根据条件筛选出数据框


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
    dplyr::filter(.data[[group_name]] %in% group_cond )

  # data_0_out <<- data_0

  var_expr <- rlang::ensym(var_name)
  group_expr <- rlang::ensym(group_name)


  d_0 <- data_0 %>%
    dplyr::select({{var_expr}},{{group_expr}})

  d_1 <- stats::setNames(d_0,c("var_0","group_0"))

  d_1$group_0 <- factor(d_1$group_0, levels = group_cond)   #将分组变量因子化，并按照输入顺序等级赋值，保证
  #输入顺序即为显示顺序
  table1::label(d_1$var_0)<-var_label
  # d_1_out <<- d_1


  #制作表头
  title_0 <- d_1 %>%
    dplyr::group_by(group_0) %>%
    dplyr::filter(group_0 %in% group_cond) %>%  # 使用filter挑选分组
    dplyr::summarise(
      n = dplyr::n() # 计数每个组的观测值数量
    )

  title_0_1 <- d_1 %>%
    dplyr::summarise(
      group_0 = '合计',
      n = dplyr::n()  # 计数每个组的观测值数量
    )

  title_0 <- dplyr::bind_rows(title_0, title_0_1)

  title_0$grp_n = paste(title_0$group_0, '\n(n = ', title_0$n, ')')

  title_0 <- title_0 %>% dplyr::select(grp_n)

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
    dplyr::group_by(group_0) %>%
    dplyr::filter(group_0 %in% group_cond) %>%  # 使用filter挑选分组
    dplyr::summarise(
      mean = sprintf('%.2f', mean(var_0, na.rm = TRUE)),  #na.rm=TRUE 表示对于数据中的NA值，确保函数在计算时值考虑非缺失值，得到一个
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
    dplyr::summarise(
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

  s_2 <- dplyr::bind_rows(s_0,s_1)

  s_2$grp_n = paste(s_2$group_0, '\n(n = ', s_2$n, ')')

  s_2 <- s_2 %>% dplyr::select(grp_n,N_Missing,Mean_SD,Median_Q1_Q3,Min_Max)


  s_3 <- data.frame(matrix(NA,nrow = nrow(s_2) + 1, ncol = ncol(s_2)), stringsAsFactors = FALSE)

  s_3[1, ] <- names(s_2)

  for (i in 1:nrow(s_2)){
    s_3[i + 1, ] <- as.character(unlist(s_2[i, ]))
  }


  # 使用as.matrix进行转置
  t_0 <- as.data.frame(t(as.matrix(s_3)))


  t_1 <- data.frame(matrix(NA,nrow = nrow(t_0) + 1, ncol = ncol(t_0)),stringsAsFactors = FALSE)

  t_1[2,1] <- table1::label(d_1$var_0)

  t_1[1, ] <- t_0[1, ]

  t_1[1,1] <- NA

  for (i in 1:4) {
    t_1[i + 2, ] <- as.character(unlist(t_0[i+1, ]))
  }


  col_names <- t_1[1, ]
  col_names[1,1] <- '  '
  t_2 <- dplyr::slice(t_1, -1)# 移除第一行
  names(t_2) <- col_names # 将第一行的值设置为列名
  # t_2[is.na(t_2)] <- ""   #将数据框中NA显示为空值
  t_2[2,1] <- '  N(Missing)'
  t_2[3,1] <- '  Mean(SD)'
  t_2[4,1] <- '  Median(Q1,Q3)'
  t_2[5,1] <- '  Min,Max'

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

  ft <- if (outyn == 1) {
    #绘制表格
    ft <- flextable::flextable(table_out)

    ft <- flextable::color(ft, part = 'footer', color = 'black')
    ft <- flextable::set_caption(ft, caption = table_title)
    ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
    ft <- flextable::hline_top(ft, border = flextable::fp_border_default(color = "black", width = 1.5), part = "header")
    ft <- flextable::hline_bottom(ft, border = flextable::fp_border_default(color = "black", width = 1.5), part = "body")
    ft <- flextable::hline(ft, i = 1, border = flextable::fp_border_default(color = "black", width = 1), part = "header")
    ft <- flextable::add_footer_lines(ft, ftnote)

    # 清理全局变量
    if (exists('table_out', envir = .GlobalEnv)) {
      rm(table_out, envir = .GlobalEnv)
    }
    if (exists('t_2', envir = .GlobalEnv)) {
      rm(t_2, envir = .GlobalEnv)
    }

    ft
  } else {
    NULL
  }

  return(ft)
}

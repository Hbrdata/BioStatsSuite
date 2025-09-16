#' q_param
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#######连续型变量描述性统计函数#########

# 用途：输出定量数据组内配对t检验、组间独立样本t检验/方差检验

#基本参数：
# data_cond=			要进行统计描述的数据集|<筛选条件>；筛选过后的数据集需要1人一条，人群要全，不然Missing算出来不对；
# group=				组别变量|组别1/组别2/……；不能为空值
# denominator_cond=	当做首行标签中N=XX的数据集的名称|条件
# varlist=			分析的变量：变量名|变量标签
# outyn=				是否输出表格，若输出填写1，不输出填写0；
# title=				输出表格的名称，当outyn=1时，title参数才会被识别；
# footnote=			输出footnote
# rowtotal=			是否输出行合计（最后一列的“合计”）；rowtotal=0，不输出；默认输出
# pairt=				是否进行配对t检验【组内】；pairt=0,不进行配对t检验；默认进行
# test_between=		如果不进行【组间检验】，则填写test_between=0；如果为两组，则进行独立样本t检验；如果组别>=3组，则进行方差检验；默认输出

#示例

# cov_adur<-read_excel("D:/studies/R语言/2 - system/函数/q_param/SAS程序与数据/cov_adur.xlsx")

# q_param(data_cond="cov_adur|DSYN=='是' & visit_no==0 &  fas=='是'  &  IPSSYZ1=='≥12分'"
#         ,denominator_cond="cov_adur|DSYN=='是' &  visit_no==0 &  fas=='是'  &  IPSSYZ1=='≥12分'"
#         ,group_c="arm3|大剂量组/小剂量组/零剂量组"
#         ,varlist="URPVVtb|基线"
#         ,rowtotal=0
#         ,pairt=1
#         ,outyn=1
#         ,test_between=1
#         ,title="q_param示例"
#         ,footnote="可进行输出统计性描述、组间/组内检验")




q_param<-function(data_cond ,denominator_cond, group_c, varlist, rowtotal,pairt,outyn=1,test_between,title,footnote)
{



  library(readxl)
  library(dplyr)
  library(table1)
  library(tibble)
  library(kableExtra)
  library(officer)
  library(flextable)
  library(rlang)









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




  ##############根据条件创建数据框###########
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

  for (i in 1:nrow(d_0)){
    for (s_ in 1:grp_num) {
      if (d_0$group_0[i] == cat_grpname[[s_]]){
        d_0$grpcd_[i] <-s_
        break
      }

    }
  }



  #制作表头
  data_cond_part_title <- unlist(strsplit(denominator_cond,"|",fixed = TRUE))
  data_n_title <- data_cond_part_title[1]
  cond_n_title <- data_cond_part_title[2]

  data_1 <- get(data_n_title)
  cond_n_title <- parse_expr(cond_n_title)
  data_1 <- data_1  %>%
    filter(!!cond_n_title) #根据条件筛选出数据框

  data_1 <- data_1 %>%
    filter(.data[[grpvar_]] %in% group_cond )


  d_1 <- data_1 %>%
    select({{var_expr}},{{group_expr}})

  d_1 <- setNames(d_1,c("var_0","group_0"))
  d_1$grpcd_ <- NA

  for (i in 1:nrow(d_0)){
    for (s_ in 1:grp_num) {
      if (d_1$group_0[i] == cat_grpname[[s_]]){
        d_1$grpcd_[i] <-s_
        break
      }

    }
  }




  title_0 <- d_1 %>%
    group_by(group_0) %>%
    # filter(group_0 %in% group_cond) %>%  # 使用filter挑选分组
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

  title_0_stat <- data.frame(grp_n=c("统计量","P值"))

  title_0<- bind_rows(title_0,title_0_stat)

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

  s_0 <- d_0 %>%
    group_by(group_0) %>%
    summarise(
      mean = sprintf('%.2f',mean(var_0, na.rm = TRUE)),  #na.rm=TRUE 表示对于数据中的NA值，确保函数在计算时值考虑非缺失值，得到一个
      #基于有效数据的统计结果
      median = sprintf('%.2f',median(var_0, na.rm = TRUE)),
      Q1 = sprintf('%.2f',quantile(var_0,probs = 0.25,type = 2, na.rm = TRUE)),
      Q2 = sprintf('%.2f',quantile(var_0,probs = 0.50,type = 2, na.rm = TRUE)),
      Q3 = sprintf('%.2f',quantile(var_0,probs = 0.75,type = 2, na.rm = TRUE)),
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

  s_1 <- d_0 %>%
    summarise(
      group_0 = '合计',
      mean = sprintf('%.2f',mean(var_0, na.rm = TRUE)),  #na.rm=TRUE 表示对于数据中的NA值，确保函数在计算时值考虑非缺失值，得到一个
      #基于有效数据的统计结果
      median = sprintf('%.2f',median(var_0, na.rm = TRUE)),
      Q1 = sprintf('%.2f',quantile(var_0,probs = 0.25,type = 2, na.rm = TRUE)),
      Q2 = sprintf('%.2f',quantile(var_0,probs = 0.50, type = 2,na.rm = TRUE)),
      Q3 = sprintf('%.2f',quantile(var_0,probs = 0.75,type = 2, na.rm = TRUE)),
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

  t_1[2,1] <- avalabel_

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
  t_2$统计量 <- NA
  t_2$P值 <- NA
  t_2$统计量 <- as.character(t_2$统计量)
  t_2$P值 <- as.character(t_2$P值)

  names(t_2) <- title_name
  #组间检验
  d_0$group_0 <- factor(d_0$group_0,levels = c(grpnames_))
  if (grp_num>2){
    test_stat <- paste(sprintf("%.2f",summary(aov(var_0~group_0,data=d_0))[[1]][4][[1]][1]),"(方差检验)")
    test_p <- sprintf("%.4f",summary(aov(var_0~group_0,data=d_0))[[1]][5][[1]][1])
  }

  if (grp_num==2){
    test_stat <- paste(sprintf("%.2f",t.test(var_0~group_0, var.equal = TRUE,data=d_0)$statistic[[1]] ),"(独立样本t检验)" )
    test_p <- sprintf("%.4f",t.test(var_0~group_0, var.equal = TRUE,data=d_0)$p.value)
  }

  if (grp_num<2){
    test_stat <- NA
    test_p <- NA
  }

  t_2[1,which(names(t_2)=="统计量")] <- test_stat
  t_2[1,which(names(t_2)=="P值")] <- test_p

  t_2_partrow <- rep(NA,ncol(t_2))
  t_2_partrow <- as.data.frame(t(t_2_partrow))
  names(t_2_partrow) <- names(t_2)
  t_2 <- rbind(t_2,t_2_partrow)

  t_2[6,1] <- "配对t检验(P值)"

  tpairtlist <- list()
  tpairttestlistv  <- list()
  tpairtlistp  <- list()
  tpairtlistvp <- list()
  for (i in 1:grp_num) {
    tpairtlist[[i]] <- d_0  %>% filter(grpcd_ == i)
    tpairttestlistv[[i]] <- t.test(tpairtlist[[i]]$var_0,alternative = "two.sided")$statistic[[1]]
    tpairtlistp[[i]] <- sprintf("%.4f", t.test(tpairtlist[[i]]$var_0,alternative = "two.sided")$p.value)

    if (as.numeric(tpairtlistp[[i]] )< 0.0001){
      tpairtlistvp[[i]] <- paste(sprintf("%.2f",tpairttestlistv[[i]]),"(","<.0001",")")
    }else {tpairtlistvp[[i]] <- paste(sprintf("%.2f",tpairttestlistv[[i]]) ,"(",tpairtlistp[[i]],")")}
  }

  for (i in 1:grp_num) {
    t_2[6,1+i]<-tpairtlistvp[[i]]
  }


  if (rowtotal==0){
    t_2 <- t_2 %>% select(-matches("合计"))
  }


  if (test_between==0){
    t_2 <- t_2 %>% select(-matches("统计量"))
    t_2 <- t_2 %>% select(-matches("P值"))
  }

  if (pairt==0){
    t_2 <- subset(t_2, t_2[,1] != '配对t检验(P值)')
  }




  t_2 <<- t_2

  #判断table_out是否存在，如果不存在，则创建一个table_out，并将其设置为title_0的值
  if (exists('table_out')==FALSE){
    table_out<-title_0
  }

  if (rowtotal==0){
    table_out <- table_out %>% select(-matches("合计"))
  }


  if (test_between==0){
    table_out <- table_out %>% select(-matches("统计量"))
    table_out <- table_out %>% select(-matches("P值"))
  }




  table_out <- bind_rows(table_out,t_2)



  table_out <<-table_out




  ft<-if(outyn==1){
    #绘制表格
    ft <- flextable(table_out)
    # ft <- theme_vanilla(ft)


    ft <- color(ft,part = 'footer', color = 'black')
    ft <- set_caption(ft,caption = title)
    # ft<-font(ft,fontname="SimSun",part="all")
    ft<-font(ft,fontname="Times New Roman",part="all")
    ft<-hline_top(ft,border = fp_border_default(color="black",width=1.5),part="header")
    ft<-hline_bottom(ft,border = fp_border_default(color="black",width=1.5),part="body")
    ft<-hline(ft,i=1,border = fp_border_default(color="black",width=1),part="header")
    ft <- add_footer_lines(ft,footnote)
    rm(table_out,t_2,envir = .GlobalEnv)
    # ft <- set_table_properties(layout = "autofit", width = 1)
    ft

  }
  ft


}
















#' lifetest
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#######生存分析表格#########

# 用途：输出生存分析表，进行log-rank检验

#基本参数：
# data_cond=                 要分析的数据集|<条件>
# group_c=                   组别变量|组别1/组别2/……；不能为缺失
# censor                     删失变量
# time_label                 时间|时间变量的label；时间不能为空值，负值；
# timelist                   指定一系列的时间点
# type                        =0时输出生存率，=1时输出失效率
# topleftlabel                最左列的表格的列名
# title                       输出的表格的title
# footnote                   输出脚注内容

#示例

# adhj <- read_excel("E:/Rlanguage/2 - system/函数/8.lifetest/SAS程序与数据/adhj.xlsx")
#
# lifetest(
#   data_cond="adhj|RANDYN=='是' & FAS=='是'  & fxyn==1 & anafl=='是'"
#   ,group_c="arm3|试验组/对照组"
#   ,censor="censor"
#   ,time_label="lgzzhj|流感症状缓解时间（h）"
#   ,timelist=c(0,2,4,6,10,14,18,24,48,72)
#   ,type=1
#   ,topleftlabel="指标"
#   ,title="各时点流感症状缓解率的Kaplan-Meier估计（FAS）"
#   ,footnote="仅对疗前存在流感症状评分>1分且评价过程中体温存在>37.2℃的受试者进行分析。"
# )





lifetest <- function(data_cond,group_c,censor,time_label,timelist,type,topleftlabel,title,footnote)
{

  #生存分析
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
  library(survminer)
  library(survival)

  ################## 拆分组别、分析变量 ################################


  #拆分组别变量，组别名称；计算组别个数
  grp_part <- unlist(strsplit(group_c,"|",fixed = TRUE))
  grpvar_ <- grp_part[1]
  grpnames_ <- grp_part[2]


  grpnames_ <- unlist(strsplit(grpnames_,"/",fixed = TRUE))

  #建立list来存储连续生成的组别名称
  cat_grpname <- list()
  s_ <- 1
  while (s_ <= length(grpnames_) && grpnames_[s_] != "") {
    cat_grpname[[s_]] <- grpnames_[s_]
    s_ <- s_ + 1
    grp_num=s_ - 1
  }

  ###########制作分析数据集###############
  #拆分分析变量和标签
  timevarlabel_part <- unlist(strsplit(time_label,"|",fixed = TRUE))
  timevar_ <- timevarlabel_part[1]
  timelabel_ <- timevarlabel_part[2]

  #拆分分析数据集及条件
  data_cond_part <- unlist(strsplit(data_cond,"|",fixed = TRUE))
  data_ <- data_cond_part[1]
  cond_ <- data_cond_part[2]

  data_0 <- get(data_)
  data_0 <- data_0 %>%
    filter(!!parse_expr(cond_))
  group_cond <- c(grpnames_)
  data_0 <- data_0 %>%
    filter(.data[[grpvar_]] %in% group_cond )
  time_var_expr <- rlang::sym(timevar_)
  censor_var_expr <- rlang::sym(censor)
  group_expr <- rlang::sym(grpvar_)

  d_0 <- data_0 %>%
    select({{time_var_expr}},{{censor_var_expr}},{{group_expr}})
  d_0 <- setNames(d_0,c("time_0","censor_0","group_0"))
  d_0$grpcd_ <- NA
  for(i in 1:nrow(d_0)){
    for (s_ in 1:grp_num){
      if (d_0$group_0[i] == cat_grpname[[s_]]){
        d_0$grpcd_[i] <- s_
      }
    }
  }


  #计算各个组别的N
  title_0 <- d_0 %>%
    group_by(grpcd_) %>%
    summarise(
      n = n(),  # 计数每个组的观测值数量
    )

  title_0$grp_name <- NA
  for (i in 1:nrow(title_0)){
    if (title_0$grpcd_[i] == i){
      title_0$grp_name[i] <- cat_grpname[i]
    }
  }
  title_0$grp_n <- NA

  title_0$grp_n <- paste(title_0$grp_name,'\n(n = ',title_0$n, ')')

  title_0_1 <- data.frame(matrix(ncol = ncol(title_0),nrow = 1))
  names(title_0_1) <- names(title_0)

  title_0 <- rbind(title_0_1,title_0)
  title_0[1,4] <- "指标"
  title_0 <- title_0 %>%
    select(grp_n)
  title_0 <- t(title_0)



  #################### 生存分析结果 ##########


  fit <- summary(survfit(Surv(time_0,censor_0)~grpcd_,conf.type="log-log",data = d_0),times = timelist)
  print(fit)
  logrank_test <- survdiff(Surv(time_0,censor_0)~grpcd_,data = d_0)
  print(logrank_test)


  #结果制表
  col_names <- title_0[1, ]

  result_0 <- matrix(NA,nrow=length(timelist)+7,ncol = length(title_0))
  result_0 <- data.frame(result_0)
  colnames(result_0) <- col_names

  time_num <- length(timelist)
  for (i in 1:time_num) {
    result_0[i+1,1] <- timelist[i]
  }

  result_0[1,1] <- paste(timelabel_,"(Log-Rank=",sprintf("%.2f",logrank_test$chisq),",","P=",sprintf("%.4f",logrank_test$pvalue),")")

  survival_result <- data.frame(fit$strata,fit$time,fit$surv,fit$lower,fit$upper)

  survival_result$fit.strata <- as.character(survival_result$fit.strata)
  for(i in 1:nrow(survival_result)){
    survival_result$grp_cd[i] <- unlist(strsplit(survival_result$fit.strata[i],"=",fixed = TRUE))[2]
  }

  for (i in 1:nrow(survival_result)) {
    for (s_ in 1:grp_num) {
      if (survival_result$grp_cd[i]==s_){
        survival_result$grp_name[i] <- grpnames_[s_]
      }
    }
  }


  if (type == 0){
    survival_result$surv <- sprintf("%.2f",survival_result$fit.surv*100)
    survival_result$lower <- sprintf("%.2f",survival_result$fit.lower*100)
    survival_result$upper <- sprintf("%.2f",survival_result$fit.upper*100)
  }else {
    if (type == 1){
      survival_result$surv <- sprintf("%.2f",(1-survival_result$fit.surv)*100)
      survival_result$lower <- sprintf("%.2f",(1-survival_result$fit.upper)*100)
      survival_result$upper <- sprintf("%.2f",(1-survival_result$fit.lower)*100)
    }
  }

  for (i in 1:nrow(survival_result)){
    if (is.na(survival_result$fit.surv[i])){
      survival_result$surv[i] <- "0.00"
    }
    if (is.na(survival_result$fit.lower[i])){
      survival_result$lower[i] <- "0.00"
    }
    if (is.na(survival_result$fit.upper[i])){
      survival_result$upper[i] <- "0.00"
    }
  }

  survival_result$surv_0 <- paste(survival_result$surv,"(",survival_result$lower,",",survival_result$upper,")")

  survival_result_1 <- survival_result %>%
    select(fit.time,grp_cd,grp_name,surv,lower,upper,surv_0)

  survival_list <- list()
  for (i in 1:grp_num){
    survival_list[[i]] <- survival_result_1 %>%
      filter(grp_cd == i)
  }

  for (i in 1:time_num){
    for (s_ in 1:grp_num){
      result_0[(1+i),(1+s_)] <- survival_list[[s_]]$surv_0[i]

    }
  }

  result_0$指标[3+time_num] <- "25%分位数(95%CI)"
  result_0$指标[4+time_num] <- "50%分位数(95%CI)"
  result_0$指标[5+time_num] <- "75%分位数(95%CI)"
  result_0$指标[nrow(result_0)] <- "删失率(%)"

  fit_50 <- data.frame(summary(survfit(Surv(time_0,censor_0)~grpcd_,conf.type="log-log",data = d_0))$table)
  quantile_info <- quantile(survfit(Surv(time_0, censor_0) ~ grpcd_, conf.type = "log-log", data = d_0), probs = c(0.25, 0.5, 0.75), conf.int = TRUE)
  # print(quantile_info)
  # 创建置信区间字符串,如果点估计值为空，那么填补点估值值为NA,
  # 如果点估计值不为空，当置信区间上下限为NA时，将NA替换为.(与SAS中的输出形式一致)
  fit_50$lsd <- sapply(1:nrow(fit_50), function(i) {
    if (is.na(quantile_info$quantile[i, "25"])) {
      "NA"
    } else {
      lower <- ifelse(is.na(quantile_info$lower[i, "25"]), ".", sprintf('%.2f', quantile_info$lower[i, "25"]))
      upper <- ifelse(is.na(quantile_info$upper[i, "25"]), ".", sprintf('%.2f', quantile_info$upper[i, "25"]))
      paste(
        sprintf('%.2f', quantile_info$quantile[i, "25"]),
        '(',
        lower,
        ',',
        upper,
        ')'
      )
    }
  })



  fit_50$msd <- paste(sprintf('%.2f',fit_50$median),'(',sprintf('%.2f',fit_50$X0.95LCL),',',sprintf('%.2f',fit_50$X0.95UCL),')')


  fit_50$usd <- sapply(1:nrow(fit_50), function(i) {
    if (is.na(quantile_info$quantile[i, "75"])) {
      "NA"
    } else {
      lower <- ifelse(is.na(quantile_info$lower[i, "75"]), ".", sprintf('%.2f', quantile_info$lower[i, "75"]))
      upper <- ifelse(is.na(quantile_info$upper[i, "75"]), ".", sprintf('%.2f', quantile_info$upper[i, "75"]))
      paste(
        sprintf('%.2f', quantile_info$quantile[i, "75"]),
        '(',
        lower,
        ',',
        upper,
        ')'
      )
    }
  })

  # 删失率
  fit_50$delete <- sprintf('%.2f',((fit_50$records-fit_50$events)/fit_50$records)*100)

  for (i in 1:grp_num){
    result_0[which(result_0$指标 == '25%分位数(95%CI)'),i+1] <- fit_50$lsd[i]
    result_0[which(result_0$指标 == '50%分位数(95%CI)'),i+1] <- fit_50$msd[i]
    result_0[which(result_0$指标 == '75%分位数(95%CI)'),i+1] <- fit_50$usd[i]
    result_0[which(result_0$指标 == '删失率(%)'),i+1] <- fit_50$delete[i]
  }


  table_out <- result_0



  #绘制表格

  ft <- flextable(table_out)
  # ft <- theme_vanilla(ft)
  ft <- color(ft,part = 'footer', color = 'black')
  ft <- set_caption(ft,caption = title)
  # ft<-font(ft,fontname="SimSun",part="all")
  # ft<-font(ft,fontname="Times New Roman",part="all")
  ft<-hline_top(ft,border = fp_border_default(color="black",width=1.5),part="header")
  ft<-hline_bottom(ft,border = fp_border_default(color="black",width=1.5),part="body")
  ft<-hline(ft,i=1,border = fp_border_default(color="black",width=1),part="header")
  ft <- add_footer_lines(ft,footnote)
  rm(table_out,envir = .GlobalEnv)
  ft <- autofit(ft)
  ft
}








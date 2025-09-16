#' covancova
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#######c_srt 秩和检验#########

# 用途：秩和检验；也可仅输出统计描述部分；

#基本参数：
# data_cond=		要进行统计描述的数据集|<筛选条件
# group_c=	分组变量及挑选组别
# varlist=	"difftbsum/用药后6周±3天|SITEID/中心|TSORRES0sum/基线"
# title1=    表格1：输出的协方差分析(ANCOVA)结果表格的title；
# title2=   表格2：输出的最小二乘均数和95%可信区间表格的title；
# footnote1=	"底注1"
# footnote2=	"底注2")




#示例

# adts <-read_excel("E:/Rlanguage/2 - system/函数/covancova/SAS程序与数据/adts.xlsx")
#
# covancova(data_cond=			"adts|RANDYN=='是' & FAS=='是'  & tstest=='胃痛' & visitnum=='2'"
#           ,group_c=	"arm3|试验组/阳性药组/安慰剂组"
#           ,varlist=	"difftbsum/用药后6周±3天|SITEID/中心|TSORRES0sum/基线"
#           ,title1=   "受试者中医证候积分相对基线变化差值(用药后6周±3天)的协方差分析（ANCOVA）结果－因素分析" #表格1：输出的协方差分析(ANCOVA)结果表格的title；
#           ,title2=   "受试者中医证候积分相对基线变化差值(用药后6周±3天)的协方差分析（ANCOVA）结果－组间比较"  #表格2：输出的最小二乘均数和95%可信区间表格的title；
#           ,footnote1=	"底注1"
#           ,footnote2=	"底注2")





covancova <- function(data_cond,group_c,varlist,title1,title2,footnote1,footnote2)
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
  library(emmeans)
  library(car)
  library(afex)





  ###############拆分组别，组别名称；计算组别个数

  grp_part <-  unlist(strsplit(group_c, "|", fixed = TRUE))

  grpvar_ <-  grp_part[1]
  grpnames_ <- grp_part[2]

  s_ <- 1
  grpnames_ <- unlist(strsplit(grpnames_, "/", fixed = TRUE))

  ########计算置信区间百分比


  #建立list来存储连续生成的组别名称
  cat_grpname <- list()

  while (s_ <= length(grpnames_) && grpnames_[s_] != "") {
    cat_grpname[[s_]] <- grpnames_[s_]
    s_ <- s_ + 1
    grp_num=s_ - 1
  }

  #############拆分数据集及条件

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


  #################拆分分析变量及其标签

  varlist_parts_1 <- unlist(strsplit(varlist, "|", fixed = TRUE))

  #因变量

  anavarlist_ <-  varlist_parts_1[1]
  anavar_ <- strsplit(anavarlist_, "/") [[1]][1]
  avalabel_ <- strsplit(anavarlist_, "/") [[1]][2]

  #中心

  sitenolist_ <- varlist_parts_1[2]
  siteno_ <- strsplit(sitenolist_, "/") [[1]][1]
  sitelabel_ <- strsplit(sitenolist_, "/") [[1]][2]

  #协变量：基线

  baselist_ <- varlist_parts_1[3]
  base_ <- strsplit(baselist_,"/")[[1]][1]
  baselabel <- strsplit(baselist_,"/")[[1]][2]

  ##################函数中分析数据集

  anavar_expr <- rlang::ensym(anavar_)
  siteno_expr <- rlang::ensym(siteno_)
  base_expr <- rlang::ensym(base_)
  group_expr <- rlang::ensym(grpvar_)
  d_0 <- data_0 %>%
    select({{anavar_expr}},{{siteno_expr}},{{base_expr}},{{group_expr}})

  d_0 <- setNames(d_0,c("anavar_0","siteno_0","base_0","group_0"))

  #对组别进行排序
  d_0$grpcd_0 <- NA

  for (i in 1:nrow(d_0)){
    for (s_ in 1:grp_num) {
      if (d_0$group_0[i] == cat_grpname[[s_]]){
        d_0$grpcd_0[i] <-s_
        break
      }

    }
  }
  ################协方差分析


  # 拟合协方差模型
  d_0$siteno_0 <- as.factor(d_0$siteno_0)
  d_0$group_0 <- factor(d_0$group_0,levels = c(grpnames_))


  ancova_model <- lm(anavar_0 ~ siteno_0 + group_0+base_0, data = d_0)

  # 查看结果
  summary(ancova_model)

  Anova(ancova_model, type = "III")
  emmeans(ancova_model, ~ group_0)
  pairs(emmeans(ancova_model, ~ group_0))  # 这将展示所有组合的均值差

  ###############制表############
  ###########因素分析表######

  t_1 <- data.frame(Anova(ancova_model, type = "III"))
  t_1$var <- NA
  t_1[1,which(names(t_1) == "var")]<-"Intercept"
  t_1[2,which(names(t_1) == "var")]<-"siteno_0"
  t_1[3,which(names(t_1) == "var")]<-"group_0"
  t_1[4,which(names(t_1) == "var")]<-"base_0"
  t_1[5,which(names(t_1) == "var")]<-"Residuals"

  t_1_1 <- t_1 %>%
    select(var,Sum.Sq,Df,F.value,Pr..F.)
  t_1_1 <- setNames(t_1_1,c("var","sum_sq","df","F_value","p_value"))
  t_1_1$sum_sq <- sprintf("%.2f",t_1_1$sum_sq)
  t_1_1$df <- as.character(t_1_1$df)
  t_1_1$F_value <- sprintf("%.2f",t_1_1$F_value)

  for (i in 1:nrow(t_1_1)-1) {
    p_value <- t_1_1$p_value[i]
    if(isTRUE(as.numeric(p_value) < 0.0001)){
      t_1_1$p_value[i] <- "<.0001"
    }else{
      t_1_1$p_value[i] <- sprintf("%.4f",as.numeric(p_value))
    }
  }


  t_1_1 <-t_1_1[-which(t_1_1$var == "Residuals"),]
  t_1_1$指标 <- NA
  t_1_1$因素 <- NA
  t_1_1$F <- NA
  t_1_1$P值 <- NA
  t_1_1$指标[1] <- avalabel_

  t_1_1[which(t_1_1$var == "siteno_0"),names(t_1_1) == "因素"] <- sitelabel_
  t_1_1[which(t_1_1$var == "group_0"),names(t_1_1) == "因素"] <- "治疗"
  t_1_1[which(t_1_1$var == "base_0"),names(t_1_1) == "因素"] <- baselabel

  for (i in 2:nrow(t_1_1)) {
    t_1_1[i,names(t_1_1) == "F"] <- t_1_1[i,names(t_1_1) == "F_value"]
    t_1_1[i,names(t_1_1) == "P值"] <- t_1_1[i,names(t_1_1) == "p_value"]
  }

  table_out_1 <- t_1_1 %>%
    select(指标,因素,F,P值)
  flextable(table_out_1)


  ############ 组间比较 ############



  t_2_1 <- data.frame(emmeans(ancova_model, ~ group_0))
  t_2_1$指标<-NA
  t_2_1$治疗水平及差值 <- t_2_1$group_0
  t_2_1$LSMean <- sprintf("%.2f",t_2_1$emmean)
  t_2_1$`95% CIL` <- sprintf("%.2f",t_2_1$lower.CL)
  t_2_1$`95% CIU` <- sprintf("%.2f",t_2_1$upper.CL)
  t_2_1 <- t_2_1 %>%
    select(指标,治疗水平及差值,LSMean,`95% CIL`,`95% CIU`)


  t_2_title <- data.frame(matrix(NA, nrow = 1, ncol = length(names(t_2_1))))
  names(t_2_title) <- names(t_2_1)
  t_2_title[1,1] <- avalabel_

  t_2_2 <- data.frame(pairs(emmeans(ancova_model, ~ group_0)))
  t_2_2$指标 <- NA
  t_2_2$治疗水平及差值 <- t_2_2$contrast
  t_2_2$LSMean <- sprintf("%.2f",t_2_2$estimate)
  t_2_2$`95% CIL` <- sprintf("%.2f",t_2_2$estimate - 1.96 * t_2_2$SE)
  t_2_2$`95% CIU` <- sprintf("%.2f",t_2_2$estimate + 1.96 * t_2_2$SE)
  t_2_2 <- t_2_2 %>%
    select(指标,治疗水平及差值,LSMean,`95% CIL`,`95% CIU`)

  table_out_2 <- bind_rows(t_2_title,t_2_1,t_2_2)


  #绘制表格

  ft1 <- flextable(table_out_1)
  ft1 <- color(ft1,part = 'footer', color = 'black')
  ft1 <- set_caption(ft1,caption = title1)
  ft1 <- font(ft1,fontname="SimSun",part="all")
  ft1 <- font(ft1,fontname="Times New Roman",part="all")
  ft1 <- hline_top(ft1,border = fp_border_default(color="black",width=1.5),part="header")
  ft1 <- hline_bottom(ft1,border = fp_border_default(color="black",width=1.5),part="body")
  ft1 <- hline(ft1,i=1,border = fp_border_default(color="black",width=1),part="header")
  ft1 <- add_footer_lines(ft1,footnote1)
  ft1 <- autofit(ft1)
  ft1 <<- ft1

  ft2 <- flextable(table_out_2)
  ft2 <- color(ft2,part = 'footer', color = 'black')
  ft2 <- set_caption(ft2,caption = title2)
  ft2 <- font(ft2,fontname="SimSun",part="all")
  ft2 <- font(ft2,fontname="Times New Roman",part="all")
  ft2 <- hline_top(ft2,border = fp_border_default(color="black",width=1.5),part="header")
  ft2 <- hline_bottom(ft2,border = fp_border_default(color="black",width=1.5),part="body")
  ft2 <- hline(ft2,i=1,border = fp_border_default(color="black",width=1),part="header")
  ft2 <- add_footer_lines(ft2,footnote2)
  ft2 <- autofit(ft2)
  ft2

  ft2<<-ft2

  return(list(table1 = ft1, table2 = ft2))

}


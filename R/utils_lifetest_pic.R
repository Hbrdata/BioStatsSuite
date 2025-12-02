#' lifetest_pic
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @importFrom dplyr filter select
#' @importFrom rlang sym parse_expr
#' @importFrom survival survfit Surv
#' @importFrom ggsurvfit ggsurvfit add_risktable add_censor_mark
#' @importFrom ggplot2 ggplot_build labs scale_x_continuous scale_y_continuous theme_classic theme element_text unit element_blank
#' @noRd
#######生存分析绘图#########

# 用途：输出生存分析图，进行log-rank检验

#基本参数：
# data_cond=                 要分析的数据集|<条件>
# group_c=                   组别变量|组别1/组别2/……；不能为缺失
# censor                     删失变量
# type                        =0时输出生存率，=1时输出失效率
# time_label                 时间|时间变量的label；时间不能为空值，负值；
# timelist                   指定一系列的时间点
# censorvalue                删失变量值
# ylabel                     图片y轴的标签
# pic_title                       输出的图片的title


#示例
# adhj <- read_excel("E:/Rlanguage/2 - system/函数/8.lifetest/SAS程序与数据/adhj.xlsx")

# lifetest_pic(
#   data_cond="adhj|RANDYN=='是' & FAS=='是'  & fxyn==1 & anafl=='是'"
#   ,group_c="arm3|试验组/对照组"
#   ,censor="censor"
#   ,type=0
#   ,time_label="lgzzhj|时间（h）"
#   ,timelist=c(0,2,4,6,10,14,18,24,48,72)
#   ,censorvalue=0
#   ,ylabel="流感症状未缓解率(%)"
#   ,pic_title="各时点流感症状缓解率的Kaplan-Meier估计（FAS）"
# )





lifetest_pic <- function(inds,data_cond,group_c,censor,type,time_label,timelist,censorvalue,ylabel,pic_title)
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
  library(ggsurvfit)
  library(ggplot2)
  library(patchwork)

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

  #直接传递数据对象，不再使用get(data_name)的形式获取数据,避免无法获得交互数据
  cond_ <- data_cond


  data_0 <- inds
  data_0 <- data_0 %>%
    dplyr::filter(!!rlang::parse_expr(cond_))
  group_cond <- c(grpnames_)
  data_0 <- data_0 %>%
    dplyr::filter(.data[[grpvar_]] %in% group_cond )
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

  # 与分析表格不同，这里直接因子化便于将标题映射到图片上
  d_0$grpcd_ <- factor(d_0$group_0, levels = grpnames_, labels = grpnames_)


  ##### 制作生存曲线 #########
  # 先获取拟合对象，然后用scale产生的颜色映射
  fit <- ggsurvfit::survfit2(survival::Surv(time_0, censor_0) ~ grpcd_, data = d_0,
                        conf.type = "log-log", start.time = 0)

  type_mapping <- switch(as.character(type),
                         "0" = "survival",
                         "1" = "risk",
                         "2" = "cumhaz",
                         "3" = "cloglog",
                         "survival"  # 默认值
  )

  # 创建绘图对象
  p_default <- ggsurvfit::ggsurvfit(fit, type = type_mapping, linetype_aes = TRUE, linewidth = 0.9)
  # 获取实际使用的颜色映射
  color_mapping <- ggplot2::ggplot_build(p_default)$data[[1]]$colour  # 获取曲线的颜色
  unique_colors <- unique(color_mapping)


  p_default +
    ggsurvfit::add_risktable(risktable_height = 0.15,
                  risktable_stats = c("{n.risk}"),
                  stats_label = list(n.risk = "No. at Risk"),
                  size = 5,
                  hjust = 0.5,
                  mapping = aes(color = strata),
                  theme = list(
                    theme_risktable_default(axis.text.y.size = 14,
                                            plot.title.size = 14),
                    theme(plot.title = element_text(face = "bold"),
                          axis.text.y = element_text(face = "bold", color = unique_colors
                          )
                    )
                  )
    )+
    ggsurvfit::add_censor_mark(size = 3,
                    stroke = 1.5,
                    aes(shape = n.censor)
    ) +#添加删失标记
    ggplot2::labs(title = pic_title,
         x = timelabel_,
         y = ylabel
    ) +
    ggplot2::scale_x_continuous(breaks = timelist, limits = c(0, max(timelist)), expand = c(0.05, 0)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20), limits = c(0, 1.05), expand = c(0.05, 0)) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text = element_text(size = 14, color = "black"),
          axis.ticks.length = unit(2, "mm"),
          axis.title.y = ggplot2::element_text(size = 14, color = "black"),
          axis.title.x = ggplot2::element_text(size = 14, color = "black"),
          panel.grid = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 13, color = "black"),
          legend.background = ggplot2::element_blank(),
          legend.position = c(0.9, 0.9),
          legend.direction = "horizontal")

}

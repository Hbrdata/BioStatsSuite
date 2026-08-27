# =============================================================================
# utils_lifetest_pic.R
# 生存分析绘图函数（R 包内部版本）
#
# 使用 ggsurvfit 绘制 Kaplan-Meier 生存曲线，
# 通过 figtitle() 自动生成带编号的图标题。
# 支持 color/lintype/marktype 参数（SAS 兼容映射），
# 手动构建风险表，patchwork 组合主图+风险表。
# =============================================================================

#' 生存分析绘图
#'
#' @param inds         数据框对象
#' @param data_cond    数据筛选条件
#' @param group_c      分组描述："分组变量名|组名1/组名2/..."
#' @param censor       删失变量名
#' @param type         0=生存率，1=失效率，2=累积风险，3=cloglog
#' @param time_label   时间描述："时间变量名|时间变量标签"
#' @param timelist     时间点列表（数值向量）
#' @param censorvalue  删失变量值（保留参数）
#' @param ylabel       Y 轴标签
#' @param pic_title    图标题文字（不含编号，编号由 figtitle 自动生成）
#' @param color        各组曲线颜色，\| 分隔
#' @param lintype       各组曲线线型，\| 分隔（支持 SAS 线型名）
#' @param marktype     删失标记形状，\| 分隔（支持 SAS 形状名）
#' @param footnote     底注内容
#' @param width_in     图形宽度（英寸），默认 5.73
#' @param height_in    图形高度（英寸），默认 4.17
#' @param dpi          图形分辨率，默认 96
#' @param top          两个图例框之间的垂直间距（像素 @ 96 DPI），默认 15
#'
#' @return ggplot 对象
#' @noRd
lifetest_pic <- function(inds, data_cond, group_c, censor, type, time_label,
                         timelist, censorvalue, ylabel, pic_title,
                         color    = "blue|red|green|black|purple|orange",
                         lintype  = "solid|dashed|dotted|dotdash|longdash|twodash",
                         marktype = "circle|triangle|triangledown|triangleleft|triangleright|circlefilled",
                         footnote    = NULL,
                         width_in    = 5.73,
                         height_in   = 4.17,
                         dpi         = 96,
                         top         = 15) {

  library(survival)
  library(ggsurvfit)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(rlang)
  library(patchwork)

  # ============================================================
  # Step 1：解析参数
  # ============================================================
  grp_parts <- strsplit(group_c, "|", fixed = TRUE)[[1]]
  grpvar    <- trimws(grp_parts[1])
  grpnames  <- trimws(strsplit(grp_parts[2], "/", fixed = TRUE)[[1]])
  grpnames  <- grpnames[nchar(grpnames) > 0]
  n_grp     <- length(grpnames)

  tl_parts <- strsplit(time_label, "|", fixed = TRUE)[[1]]
  time_var <- trimws(tl_parts[1])
  x_label  <- if (length(tl_parts) >= 2) trimws(tl_parts[2]) else time_var

  # type 映射（兼容数值和字符输入）
  type_mapping <- switch(as.character(type),
    "0" = "survival",
    "1" = "risk",
    "2" = "cumhaz",
    "3" = "cloglog",
    "survival" = "survival",
    "risk"     = "risk",
    "cumhaz"   = "cumhaz",
    "cloglog"  = "cloglog",
    "survival"
  )

  # ============================================================
  # Step 1b：解析 color / lintype / marktype 参数
  # ============================================================
  # 颜色
  color_vals <- tolower(trimws(strsplit(color, "|", fixed = TRUE)[[1]]))
  color_vals <- rep_len(color_vals, n_grp)

  # 线型：SAS 线型名 → ggplot2 linetype 字符串映射
  sas_lintype_map <- c(
    solid               = "solid",
    shortdash           = "dashed",
    mediumdash          = "longdash",
    longdash            = "longdash",
    mediumdashshortdash = "dotdash",
    dashdashdot         = "twodash",
    dashed              = "dashed",
    dotted              = "dotted",
    dotdash             = "dotdash",
    twodash             = "twodash"
  )
  lintype_raw  <- trimws(strsplit(lintype, "|", fixed = TRUE)[[1]])
  lintype_vals <- vapply(tolower(lintype_raw), function(x) {
    if (x %in% names(sas_lintype_map)) sas_lintype_map[[x]] else x
  }, character(1))
  lintype_vals <- rep_len(lintype_vals, n_grp)

  # 删失形状：SAS marktype 名 → ggplot2 shape 数值映射
  sas_shape_map <- c(
    circle        = 1L,
    triangle      = 2L,
    triangledown  = 6L,
    triangleleft  = 60L,
    triangleright = 62L,
    circlefilled  = 16L
  )
  marktype_raw <- trimws(strsplit(marktype, "|", fixed = TRUE)[[1]])
  shape_vals   <- vapply(tolower(marktype_raw), function(x) {
    if (x %in% names(sas_shape_map)) sas_shape_map[[x]] else 3L
  }, integer(1))
  shape_vals <- rep_len(shape_vals, n_grp)

  # 命名向量：key = 组名
  color_named    <- setNames(color_vals, grpnames)
  shape_named    <- setNames(shape_vals, grpnames)
  linetype_named <- setNames(lintype_vals, grpnames)

  # ============================================================
  # Step 2：筛选数据
  # ============================================================
  data_0 <- inds
  data_0 <- data_0 |> dplyr::filter(!!rlang::parse_expr(data_cond))
  data_0 <- data_0 |> dplyr::filter(.data[[grpvar]] %in% grpnames)

  d_0 <- data_0 |>
    dplyr::select(dplyr::all_of(c(time_var, censor, grpvar))) |>
    stats::setNames(c("time_0", "censor_0", "group_0"))

  d_0$grpcd_ <- factor(d_0$group_0, levels = grpnames, labels = grpnames)

  # ============================================================
  # Step 3：标题（由 plot_to_docx 中的 figtitle 统一生成编号）
  # ============================================================

  # ============================================================
  # Step 4：KM 生存曲线基础对象（不含风险表）
  # ============================================================
  fit <- ggsurvfit::survfit2(
    survival::Surv(time_0, censor_0) ~ grpcd_, data = d_0,
    conf.type = "log-log", start.time = 0
  )

  p_main <- ggsurvfit::ggsurvfit(fit, type = type_mapping, linetype_aes = TRUE,
                                  linewidth = 0.9) +
    # 手动添加删失标记
    ggplot2::geom_point(
      data = function(d) {
        d2 <- d[!is.na(d$n.censor) & d$n.censor > 0, , drop = FALSE]
        if (nrow(d2) == 0) return(d2)
        tidyr::uncount(d2, weights = n.censor)
      },
      ggplot2::aes(x = .data$time, y = .data$estimate,
                    color = .data$strata, shape = .data$strata),
      size = 3, stroke = 1.5, na.rm = TRUE, show.legend = TRUE
    ) +
    ggplot2::labs(
      title = NULL,
      x     = x_label,
      y     = if (!is.null(ylabel)) ylabel else
                switch(type_mapping,
                  "survival" = "生存率(%)",
                  "risk"     = "失效率(%)",
                  "cumhaz"   = "累积风险",
                  "生存率(%)"
                )
    ) +
    ggplot2::scale_x_continuous(breaks = timelist, expand = c(0.02, 0)) +
    ggplot2::coord_cartesian(xlim = c(0, max(timelist)), clip = "off") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20),
                                limits = c(0, 1.05), expand = c(0.05, 0)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text          = ggplot2::element_text(size = 14, color = "black"),
      axis.ticks.length  = ggplot2::unit(2, "mm"),
      axis.title.y       = ggplot2::element_text(size = 14, color = "black"),
      axis.title.x       = ggplot2::element_text(size = 14, color = "black"),
      panel.grid         = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = 13, color = "black"),
      legend.background  = ggplot2::element_blank(),
      legend.position    = if (type_mapping == "survival") c(0.9, 0.9) else c(0.1, 0.9),
      legend.box         = "vertical",
      legend.direction   = "horizontal",
      legend.spacing.y   = ggplot2::unit(5, "pt"),
      plot.margin        = ggplot2::margin(t = 5.5, r = 5.5, b = 0, l = 5.5)
    ) +
    # 隐藏颜色图例（避免与 linetype 图例重复）
    ggplot2::scale_color_manual(values = color_named, guide = "none") +
    # 线条类型图例
    ggplot2::scale_linetype_manual(
      values = linetype_named,
      guide  = ggplot2::guide_legend(
        title = NULL, order = 1, keywidth = ggplot2::unit(1.5, "cm"),
        override.aes = list(shape = NA, color = unname(color_named),
                            linetype = unname(linetype_named))
      )
    ) +
    # 删失形状图例
    ggplot2::scale_shape_manual(
      values = shape_named,
      guide  = ggplot2::guide_legend(
        title        = "删失",
        title.theme  = ggplot2::element_text(
          size   = 10.5,
          color  = "black",
          margin = ggplot2::margin(
            l = (1.5 - 0.4) * 72 / 2.54 - 4,
            r = 4, b = 2, t = 0, unit = "pt"
          )
        ),
        order        = 2,
        keywidth     = ggplot2::unit(0.4, "cm"),
        override.aes = list(color = unname(color_named))
      )
    )

  # ============================================================
  # Step 5：手动构建风险表（独立 ggplot，颜色完全可控）
  # ============================================================
  strata_names <- names(fit$strata)
  grp_labels   <- sub("^[^=]*=", "", strata_names)
  risk_levels  <- rev(grp_labels)

  summ_risk    <- summary(fit, times = timelist, extend = TRUE)
  risk_summ_df <- data.frame(
    grp    = sub("^[^=]*=", "", as.character(summ_risk$strata)),
    time   = summ_risk$time,
    n_risk = summ_risk$n.risk,
    stringsAsFactors = FALSE
  )

  risk_df <- do.call(rbind, lapply(seq_along(grp_labels), function(s) {
    sub_df <- risk_summ_df[risk_summ_df$grp == grp_labels[s], ]
    data.frame(
      time   = sub_df$time,
      n_risk = sub_df$n_risk,
      group  = factor(grp_labels[s], levels = risk_levels),
      stringsAsFactors = FALSE
    )
  }))

  risk_color_named <- setNames(color_vals[seq_along(grp_labels)], grp_labels)

  rt <- ggplot2::ggplot(risk_df, ggplot2::aes(x = time, y = group, label = n_risk)) +
    ggplot2::geom_text(ggplot2::aes(color = group), size = 3.7, hjust = 0.5, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = risk_color_named) +
    ggplot2::scale_x_continuous(breaks = timelist, expand = c(0.02, 0)) +
    ggplot2::coord_cartesian(xlim = c(0, max(timelist)), clip = "off") +
    ggplot2::labs(x = NULL, y = NULL, title = "No. at Risk") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 10.5, face = "bold", hjust = 0,
                                               margin = ggplot2::margin(b = 0.5, unit = "mm")),
      panel.spacing.y  = ggplot2::unit(1, "mm"),
      axis.text.x      = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_text(size = 10.5, face = "bold",
                                               color = risk_color_named[risk_levels]),
      axis.ticks       = ggplot2::element_blank(),
      panel.grid       = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(t = 0, r = 5.5, b = 0, l = 5.5)
    )

  # ============================================================
  # Step 6：组合主图 + 风险表（patchwork 垂直堆叠）
  # ============================================================
  risk_h    <- 0.2 + 0.25 * n_grp
  risk_frac <- risk_h / height_in
  main_frac <- 1 - risk_frac

  p <- p_main / rt +
    patchwork::plot_layout(heights = c(main_frac, risk_frac))

  return(p)
}

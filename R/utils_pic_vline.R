# =============================================================================
# utils_pic_vline.R
# 折线图绘制函数（R 包内部版本）
#
# 以不同组别分组，绘制不同时间点的 freq/mean/median/percent/sum 折线图。
# 返回 ggplot2 对象，供 Shiny 结果面板显示和报告导出。
# =============================================================================

#' 折线图绘制
#'
#' @param inds       数据框对象
#' @param data_cond  数据筛选条件（R 表达式字符串）
#' @param group      分组描述："分组变量名|组名1/组名2/..."
#' @param color      各组曲线颜色，使用 | 分隔
#' @param lintype    各组曲线线型，使用 | 分隔
#' @param marktype   各组数据点形状，使用 | 分隔
#' @param title      图标题文字（不含编号）
#' @param stat       统计量类型："mean"/"median"/"freq"/"percent"/"sum"
#' @param xvar       横轴变量名（数值型）
#' @param yvar       纵轴变量名（数值型）
#' @param xlabel     x 轴标签
#' @param ylabel     y 轴标签
#' @param xvalue     x 轴刻度值（数值向量）
#' @param yvalue     y 轴刻度值（数值向量）
#' @param showdata   是否在数据点旁显示统计量数值
#' @param xformat    x 轴刻度标签映射（命名字符向量）
#' @param footnote   图的脚注
#' @param limitstat  误差线类型："CLM"/"STDDEV"/"STDERR"
#'
#' @return ggplot2 对象
#' @noRd
pic_vline <- function(inds, data_cond, group,
                      # SAS styles.printer DATACONTRASTCOLORS 默认序列
                      color   = "#003F7F|#BF0000|#007F00|#7F3F00|#3F007F|#7F0040",
                      # SAS 默认 DATALINEPATTERNS 轮转序列
                      lintype = "solid|shortdash|mediumdashshortdash|longdash|mediumdash|dashdashdot",
                      # SAS attrpriority=none 默认 DATASYMBOLS 轮转序列
                      marktype = "circle|plus|X|square|diamond|triangle",
                      title = "", stat = "mean", xvar, yvar,
                      xlabel = "", ylabel = "", xvalue = NULL, yvalue = NULL,
                      showdata = FALSE, xformat = NULL, footnote = NULL,
                      limitstat = NULL) {

  # ============================================================
  # 1. 解析 group 参数
  # ============================================================
  grp_parts <- strsplit(group, "|", fixed = TRUE)[[1]]
  grp_var   <- trimws(grp_parts[1])
  grp_names <- trimws(strsplit(grp_parts[2], "/", fixed = TRUE)[[1]])
  grp_names <- grp_names[nchar(grp_names) > 0]

  # ============================================================
  # 2. 解析可选的 color / lintype / marktype
  #    对齐 lifetest_pic.R 规范：拆分 → tolower/trim → rep_len(n_grp)
  #    → setNames(vals, grp_names) 生成具名向量，按因子 levels 精确对应
  # ============================================================
  n_grp <- length(grp_names)

  # 颜色：R 颜色名大小写不敏感，统一 tolower；十六进制原样保留
  color_vals  <- tolower(trimws(strsplit(color, "|", fixed = TRUE)[[1]]))
  color_vals  <- rep_len(color_vals, n_grp)
  color_named <- stats::setNames(color_vals, grp_names)

  # 线型：SAS 线型名 → ggplot2 linetype；ggplot2 原生名直接透传
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
  lintype_raw   <- trimws(strsplit(lintype, "|", fixed = TRUE)[[1]])
  lintype_vals  <- vapply(tolower(lintype_raw), function(x) {
    if (x %in% names(sas_lintype_map)) sas_lintype_map[[x]] else x
  }, character(1))
  lintype_vals  <- rep_len(lintype_vals, n_grp)
  lintype_named <- stats::setNames(lintype_vals, grp_names)

  # 形状：SAS marktype 名 → ggplot2 shape 数值
  sas_shape_map <- c(
    circle        =  1L, triangle      =  2L, plus          =  3L,
    x             =  4L, cross         =  4L, triangledown  =  6L,
    triangleleft  = 60L, triangleright = 62L, circlefilled  = 16L,
    square        =  0L, diamond       =  5L, squarefilled  = 15L,
    diamondfilled = 18L
  )
  marktype_raw  <- trimws(strsplit(marktype, "|", fixed = TRUE)[[1]])
  shape_vals    <- vapply(tolower(marktype_raw), function(x) {
    if (x %in% names(sas_shape_map)) sas_shape_map[[x]] else
      suppressWarnings(as.integer(x))
  }, integer(1))
  shape_vals    <- rep_len(shape_vals, n_grp)
  shape_named   <- stats::setNames(shape_vals, grp_names)

  # ============================================================
  # 3. 筛选数据集
  # ============================================================
  s0 <- inds
  if (!is.null(data_cond) && nzchar(trimws(data_cond))) {
    s0 <- s0 |> dplyr::filter(!!rlang::parse_expr(data_cond))
  }

  # ============================================================
  # 4. 组别因子化并排序
  # ============================================================
  s0[[grp_var]] <- factor(s0[[grp_var]], levels = grp_names)
  s0[[xvar]]    <- as.numeric(s0[[xvar]])
  s0[[yvar]]    <- as.numeric(s0[[yvar]])
  s0 <- s0 |> dplyr::arrange(.data[[grp_var]])

  # ============================================================
  # 5. 计算统计量
  # ============================================================
  stat_fn <- switch(
    tolower(stat),
    "mean"    = function(x) mean(x, na.rm = TRUE),
    "median"  = function(x) stats::median(x, na.rm = TRUE),
    "sum"     = function(x) sum(x, na.rm = TRUE),
    "freq"    = function(x) length(x[!is.na(x)]),
    "percent" = function(x) mean(x, na.rm = TRUE) * 100,
    function(x) mean(x, na.rm = TRUE)
  )

  agg <- s0 |>
    dplyr::group_by(.data[[xvar]], .data[[grp_var]]) |>
    dplyr::summarise(
      .stat      = stat_fn(.data[[yvar]]),
      .n         = dplyr::n(),
      .sd        = stats::sd(.data[[yvar]], na.rm = TRUE),
      .se        = stats::sd(.data[[yvar]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[yvar]]))),
      .lower_clm = tryCatch(stats::t.test(.data[[yvar]])$conf.int[1], error = function(e) NA_real_),
      .upper_clm = tryCatch(stats::t.test(.data[[yvar]])$conf.int[2], error = function(e) NA_real_),
      .groups    = "drop"
    )

  # 误差线上下界
  if (!is.null(limitstat) && nchar(limitstat) > 0) {
    agg <- agg |> dplyr::mutate(
      .ymin = dplyr::case_when(
        toupper(limitstat) == "CLM"    ~ .lower_clm,
        toupper(limitstat) == "STDDEV" ~ .stat - .sd,
        toupper(limitstat) == "STDERR" ~ .stat - .se,
        TRUE ~ NA_real_
      ),
      .ymax = dplyr::case_when(
        toupper(limitstat) == "CLM"    ~ .upper_clm,
        toupper(limitstat) == "STDDEV" ~ .stat + .sd,
        toupper(limitstat) == "STDERR" ~ .stat + .se,
        TRUE ~ NA_real_
      )
    )
  }

  # ============================================================
  # 6. 绘制主图
  # ============================================================
  p <- ggplot2::ggplot(
    agg,
    ggplot2::aes(
      x        = .data[[xvar]],
      y        = .stat,
      group    = .data[[grp_var]],
      color    = .data[[grp_var]],
      linetype = .data[[grp_var]],
      shape    = .data[[grp_var]]
    )
  ) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(
      x        = xlabel,
      y        = ylabel,
      color    = "组别",
      linetype = "组别",
      shape    = "组别",
      caption  = if (!is.null(footnote) && nchar(footnote) > 0) footnote else NULL
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.box        = "horizontal",
      legend.background = ggplot2::element_rect(color = "black", fill = "white", linewidth = 0.4),
      legend.key        = ggplot2::element_rect(fill = "white"),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank()
    )

  # 6a. 颜色（对应 SAS STYLEATTRS DATACONTRASTCOLORS=...）
  #     color_named 已在 Step 2 中构建为具名向量，key=组名，按因子 levels 精确对应。
  p <- p + ggplot2::scale_color_manual(values = color_named)

  # 6b. 线型（对应 SAS STYLEATTRS DATALINEPATTERNS=...）
  #     lintype_named 已在 Step 2 中完成 SAS→ggplot2 映射，具名向量直接传入。
  p <- p + ggplot2::scale_linetype_manual(values = lintype_named)

  # 6c. 形状（对应 SAS STYLEATTRS DATASYMBOLS=...）
  #     shape_named 已在 Step 2 中完成 SAS marktype→ggplot2 shape 数值映射，具名向量直接传入。
  p <- p + ggplot2::scale_shape_manual(values = shape_named)

  # 6d. x 轴刻度 & xformat 标签映射
  if (!is.null(xvalue) && !is.null(xformat) && length(xformat) > 0) {
    p <- p + ggplot2::scale_x_continuous(
      breaks = xvalue,
      labels = function(b) { v <- xformat[as.character(b)]; ifelse(is.na(v), as.character(b), v) }
    )
  } else if (!is.null(xvalue)) {
    p <- p + ggplot2::scale_x_continuous(breaks = xvalue)
  } else if (!is.null(xformat) && length(xformat) > 0) {
    xbrk <- as.numeric(names(xformat))
    p <- p + ggplot2::scale_x_continuous(
      breaks = xbrk,
      labels = function(b) { v <- xformat[as.character(b)]; ifelse(is.na(v), as.character(b), v) }
    )
  }

  # 6e. y 轴刻度
  if (!is.null(yvalue)) {
    p <- p + ggplot2::scale_y_continuous(breaks = yvalue, limits = range(yvalue))
  }

  # 6f. 显示数据标签
  if (isTRUE(showdata)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = round(.stat, 2)),
      vjust       = 1.8,
      size        = 3,
      show.legend = FALSE
    )
  }

  # 6g. 误差线
  if (!is.null(limitstat) && nchar(limitstat) > 0) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .ymin, ymax = .ymax),
      width = 0.15
    )
  }

  p
}

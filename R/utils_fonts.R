#' fonts
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' 部署 inst/Fonts 到 Linux 并一次性注册所有目标字体
#' @param app_font_dir 默认为 system.file("Fonts", package = "BioStatsSuite")
#' @param system_dir   拷贝目标，默认 ~/.local/share/fonts
#' @param update_cache 是否 fc-cache -f
#' @importFrom sysfonts font_add
#' @importFrom showtext showtext_auto
#' @noRd
deploy_local_fonts <- function(app_font_dir = NULL,
                               system_dir   = "~/.local/share/fonts",
                               update_cache = TRUE) {

  if (Sys.info()[["sysname"]] != "Linux") return(invisible(NULL))

  ## 1. 字体来源 -------------------------------------------------------------
  if (is.null(app_font_dir))
    app_font_dir <- system.file("Fonts", package = "BioStatsSuite")
  if (!dir.exists(app_font_dir)) {
    message("⚠️ 字体目录不存在：", app_font_dir)
    return(invisible(NULL))
  }

  src_files <- list.files(app_font_dir,
                          pattern = "\\.(ttf|ttc)$",
                          full.names = TRUE, ignore.case = TRUE)
  if (length(src_files) == 0) {
    message("⚠️ 未发现 .ttf 或 .ttc 字体文件于 ", app_font_dir)
    return(invisible(NULL))
  }

  ## 2. 拷贝到用户字体目录 ---------------------------------------------------
  system_dir <- path.expand(system_dir)
  dir.create(system_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(src_files, system_dir, overwrite = TRUE)
  message("✅ 已拷贝 ", length(src_files), " 个字体到 ", system_dir)

  if (update_cache) {
    system("fc-cache -f") |> invisible()
    message("✅ 字体缓存已刷新")
  }

  ## 3. 从目标目录重新扫描 ---------------------------------------------------
  font_files <- list.files(system_dir,
                           pattern = "\\.(ttf|ttc)$",
                           full.names = TRUE, ignore.case = TRUE)

  ## 4. 按“基础名”分组注册 ---------------------------------------------------
  base_names <- tools::file_path_sans_ext(basename(font_files))  # 去掉扩展名
  base_names <- unique(gsub("[ _-](regular|bold|italic|bd|it)$", "", base_names,
                            ignore.case = TRUE))

  find_face <- function(vec, pat) vec[grepl(pat, vec, ignore.case = TRUE)][1]

  for (base in base_names) {
    reg  <- find_face(font_files, paste0(base, ".*Regular|", base, ".*normal|^", base, "\\."))
    bold <- find_face(font_files, paste0(base, ".*Bold|",  base, ".*bd"))
    ital <- find_face(font_files, paste0(base, ".*Italic|", base, ".*it"))
    boit <- find_face(font_files, paste0(base, ".*Bold.*Italic|", base, ".*Italic.*Bold"))

    if (is.na(reg)) next   # 连 Regular 都没有就跳过

    sysfonts::font_add(family     = "Linux_sans",
                       regular    = reg,
                       bold       = if (!is.na(bold)) bold else NULL,
                       italic     = if (!is.na(ital)) ital else NULL,
                       bolditalic = if (!is.na(boit)) boit else NULL)
    message("✅ 注册字体家族：Linux_sans", base,
            "  (R=", basename(reg),
            " B=", if (!is.na(bold)) basename(bold) else "-",
            " I=", if (!is.na(ital)) basename(ital) else "-", ")")
  }

  showtext::showtext_auto()
  invisible(font_files)
}

# /* 5. 包加载时执行 ----------------------------- */
.onLoad <- function(libname, pkgname) {
  tryCatch(
    suppressWarnings(deploy_local_fonts()),
    error = function(e) message("⚠️ 字体部署失败：", e$message)
  )
}

#' @noRd
ggplot_font_family_local <- function() {
  os <- Sys.info()[["sysname"]]
  if (os == "Windows") "sans"
  else if (os == "Linux")   "Linux_sans"
  else "Darwin_sans"
}

#' Plot model coefficients
#' 
#' Experimental redesign of [GGally::ggcoef()] using [gtsummary::tbl_regression()].
#' Use wit caution, syntax is subject to change.
#' 
#' @param x a regression model object
#' @param label list of formulas specifying variables labels, e.g. `list(age ~ "Age, yrs", stage ~ "Path T Stage")`
#' @param exponentiate if `TRUE`, exponentiate the coefficient estimates
#' @param include variables to include in output, may be a vector of quoted variable names, unquoted variable names, or tidyselect select helper functions
#' @param show_single_row by default categorical variables are printed on multiple rows. If a variable is dichotomous (e.g. Yes/No) and you wish to print the regression coefficient on a single row, include the variable name(s) here–quoted and unquoted variable name accepted
#' @param conf.level confidence level (between 0 and 4) for confidence intervals, default to .95, `NULL` to not display confidence intervals
#' @param intercept if `TRUE`, include the intercept in the output
#' @param signif_stars if `TRUE`, add significant stars to labels
#' @param significance level (between 0 and 1) below which a coefficient is consider to be significantly different from 0 (or 1 if `exponentiate = TRUE`), `NULL` for not highlighting such coefficients
#' @param return_data if `TRUE`, will return the data.frame used for plotting instead of the plot
#' @param ... parameters passed to [ggcoef_plot()]
#' @export
#' @examples 
#' data(trial, package = "gtsummary")
#' trial$high_marker <- factor(trial$marker > 1, label = c("low", "high"))
#' attr(trial$high_marker, "label") <- "Marker level"
#' mod <- glm(response ~ age + stage + grade + high_marker, trial, family = binomial(link = "logit"))
#' ggcoef_model(mod)
#' ggcoef_model(mod, exponentiate = TRUE)
#' ggcoef_model(mod, exponentiate = TRUE, label = c(age = "Age in years", stage = "Stage of the disease"))
#' ggcoef_model(mod, exponentiate = TRUE, show_single_row = "high_marker", intercept = TRUE)
#' ggcoef_model(mod, exponentiate = TRUE, include = c("stage", "age"))
#' ggcoef_model(mod, exponentiate = TRUE, colour = NULL, stripped_rows = FALSE, significance = NULL)
#' ggcoef_model(mod, exponentiate = TRUE, conf.level = NULL)
#' 
#' mod <- glm(response ~ stage:age + grade:stage, trial, family = binomial(link = "logit"))
#' ggcoef_model(mod, exponentiate = TRUE)
#' 
#' if (require(survival)) {
#'   test <- list(time = c(4,3,1,1,2,2,3), 
#'                 status = c(1,1,1,0,1,1,0), 
#'                 x = c(0,2,1,1,1,0,0), 
#'                 sex = c("f", "f", "f", "f", "m", "m", "m")) 
#'   mod <- coxph(Surv(time, status) ~ x + sex, test)
#'   ggcoef_model(mod, exponentiate = TRUE)
#' }
ggcoef_model <- function (
  x,
  label = NULL,
  exponentiate = FALSE,
  include = dplyr::everything(),
  show_single_row = NULL,
  conf.level = .95,
  intercept = FALSE,
  signif_stars = TRUE,
  significance = NULL,
  return_data = FALSE,
  ...
){
  data <- ggcoef_data(
    x,
    label = label,
    exponentiate = exponentiate,
    include = include,
    show_single_row = show_single_row,
    conf.level = conf.level,
    intercept = intercept,
    significance = significance
  )
  
  if (signif_stars)
    data$label <- forcats::fct_inorder(factor(paste(data$label, data$signif_stars)))
  
  if (return_data)
    return(data)
  
  args <- list(...)
  args$data <- data
  args$exponentiate <- exponentiate
  
  if (!"colour" %in% names(args)) {
    args$colour <- "variable_label"
    if (!"colour_guide" %in% names(args)) {
      args$colour_guide <- FALSE
    }
  }
  
  do.call(ggcoef_plot, args)
}

#' @rdname ggcoef_model
#' @export
#' @param mods named list of models
#' @param type a dodged plot or a facetted plot?
#' @examples 
#' 
#' # Comparison of several models
#' mod1 <- glm(response ~ age + stage + grade + high_marker, trial, family = binomial())
#' mod2 <- step(mod1, trace = 0)
#' mod3 <- glm(response ~ high_marker * stage, trial, family = binomial())
#' mods <- list("Full model" = mod1, "Simplified model" = mod2, "With interaction" = mod3)
#' 
#' ggcoef_compare(mods, exponentiate = TRUE)
#' ggcoef_compare(mods, exponentiate = TRUE, type = "faceted")
#' 
#' # you can reverse the vertical position of the point by using a negative value
#' # for dodged_width (but it will produce some warnings)
#' \dontrun{
#'   ggcoef_compare(mods, exponentiate = TRUE, dodged_width = -.9)
#' }
ggcoef_compare <- function (
  mods,
  type = c("dodged", "faceted"),
  label = NULL,
  exponentiate = FALSE,
  include = NULL,
  show_single_row = NULL,
  conf.level = .95,
  intercept = FALSE,
  significance = .05,
  return_data = FALSE,
  ...
){
  if (is.null(include)) {
    data <- lapply(
      X = mods,
      FUN = ggcoef_data,
      label = label,
      exponentiate = exponentiate,
      #include = include,
      show_single_row = show_single_row,
      conf.level = conf.level,
      intercept = intercept,
      significance = significance
    )
  } else {
    data <- lapply(
      X = mods,
      FUN = ggcoef_data,
      label = label,
      exponentiate = exponentiate,
      include = include,
      show_single_row = show_single_row,
      conf.level = conf.level,
      intercept = intercept,
      significance = significance
    )
  }
  
  data <- dplyr::bind_rows(data, .id = "model")
  x_label <- attr(data, "x_label")
  
  data$model <- forcats::fct_inorder(data$model)
  
  # Add NA values for unobserved combinations
  # (i.e. for a term present in one model but not in another)
  data <- data %>%
    tidyr::expand(model, nesting(variable, variable_label, var_type, row_ref, row_type, label)) %>% 
    dplyr::left_join(data, by = c("model", "variable_label", "variable", "var_type", "row_ref", "row_type", "label"))
  
  attr(data, "x_label") <- x_label
  
  if (return_data)
    return(data)
  
  type <- match.arg(type)
  
  args <- list(...)
  args$data <- data
  args$exponentiate <- exponentiate
  
  if (type == "dodged") {
    if (!"dodged " %in% names(args)) {
      args$dodged  <- TRUE
    }
    if (!"colour" %in% names(args)) {
      args$colour <- "model"
    }
    if (!"errorbar_coloured" %in% names(args)) {
      args$errorbar_coloured <- TRUE
    }
  } else {
    if (!"facet_col" %in% names(args)) {
      args$facet_col <- "model"
    }
    if (!"colour" %in% names(args)) {
      args$colour <- "variable_label"
      if (!"colour_guide" %in% names(args)) {
        args$colour_guide <- FALSE
      }
    }
  }
  
  do.call(ggcoef_plot, args)
}

#' @rdname ggcoef_model
#' @description 
#' [ggcoef_multinom()] is a variation of [ggcoef_model()] adapted to multinomial
#' logistic regressions performed with [nnet::multinom()].
#' [ggcoef_multinom()] works only with the dev version of `gtsummary`.
#' @param y.level_label an optional named vector for labelling `y.level` (see examples)
#' @export
#' @examples 
#' 
#' # specific function for multinom models
#' data(tips, package = "reshape")
#' library(nnet)
#' mod <- multinom(day ~ ., data = tips)
#' ggcoef_multinom(mod)
#' ggcoef_multinom(mod, y.level = c(Thur = "Thursday", Sat = "Saturday", Sun = "Sunday"))
ggcoef_multinom <- function (
  x,
  type = c("dodged", "faceted"),
  y.level_label = NULL,
  label = NULL,
  exponentiate = TRUE,
  include = dplyr::everything(),
  show_single_row = NULL,
  conf.level = .95,
  intercept = FALSE,
  significance = .05,
  return_data = FALSE,
  ...
){
  data <- ggcoef_data(
    x,
    label = label,
    exponentiate = exponentiate,
    include = include,
    show_single_row = show_single_row,
    conf.level = conf.level,
    intercept = intercept,
    significance = significance
  )
  
  if (!is.null(y.level_label))
    data$y.level <- factor(
      data$y.level, 
      levels = names(y.level_label), 
      labels = y.level_label
  ) else
    data$y.level <- forcats::fct_inorder(factor(data$y.level))
  
  # reference rows need to be duplicated for each value of y.levels
  rr <- data[!is.na(data$row_ref) & data$row_ref, ]
  yl <- levels(data$y.level)
  data[!is.na(data$row_ref) & data$row_ref, "y.level"] <- yl[1]
  
  for (i in 2:length(yl)) {
    rr$y.level <- yl[i]
    data <- dplyr::bind_rows(data, rr)
  }
  
  if (exponentiate)
    attr(data, "x_level") <- "OR"
  
  if (return_data)
    return(data)
  
  type <- match.arg(type)
  
  args <- list(...)
  args$data <- data
  args$exponentiate <- exponentiate
  
  if (type == "dodged") {
    if (!"dodged " %in% names(args)) {
      args$dodged  <- TRUE
    }
    if (!"colour" %in% names(args)) {
      args$colour <- "y.level"
    }
    if (!"errorbar_coloured" %in% names(args)) {
      args$errorbar_coloured <- TRUE
    }
  } else {
    if (!"facet_col" %in% names(args)) {
      args$facet_col <- "y.level"
    }
    if (!"colour" %in% names(args)) {
      args$colour <- "variable_label"
      if (!"colour_guide" %in% names(args)) {
        args$colour_guide <- FALSE
      }
    }
  }
  
  do.call(ggcoef_plot, args)
}

#' @rdname ggcoef_model
#' @export
ggcoef_data <- function (
  x,
  label = NULL,
  exponentiate = FALSE,
  include = dplyr::everything(),
  show_single_row = NULL,
  conf.level = .95,
  intercept = FALSE,
  significance = .05
){
  if (!requireNamespace("gtsummary"))
    stop("Package gtsummary is required.")
  
  # transform label from a named vector to a list of formulas
  if (!is.null(label) && is.character(label))
    label <- unname(mapply(
      FUN = function(n, l){formula(paste0(n, " ~ \"", l, "\""))}, 
      names(label), 
      label
    ))
  
  tbl <- gtsummary::tbl_regression(
    x = x, 
    label = label, 
    exponentiate = exponentiate,
    include = include, 
    show_single_row = show_single_row, 
    conf.level = conf.level,
    intercept = intercept
  )
  data <- tbl$table_body
  
  data[!is.na(data$row_ref) & data$row_ref, "estimate"] <- 
    ifelse(exponentiate, 1, 0)
  
  if(is.null(conf.level)) {
    data <- data[!names(data) %in% c("conf.low", "conf.high")]
  }
  
  if(!is.null(significance)) {
    data$significance <- factor(
      !is.na(data$p.value) & data$p.value <= significance, 
      levels = c(TRUE, FALSE),
      labels = paste(c("p \u2264", "p >"), significance)
    )
  }
  
  data$signif_stars <- signif_stars(data$p.value, point = NULL)
  
  # add variable labels to all rows
  var_labs <- data[data$row_type == "label", c("variable", "label")]
  names(var_labs) <- c("variable", "variable_label")
  data <- dplyr::left_join(data, var_labs, by = "variable")
  
  # keep only rows with estimate
  data <- data[!is.na(data$estimate), ]
  
  data$variable_label <- forcats::fct_inorder(data$variable_label)
  data$label <- forcats::fct_inorder(data$label)

  
  # label for x axis
  th <- tbl$table_header
  x_label <- th[th$column == "estimate", "label"]
  x_label <- gsub("\\*\\*", "", x_label)
  attr(data, "x_label") <- x_label
  
  data
}

#' @rdname ggcoef_model
#' @param data a data frame containing data to be plotted, typically the output of [ggcoef_data()]
#' @param exponentiate if `TRUE` a logarithmic scale will be used for x-axis
#' @param point_size size of the points
#' @param point_stroke thickness of the points
#' @param point_fill fill colour for the points
#' @param colour optional variable name to be mapped to colour aesthetic
#' @param colour_guide should colour guide be displayed in the legend?
#' @param colour_lab label of the colour aesthetic in the legend
#' @param shape optional variable name to be mapped to the shape aesthetic
#' @param shape_values values of the different shapes to use in [ggplot2::scale_shape_manual()]
#' @param shape_guide should shape guide be displayed in the legend?
#' @param shape_lab label of the shape aesthetic in the legend
#' @param errorbar should error bars be plotted?
#' @param errorbar_height height of error bars
#' @param errorbar_coloured should error bars be coloured as the points?
#' @param stripped_rows should stripped rows be displayed in the background?
#' @param strips_odd color of the odd rows
#' @param strips_even color of the even rows
#' @param vline should a vertical line de drawn at 0 (or 1 if `exponentiate = TRUE`)?
#' @param vline_colour colour of vertical line
#' @param dodged should points be dodged (according to the colour aesthetic)?
#' @param dodged_width width value for [ggplot2::position_dodge()]
#' @param facet_col optional variable name to be used for column facets
#' @export
ggcoef_plot <- function (
  data,
  exponentiate = FALSE,
  point_size = 2,
  point_stroke = 2,
  point_fill = "white",
  colour = NULL,
  colour_guide = TRUE,
  colour_lab = "",
  shape = "significance",
  shape_values = c(16, 21),
  shape_guide = TRUE,
  shape_lab = "",
  errorbar = TRUE,
  errorbar_height = .1,
  errorbar_coloured = FALSE,
  stripped_rows = TRUE,
  strips_odd = "#11111111", 
  strips_even = "#00000000",
  vline = TRUE,
  vline_colour = "grey50",
  dodged = FALSE,
  dodged_width = .8,
  facet_col = NULL
){
  data$label <- forcats::fct_rev(data$label)
  
  if (stripped_rows)
    data <- data %>% 
      mutate(.fill = dplyr::if_else(as.integer(label) %% 2L == 1, strips_even, strips_odd))
  
  # mapping
  mapping <- aes_string(x = "estimate", y = "label")
  
  errorbar <- errorbar & all(c("conf.low", "conf.high") %in% names(data))
  if(errorbar) {
    mapping$xmin <- aes_string(xmin = "conf.low")$xmin
    mapping$xmax <- aes_string(xmax = "conf.high")$xmax
  }
  if(!is.null(shape) && shape %in% names(data)) {
    mapping$shape <- aes_string(shape = shape)$shape
  }
  if(!is.null(colour) && colour %in% names(data)) {
    mapping$colour <- aes_string(colour = colour)$colour
    mapping$group <- aes_string(group = colour)$group
  }
  
  # position
  if (dodged)
    position <- position_dodge(dodged_width)
  else
    position <- position_identity()
  
  # plot
  p <- ggplot(data = data, mapping = mapping)
  
  if (stripped_rows)
    p <- p + 
      geom_stripped_rows(
        mapping = aes_string(y = "label", odd = ".fill", even = ".fill"), 
        inherit.aes = FALSE
      )
  
  if (vline)
    p <- p + geom_vline(xintercept = ifelse(exponentiate, 1, 0), colour = vline_colour)
  
  if(errorbar) {
    if (!is.null(colour) & errorbar_coloured) {
      p <- p + 
        geom_errorbarh(
          na.rm = TRUE, 
          height = errorbar_height,
          position = position
        )
    } else {
      p <- p + 
        geom_errorbarh(
          mapping = aes(colour = NULL), 
          na.rm = TRUE, 
          height = errorbar_height, 
          colour = "black",
          position = position
        )
    }
  }
  
  if (!is.null(facet_col))
    facet_formula <- as.formula(paste("variable_label ~ ", facet_col))
  else
    facet_formula <- variable_label ~ .
    
  p <- p + 
    geom_point(
      size = point_size, 
      stroke = point_stroke, 
      fill = point_fill,
      position = position,
      na.rm = TRUE
    ) +
    facet_grid(
      facet_formula, 
      scales = "free_y", space = "free_y", switch = "y"
    ) +
    ylab("") +
    scale_y_discrete(expand = expansion(mult = 0, add = .5)) +
    theme_light() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.placement = "outside", 
      strip.text.y.left = element_text(face = "bold", angle = 0, colour = "black", hjust = 0, vjust = 1), 
      strip.text.x = element_text(face = "bold", colour = "black"), 
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(linetype = "dashed"),
      axis.title.x = element_text(face = "bold"),
      axis.ticks.y = element_blank()
    )
  
  if(!is.null(colour) && colour %in% names(data)) {
    if (colour_guide)
      colour_guide <- guide_legend()
    p <- p +
      scale_colour_discrete(guide = colour_guide) +
      labs(colour = colour_lab)
  }
  
  if(!is.null(shape) && shape %in% names(data)) {
    if (shape_guide)
      shape_guide <- guide_legend()
    p <- p + 
      scale_shape_manual(
        values = shape_values, 
        drop = FALSE, 
        guide = shape_guide
      ) +
      labs(shape = shape_lab)
  }
  
  if (exponentiate)
    p <- p + scale_x_log10()
  
  if (!is.null(attr(data, "x_label")))
    p <- p + xlab(attr(data, "x_label"))
  
  p
}


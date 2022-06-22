#' ANOVA Plot
#'
#' @param model fitted model (usually `lm` or `aov` object)
#' @param predictor predictor variable (must specified for non-interaction plot)
#' @param graph_label_name graph label name
#'
#' @return plot object
#' @export
#'
#' @examples
#' fit = iris %>% lm(data = ., Sepal.Length ~ Species)
#' anova_plot(fit,predictor = Species)
#'
anova_plot <- function(model,
                       predictor = NULL,
                       graph_label_name = NULL) {
  response_var_name = insight::find_response(model)

  response_var = dplyr::enquo(response_var_name)

  model_data = insight::get_data(x = model)

  interaction_term = get_interaction_term(model)

  ########################################### ANOVA plot without interaction ##################################################################
  if (is.null(interaction_term)) {
    predictor = dplyr::enquo(predictor)

    mean = model_data %>%
      dplyr::group_by(!!predictor) %>%
      dplyr::summarise(dplyr::across(!!response_var, ~ mean(., na.rm = T))) %>%
      dplyr::rename(mean = !!response_var)

    se = model_data %>%
      dplyr::group_by(!!predictor) %>%
      dplyr::summarise(dplyr::across(
        !!response_var,
        ~ stats::sd(., na.rm = TRUE) / sqrt(dplyr::n())
      )) %>%
      dplyr::rename(se = !!response_var)

    plot_df = mean %>% dplyr::full_join(se) %>%
      dplyr::rename(predict_var1 = !!predictor)

    # label name
    predictor_name = model_data %>% dplyr::select(!!predictor) %>% colnames()
    label_name = label_name(
      graph_label_name = graph_label_name,
      response_var_name = response_var_name,
      predict_var1_name = predictor_name,
      predict_var2_name = NULL,
      predict_var3_name = NULL
    )

    main_plot = plot_df %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$predict_var1, y = mean)) +
      ggplot2::geom_bar(
        stat = 'identity',
        width = 0.5,
        color = 'black',
        fill = 'white',
        position = ggplot2::position_dodge(0.9)
      ) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                    position = ggplot2::position_dodge(0.9),
                    width = 0.1) +
      ggplot2::labs(y = label_name[1],
                    x = label_name[2])

    ########################################### ANOVA plot with interaction ##################################################################
  } else{
    if (is.null(predictor)) {
      # get predictor if predictor is not supplied
      predict_var1 = get_interaction_term(model)$predict_var1
      predict_var2 = get_interaction_term(model)$predict_var2
      predict_vars = c(predict_var1, predict_var2)
      if (length(get_interaction_term(model)) == 3) {
        predict_var3 = get_interaction_term(model)$predict_var3
        predict_vars = c(predict_vars, predict_var3)
      }
    } else{
      predict_var1 = predictor[1]
      predict_var2 = predictor[2]
      predict_vars = c(predict_var1, predict_var2)
      if (length(predictor) == 3) {
        predict_var2 = predictor[3]
        predict_vars = c(predict_vars, predict_var3)
      }
    }

    predict_vars = ggplot2::enquos(predict_vars)

    mean = model_data %>%
      dplyr::group_by(dplyr::across(!!!predict_vars)) %>%
      dplyr::summarise(dplyr::across(!!response_var, ~ mean(., na.rm = T))) %>%
      dplyr::rename(mean = !!response_var)

    se = model_data %>%
      dplyr::group_by(dplyr::across(!!!predict_vars)) %>%
      dplyr::summarise(dplyr::across(
        !!response_var,
        ~ stats::sd(., na.rm = TRUE) / sqrt(dplyr::n())
      )) %>%
      dplyr::rename(se = !!response_var)

    # two-way interaction plot
    if (length(get_interaction_term(model)) == 2) {
      plot_df =  mean %>% dplyr::full_join(se) %>%
        dplyr::rename(predict_var1 = !!dplyr::enquo(predict_var1)) %>%
        dplyr::rename(predict_var2 = !!dplyr::enquo(predict_var2))

      label_name = label_name(
        graph_label_name = graph_label_name,
        response_var_name = response_var_name,
        predict_var1_name = predict_var1,
        predict_var2_name = predict_var2,
        predict_var3_name = NULL
      )

      main_plot = plot_df %>%
        ggplot2::ggplot(data = .,
                        ggplot2::aes(
                          x = .data$predict_var1,
                          y = .data$mean,
                          fill = .data$predict_var2
                        )) +
        ggplot2::geom_bar(
          stat = 'identity',
          width = 0.5,
          color = 'black',
          position = ggplot2::position_dodge(0.9)
        ) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                      position = ggplot2::position_dodge(0.9),
                      width = 0.1) +
        ggplot2::labs(y = label_name[1],
                      x = label_name[2],
                      fill = label_name[3])

      # three-way interaction plot
    } else if (length(get_interaction_term(model)) == 3) {
      plot_df =  mean %>% dplyr::full_join(se) %>%
        dplyr::rename(predict_var1 = !!dplyr::enquo(predict_var1)) %>%
        dplyr::rename(predict_var2 = !!dplyr::enquo(predict_var2)) %>%
        dplyr::rename(predict_var3 = !!dplyr::enquo(predict_var3))

      label_name = label_name(
        graph_label_name = graph_label_name,
        response_var_name = response_var_name,
        predict_var1_name = predict_var1,
        predict_var2_name = predict_var2,
        predict_var3_name = predict_var3
      )

      main_plot = plot_df %>%
        ggplot2::ggplot(data = .,
                        ggplot2::aes(
                          x = .data$predict_var1,
                          y = .data$mean,
                          fill = .data$predict_var2
                        )) +
        ggplot2::geom_bar(
          stat = 'identity',
          width = 0.5,
          color = 'black',
          position = ggplot2::position_dodge(0.9)
        ) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                      position = ggplot2::position_dodge(0.9),
                      width = 0.1) +
        ggplot2::labs(y = label_name[1],
                      x = label_name[2],
                      fill = label_name[3]) +
        ggplot2::facet_wrap( ~ .data$predict_var3)
    }
  }

  # add aesthetics
  final_plot = main_plot +
    ggplot2::scale_fill_brewer() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black")
    )

  return(final_plot)
}

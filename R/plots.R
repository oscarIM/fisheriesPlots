#' @title plot_multipanel
#' @description Función para realizar gráfico multipanel
#' @param data dataframe de entrada
#' @param dir_input directory en donde se almacenan las imágenes L2
#' @param dir_output directory en donde se almacenaran las imágenes L3 resultantes
#' @param var_name nombre de la variable a analizar ("chlor_a", "sst", "Rrs_645", "pic", "poc", "nflh", etc)
#' @param n_cores número de núcleos a usar. Por defecto, n_cores = 1 (corrida secuencial). La parelelización es respecto de la cantidad de sub_folder procesados simultaneamente
#' @param res_l2 resolución para l2bin. Por defecto, res = "1" (H: 0.5km, Q: 250m, HQ: 100m, HH: 50m, 1: 1.1km, 2: 2.3km, 4: 4.6km, 9: 9.2km, 18: 18.5km, 36: 36km, 1D: 1 degree, HD: 0.5 degree, QD: 0.25 degree)
#' @param res_l3 resolución para l3mapgen. Por defecto, res = "1km" (36km: 1080 x 540, 18km: 2160 x 1080, 9km: 4320 x 2160, 4km: 8640 x 4320, 2km: 17280 x 8640, 1km: 34560 x 17280, hkm: 69120 x 34560, qkm: 138240 x 69120, smi: 4096 x 2048, smi4: 8192 x 4096, land: 8640 x 4320)
#' @param north latitud norte para la generación de las imágenes L3
#' @param south latitud sur para la generación de las imágenes L3
#' @param west latitud oeste para la generación de las imágenes L3
#' @param east latitud este para la generación de las imágenes L3
#' @param need_extract mantener sistema de archivos año/mes? (TRUE/FALSE).Por defecto, FALSE
#' @param sort_files crearr sistema de archivos año/mes? (TRUE/FALSE).Por defecto, FALSE
#' @return imágenes L3
#' @importFrom fs dir_ls dir_create file_move dir_delete file_delete path_file
#' @importFrom readr read_delim
#' @importFrom dplyr filter mutate
#' @importFrom lubridate as_date year month
#' @importFrom purrr walk walk2 possibly
#' @importFrom stringr str_remove str_detect str_replace
#' @importFrom tibble tibble
#' @importFrom furrr future_walk future_walk2
#' @importFrom future plan cluster
#' @importFrom parallel stopCluster makeForkCluster
#' @importFrom utils untar
#' @importFrom tictoc tic toc
#' @importFrom progressr with_progress progressor
#' @export plot_multipanel
#' @examples
#' \dontrun{
#' code here
#' }
###especificar las columnas de los meses
plot_multipanel <- function(data, dicc, caletas = NULL, sp_remove = NULL, ylab_text_a, xlab_text_a, ylab_text_c, xlab_text_c, n_break_ylab, width, height, output_name, n_pie, round) {
  options(scipen = 999)
  # read data and dicc
  dicc <- readr::read_delim(dicc, delim = ";")
  data <- janitor::clean_names(data_desembarque) %>%
    pivot_longer(data = data, cols = ene:dic, names_to = "Meses", values_to = "desembarque", values_drop_na = TRUE)
  data <- data %>% dplyr::mutate(recurso = stringr::str_to_upper(recurso),
                          Meses = stringr::str_to_upper(Meses))
  if (!missing(caletas)) {
    data <- data %>% dplyr::filter(caleta %in% caletas)
    names <- data$recurso
  } else {
    data <- data
    names <- data$recurso
  }
  #cambiar a map cuando se pueda
  for (i in 1:nrow(dicc)) {
    names <- stringr::str_replace(names, dicc$original_names[i], dicc$fixed_names[i])
  }
  data$recurso <- names
  if (missing(sp_remove)) {
    data <- data
  } else {
    data <- data %>% dplyr::filter(!recurso %in% sp_remove)
  }
  #### empezar los plots####
  #### plot A: total anual, apilado por tipo de recurso con todas las especies####
  col_tipo <- c("#33a02c", "#fdbf6f", "#1f78b4")
  names(col_tipo) <- c("Algas", "Invertebrados", "Peces")
  summ <- data %>%
    dplyr::group_by(tipo, ano) %>%
    dplyr::summarise(total_desembarque = sum(desembarque, na.rm = T))
  plot_a <- ggplot(summ, aes(fill = tipo, y = total_desembarque, x = as.factor(ano)), na.rm = T) +
    geom_bar(position = "stack", stat = "identity") +
    ylab(ylab_text_a) +
    xlab(xlab_text_a) +
    scale_y_continuous(n.breaks = n_break_ylab) +
    ggtitle(label = "Tipo Especies") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 11)) +
    scale_fill_manual(breaks = c("Peces", "Algas", "Invertebrados"), values = alpha(col_tipo, 0.7)) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank())
  #### plot B: pie plot for n resources to display: total. Como esta escrito actualmente, solo funciona para 8 recursos
  data_desembarque_list <- data %>%
    group_by(tipo) %>%
    group_split()
  plot_torta <- function(data) {
    summ <- data %>%
      group_by(recurso) %>%
      summarise(total_desembarque = sum(desembarque, na.rm = T)) %>%
      ungroup()
    summ_perct <- summ %>%
      mutate(perct = total_desembarque / sum(total_desembarque) * 100) %>%
      arrange(desc(perct)) %>%
      mutate(sum_cum = cumsum(perct))
    if (nrow(summ_perct) <= n_pie) {
      selected_sp <- summ_perct %>% pull(recurso)
    } else {
      selected_sp <- summ_perct %>%
        slice_head(n = n_pie) %>%
        pull(recurso)
    }
    summ <- summ %>%
      mutate(recurso = if_else(recurso %in% selected_sp, recurso, "OTROS")) %>%
      group_by(recurso) %>%
      summarise(total_desembarque = sum(total_desembarque, na.rm = T)) %>%
      arrange(desc(total_desembarque)) %>%
      ungroup()
    order <- summ$recurso
    summ <- summ %>% mutate(
      recurso = factor(recurso, levels = order),
      cumulative = rev(cumsum(rev(total_desembarque))),
      pos = total_desembarque / 2 + lead(cumulative, 1),
      pos = if_else(is.na(pos), total_desembarque / 2, pos),
      labels = paste0(round((total_desembarque / sum(total_desembarque)) * 100, round), "%")
    )
    summ <- summ %>% mutate(recurso = paste0(recurso, " ", "(", labels, ")"))
    ot <- summ %>% filter(str_detect(recurso, "^OTROS"))
    if (nrow(ot) > 0) {
      not_ot <- summ %>% filter(!recurso == ot$recurso)
      summ <- bind_rows(not_ot, ot)
    } else {
      summ <- summ
    }
    order <- summ$recurso
    # evaluar si es necesesario agregar el gris a la categoria
    ### if(para la paleta de colores)
    if (unique(data$tipo == "Peces")) {
      if (any(grepl("otros", ignore.case = T, x = summ$recurso) == TRUE)) {
        cols <- brewer.pal(length(order), "Blues")
        cols <- rev(cols)
        names(cols) <- order
        cols[n_pie + 1] <- "gray50"
      } else {
        cols <- brewer.pal(length(order), "Blues")
        cols <- rev(cols)
        names(cols) <- order
      }
      title <- "PECES"
    }
    if (unique(data$tipo == "Algas")) {
      if (any(grepl("otros", ignore.case = T, x = summ$recurso) == TRUE)) {
        cols <- brewer.pal(length(order), "Greens")
        cols <- rev(cols)
        names(cols) <- order
        cols[n_pie + 1] <- "gray50"
      } else {
        cols <- brewer.pal(length(order), "Greens")
        cols <- rev(cols)
        names(cols) <- order
      }
      summ$recurso <- str_replace(summ$recurso, pattern = "OTROS", replacement = "OTRAS")
      title <- "ALGAS"
    }
    if (unique(data$tipo) == "Invertebrados") {
      if (any(grepl("otros", ignore.case = T, x = summ$recurso) == TRUE)) {
        cols <- brewer.pal(length(order), "Oranges")
        cols <- rev(cols)
        names(cols) <- order
        cols[n_pie + 1] <- "gray50"
      } else {
        cols <- brewer.pal(length(order), "Oranges")
        cols <- rev(cols)
        names(cols) <- order
      }
      title <- "INVERTEBRADOS"
    }
    # title agregar if para el title
    plot <- ggplot(summ, aes(x = "", y = total_desembarque, fill = fct_inorder(recurso))) +
      geom_col(width = 1, color = "white") +
      coord_polar("y", start = 0, direction = -1) +
      guides(fill = guide_legend(title = title, ncol = 1)) +
      scale_fill_manual(breaks = order, values = cols) +
      theme_void() +
      theme(legend.text = element_text(size = 11))
    plot
  }
  p_list <- map(data_desembarque_list, ~ plot_torta(.))
  if (length(p_list) == 3) {
    layout_matrix <- matrix(c(1, 1, 2, 2, NA, 3, 3, NA), nrow = 2, byrow = TRUE)
  }
  if (length(p_list) == 2) {
    layout_matrix <- matrix(c(1, 1, 2, 2), nrow = 1, byrow = TRUE)
  }
  if (length(p_list) == 1) {
    layout_matrix <- matrix(c(1, 1), nrow = 1, byrow = TRUE)
  }
  plot_b <- grid.arrange(grobs = p_list, layout_matrix = layout_matrix)
  ##### plot C: desembarco promedio por medio por tipo de recurso#####
  summ_tipo_mensual <- data %>%
    group_by(tipo, Meses) %>%
    summarise(desembarque_promedio = mean(desembarque, na.rm = T)) %>%
    ungroup()
  col_tipo <- c("#33a02c", "#fdbf6f", "#1f78b4")
  names(col_tipo) <- c("Algas", "Invertebrados", "Peces")
  pos <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
  plot_c <- ggplot(summ_tipo_mensual, aes(fill = tipo, y = desembarque_promedio, x = as.factor(Meses)), na.rm = T) +
    geom_bar(position = "stack", stat = "identity") +
    ylab(ylab_text_c) +
    xlab(xlab_text_c) +
    scale_y_continuous(n.breaks = n_break_ylab) +
    scale_x_discrete(limits = pos) +
    ggtitle(label = "Tipo Especies") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 11)) +
    scale_fill_manual(breaks = c("Peces", "Algas", "Invertebrados"), values = alpha(col_tipo, 0.7)) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank())
  p_final <- grid.arrange(grobs = list(plot_a, plot_b, plot_c), nrow = 3, rel_heights = c(1, 1, 1), rel_widths = c(1, 1, 1), align = "v")
  ggsave(output_name, p_final, units = "in", width = width, height = height, dpi = 300)
}

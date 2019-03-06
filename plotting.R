color_scale <- function(n) {
  colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
  return(colors[1:n])
}

error_history_plot_sc <- function(serial_number) {
  
  sensor_id <- tbl(dbcon, "SENSOR") %>%
    filter(SENSOR_SN == serial_number & PARAMETER == "Specific cond at 25C") %>%
    head(1) %>%
    pull(SENSOR_ID)
  
  readings <- tbl(dbcon, "SC_CHECK") %>%
    select(SC_ID, SENSOR_ID) %>%
    filter(SENSOR_ID == sensor_id) %>%
    inner_join(tbl(dbcon, "SC_READING"), by = "SC_ID") %>%
    filter(STD_VALUE != 0 & TYPE == "CALI") %>%
    mutate(ERROR = ((READING - STD_VALUE) / STD_VALUE) * 100) %>%
    collect()
  readings$DATETIME <- as.POSIXct(readings$DATETIME)
  readings$STD_VALUE <- as.character(readings$STD_VALUE)
  
  ylimits <- c(max(abs(readings$ERROR), na.rm = TRUE) * -1.1, 
               max(abs(readings$ERROR), na.rm = TRUE) * 1.1)
  n_standards <- length(unique(readings$STD_VALUE))
  
  plot <- ggplot(data = readings) +
    geom_point(aes(x = DATETIME, y = ERROR, group = STD_VALUE, color = STD_VALUE),
               size = 3, shape = 17) + 
    scale_color_manual(values = color_scale(n_standards), name = "Standard") + 
    scale_y_continuous(limits = ylimits) +
    geom_hline(yintercept = 0) +
    xlab("Date") +
    ylab("Percent error") +
    ggtitle("Calibration history", 
            subtitle = paste(serial_number, "Specific conductance at 25C", sep = ", "))
  
  return(plot)
  
}

error_history_plot_tby <- function(serial_number) {
  
  sensor_id <- tbl(dbcon, "SENSOR") %>%
    filter(SENSOR_SN == serial_number & PARAMETER == "Turbidity, FNU") %>%
    head(1) %>%
    pull(SENSOR_ID)
  
  readings <- tbl(dbcon, "TBY_CHECK") %>%
    select(TBY_ID, SENSOR_ID) %>%
    filter(SENSOR_ID == sensor_id) %>%
    inner_join(tbl(dbcon, "TBY_READING"), by = "TBY_ID") %>%
    filter(STD_VALUE != 0 & TYPE == "CALI") %>%
    mutate(ERROR = ((READING - STD_VALUE) / STD_VALUE) * 100) %>%
    collect()
  readings$DATETIME <- as.POSIXct(readings$DATETIME)
  readings$STD_VALUE <- as.character(readings$STD_VALUE)
  
  ylimits <- c(max(abs(readings$ERROR), na.rm = TRUE) * -1.1, 
               max(abs(readings$ERROR), na.rm = TRUE) * 1.1)
  n_standards <- length(unique(readings$STD_VALUE))
  
  plot <- ggplot(data = readings) +     
    geom_point(aes(x = DATETIME, y = ERROR, group = STD_VALUE, color = STD_VALUE),
               size = 3, shape = 17) + 
    scale_color_manual(values = color_scale(n_standards), name = "Standard") + 
    scale_y_continuous(limits = ylimits) +
    geom_hline(yintercept = 0) +
    xlab("Date") +
    ylab("Percent error") +
    ggtitle("Calibration history", 
            subtitle = paste(serial_number, "Turbidity, FNU", sep = ", "))
  
  return(plot)
  
}


which_ph_std <- function(std_values) {
  cat <- data.frame(std_values)
  cat$s4 <- abs(4 - cat$std_values)
  cat$s7 <- abs(7 - cat$std_values)
  cat$s10 <- abs(10 - cat$std_values)
  cat$standard <- NA
  cat$standard[cat$s4 < 0.5] <- 4
  cat$standard[cat$s7 < 0.5] <- 7
  cat$standard[cat$s10 < 0.5] <- 10
  return(cat$standard)
}

error_history_plot_ph <- function(serial_number) {
  
  sensor_id <- tbl(dbcon, "SENSOR") %>%
    filter(SENSOR_SN == serial_number & PARAMETER == "pH") %>%
    head(1) %>%
    pull(SENSOR_ID)
  
  readings <- tbl(dbcon, "PH_CHECK") %>%
    select(PH_ID, SENSOR_ID) %>%
    filter(SENSOR_ID == sensor_id) %>%
    inner_join(tbl(dbcon, "PH_READING"), by = "PH_ID") %>%
    filter(STD_VALUE != 0 & TYPE == "CALI") %>%
    mutate(ERROR = ((READING - STD_VALUE) / STD_VALUE) * 100) %>%
    collect()
  readings$DATETIME <- as.POSIXct(readings$DATETIME)
  readings$STD_VALUE <- as.character(which_ph_std(readings$STD_VALUE))
  
  ylimits <- c(max(abs(readings$ERROR), na.rm = TRUE) * -1.1, 
               max(abs(readings$ERROR), na.rm = TRUE) * 1.1)
  n_standards <- length(unique(readings$STD_VALUE))
  
  plot <- ggplot(data = readings) +     
    geom_point(aes(x = DATETIME, y = ERROR, group = STD_VALUE, color = STD_VALUE),
               size = 3, shape = 17) + 
    scale_color_manual(values = color_scale(n_standards), name = "Standard") + 
    scale_y_continuous(limits = ylimits) +
    geom_hline(yintercept = 0) +
    xlab("Date") +
    ylab("Percent error") +
    ggtitle("Calibration history", subtitle = paste(serial_number, "pH", sep = ", "))
  
  return(plot)
  
}
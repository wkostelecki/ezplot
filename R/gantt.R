#Define timestamp format - DO NOT EDIT, AUTO UPDATES IN PLOT
# timeStamp <- stamp("Created Monday 23 January 1999, 14:35", quiet = TRUE)


#' Create Gantt Chart
#'
#' @description Function to create a ggplot2 gantt chart
#'
#' @param data Data frame
#' @param task Column name giving task descriptions
#' @param start Column name giving column containing start dates (in `POSIXct` format)
#' @param end Column name giving column containing start dates (in `POSIXct` format)
#' @param category (optional) Column name defining categories to be used to
#' split the gantt chart
#' @param sprint.start (optional) Start date for the sprint (`POSIXct`)
#' @param sprint.end (optional) End date for the sprint (`POSIXct`)
#' @param progress (optional) Column name giving progress as a percentage.
#' Will only be used if sprint start and end are provided.
#'
#' @import ggplot2 rlang dplyr
#' @importFrom lubridate now
#' @export
gantt_plot <- function(data,
                       task,
                       start,
                       end,
                       category,
                       sprint.start = NULL,
                       sprint.end = NULL,
                       progress = NULL){

  timeStamp <- stamp("Created Monday 23 January 1999, 14:35", quiet = TRUE)
  task <- enquo(task)
  start <- enquo(start)
  end <- enquo(end)
  category <- enquo(category)
  progress <- enquo(progress)

  gantt <- ggplot(data = data)

  # set a blank layer to force the coordinates
  gantt <- gantt + geom_blank(aes(x = !!task, y = !!start))

  # add the sprint as a coloured rectangle
  if(!is.null(sprint.start) & !is.null(sprint.end)){
    gantt <- gantt + geom_rect(aes(xmin = -Inf, xmax = Inf,
                                   ymin = sprint.start,
                                   ymax = sprint.end), fill= "grey81")
  }



  # add the tasks
  gantt <- gantt + geom_linerange(aes(x = !!task,
                                      ymin = !!start,
                                      ymax = !!end,
                                      colour = !!category),
                                  size = 8, alpha = 0.9)

  # mark today
  gantt <- gantt + geom_hline(aes(yintercept = now()), colour = "red")


  range_plan = data %>%
    summarize(min = min(!!start, sprint.start),
             max = max(!!end, sprint.end)) %>%
    mutate(diff = max - min) %>%
    pull(diff)

  # add the % progress, only for tasks in current sprint
  if(!is.null(progress) & !is.null(sprint.start) & !is.null(sprint.end)){

    complete <- data %>%
      filter(!!start < sprint.end) %>%
      mutate(!!quo_name(progress) := paste0(!!progress, "%"))

    gantt <- gantt + geom_text(aes(x = !!task,
                                   y = !!end + 0.003 * range_plan,
                                   label = !!progress),
                               data = complete, hjust = 0,
                               vjust = 0.38)
  }

  # Split by category, let space be defined by category and don't include empty tasks
  if(!is.null(category)){
    gantt <- gantt + facet_grid(vars(!!category), scales = "free", space = "free")
  }

  # Style layers
  gantt <- gantt +
    coord_flip(
      # ylim = c(startDate, endDate)
      ) +
    labs(x = NULL, y = NULL) +
    scale_y_datetime(date_breaks = "1 month", date_labels = "%b %y") +
    scale_colour_viridis_d(guide = FALSE) +
    theme_light() +
    theme(panel.grid.major.x = element_line(linetype = 2),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank()) +
    labs(caption = timeStamp(now()))

  return(gantt)

}

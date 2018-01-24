#' Convert variables from matlab file into a tidy format table
#'
#' Given variable of a matlab file,
#' identify important variables, which are cell_id, centroid_x, centroid_y,
#' mean_resp_measured, trace_response,modify these variables
#' correct the names of these columns and put in tidy format
#'
#' The output is a tidy table with the following columns
#' cell_id, SF, Repeat, Orientation, Frame, variable and value
#'
#' The variables are centroid_x, centroid_y, mean_resp_measured, trace_response.
#'
#' @importFrom magrittr %<>%
#' @importFrom dplyr filter select mutate bind_rows %>%
#' @importFrom reshape2 melt
#' @param x is the raw data from matlab file
#' @examples
#' path =  system.file('extdata', 'test', 'matlab', package = 'rPacu')
#' object <- R.matlab::readMat(file.path(path, "096_001_000.mat"))
#' df <- rPacu:::tidy_pacu_table_matlab(object)


tidy_pacu_table_matlab <- function(x) {
  stopifnot(is.list(x))

  # Set colnames
  matlab_namecols(x)
  cell_length<-matlab_celllength(x)
  sf_num <- cell_length[1]
  orientations_num <- cell_length[2]
  trials_num <- cell_length[3]
  frames_num <- cell_length[4]
  num_rows <- 2 + sf_num * orientations_num + sf_num * orientations_num * trials_num * frames_num
  # Initialize table
  cellinfo <- x$merged.dict$rois
  num_cells <- dim(cellinfo)[1]
  data_cell <- matrix(nrow=num_rows * num_cells,ncol=7)

  for (c in 1:num_cells){
    ## Identify variables
    # Variable: cell.id
    # rownames(cellinfo)[c] %>% substr(9,11) -> cell_id
    cell_id <- substr(rownames(cellinfo)[c],9,11)

    #variable: centroid_x, centroid_y
    roi_centroid_location <- cellinfo[[c]]$rois[[1]][[1]]$attributes$centroid

    #variable(matrix): mean_resp_measured, 5 space-frequencies * 8 orientations
    sf_num <- length(cellinfo[[c]]$dtorientationsfits)
    mean_resp_measured <- list()
    sf_mean_resp_measured <- list()
    orientations <- list()
    for (s in 1:sf_num){
      sf_mean_resp_measured[s]<-cellinfo[[c]]$dtorientationsfits[[s]][[1]]$attributes$trial.sf
      orientations[[s]]<-c(cellinfo[[c]]$dtorientationsfits[[s]][[1]]$attributes$value$orientations)
      temp_matrix <- cellinfo[[c]]$dtorientationsfits[[s]][[1]]$attributes$value$y.meas
      mean_resp_measured[[s]] <- temp_matrix[c(orientations[[s]] + 1)]
    }


    # variable(lists): trace_response 5 space-frequencies * 8 repeats * 8 orientations * 31(15+16) frames
    sf_num <- length(cellinfo[[c]]$dtorientationsmeans)
    orientations_num <- length(orientations[[1]])
    trials_num <- dim(cellinfo[[c]]$dtorientationsmeans[[1]][[1]]$attributes$matrix)[1]
    frames_num <- dim(cellinfo[[c]]$dtorientationsmeans[[1]][[1]]$attributes$matrix)[2] / orientations_num
    sf_trace_response <- list()

    # trials is counted by trials_num, frames is counted by frame_num
    trace_response <- array(rep(NA, sf_num * orientations_num * trials_num * frames_num), dim=c(sf_num, orientations_num, trials_num, frames_num))
    for (s in 1:sf_num) {
      sf_trace_response[s] <- cellinfo[[c]]$dtorientationsmeans[[s]][[1]]$attributes$trial.sf
      temp_matrix <- cellinfo[[c]]$dtorientationsmeans[[s]][[1]]$attributes$matrix
      for (t in 1:trials_num) {
        for (o in 1:orientations_num) {
          trace_response[s, t, o, ] <- temp_matrix[t, c(((o - 1) * frames_num + 1):(o * frames_num))]
        }
      }
    }
    # change the dimensional order of list
    inverse_trace_response <- aperm(trace_response,c(4, 3, 2, 1))

    ## Change variables to tidy format
    idx <- (num_rows * (c - 1) + 1)
    # centroid_x, centroid_y
    data_cell[idx, ] <- c(cell_id, '', '', '', '', 'centroid_x', roi_centroid_location$x[1])
    idx <- idx + 1
    data_cell[idx, ] <- c(cell_id, '', '', '', '', 'centroid_y', roi_centroid_location$y[1])
    idx <- idx + 1

    # mean_resp_measured
    for (s in 1:sf_num){
      for (o in 1:orientations_num) {
        data_cell[idx, ] <- c(cell_id, sf_mean_resp_measured[[s]], orientations[[s]][o], '', '', 'mean_resp_measured', mean_resp_measured[[s]][o])
        idx <- idx + 1
      }
    }

    # trace_response
    for (s in 1:sf_num) {
      for (o in 1:orientations_num) {
        for (t in 1:trials_num) {
          for (f in 1:frames_num) {
            data_cell[idx, ] <- c(cell_id, sf_mean_resp_measured[[s]], orientations[[s]][o], t, f, "trace_response", inverse_trace_response[ , , t, s][ , o][f])
            idx <- idx + 1
          }
        }
      }
    }
  }

  data_cell <- as.data.frame(data_cell,stringsAsFactors = FALSE)
  colnames(data_cell) <- c("cell_id", "SF", "orientation", "trial", "frame", "variable", "value")
  return(data_cell)
}

matlab_celllength <- function(x) {
  # for the same matlab file the sf_num, orientations_num, trials_num, frames_num are the same
  sf_num <- length(x$merged.dict$rois[[1]]$dtorientationsfits)
  orientations_num <- length(x$merged.dict$rois[[1]]$dtorientationsfits[[1]][[1]]$attributes$value$orientations)
  trials_num <- dim(x$merged.dict$rois[[1]]$dtorientationsmeans[[1]][[1]]$attributes$matrix)[1]
  frames_num <- dim(x$merged.dict$rois[[1]]$dtorientationsmeans[[1]][[1]]$attributes$matrix)[2]/orientations_num

  c(sf_num,orientations_num,trials_num,frames_num)
}

matlab_namecols <- function(x) {
  # first dimention
  eval.parent(substitute(rownames_to_colnames(x$merged.dict)))
  # second dimention
  eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois)))
  n<- dim(x$merged.dict$rois)[1]
  for (c in 1:n) {
  # other dimentions
    #variable: centroid_x, centroid_y
    eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois[[c]])))
    eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois[[c]]$rois[[1]][[1]])))
    eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois[[c]]$rois[[1]][[1]]$attributes)))
    eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois[[c]]$rois[[1]][[1]]$attributes$centroid)))
    #variable: mean_resp_measured
    SF <- length(eval.parent(substitute(x$merged.dict$rois[[c]]$dtorientationsfits)))
    for (s in 1:SF) {
      eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois[[c]]$dtorientationsfits[[s]][[1]])))
      eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois[[c]]$dtorientationsfits[[s]][[1]]$attributes)))
      eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois[[c]]$dtorientationsfits[[s]][[1]]$attributes$value)))
    }

    # variable: trace_response
    SF <- length(eval.parent(substitute(x$merged.dict$rois[[c]]$dtorientationsmeans)))
    for (s in 1:SF) {
      eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois[[c]]$dtorientationsmeans[[s]][[1]])))
      eval.parent(substitute(rownames_to_colnames(x$merged.dict$rois[[c]]$dtorientationsmeans[[s]][[1]]$attributes)))
    }
   }
}

rownames_to_colnames <- function(x) {
  eval.parent(substitute(names(x) <- rownames(x)))
}

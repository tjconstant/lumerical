#' Read lumerical data
#'
#' @param filename
#' the name of the file which the data are to be read from. If it does not contain an absolute path, the file name is relative to the current working directory, getwd(). Tilde-expansion is performed where supported.
#'
#' @return
#' returns either a data.frame for the 1D data type, or a list object with coordinates and a matrix of extracted values for the 2D data type.
#' @export
#'
#' @examples
#' # read.lumeircal("results.txt")
#' @author Tom Constant
#' @details
#' read.lumerical will attempt to query the file to discover if it is a 1D or 2D data type. The line-scans of the 1D type retuns a data.frame. The 2D dat type returns a list object containing coordinated and a matrix of values for the 2D plane.


read.lumerical <- function(filename) {

  header <- read.csv(filename, nrows = 1)
  if (length(header) == 1) {
    data <- read.csv(filename, skip = 1)
    return(data)
  } else
    if (length(header == 3)) {
      file <- readLines(filename)
      dimension_locations <-
        grep(file, pattern = "([0-999],[0-999])")

      c1 <- regexpr(file[dimension_locations], pattern = "[0-999]")
      c2 <- regexpr(file[dimension_locations], pattern = ",")

      nv <-
        as.numeric(substr(file[dimension_locations[1]], c1[1], c2[1] - 1))
      nu <-
        as.numeric(substr(file[dimension_locations[2]], c1[2], c2[2] - 1))

      u <-
        as.numeric(file[(dimension_locations[1] + 1):(dimension_locations[1] +
                                                        nu)])
      v <-
        as.numeric(file[(dimension_locations[2] + 1):(dimension_locations[2] +
                                                        nv)])

      w <-
        na.omit(as.numeric(unlist(strsplit(file[((dimension_locations[3] + 1):length(file))], split = " "))))
      w_m <- matrix(w,
                    byrow = T,
                    nrow = nu,
                    ncol = nv)

      name_vector <-
        c(substr(file[dimension_locations[1]], 0, c1[1] - 2),
          substr(file[dimension_locations[2]], 0, c1[2] - 2),
          file[dimension_locations[3]])

      out_df <- data.frame(u = u, v = v)
      names(out_df) <- name_vector[1:2]

      out <- list(coordinates = out_df,
                  matrix = w_m,
                  title = name_vector[3])

      return(out)
    } else{
      stop("unable to read lumerical file")
    }


}


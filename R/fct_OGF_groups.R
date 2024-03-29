#' OGF_groups 
#'
#' @description This function generates groups of genes and biological processes
#' based on an incidence matrix.
#' @param incidence_matrix A binary incidence matrix that relates genes
#' and biological processes
#' @param gorups The maximum number of groups to be formed.
#' @param mpg The maximum number of biological processes per group. 
#' @return A list of indices defining genes, biological processes, 
#' and the groups to which they belong.
#'
#' @export
OGF_groups <- function(incidence_matrix ,gorups, mpg){
  if (missing(incidence_matrix)) stop("The incidence matix is required")
  if (missing(gorups)) stop("The gorups number is required")
  if (missing(mpg)) stop("maximun number of proceeces by groups is required")
  url <- "http://127.0.0.1:5000/OGF"
  body_list <- list("a" = incidence_matrix, "C" = gorups, "NM" = mpg)
  print(incidence_matrix)
  response <- httr::content(httr::POST(url = url, body = body_list, encode = "json"))
  print(response)
  decisions <- unlist(response$decisons)
  decisions_w <- decisions[grepl("w_", decisions)]
  OGF_matrix_decisions <- matrix(data = NA, nrow = length(decisions_w), ncol = 4, byrow = TRUE)
  for(i in 1:length(decisions_w)){
    OGF_matrix_decisions[i,] <- as.vector(unlist(strsplit(decisions_w[i], split = "_")))
  }
  OGF_matrix_decisions <- as.data.frame(OGF_matrix_decisions)
  names(OGF_matrix_decisions) <- c("Variable_decision","Gen","Process", "Group")
  OGF_matrix_decisions$Gen <- as.numeric(OGF_matrix_decisions$Gen)+1
  OGF_matrix_decisions$Process <- as.numeric(OGF_matrix_decisions$Process)+1
  OGF_matrix_decisions$Group <- as.numeric(OGF_matrix_decisions$Group)+1
  objective_value <- response$objective_value
  return(list(OGF_matrix_decisions = OGF_matrix_decisions,objective_value = objective_value))
}
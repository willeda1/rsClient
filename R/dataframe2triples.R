#' converts a rectangular data frame to a list of triple definitions
#'
#' Returns a list of triples definitions suitable for \link[rsRDF]{add.edges}
#' from a single input data frame. Is linked back to a node given by the
#' parameter `root`.
#'
#' @param data the data frame to be translated
#' @param root the node, potentially a blank node, to which all observations are linked
#'
#' @returns a list of character vectors
#' @export
#' @md
#' @examples
#' A=data.frame(x=seq(1,5,1))
#' A$y1=sin(A$x)
#' A$y2=cos(A$x)
#'
#' dataframe2triples(A) |> lapply(paste,collapse=" ") |> unlist()

dataframe2triples=function(data,root="_:data"){

  triples=list()

  n=0; iobs = 0
  cols = colnames(data)
  for (i in 1:nrow(data)){
    row = paste0("row-",i)
    # n=n+1; triples[[n]] = c("_:data","d:hasRow",row,"$")
    for (j in 1:ncol(data)){
      iobs=iobs+1
      obs=paste0("_:obs",iobs)
      n=n+1; triples[[n]] = c(root,"d:hasObs",obs)
      n=n+1; triples[[n]] = c(obs,"d:hasRow",row,"$")
      n=n+1; triples[[n]] = c(obs,"d:hasCol",cols[j],"$")
      n=n+1; triples[[n]] = c(obs,"d:hasValue",data[i,j,drop=T],"$")
    }
  }

  triples
}



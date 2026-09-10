#' converts a rectangular data frame to a list of triple definitions
#'
#' Returns a list of triples definitions suitable for \link[rsRDF]{add.edges}
#' from a single input data frame. Is linked back to a node given by the
#' parameter `root`.
#'
#' @param data the data frame to be translated
#' @param root the node, potentially a blank node, to which all observations are linked
#' @param read
#'
#' @returns a list of character vectors
#' @export
#' @md
#' @examples
#' A=data.frame(x=seq(1,5,1))
#' A$y1=sin(A$x)
#' A$y2=cos(A$x)
#'
#' dataframeEncoder(A) |> lapply(paste,collapse=" ") |> unlist()

dataframeEncoder=function(data,root="_:data",tag=NULL,read=NULL){

  if (!is.null(read)){


    target=read$target

    print(target)

    results = list()

    for (i in 1:nrow(target)){

      thisID = target[i,"reportID"]
      thisTag = target[i,"tag"]
      if (!is.na(thisID)) my.filter=glue("filter(?reportID = '{thisID}') .")
      if (!is.na(thisTag)) my.filter=my.filter |>
        paste(glue("filter(?tag = '{thisTag}') ."),sep="\n")

      paste0('select ?reportID ?tag ?encoder ?author ?posted ?row ?col ?value

             where {
               ?data rdf:type d:data_posting ;
                 d:inReport ?reportID ;
                 d:author ?author ;
                 d:posted ?posted ;
                 d:hasObs ?obs .
               ?obs  d:hasRow ?row ;
                 d:hasCol ?col ;
                 d:hasValue ?value .
                 optional {?data d:hasTag ?tag }
                 optional {?data d:encoder ?encoder }
                 ',my.filter,'}')  |> read$w$query() %>%
        as.data.frame() %>%
        pivot_wider(names_from="col",values_from="value") -> results[[i]]
    }

    bind_rows(results)

  } else {

    triples=list()
    n=0; iobs = 0

    if (!is.null(tag)){
      n=n+1; triples[[n]]=c("_:data","d:hasTag",tag,"$")
    }
    n=n+1; triples[[n]]=c("_:data","d:encoder","dataframe","$")

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

}



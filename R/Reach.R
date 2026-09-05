#' connects to an external community hosting RDF store
#'
#' @import R6
#' @import rsRDF
#' @import glue
#' @import dplyr
#' @export

Reach = R6::R6Class(
  "Reach",

  private=list(),
  public=list(
    #' @field w the \link[rsAdmin]{Remote} object supporting the datastore connection
    #' @field endpoint the URL of the remote triple store
    #' @field userid the userid of the current client
    #' @field created the time of creation of this R6 object

    w=NULL,
    endpoint=NULL,
    userid=NULL,
    created=NULL,

    # ...........................................................................
    #' @description
    #' initialises a connection object to a remote server
    #' @param endpoint the URL of the remote RDF triple store
    #' @param userid the clients userid

    initialize=function(endpoint="http://localhost:8080/rdf4j-server/repositories/01",
                        userid="007"){


      self$endpoint = endpoint
      self$userid = userid
      self$created = date()

      self$w = rsRDF::Remote$new(endpoint)
    },

    # ...........................................................................
    #' @description
    #' prints information about the connection and userid

    print=function(){
      cat("remote connection\n")
      self$w$print()
      cat("\n--- userid\n\n  ",self$userid,"\n")
      cat("\n--- created\n\n  ",self$created,"\n")
    },

    # ...........................................................................
    #' @description
    #' checks the current given user name against the remote database. If found
    #' the remote entry is printed. If not a value of NULL is return
    #' @returns a data frame or NULL

    myName=function(){

      # currently glue in $query only sees its internal namespace

      glue('select ?userid ?name ?added
      where {
         ?person foaf:member d:collabCommunity;
                  d:type d:collaborator;
                  d:userid "<<<self$userid>>>";
                  foaf:name ?name;
                  d:added ?added.}',
                  .open="<<<",.close=">>>") |> self$w$query() %>%
        as.data.frame() %>%
        mutate(userid=self$userid) %>%
        relocate(userid) -> result

      # return NULL if there is not match

      if (nrow(result) >0){
        result
      } else {
        NULL
      }

    }
  )
)

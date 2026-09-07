#' connects to an external community hosting RDF store
#'
#' @import R6
#' @import rsRDF
#' @import glue
#' @import dplyr
#' @export

rsComms = R6::R6Class(
  "rsComms",

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

    },

    # ...........................................................................
    #' @description
    #' Lists or requests memberships of a collaboration. Where operations are not
    #' logically possible, an explanatory message is printed to the screen.
    #' @param action the desired action: `list` prints a list of all collaborations
    #'   and whether the current user is already a member, whilst `join` requests to
    #'   join a collaboration with the id `target` below.
    #' @param target the id of a collaboration to join should action be set to `join`
    #' @returns a data frame or NULL

    collaboration=function(action="list",target="01"){

      # query the database for the collaboration status

      'select ?id ?name ?created ?participant

            where {?collab d:partOf d:rsCommunity ;
                           d:collabId ?id ;
                           d:name ?name ;
                           d:created ?created .
            optional { ?membership d:collaboration ?id ;
                                 d:hasParticipant ?participant . }}' |>
        self$w$query() -> result

      # if collaborations have been found, assess whether the current user is
      # already a member

      if (nrow(result) > 0){

        result %>%
          as.data.frame() %>%
          group_by(id,name,created) %>%
          summarise(member = ifelse(
            any(!is.na(participant) & participant == self$userid),"yes","no")) %>%
          as.data.frame() -> status

      } else {
        cat("no collaboarations found\n")
        return(invisible(NULL))
      }

      # perform the requested user action

      action = tolower(action)

      if (action == "list"){

        status

      } else if (action == "join"){

        requested.target = status %>% filter(id == target)

        if (nrow(requested.target) == 0){
          cat("requested collaboaration not found\n")
          return(invisible(NULL))
        }

        if (nrow(requested.target %>% filter(member == "yes")) != 0){
          cat("already following collaboaration\n")
          return(invisible(NULL))
        }

        # if OK, proceed to update the database

        self$w$add.edges(
          list(
            c("_:membership","d:hasParticipant",self$userid,"$"),
            c("_:membership","d:collaboration",target,"$"),
            c("_:membership","d:added",Zulu(),"$")
          )
        )

        cat("joining collaboration\n")
        return(invisible(NULL))

      } else {
        stop("action '",action,"' unknown")
      }
    },
    # ...........................................................................
    #' @description
    #' lists or creates reports
    #' @param action one of 'create' to create a new report or 'list' to list
    #'    all available reports
    #' @param id the report ID if creating a report
    #' @param info an info string if creating a report
    report=function(action="create",id="01",info="this is a report"){

      action = tolower(action)

      if (action == "create"){

        if (id %in% (self$report("list") %>% as.data.frame() %>% pull(id))){
          cat("report id already assigned\n")
          return(invisible(NULL))
        }

        self$w$add.edges(
          list(
            c("_:report","rdf:type","d:report"),
            c("_:report","d:hasId",id,"$"),
            c("_:report","d:info",info,"$"),
            c("_:report","d:created",Zulu(),"$")
          )
        )

      } else if (action == "list"){

        'select ?id ?info ?created
        where { ?report rdf:type d:report ;
                        d:hasId ?id ;
                        d:info ?info ;
                        d:created ?created .}' |> self$w$query()

      } else {
        stop("action '",action,"' not understood")
      }




    }


  )
)

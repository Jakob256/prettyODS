#~~~~~~~~~~~~~~~~
## 1. Styles ####
#~~~~~~~~~~~~~~~~


#' Title
#'
#' @param font todo
#' @param size todo
#' @param color todo
#' @param bold todo
#' @param italic todo
#' @param underline todo
#' @param cellcolor todo
#' @param vAlign todo
#' @param hAlign todo
#' @param margin todo
#' @param wrap todo
#' @param rotate todo
#' @param digits todo
#'
#' @returns todo
#' @export
#'
#' @examples ODS_createStyle(font="Arial",bold=TRUE)
ODS_createStyle <- function(font=NULL, size=NULL, color=NULL, bold=NULL, italic=NULL,
                            underline=NULL, cellcolor=NULL, vAlign=NULL, hAlign=NULL,
                            margin=NULL, wrap=NULL, rotate=NULL, digits=NULL){

  style=as.list(environment())
  style=style[!sapply(style, is.null)]
  class(style)="ODSstyle"

  ## these throw an error, if the colors are not valid:
  .col2hex(color)
  .col2hex(cellcolor)


  temp=unlist(lapply(style,length))
  temp=temp[temp>1]
  if (length(temp)!=0){stop(paste(names(temp),collapse=", "),": must be a single value")}

  return(style)
}



#' Title
#'
#' @param x todo
#' @param ... todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
print.ODSstyle <- function(x, ...){
  cat("ODSstyle\n")
  for (name in names(x)){
    cat("  ",name,": ",x[[name]],"\n", sep = "")
  }
}



#' Title
#'
#' @param style1 todo
#' @param style2 todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_mergeStyles <- function(style1,style2){
  ## style1 has priority!
  ## i.e. fills empty slots of style1 with style2
  if (!inherits(style1, "ODSstyle")){stop("style1 must be an ODSstyle")}
  if (!inherits(style2, "ODSstyle")){stop("style2 must be an ODSstyle")}

  style <- style2
  for (name in names(style1)) {
    style[[name]] <- style1[[name]]
  }
  class(style) <- "ODSstyle"

  return(style)
}



#~~~~~~~~~~~~~~~~
## 2. Sheets ####
#~~~~~~~~~~~~~~~~

#' @importFrom R6 R6Class
SHEET <- R6Class("ODSsheet",
                 public = list(
                   sheetName=NA,
                   cellsContent = data.table::data.table(
                     row = integer(),
                     column = integer(),
                     type = character(),
                     data_string = character(),
                     data_float = numeric(),
                     styleNumber = integer()
                   ),
                   mergedCells = matrix(
                     nrow = 0, ncol = 4,
                     dimnames = list(NULL, c("fromRow", "toRow", "fromColumn", "toColumn"))
                   ),
                   colWidths=c(),
                   rowHeights=c(),
                   stylesTable = data.table::data.table(
                     font = character(),
                     size = integer(),
                     color= character(),
                     bold = logical(),
                     italic=logical(),
                     underline=logical(),
                     cellcolor=character(),
                     vAlign=character(),
                     hAlign=character(),
                     margin=numeric(),
                     wrap=logical(),
                     rotate=integer(),
                     digits=integer()
                   ),



                   cleanup = function(){
                     ## fixes cells, that were accessed repeatedly (hence overwritten)
                     ## we could also fix styles that aren't used, but this seems tricky
                     if (nrow(self$cellsContent)<=1){return()}
                     cell_id <- self$cellsContent[, paste(row, column)]
                     keep <- !duplicated(cell_id, fromLast = TRUE)
                     self$cellsContent <- self$cellsContent[keep,]
                     #invisible(self)
                   },


                   print = function(...) {
                     self$cleanup()
                     bold <- function(x) paste0("\033[1m", x, "\033[0m")
                     cat(bold("class:"),"       ODSsheet\n")
                     cat(bold("sheetName:   "),self$sheetName,"\n")

                     cat(bold("cellsContent:\n"))
                     if (nrow(self$cellsContent)!=0){
                       print(self$cellsContent)
                       cat("\n")
                     }

                     cat(bold("mergedCells:\n"))
                     if (nrow(self$mergedCells)!=0){
                       print(self$mergedCells)
                     }

                     cat(bold("colWidths:   "),self$colWidths,"\n")
                     cat(bold("rowHeights:  "),self$rowHeights,"\n")
                     cat(bold("styles:"),"\n")
                     for (i in seq_len(nrow(self$stylesTable))){
                       cat("style ",i,":\n",sep="")
                       temp=as.list(self$stylesTable[i,])
                       temp=temp[!sapply(temp, is.na)]
                       for (name in names(temp)){
                         cat("  ",name,": ",temp[[name]],"\n", sep = "")
                       }
                     }
                   },


                   nrow = function(...){
                     return(max(0,self$cellsContent$row))
                   },

                   ncol = function(...){
                     return(max(0,self$cellsContent$column))
                   },


                   View = function(...){
                     self$cleanup()
                     sheet = matrix(NA_character_,nrow=self$nrow(),ncol=self$ncol())


                     ## for displaying, we abuse the variable "cellsContent"
                     stylesTable=data.table::copy(self$stylesTable)[,styleNumber:=seq_len(.N)]
                     cellsContent=merge(self$cellsContent,stylesTable[,c("styleNumber","digits")])
                     cellsContent[type=="float" &  is.na(digits),data_string:=as.character(data_float)]
                     cellsContent[type=="float" & !is.na(digits),data_string:=.round2character(data_float,digits)]

                     sheet[cbind(cellsContent$row,cellsContent$column)]=cellsContent$data_string
                     sheet=data.table::data.table(sheet)
                     NAMES=LETTERS
                     while(self$ncol()>length(NAMES)){
                       NAMES=as.vector(outer(c("",LETTERS),NAMES,paste0))
                       NAMES=unique(NAMES)
                       NAMES=NAMES[order(nchar(NAMES),NAMES)]
                     }
                     colnames(sheet)=NAMES[seq_len(ncol(sheet))]
                     View(sheet)
                     return(invisible(sheet))
                   }
                 )
)




#' Title
#'
#' @param sheetName todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
ODS_createSheet = function(sheetName=NULL){
  if (is.null(sheetName)){sheetName=NA}
  SHEET = SHEET$new()
  SHEET$sheetName=sheetName
  return(SHEET)
}


#' Title
#'
#' @param x todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
dim.ODSsheet <- function(x){c(x$nrow(),x$ncol())}

#' Title
#'
#' @param sheet todo
#'
#' @returns todo
#' @export
#'
#' @examples 1+1
preview <- function(sheet){return(invisible(sheet$View()))}


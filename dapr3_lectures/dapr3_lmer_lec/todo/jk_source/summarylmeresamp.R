summarytest <- function(object, ...){ 
  cat(paste("Bootstrap type:", object$type, "\n"))
  cat(paste("\n"))
  cat(paste("Number of resamples:", object$B, "\n"))
  cat(paste("\n"))
  print(as.data.frame(object$stats))
  
  messages <- unlist(object$message)
  warnings <- unlist(object$warnings)
  errors   <- unlist(object$error)
  
  cat(paste("\n"))
  cat(paste("There were", length(messages), "messages,", 
            length(warnings), "warnings, and", 
            length(errors), "errors."))
  cat(paste("\n"))
  
  # finding most commonly occuring message/warning/error
  object$message <- as.factor(messages)
  object$warning <- as.factor(warnings)
  object$error <- as.factor(errors)
  
  top_message <- names(sort(summary(object$message), decreasing=T)[1])
  if(!is.null(top_message)){
    cat(paste("\n"))
    cat(paste("The most commonly occurring message was:", top_message))
    cat(paste("\n"))
  }
  
  top_warning <- names(sort(summary(object$warning), decreasing=T)[1])
  if(!is.null(top_warning)){
    cat(paste("\n"))
    cat(paste("The most commonly occurring warning was:", top_warning))
    cat(paste("\n"))
  }
  
  top_error <- names(sort(summary(object$error), decreasing=T)[1])
  if(!is.null(top_error)){
    cat(paste("\n"))
    cat(paste("The most commonly occurring error was:", top_error))
  }
}

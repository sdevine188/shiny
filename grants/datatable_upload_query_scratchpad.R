# https://rstudio.github.io/DT/007-search.html
# https://github.com/rstudio/DT/issues/146
# https://github.com/vnijs/dt_state

mtcars2 <- mtcars

datatable(
        mtcars2, colnames = c('model' = 1),
        filter = list(position = 'top', clear = FALSE),
        options = list(
                search = list(regex = TRUE, caseInsensitive = FALSE, search = 'M[ae]'),
                pageLength = 5
        )
)

datatable(
        mtcars2, colnames = c('model' = 1),
        filter = list(position = 'top', clear = FALSE),
        options = list(pageLength = 5, searchCols = list(
                list(search = 'er'),
                list(search = '15 ... 25'),
                NULL, NULL, NULL, NULL,
                list(search = '["automatic"]')
        ))
)

keyword_list <- list(
        list(search = 'er'),
        list(search = '15 ... 25'),
        NULL, NULL, NULL, NULL,
        list(search = '["automatic"]')
)



















# this works below

DT::datatable(
        mtcars2, colnames = c('model' = 1),
        filter = list(position = 'top', clear = FALSE),
        options = list(pageLength = 5, stateSave = TRUE, searchCols = list(list(search = 'er'), list(search = '15 ... 25'),
                                                                           NULL, NULL, NULL, NULL), search = list(regex = TRUE, caseInsensitive = TRUE, search = 'M[ae]')))
# create functionized input for column filters
# keyword_series <- ""
keyword_series <- c("er", "15 ... 25", "", "", "", "", '["automatic"]')

create_column_keyword_list <- function(keyword_series) {
        column_keyword_list <- list()
        for(i in 1:length(keyword_series)) {
                if(keyword_series[i] == "") {
                        column_keyword_list[[i]] <- NULL
                } else {
                        column_keyword_list[[i]] <- list(search = keyword_series[i])
                }
                
        }
        column_keyword_list
}

column_keyword_list_test <- create_column_keyword_list(keyword_series)


# create functionized input for global search
global_input <- "mer"

create_global_keyword_list <- function(global_input) {
        global_keyword_list <- list(regex = TRUE, caseInsensitive = TRUE, search = global_input)
        global_keyword_list
}

global_keyword_list_test <- create_global_keyword_list(global_input)

# test building datatables with and without functionized inputs
datatable(
        mtcars2, colnames = c('model' = 1),
        filter = list(position = 'top', clear = FALSE),
        options = list(pageLength = 5, searchCols = column_keyword_list_test, search = list(regex = TRUE, caseInsensitive = TRUE, search = 'M[ae]'))
)

DT::datatable(
        mtcars2, colnames = c('model' = 1),
        filter = list(position = 'top', clear = TRUE),
        options = list(pageLength = 5, searchCols = column_keyword_list_test, search = list(regex = TRUE, caseInsensitive = TRUE, search = 'M[ae]'))
)

DT::datatable(
        mtcars2, colnames = c('model' = 1),
        filter = list(position = 'top', clear = FALSE),
        options = list(pageLength = 5, stateSave = TRUE, searchCols = list(list(search = 'er'), list(search = '15 ... 25'),
                                                                           NULL, NULL, NULL, NULL), search = global_keyword_list_test)
        )

DT::datatable(
        mtcars2, colnames = c('model' = 1),
        filter = list(position = 'top', clear = FALSE),
        options = list(pageLength = 5, searchCols = column_keyword_list_test, search = global_keyword_list_test)
)




# test from server script
# it works
create_column_keyword_list <- function(keyword_series) {
        column_keyword_list <- list()
        if(length(keyword_series) > 0) {
                for(i in 1:length(keyword_series)) {
                        if(keyword_series[i] == "") {
                                column_keyword_list[[i]] <- NULL
                        } else {
                                column_keyword_list[[i]] <- list(search = keyword_series[i])
                        }
                        
                }
                column_keyword_list
        } else {
                column_keyword_list[[1]] <- NULL
        }
}

keyword_series <- c("m", "15 ... 25", "", "", "", "", '["automatic"]')
keyword_series <- NULL
column_keyword_list_test2 <- create_column_keyword_list(keyword_series)

DT::datatable(
        mtcars2, colnames = c('model' = 1),
        filter = list(position = 'top', clear = FALSE),
        options = list(pageLength = 5, searchCols = column_keyword_list_test2, search = global_keyword_list_test)
)

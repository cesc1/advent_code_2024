library(R6)
library(stringr)

WordSearcher <- R6Class(
  "WordSearcher",
  
  public = list(
    data = NULL,
    word = NULL,
    rows = NULL,
    cols = NULL,
    result = NULL,

    initialize = function(data, word) {
      self$data <- data
      self$word <- str_split_1(word, pattern = "")
      self$rows <- nrow(data)
      self$cols <- ncol(data)
      self$result <- 0
    },
    
    calc = function() {
      self$result <- 0
      for(row in seq_len(self$rows)) {
        for(col in seq_len(self$cols)) {
          private$check_all(row, col)
        }
      }
      return(self$result)
    }

  ),
  private = list(
    check_E = function(row, col) {
      for(i in seq_along(self$word)) {
        m_row = row
        m_col = col + (i - 1)
        if (m_col > self$cols)
          return(FALSE)
        if (self$data[m_row, m_col] != self$word[i])
          return(FALSE)
      }
      return(TRUE)
    },
    
    check_SE = function(row, col) {
      for(i in seq_along(self$word)) {
        m_row = row - (i - 1)
        m_col = col + (i - 1)
        if (m_col > self$cols || m_row <= 0)
          return(FALSE)
        if (self$data[m_row, m_col] != self$word[i])
          return(FALSE)
      }
      return(TRUE)
    },
    
    check_S = function(row, col) {
      for(i in seq_along(self$word)) {
        m_row = row - (i - 1)
        m_col = col
        if (m_row <= 0)
          return(FALSE)
        if (self$data[m_row, m_col] != self$word[i])
          return(FALSE)
      }
      return(TRUE)
    },
    
    check_SW = function(row, col) {
      for(i in seq_along(self$word)) {
        m_row = row - (i - 1)
        m_col = col - (i - 1)
        if (m_row <= 0 || m_col <= 0)
          return(FALSE)
        if (self$data[m_row, m_col] != self$word[i])
          return(FALSE)
      }
      return(TRUE)
    },
    
    check_W = function(row, col) {
      for(i in seq_along(self$word)) {
        m_row = row
        m_col = col - (i - 1)
        if (m_col <= 0)
          return(FALSE)
        if (self$data[m_row, m_col] != self$word[i])
          return(FALSE)
      }
      return(TRUE)
    },
    
    check_NW = function(row, col) {
      for(i in seq_along(self$word)) {
        m_row = row + (i - 1)
        m_col = col - (i - 1)
        if (m_row > self$rows || m_col <= 0)
          return(FALSE)
        if (self$data[m_row, m_col] != self$word[i])
          return(FALSE)
      }
      return(TRUE)
    },
    
    check_N = function(row, col) {
      for(i in seq_along(self$word)) {
        m_row = row + (i - 1)
        m_col = col
        if (m_row > self$rows)
          return(FALSE)
        if (self$data[m_row, m_col] != self$word[i])
          return(FALSE)
      }
      return(TRUE)
    },
    
    check_NE = function(row, col) {
      for(i in seq_along(self$word)) {
        m_row = row + (i - 1)
        m_col = col + i - 1
        if (m_row > self$rows ||m_col > self$cols)
          return(FALSE)
        if (self$data[m_row, m_col] != self$word[i])
          return(FALSE)
      }
      return(TRUE)
    },
    
    check_all = function(row, col) {
      for(fun in private$fun_list()) {
        if (fun(row, col)) {
          self$result = self$result + 1
        }
      }
    },
    
    fun_list = function() {
      list(
        private$check_N,
        private$check_NE,
        private$check_E,
        private$check_SE,
        private$check_S,
        private$check_SW,
        private$check_W,
        private$check_NW
      )
    }
  )
)

read_data <- function(path) {
  data <- readLines(path) |> 
    str_split(pattern = "") |> 
    sapply(function(x){x}) |> 
    t()
}

data <- read_data("data/input.txt")

ws <- WordSearcher$new(data, "XMAS")
ws$calc()

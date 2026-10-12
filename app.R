
library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(ggplot2)

# ---------------- DATABASE SETUP ----------------

db_path <- file.path(getwd(), "department_store.db")
con <- dbConnect(SQLite(), db_path)

dbExecute(con, "
CREATE TABLE IF NOT EXISTS products (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  category TEXT NOT NULL,
  price REAL NOT NULL CHECK(price >= 0),
  stock INTEGER NOT NULL CHECK(stock >= 0)
)")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS sales (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  product_id INTEGER NOT NULL,
  product_name TEXT NOT NULL,
  quantity INTEGER NOT NULL,
  unit_price REAL NOT NULL,
  total REAL NOT NULL,
  sale_date TEXT NOT NULL
)")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS feedback (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  customer_name TEXT,
  rating INTEGER NOT NULL,
  comments TEXT,
  submitted_at TEXT NOT NULL
)")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS customer_requests (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  customer_name TEXT,
  request TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'Pending',
  submitted_at TEXT NOT NULL
)")

# Insert sample products only when the database is empty
if (dbGetQuery(con, "SELECT COUNT(*) AS n FROM products")$n[1] == 0) {
  sample_products <- data.frame(
    name = c("Rice", "Milk", "Bread", "Soap",
             "Shampoo", "Biscuits", "Cooking Oil",
             "Sugar", "Coffee", "Toothpaste"),
    category = c("Grocery", "Dairy", "Bakery", "Personal Care",
                 "Personal Care", "Snacks", "Grocery",
                 "Grocery", "Beverages", "Personal Care"),
    price = c(60, 30, 40, 35, 120, 20, 150, 45, 90, 55),
    stock = c(100, 50, 40, 60, 30, 100, 25, 70, 35, 45)
  )
  
  dbWriteTable(con, "products", sample_products, append = TRUE)
}

# ---------------- USER INTERFACE ----------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f3f6fb;
        font-family: Arial, sans-serif;
      }
      .main-title {
        background: #17365d;
        color: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 8px;
      }
      .metric {
        background: white;
        padding: 18px;
        margin: 5px 0;
        border-radius: 10px;
        border-left: 5px solid #2780d9;
        box-shadow: 0 2px 6px #d8dfea;
      }
      .metric h4 { color: #53657c; }
      .metric h2 { color: #17365d; }
      .panel-box {
        background: white;
        padding: 18px;
        margin-bottom: 15px;
        border-radius: 10px;
      }
    "))
  ),
  
  div(
    class = "main-title",
    h2("Smart Department Store Management System"),
    p("Inventory | Billing | Customer Chatbot | Sales Analytics")
  ),
  
  tabsetPanel(
    id = "main_tabs",
    
    # -------- DASHBOARD --------
    
    tabPanel(
      "Dashboard",
      br(),
      fluidRow(
        column(3, div(class = "metric",
                      h4("Total Products"),
                      h2(textOutput("total_products")))),
        column(3, div(class = "metric",
                      h4("Total Stock Units"),
                      h2(textOutput("total_stock")))),
        column(3, div(class = "metric",
                      h4("Total Sales"),
                      h2(textOutput("total_sales")))),
        column(3, div(class = "metric",
                      h4("Inventory Value"),
                      h2(textOutput("inventory_value")))
        )
      ),
      br(),
      div(class = "panel-box",
          h3("Product Stock Overview"),
          plotOutput("stock_plot", height = "350px")),
      div(class = "panel-box",
          h3("Recent Sales"),
          DTOutput("recent_sales"))
    ),
    
    # -------- PRODUCT SEARCH --------
    
    tabPanel(
      "Product Search",
      br(),
      div(class = "panel-box",
          h3("Find a Product"),
          textInput("search_name", "Product Name", ""),
          selectInput(
            "search_category", "Category",
            choices = c("All", "Grocery", "Dairy", "Bakery",
                        "Personal Care", "Snacks", "Beverages")
          ),
          actionButton("search_btn", "Search Products"),
          br(), br(),
          DTOutput("search_results"))
    ),
    
    # -------- CUSTOMER CHATBOT --------
    
    tabPanel(
      "Customer Chatbot",
      br(),
      div(class = "panel-box",
          h3("Store Assistant"),
          p("Ask about product prices, availability, categories or budget suggestions."),
          textInput(
            "chat_message", "Enter your question",
            placeholder = "Example: What is the price of milk?"
          ),
          actionButton("send_chat", "Send Message"),
          br(), br(),
          h4("Chatbot Response"),
          uiOutput("chat_response"),
          hr(),
          h4("Example questions"),
          tags$ul(
            tags$li("What is the price of rice?"),
            tags$li("Is milk available?"),
            tags$li("Show grocery products"),
            tags$li("Suggest products under 100")
          ))
    ),
    
    # -------- BILLING --------
    
    tabPanel(
      "Billing",
      br(),
      div(class = "panel-box",
          h3("Customer Billing"),
          selectInput("bill_product", "Select Product",
                      choices = character(0)),
          numericInput("bill_quantity", "Quantity", 1,
                       min = 1, max = 100, step = 1),
          actionButton("calculate_bill", "Calculate Bill"),
          br(), br(),
          h4("Bill Details"),
          verbatimTextOutput("bill_preview"),
          actionButton("confirm_bill", "Confirm Sale",
                       class = "btn-success"),
          br(), br(),
          textOutput("bill_message"))
    ),
    
    # -------- INVENTORY --------
    
    tabPanel(
      "Inventory",
      br(),
      div(class = "panel-box",
          h3("Current Inventory"),
          actionButton("refresh_inventory", "Refresh Inventory"),
          br(), br(),
          DTOutput("inventory_table")),
      fluidRow(
        column(
          6,
          div(class = "panel-box",
              h3("Add New Product"),
              textInput("new_name", "Product Name"),
              selectInput(
                "new_category", "Category",
                choices = c("Grocery", "Dairy", "Bakery",
                            "Personal Care", "Snacks", "Beverages",
                            "Household", "Other")
              ),
              numericInput("new_price", "Price (₹)", 1, min = 0),
              numericInput("new_stock", "Opening Stock", 1, min = 0),
              actionButton("add_product", "Add Product"),
              textOutput("add_message"))
        ),
        column(
          6,
          div(class = "panel-box",
              h3("Restock Product"),
              selectInput("restock_product", "Select Product",
                          choices = character(0)),
              numericInput("restock_quantity", "Quantity to Add",
                           1, min = 1),
              actionButton("restock_btn", "Update Stock"),
              textOutput("restock_message"))
        )
      )
    ),
    
    # -------- CUSTOMER REQUESTS --------
    
    tabPanel(
      "Customer Requests",
      br(),
      div(class = "panel-box",
          h3("Submit a Customer Request"),
          textInput("request_name", "Customer Name"),
          textAreaInput("request_text", "Request Details",
                        rows = 3,
                        placeholder = "Enter the customer's request"),
          actionButton("submit_request", "Submit Request"),
          br(), br(),
          textOutput("request_message"),
          hr(),
          h3("Request Records"),
          DTOutput("request_table"))
    ),
    
    # -------- FEEDBACK --------
    
    tabPanel(
      "Feedback",
      br(),
      div(class = "panel-box",
          h3("Customer Feedback"),
          textInput("feedback_name", "Customer Name"),
          sliderInput("feedback_rating", "Rating",
                      min = 1, max = 5, value = 5, step = 1),
          textAreaInput("feedback_comments", "Comments", rows = 3),
          actionButton("submit_feedback", "Submit Feedback"),
          br(), br(),
          textOutput("feedback_message"),
          hr(),
          h3("Feedback Records"),
          DTOutput("feedback_table"))
    ),
    
    # -------- ANALYTICS --------
    
    tabPanel(
      "Sales Analytics",
      br(),
      div(class = "panel-box",
          h3("Sales by Product"),
          plotOutput("sales_plot", height = "350px"),
          downloadButton("download_sales", "Download Sales CSV"),
          br(), br(),
          DTOutput("sales_table"))
    )
  )
)

# ---------------- SERVER LOGIC ----------------

server <- function(input, output, session) {
  
  refresh <- reactiveVal(0)
  bill_data <- reactiveVal(NULL)
  chat_history <- reactiveVal(
    data.frame(sender = character(), message = character())
  )
  
  get_products <- function() {
    refresh()
    dbGetQuery(con, "SELECT * FROM products ORDER BY name")
  }
  
  get_sales <- function() {
    refresh()
    dbGetQuery(con, "SELECT * FROM sales ORDER BY id DESC")
  }
  
  # Refresh product selectors
  observe({
    products <- get_products()
    
    product_choices <- setNames(
      as.character(products$id),
      paste0(products$name, " (₹", products$price,
             ", stock: ", products$stock, ")")
    )
    
    updateSelectInput(session, "bill_product",
                      choices = product_choices)
    
    updateSelectInput(session, "restock_product",
                      choices = product_choices)
  })
  
  # -------- DASHBOARD --------
  
  output$total_products <- renderText({
    nrow(get_products())
  })
  
  output$total_stock <- renderText({
    products <- get_products()
    sum(products$stock)
  })
  
  output$total_sales <- renderText({
    sales <- get_sales()
    paste0("₹", format(sum(sales$total), big.mark = ",",
                       nsmall = 2))
  })
  
  output$inventory_value <- renderText({
    products <- get_products()
    value <- sum(products$price * products$stock)
    paste0("₹", format(value, big.mark = ",", nsmall = 2))
  })
  
  output$stock_plot <- renderPlot({
    products <- get_products()
    
    validate(need(nrow(products) > 0, "No products available."))
    
    ggplot(products, aes(
      x = reorder(name, stock), y = stock
    )) +
      geom_col(fill = "#2780d9") +
      coord_flip() +
      labs(x = "Product", y = "Available Units",
           title = "Stock Available by Product") +
      theme_minimal()
  })
  
  output$recent_sales <- renderDT({
    sales <- get_sales()
    datatable(head(sales, 10),
              options = list(pageLength = 5),
              rownames = FALSE)
  })
  
  # -------- PRODUCT SEARCH --------
  
  search_results_data <- eventReactive(input$search_btn, {
    products <- get_products()
    term <- trimws(input$search_name)
    
    if (nzchar(term)) {
      products <- products[
        grepl(term, products$name, ignore.case = TRUE), ,
        drop = FALSE
      ]
    }
    
    if (!is.null(input$search_category) &&
        input$search_category != "All") {
      products <- products[
        products$category == input$search_category, ,
        drop = FALSE
      ]
    }
    
    products
  }, ignoreInit = TRUE)
  
  output$search_results <- renderDT({
    datatable(search_results_data(),
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # -------- CUSTOMER CHATBOT --------
  
  observeEvent(input$send_chat, {
    question <- trimws(input$chat_message)
    
    if (!nzchar(question)) {
      showNotification("Please enter a question.", type = "warning")
      return()
    }
    
    products <- get_products()
    q <- tolower(question)
    
    response <- if (grepl("hello|hi|hey", q)) {
      "Hello! Welcome to Smart Department Store. How can I help you?"
    } else if (grepl("budget|under|within|suggest", q)) {
      amount <- suppressWarnings(
        as.numeric(sub(".*?(\\d+(\\.\\d+)?).*", "\\1", q))
      )
      
      if (length(amount) == 0 || is.na(amount) ||
          !grepl("[0-9]", q)) {
        "Please mention your budget. Example: Suggest products under 100."
      } else {
        matches <- products[
          products$price <= amount & products$stock > 0, ,
          drop = FALSE
        ]
        
        if (nrow(matches) == 0) {
          paste("No currently available products found under ₹",
                amount, ".", sep = "")
        } else {
          paste0(
            "Products within your budget: ",
            paste0(matches$name, " (₹", matches$price, ")",
                   collapse = ", ")
          )
        }
      }
    } else {
      matching <- products[
        vapply(products$name, function(nm) {
          grepl(tolower(nm), q, fixed = TRUE)
        }, logical(1)), , drop = FALSE
      ]
      
      if (nrow(matching) > 0) {
        p <- matching[1, ]
        
        if (grepl("price|cost|how much", q)) {
          paste0(p$name, " costs ₹", p$price, " per unit.")
        } else if (grepl("available|stock|quantity|left", q)) {
          if (p$stock > 0) {
            paste0(p$name, " is available. Stock: ",
                   p$stock, " units.")
          } else {
            paste0(p$name, " is currently out of stock.")
          }
        } else {
          paste0(p$name, ": price ₹", p$price,
                 ", available stock ", p$stock, " units.")
        }
      } else if (grepl("category|categories|grocery", q)) {
        paste("Available categories:",
              paste(unique(products$category), collapse = ", "))
      } else if (grepl("product|list|items", q)) {
        paste("Our products are:",
              paste(products$name, collapse = ", "))
      } else {
        "Sorry, I could not understand that question. Ask about a product's price, stock, categories or your budget."
      }
    }
    
    old <- chat_history()
    new_rows <- data.frame(
      sender = c("You", "Store Assistant"),
      message = c(question, response)
    )
    
    chat_history(rbind(old, new_rows))
    updateTextInput(session, "chat_message", value = "")
  })
  
  output$chat_response <- renderUI({
    history <- chat_history()
    
    if (nrow(history) == 0) {
      return(tags$p("Your conversation will appear here."))
    }
    
    tagList(lapply(seq_len(nrow(history)), function(i) {
      div(
        style = paste0(
          "padding:10px; margin:8px 0; border-radius:8px;",
          if (history$sender[i] == "You")
            "background:#e1edff;"
          else
            "background:#e4f7e9;"
        ),
        strong(paste0(history$sender[i], ": ")),
        history$message[i]
      )
    }))
  })
  
  # -------- BILLING --------
  
  observeEvent(input$calculate_bill, {
    req(input$bill_product)
    
    product_id <- as.integer(input$bill_product)
    
    product <- dbGetQuery(
      con, "SELECT * FROM products WHERE id = ?",
      params = list(product_id)
    )
    
    if (nrow(product) == 0) {
      bill_data(NULL)
      return()
    }
    
    qty <- input$bill_quantity
    
    if (is.null(qty) || is.na(qty) || qty < 1 ||
        qty != floor(qty)) {
      bill_data(NULL)
      return()
    }
    
    if (qty > product$stock[1]) {
      bill_data(NULL)
      showNotification(
        paste("Only", product$stock[1], "units are available."),
        type = "error"
      )
      return()
    }
    
    bill_data(data.frame(
      id = product$id[1],
      name = product$name[1],
      quantity = as.integer(qty),
      unit_price = product$price[1],
      total = product$price[1] * qty
    ))
  })
  
  output$bill_preview <- renderText({
    b <- bill_data()
    
    if (is.null(b)) {
      return("Select a product and calculate the bill.")
    }
    
    paste0(
      "Product: ", b$name, "\n",
      "Quantity: ", b$quantity, "\n",
      "Price per unit: ₹", sprintf("%.2f", b$unit_price), "\n",
      "Total amount: ₹", sprintf("%.2f", b$total)
    )
  })
  
  observeEvent(input$confirm_bill, {
    b <- bill_data()
    
    if (is.null(b)) {
      showNotification(
        "Calculate a valid bill first.", type = "warning"
      )
      return()
    }
    
    dbBegin(con)
    
    tryCatch({
      changed <- dbExecute(
        con,
        "UPDATE products
         SET stock = stock - ?
         WHERE id = ? AND stock >= ?",
        params = list(b$quantity, b$id, b$quantity)
      )
      
      if (changed != 1) {
        stop("Insufficient stock. Please calculate the bill again.")
      }
      
      dbExecute(
        con,
        "INSERT INTO sales
         (product_id, product_name, quantity,
          unit_price, total, sale_date)
         VALUES (?, ?, ?, ?, ?, ?)",
        params = list(
          b$id, b$name, b$quantity,
          b$unit_price, b$total,
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )
      
      dbCommit(con)
      refresh(refresh() + 1)
      bill_data(NULL)
      
      output$bill_message <- renderText({
        "Sale confirmed successfully! Stock has been updated."
      })
      
    }, error = function(e) {
      dbRollback(con)
      showNotification(conditionMessage(e), type = "error")
    })
  })
  
  # -------- INVENTORY --------
  
  output$inventory_table <- renderDT({
    datatable(get_products(),
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  observeEvent(input$refresh_inventory, {
    refresh(refresh() + 1)
  })
  
  observeEvent(input$add_product, {
    name <- trimws(input$new_name)
    
    if (!nzchar(name) || is.null(input$new_price) ||
        is.null(input$new_stock) ||
        !is.finite(input$new_price) ||
        !is.finite(input$new_stock) ||
        input$new_price < 0 || input$new_stock < 0 ||
        input$new_stock != floor(input$new_stock)) {
      output$add_message <- renderText({
        "Enter a valid product name, price and whole-number stock."
      })
      return()
    }
    
    existing <- dbGetQuery(
      con, "SELECT id FROM products WHERE lower(name) = lower(?)",
      params = list(name)
    )
    
    if (nrow(existing) > 0) {
      output$add_message <- renderText({
        "This product already exists. Use Restock Product instead."
      })
      return()
    }
    
    dbExecute(
      con,
      "INSERT INTO products (name, category, price, stock)
       VALUES (?, ?, ?, ?)",
      params = list(name, input$new_category,
                    input$new_price, as.integer(input$new_stock))
    )
    
    refresh(refresh() + 1)
    
    output$add_message <- renderText({
      "Product added successfully!"
    })
    
    updateTextInput(session, "new_name", value = "")
  })
  
  observeEvent(input$restock_btn, {
    req(input$restock_product)
    
    qty <- input$restock_quantity
    
    if (is.null(qty) || is.na(qty) ||
        qty < 1 || qty != floor(qty)) {
      output$restock_message <- renderText({
        "Enter a valid whole-number quantity."
      })
      return()
    }
    
    dbExecute(
      con,
      "UPDATE products SET stock = stock + ? WHERE id = ?",
      params = list(as.integer(qty),
                    as.integer(input$restock_product))
    )
    
    refresh(refresh() + 1)
    
    output$restock_message <- renderText({
      "Stock updated successfully!"
    })
  })
  
  # -------- CUSTOMER REQUESTS --------
  
  output$request_table <- renderDT({
    refresh()
    requests <- dbGetQuery(
      con, "SELECT * FROM customer_requests ORDER BY id DESC"
    )
    datatable(requests,
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  observeEvent(input$submit_request, {
    request_text <- trimws(input$request_text)
    
    if (!nzchar(request_text)) {
      output$request_message <- renderText({
        "Please enter the request details."
      })
      return()
    }
    
    customer <- trimws(input$request_name)
    
    dbExecute(
      con,
      "INSERT INTO customer_requests
       (customer_name, request, status, submitted_at)
       VALUES (?, ?, 'Pending', ?)",
      params = list(
        if (nzchar(customer)) customer else "Guest",
        request_text,
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    )
    
    refresh(refresh() + 1)
    
    output$request_message <- renderText({
      "Customer request submitted successfully!"
    })
    
    updateTextAreaInput(session, "request_text", value = "")
  })
  
  # -------- CUSTOMER FEEDBACK --------
  
  output$feedback_table <- renderDT({
    refresh()
    feedback <- dbGetQuery(
      con, "SELECT * FROM feedback ORDER BY id DESC"
    )
    datatable(feedback,
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  observeEvent(input$submit_feedback, {
    comments <- trimws(input$feedback_comments)
    customer <- trimws(input$feedback_name)
    
    dbExecute(
      con,
      "INSERT INTO feedback
       (customer_name, rating, comments, submitted_at)
       VALUES (?, ?, ?, ?)",
      params = list(
        if (nzchar(customer)) customer else "Guest",
        as.integer(input$feedback_rating),
        comments,
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    )
    
    refresh(refresh() + 1)
    
    output$feedback_message <- renderText({
      "Thank you! Your feedback has been saved."
    })
    
    updateTextAreaInput(session, "feedback_comments", value = "")
  })
  
  # -------- SALES ANALYTICS --------
  
  output$sales_table <- renderDT({
    datatable(get_sales(),
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  output$sales_plot <- renderPlot({
    sales <- get_sales()
    
    validate(need(nrow(sales) > 0,
                  "Complete a sale to see the sales chart."))
    
    totals <- aggregate(total ~ product_name, data = sales, sum)
    
    ggplot(totals, aes(
      x = reorder(product_name, total), y = total
    )) +
      geom_col(fill = "#20a486") +
      coord_flip() +
      labs(x = "Product", y = "Sales Amount (₹)",
           title = "Total Sales by Product") +
      theme_minimal()
  })
  
  output$download_sales <- downloadHandler(
    filename = function() {
      paste0("department_store_sales_",
             Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(get_sales(), file, row.names = FALSE)
    }
  )
}

# ---------------- RUN APPLICATION ----------------

app <- shinyApp(ui = ui, server = server)

onStop(function() {
  if (DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
})

app

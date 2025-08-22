
library(libresense)


#### 4 copas rep 1 ########
run_panel(
  products_file = "Prueba/vinos_1.csv",
  attributes_file = "atributos.csv",
  answers_dir = "Prueba/answers1/",
  product_name = "CodigoProducto")





run_panel(
  products_file = "Prueba/vinos_1.csv",
  attributes_file = "atributos.csv",
  answers_dir = "Prueba/answers1/",
  product_name = "CodigoProducto",
  dest_url="192.168.1.122:4000")

run_panel(
  products_file = "Prueba/vinos_1.csv",
  attributes_file = "atributos.csv",
  answers_dir = "Prueba/answers1/",
  product_name = "CodigoProducto")


run_board(answers_dir = "Prueba/answers1/")
?run_panel


library(libresense)


####  ########
run_panel(
  products_file = "grupo_f1/vinos_f1.csv",
  attributes_file = "atributos.csv",
  answers_dir = "grupo_f1/answers_f1",  
  product_name = "CodigoProducto",
  design_file="grupo_f1/grupo_f1.csv",
  dest_url="192.168.1.122:4001")
  
run_panel(
  products_file = "grupo_f1/vinos_f1.csv",
  attributes_file = "atributos.csv",
  answers_dir = "grupo_f1/answers_f1",  
  product_name = "CodigoProducto",
  design_file="grupo_f1/grupo_f1.csv")

run_panel(
  products_file = "grupo_f2/vinos_f2.csv",
  attributes_file = "atributos.csv",
  answers_dir = "grupo_f2/answers_f2",  
  product_name = "CodigoProducto",
  design_file="grupo_f2/grupo_f2.csv",
  dest_url="192.168.1.122:4002")

run_panel(
  products_file = "grupo_f2/vinos_f2.csv",
  attributes_file = "atributos.csv",
  answers_dir = "grupo_f2/answers_f2",  
  product_name = "CodigoProducto",
  design_file="grupo_f2/grupo_f2.csv")







run_board(answers_dir = "grupo_f25/answers_f25/")
# Llegue hasta la 24 inclusive

library(stringr)
library(httr2)
library(lubridate)
library(purrr)
library(dotenv)
library(jsonlite)

dotenv::load_dot_env("C:/Users/Dev/Documents/repo/secrets.env")

url_factura<-'https://www.facturapi.io/v2/'
auth_factura<-Sys.getenv("Auth_Factura")

#-----------Clientes----------------
#crear cliente
crear_cliente <- function(nombre,email,rfc,regimen,direccion){
  res<-request(paste0(url_factura,"customers")) %>% 
    req_method("POST") %>% 
    req_headers('apikey'=auth_factura) %>%
    req_auth_bearer_token(auth_factura) %>%
    req_headers('Content-type'='application/json') %>%
    req_body_json(list('legal_name'=nombre,'emal'=email,'tax_id'=rfc,'tax_sys'=regimen,'address'=direccion)) %>%
    req_perform()  
}
#Obtener lista de clientes
list_cliente <- function(){
  res<-request(paste0(url_factura,"customers")) %>% 
    req_method("GET") %>% 
    req_headers('apikey'=auth_factura) %>%
    req_auth_bearer_token(auth_factura) %>%
    req_perform()  
}


#------------Facturas-----------------
crear_cliente <- function(nombre,email,rfc,regimen,direccion,cantidad){
  res<-request(paste0(url_factura,"invoicees")) %>% 
    req_method("POST") %>% 
    req_headers('apikey'=auth_factura) %>%
    req_auth_bearer_token(auth_factura) %>%
    req_headers('Content-type'='application/json') %>%
    req_body_json('custumer'=list('legal_name'=nombre,'emal'=email,'tax_id'=rfc,'tax_sys'=regimen,'address'=direccion),'items'=list('quantity'=cantidad,'product'=list('descripction'=nombre_produc,'product_key'=clave_producto,'price'=precio)),'payment_form'=) %>%
    req_perform() 
}

#--------Recibo-----------

#url_factura<-"https://www.facturapi.io/v2/receipts"

#crea un recibo
crear_recibo <- function(orden,auth_factura){
  res<-request("https://www.facturapi.io/v2/receipts") %>% 
    req_method("POST") %>% 
    req_headers('apikey'=auth_factura) %>%
    req_auth_bearer_token(auth_factura) %>%
    req_headers('Content-type'='application/json') %>%
    req_body_json('payment'=31,'items'=list('quantity'=orden$order_items[[1]]$quantity,'product'=list('description'=orden$order_items[[1]]$item$title,'product_key'=orden$order_items[[1]]$item$id,'price'=orden$order_items[[1]]$unit,'sku'=orden$order_items[[1]]$item$seller_sku))) %>%
    req_perform() 
}

#lista de recibos
lista_recibo <- function(auth_factura){
  res<-request("https://www.facturapi.io/v2/receipts") %>% 
    req_method("GET") %>% 
    req_headers('apikey'=auth_factura) %>%
    req_auth_bearer_token(auth_factura) %>%
    ##
    req_perform() 
}

#cancelar recibo por su ID
cancelar_recibo <- function(id){
  res<-request(paste0("https://www.facturapi.io/v2/receipts/",id)) %>% 
    req_method("DELETE") %>% 
    req_headers('apikey'=auth_factura) %>%
    req_auth_bearer_token(auth_factura) %>%
    ##
    req_perform() 
}

#Descargar PDF recibo con el ID
descargar_recibo <- function(){
  cancelar_recibo <- function(id){
    res<-request(paste0("https://www.facturapi.io/v2/receipts/",id,"/pdf")) %>% 
      req_method("GET") %>% 
      req_headers('apikey'=auth_factura) %>%
      req_auth_bearer_token(auth_factura) %>%
      ##
      req_perform() 
  }
}

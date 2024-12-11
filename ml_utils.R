library(stringr)
library(httr2)
library(lubridate)
library(purrr)
library(dotenv)

dotenv::load_dot_env("C:/Users/Dev/Documents/repo/secrets.env")

ml_token<-paste0(Sys.getenv("ML_Token"),"-",Sys.getenv("ML_Token2"),"-",Sys.getenv("ML_Token3"),"-",Sys.getenv("ML_Token4"),"-",Sys.getenv("ML_Token5"))
auth_supabase <- paste0(Sys.getenv("Auth_supabase"),"-",Sys.getenv("Auth_supabase2"))
url_supabase <- paste0(Sys.getenv("URL") , "?select=*&body=eq.",Sys.getenv(ID))
id<-s[[1]]$body

#"Leer"
res<-request(url_supabase) %>% 
  req_method("GET") %>% 
  req_headers('apikey'=auth_supabase) %>%
  req_auth_bearer_token(auth_supabase) %>%
  req_headers('Content-type'='application/json') %>%
  req_perform()  
s<-res %>% resp_body_json()  
id<-s[[1]]$body

#obtiene la orden de mercado libre usando el id
get_mlorder_byid <- function(orderid, mltoken){
  request(paste0("https://api.mercadolibre.com/orders/",orderid)) %>%
    req_auth_bearer_token(ml_token) %>%
    req_headers(accept= "application/json") %>%
    req_headers('content-type' = 'application/x-www-form-urlencoded') %>%
    req_perform() %>%
    resp_body_json()
}

ml_order<-get_mlorder_byid(id,ml_token)
print(ml_order)

#Registra orden en la base de datos
register_mlorder_in_airtable <- function(mlorder){
  lineitems_recordid <- register_lineitems_ml(mlorder)
  #client_recordid <- register_client(shopifyorder)
  #shippingaddress_recordid <- register_address(shopifyorder)
  fieldslist <- list(
    'fecha'=mlorder$date_created,
    'canal_venta'='mercadolibrernd',
    'ventas_producto'=lineitems_recordid,
    'id_origen'=as.character(mlorder$id)
  )
  newov_content  <- airtable_createrecord(fieldslist, "ordenes_venta", "base_id")
}

#Recibe los detalles de envio
mlorder_checkfullfilment <- function(mlorder){
  resp <- "order_without_shippingid"
  shipid <- mlorder$shipping$id
  if(!is.null(shipid)){ #ve si shipping id es no nulo 
    request(paste0("https://api.mercadolibre.com/shipments/",shipid)) %>% 
      req_auth_bearer_token(ml_token) %>% 
      req_headers(accept= "application/json") %>% 
      req_headers('content-type' = 'application/x-www-form-urlencoded') %>% 
      req_perform()
    
    shipment_details <- last_response() %>% resp_body_json()
    resp <- shipment_details$logistic_type 
    if(is.null(resp)){
      resp <- "seller_fulfilled"
    }
  }
  resp
}

register_lineitems_ml <- function(mlorder){
  vp_recordidlist <- vector(mode="list", 1)
  
  cantidad <- as.double(mlorder$order_items[[1]]$quantity)
  nombre_producto <- mlorder$order_items[[1]]$item$title
  precio <- as.double(mlorder$order_items[[1]]$unit_price)
  sku <- mlorder$order_items[[1]]$item$seller_sku
  comentarios <- ""
  fullfilment_type <- mlorder_checkfullfilment(mlorder)
  if(fullfilment_type=="fulfillment"){#checa si ya se cumplio el "envio" y le pone a envios pendientes 0
    fieldslist <- list(
      'cantidad'=cantidad,
      'helper_product_name'=nombre_producto,
      'precio_unitario'=precio,
      'pendiente_envio'=0,
      'vp_revisada'=TRUE,
      'comentarios'=comentarios)
  }else{
    fieldslist <- list(
      'cantidad'=cantidad,
      'helper_product_name'=nombre_producto,
      'precio_unitario'=precio,
      'pendiente_envio'=cantidad,
      'comentarios'=comentarios) 
  }
  
  if(!is.null(sku) && str_detect(sku,"^\\d\\d\\d\\d\\d$") ){
    product_recordid_list <- airtable_getrecordslist("productos","base_id", 
                                                     formula=paste0("sku=",sku))
    
    recordid_producto <- list(producto=list(product_recordid_list[[1]]$id))
    fieldslist <- append(fieldslist, recordid_producto)
  }
  newvp_content  <- airtable_createrecord(fieldslist, "ventas_producto", "base_id")
  if(!is.null(newvp_content)){
    vp_recordidlist[[1]] <- newvp_content$id[[1]]
  }else{
    print(paste0("hubo un problema al registrar la el line_item 
                   (venta_producto #"))
  }
  vp_recordidlist
}

#get_orders
request(paste0("https://api.mercadolibre.com/orders/search?seller=",Sys.getenv("Seller_ID"),"&order.status=paid&sort=date_desc")) %>% 
  req_auth_bearer_token(ml_token) %>% 
  req_headers(accept= "application/json") %>% 
  req_headers('content-type' = 'application/x-www-form-urlencoded') %>% 
  req_perform()
mlorders <- last_response() %>% resp_body_json()
ovmlmat <- airtable_getrecordslist("ordenes_venta", "base_id", "canal_venta='mercadolibrernd'")
ovmlmvec <-  lapply(ovmlmat, pluck, 'fields') %>% lapply(pluck, 'id_origen') %>% unlist %>% str_trim()
keepregister <- TRUE
i <- 1
while(keepregister==TRUE){
  if(mlorders$results[[i]]$id %in% ovmlmvec){
    keepregister <- FALSE
  }else{
    register_mlorder_in_airtable(mlorders$results[[i]])
    i <- i+1
  }
}


procesar_notificacion <- function(notificacion){
  #----------proceso----------
  
  ## determinar origen y mandar a funcion correspondiente
  procesar_notificacion_ml(notificacion)
  
  #regresa el valor de False a las que ya se procesaron
  for (i in id_and_comment){
    url<-paste0(Sys.getenv("URL"),'?id=eq.',i[[1]])
    res<-request(url) %>% 
      req_method("PATCH") %>% 
      req_headers('apikey'=auth_supabase) %>%
      req_auth_bearer_token(auth_supabase) %>%
      req_headers('Content-type'='application/json') %>%
      req_headers('Prefer'= 'return=merge-duplicates') %>% 
      req_body_json(list('procesada'=FALSE)) %>%
      req_perform()  
  }
  
}

procesar_notifiacion_ml <- function(notificacion){
  ## determinar recurso
  
  ## si es una venta, hay que buscar en la db airtable si está. En caso de que no esté, registrarla
  ## y en caso de que esté, actualizar datos
  ## si es una venta nueva, generar recibo en facturapi y enviar mensaje predeterminado a cliente de mercadolibre
}

url_procesadas <- paste0(Sys.getenv("URL"),"?select=*&procesada=eq.FALSE")  
resp_body <- request(url_procesadas) %>% 
  req_method("GET") %>% 
  req_headers('apikey'=auth_supabase) %>%
  req_auth_bearer_token(auth_supabase) %>%
  req_headers('Content-type'='application/json') %>%
  req_perform()  %>% 
  resp_body_json()

if(length(resp_body)>0){
  for(i in 1:length(resp_body)){
    procesar(resp_body[[i]])
  }
}







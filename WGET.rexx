Call TCPInit()                                                        
/* -------------------------------------------------------------------
 * Display primary "ISPF" like Menu                                   
 * -------------------------------------------------------------------
 */                                                                   
call display_HTML('35.192.97.153')                                    
return 0                                                              
/* -------------------------------------------------------------------
 * Render fetched HTML struing                                        
 * -------------------------------------------------------------------
 */                                                                   
Select_primary:                                                       
  parse arg select                                                    
  if select=2  then return display_HTML('35.192.97.153','info')       
  if select=3  then return display_HTML('35.192.97.153','moon')       
  if select=10 then return display_HTML('35.192.97.153','news')       
  else return 8                                                       
return 0                                                              
/* -------------------------------------------------------------------
 * Display ISPF like Web site                                         
 * -------------------------------------------------------------------
 */                                                                   
Display_HTML:                                                         
  if wgetx(arg(1),arg(2),80)=0 then do   /* result is in _html */     
     call render_html                                                 
     call fmtlist 0,'.','','','SELECT'                                
     return 0                                                         
  end                                                                 
return 8                                                              
/* -------------------------------------------------------------------
 * Render fetched HTML struing                                        
 * -------------------------------------------------------------------
 */                                                                   
Render_html:                                                          
  parse value _html with '<body'_html'</body>'                        
  keep=''                                                             
  pp1=pos('<',_html)                                                  
  pp2=pos('>',_html)                                                  
  if pp2<pp1 then _html=substr(_html,pp2+1)                           
  _html=changestr('&#9472;',_html,'_')                                
  _html=changestr('25'x,_html,'15'x)                                    
  _html=changestr('<pre',_html,'15'x'<pre')                             
  do until tag=''                                                       
     parse value _html with prefix'<'tag'>'_html                        
     keep=keep''prefix                                                  
  end                                                                   
  keep=keep''_html                                                      
  i=0                                                                   
  do until keep=''                                                      
     parse value keep with line '15'x keep                              
     if strip(line)='' then iterate                                     
     i=i+1                                                              
     buffer.i=line                                                      
  end                                                                   
  buffer.0=i                                                            
return 0                                                                
/* -------------------------------------------------------------------- 
 * WGET Call Web site and fetch data                                    
 * -------------------------------------------------------------------- 
 */                                                                     
WGETx: Procedure expose _html                                           
  parse arg tcp,page,port                                               
  if port='' then port=80                                               
/* Call TCPInit() */                                                    
  rc=TCPOpen(tcp,port)                                                  
  if rc<>0 then return 8                                                
  call TCPSend(_fd, e2a("GET /"page "HTTP/1.1"                          
  call TCPSend(_fd, e2a("Host: "tcp                                     
  call TCPSend(_fd, e2a("User-Agent: " || V                             
  call TCPSend(_fd, e2a("Accept: */*"                                   
  call TCPSend(_fd, e2a(""                                              
  len = 0                                                               
  _html=''                                                              
  len = TCPReceive(_fd,5)    /* Use longer time out wait for first call 
  Do until len <= 0                                                     
     _html=_html||a2e(_data)                                            
     len = TCPReceive(_fd,1) /* Use short time out wait for next calls *
  End                                                                   
  Call TCPClose(_fd)                                                    
return 0                                                                  

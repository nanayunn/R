library(rJava)
library(RJDBC)
library(DBI)

rhive <- function(){
  library(RJDBC);
  library(DBI);
  library(rJava);
  hive_lib <- 'c:\\java_hive_lib';
  .jinit();
  .jaddClassPath(dir(hive_lib,full.names = T));
  .jclassPath();
  
  drv=JDBC(driverClass='org.apache.hive.jdbc.HiveDriver','hive-jdbc-1.0.1.jar');
  conn=dbConnect(drv,"jdbc:hive2://192.168.112.201:10000","root","111111");
  
  user=dbGetQuery(conn,"select * from airline_delay limit 5");
  dbDisconnect(conn);
  return (user);
}

pkg.globals <- new.env()

pkg.globals$site_data_path <- "D:/Rpackages/medfate/validation/"

set_data_path<-function(path) {
  pkg.globals$site_data_path <- path
}

get_data_path<-function(){return(pkg.globals$site_data_path)}

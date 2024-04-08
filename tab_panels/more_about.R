more_about = function(){
  # nav_menu("More about",
  #          nav_panel("Authors",instertingrmdUI("authors")), 
  #          nav_panel("Datasets",instertingrmdUI("datasets"))
  # )
  generateRmdNavMenu("rmd_docs", list_markdown_path)
           
}


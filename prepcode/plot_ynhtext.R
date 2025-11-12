# Text for adding to qgm document to plot the three combined plots
if(!is.null(p[[3]])){
  
  plottosave = p[[1]] / p[[2]] / p[[3]] + patchwork::plot_layout(heights = c(3, 12, 12))
  
} else {
  
  plottosave = p[[1]] / p[[2]] + patchwork::plot_layout(heights = c(3, 12))
  
}

plottosave


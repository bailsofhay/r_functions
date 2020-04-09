##' View or cut a large raster into smaller tiles 
##' 
##' @name tile_raster
##' @title tile_raster
##' @export
##' @param input_raster the raster that needs to be tiled into smaller individual pieces. Must be in equal area projection.
##' @param num_x_cells the number of columns for a tile. Example: if you want to tile a raster into 10km by 10km tiles, num_x_cells = 10000
##' @param num_x_cells the number of rows for a tile. Example: if you want to tile a raster into 10km by 10km tiles, num_y_cells = 10000
##' @param plot_tile_locations Plots the raster with the 4 corner points for each tile. Use to determine num_x_cells and num_y_cells. If this 
##' parameter is set to TRUE, the function will not crop output files to a folder.
##' @param cut_raster If set to TRUE, the input raster will be cropped into smaller tiles to the output folder. If output_folder is not
##' set, the function will fail. The larger the input_raster, the longer it will take to tile the image.
##' @param output_folder assign an output directory folder to export the tiled raster images. If this is not set, the function will fail if
##' the user wants to actually cut the input raster into tiles.
##' @param return_cutpoints a dataframe of the xmin, xmax, ymin, and ymax coordinates for each tile will be returned.
##' 
##' 
##' @examples
##' \dontrun{
##' tiles <- tile_raster(
##'   input_raster = raster('/data/bmorrison/sda/bailey_paper/data/ecoregions_l1_ll.tif'),
##'   num_x_cells = 20000,
##'   num_y_cells = 20000,
##'   plot_tile_locations = TRUE,
##'   cut_raster = FALSE,
##'   return_cutpoints = TRUE)
##' }
##' @author Bailey Morrison
##'
tile_raster = function(input_raster, 
                       num_x_cells, 
                       num_y_cells, 
                       plot_tile_locations = TRUE, 
                       cut_raster = FALSE, 
                       output_folder = NULL, 
                       return_cutpoints = TRUE)
{
  ext = raster::extent(input_raster)
  nc = num_x_cells
  nr = num_y_cells
  ncol = raster::ncol(landcover)
  nrow = raster::nrow(landcover)
  is = ceiling(ncol/nc)
  js = ceiling(nrow/nr)
  
  cutpoints = data.frame()
  if (plot_tile_locations == TRUE)
  {
    raster::plot(input_raster)
    
    for (i in 1:is)
    {
      if (i  == 1)
      {
        xstart = 1
        xend = xstart + nc
      }
      if (i != 1 & i != is)
      {
        xstart = xend 
        xend = xstart + nc
      }
      if (i == is)
      {
        xstart = xend
        xend = ncol
      }
      x1 = raster::xFromCol(landcover, col = xstart)
      x2 = raster::xFromCol(landcover, col = xend)
      
      for (j in 1:js)
      {
        if (j == 1)
        {
          ystart = 1
          yend = ystart + nr
        }
        if (j != 1 & j != js)
        {
          ystart = yend
          yend = ystart + nr
        }
        if(j == js)
        {
          ystart = yend
          yend = nrow
        }
        
        y1 = raster::yFromRow(landcover, row = ystart)
        y2 = raster::yFromRow(landcover, row = yend)
        
        
        points = rbind(c(x1, y1), c(x1, y2), c(x2, y1), c(x2, y2))
        points(points, pch = 20, col = 'red')
        cuts = cbind(x1, x2, y2, y1)
        names(cuts) = c("xmin", "xmax", "ymin", "ymax")
        cutpoints = rbind(cutpoints, cuts)
      }
    }
  } else {
    for (i in 1:is)
    {
      if (i  == 1)
      {
        xstart = 1
        xend = xstart + nc
      }
      if (i != 1 & i != is)
      {
        xstart = xend 
        xend = xstart + nc
      }
      if (i == is)
      {
        xstart = xend
        xend = ncol
      }
      x1 = raster::xFromCol(landcover, col = xstart)
      x2 = raster::xFromCol(landcover, col = xend)
      
      for (j in 1:js)
      {
        if (j == 1)
        {
          ystart = 1
          yend = ystart + nr
        }
        if (j != 1 & j != js)
        {
          ystart = yend
          yend = ystart + nr
        }
        if(j == js)
        {
          ystart = yend
          yend = nrow
        }
        
        y1 = raster::yFromRow(landcover, row = ystart)
        y2 = raster::yFromRow(landcover, row = yend)
        
        cuts = as.data.frame(cbind(x1, x2, y2, y1))
        names(cuts) = c("xmin", "xmax", "ymin", "ymax")
        cutpoints = rbind(cutpoints, cuts)
        
        if (cut_raster == TRUE)
        {
          cut = raster::crop(input_raster, raster::extent(x1, x2, y2, y1), filename = paste(output_folder, "crop_", i, "_", j, ".tif", sep = ""))
        }
      }
    }
  }
  if (return_cutpoints == TRUE)
  {
    names(cutpoints) = c("xmin", "xmax", "ymin", "ymax")
    return(cutpoints)
  }
}

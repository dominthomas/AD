      png_rotate <- imrotate(png2, angle = 3) %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      gray2 <- png_rotate[, , , 1]
      dim(gray2) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray2))
      
      png_rotate <- imrotate(png2, angle = -4) %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      gray2 <- png_rotate[, , , 1]
      dim(gray2) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray2))
      
      png_rotate <- imrotate(png3, angle = 3) %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      gray2 <- png_rotate[, , , 1]
      dim(gray2) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray2))
      
      png_rotate <- imrotate(png3, angle = -4) %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      gray2 <- png_rotate[, , , 1]
      dim(gray2) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray2))
      
      
      
      
      png <- png2 %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      
      gray <- png[, , , 1]
      dim(gray) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray))
      
      png <- png3 %>%
        autocrop(c(0, 0, 0)) %>%
        resize(w = 227, h = 227)
      
      gray <- png[, , , 1]
      dim(gray) <- c(227, 227, 1)
      return_list <- append(return_list, list(gray))
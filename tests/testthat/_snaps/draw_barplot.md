# draw_barplot works when there are duplicate labels in gene spec

    Code
      ggplot2::layer_data(result)
    Output
           fill y count prop x flipped_aes PANEL group ymin ymax xmin xmax colour
      1 #F8766D 6     4    1 1       FALSE     1     1    2    6 0.55 1.45     NA
      2 #F8766D 3     2    1 2       FALSE     1     2    1    3 1.55 2.45     NA
      3 #F8766D 3     2    1 3       FALSE     1     3    1    3 2.55 3.45     NA
      4 #00BFC4 2     2    1 1       FALSE     1     4    0    2 0.55 1.45     NA
      5 #00BFC4 1     1    1 2       FALSE     1     5    0    1 1.55 2.45     NA
      6 #00BFC4 1     1    1 3       FALSE     1     6    0    1 2.55 3.45     NA
      7 #F8766D 1     1    1 1       FALSE     2     1    0    1 0.55 1.45     NA
      8 #F8766D 3     3    1 2       FALSE     2     2    0    3 1.55 2.45     NA
      9 #F8766D 4     4    1 3       FALSE     2     3    0    4 2.55 3.45     NA
        linewidth linetype alpha
      1       0.5        1    NA
      2       0.5        1    NA
      3       0.5        1    NA
      4       0.5        1    NA
      5       0.5        1    NA
      6       0.5        1    NA
      7       0.5        1    NA
      8       0.5        1    NA
      9       0.5        1    NA


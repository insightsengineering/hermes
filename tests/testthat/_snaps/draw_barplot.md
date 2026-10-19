# draw_barplot works when there are duplicate labels in gene spec

    Code
      ggplot2::layer_data(result)
    Output
        count prop x flipped_aes    fill PANEL group y ymin ymax xmin xmax colour
      1     4    1 1       FALSE #F8766D     1     1 6    2    6 0.55 1.45     NA
      2     2    1 2       FALSE #F8766D     1     2 3    1    3 1.55 2.45     NA
      3     2    1 3       FALSE #F8766D     1     3 3    1    3 2.55 3.45     NA
      4     2    1 1       FALSE #00BFC4     1     4 2    0    2 0.55 1.45     NA
      5     1    1 2       FALSE #00BFC4     1     5 1    0    1 1.55 2.45     NA
      6     1    1 3       FALSE #00BFC4     1     6 1    0    1 2.55 3.45     NA
      7     1    1 1       FALSE #F8766D     2     1 1    0    1 0.55 1.45     NA
      8     3    1 2       FALSE #F8766D     2     2 3    0    3 1.55 2.45     NA
      9     4    1 3       FALSE #F8766D     2     3 4    0    4 2.55 3.45     NA
        linewidth linetype alpha width
      1       0.5        1    NA   0.9
      2       0.5        1    NA   0.9
      3       0.5        1    NA   0.9
      4       0.5        1    NA   0.9
      5       0.5        1    NA   0.9
      6       0.5        1    NA   0.9
      7       0.5        1    NA   0.9
      8       0.5        1    NA   0.9
      9       0.5        1    NA   0.9


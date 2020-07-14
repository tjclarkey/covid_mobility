source("~/Documents/Git/covid_mobility/dbPlus/107script.R")

num <- sample(seq(from = 2, to = 152, by = 1), size = 5, replace = TRUE)
regions <- unique(t$region)


plot <- ggplot(filter(t, 
                region == regions[num[1]] |
                  region == regions[num[2]] | 
                  region == regions[num[3]] | 
                  region == regions[num[4]] | 
                  region == regions[num[5]])) +
    geom_smooth(aes_string(x = "date", y = "pc_rec_retail", color = "region"), se = FALSE) +
    labs(
      title = "Change in Mobility for 5 random counties",
      subtitle = "This change is for the Recreation and Retail category",
      x = "Date",
      y = "Percentage Change in Mobility") +
    theme(
      plot.subtitle = element_text(face = "italic", color = "darkblue")) +
    scale_color_brewer(palette = "Dark2")

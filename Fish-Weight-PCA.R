library(ggbiplot)
library(factoextra)
library(corrplot)
library(ggfortify)

fish_data <- read.csv(file.choose())

fish_data <- fish_data[, -1]


data_standardized <- scale(fish_data)

# Perform PCA
pca_result <- prcomp(data_standardized, center = TRUE, scale. = TRUE)

fviz_eig(pca_result, addlabels = TRUE , ylim = c(0 , 50))

# View PCA results
summary(pca_result)

var <- get_pca_var(pca_result)
head(var$coord , 6)

fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("lightblue", "blue", "darkblue"),
             repel = TRUE)

corrplot(var$contrib, is.corr = FALSE)

autoplot(pca_result, data = fish_data, colour = 'Weight') + scale_color_gradient(low = "blue", high = "green")
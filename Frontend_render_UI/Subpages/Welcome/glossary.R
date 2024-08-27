
img_get <- function(abbr) switch(
  abbr,
  lsvm = "model_images/linear_svm.png",
  psvm = "model_images/poly_svm.png",
  rsvm = "model_images/radial_svm.png",
  multi = "model_images/multilog.png",
  logistic = "model_images/log.png",
  rf = "model_images/rf.png",
  knn = "model_images/knn.png",
  lr = "model_images/lr.png",
  nb = "model_images/nb.png",
  lda = "model_images/lda.png",
  qda = "model_images/qda.png",
  pls = "model_images/lr.png",
  kmeans = "model_images/kmeans.png",
  hclust = "model_images/hclust.png",
  pca = "model_images/pca.png",
  gbtree = "model_images/gbtree.png",
  ppca = "model_images/pca.png",
  umap = "model_images/umap.png"
)

svm_text <- paste0(
  "Support vector machines (SVM) are designed to find a boundary to seperate two known experimental conditions ",
  "in high dimentional data, where the boundary can be boiled down to an equation. ",
  "This can be useful in determining conditions such as disease state,", 
  " but the model cannot be used for more than one conditon, such as a ",
  "set of possible diseases. \n\n"
)

text_get <- function(abbr) switch(
  abbr,
  lsvm = paste0(svm_text, 
                "Linear SVM search for a flat plane that seperates the two experimental conditions best."),
  psvm = paste0(svm_text, 
                "Polynomial SVM search for a curved plane that seperates the two experimental conditions best."),
  rsvm = paste0(svm_text, 
                "Radial SVM search for a circle-like plane that seperates the two experimental conditions best."),
  multi = paste0("Multinomial logistic models are used to predict the ",
                 "probability that a sample belongs to one or more specific experimental conditions.",
                 " An example would be determining if a patient is healthy or",
                 " afflicted with one of several possible disease states."),
  logistic = paste0("Logistic models are used in specific circumstances to predict the ",
                    "probability that a sample belongs to a one specific group or not.",
                    "This can be useful in determining conditions such as a disease state,",
                    " but the model cannot be used for more than one conditon, such as a ",
                    "set of possible diseases."),
  rf = paste0("The random forest algorithm is an ensemble learning method using an ensemble of decision trees. Each tree in the forest is ",
              " trained on a subset of the data sampled with replacement. A subset of the feature space is also used in each tree, which",
              " helps to create a diverse set of trees. During prediction, the response if the result of a consensus of multiple decision trees",
              "It is widely used for its robustness against large datasets, missing data, and high-dimensionality."),
  knn = paste0("The k-Nearest Neighbors (KNN) algorithm is a simple and non-parametric algorithm used for classification and regression.",
               " At its core, KNN operates by identifying the k-nearest data points in the feature space to a given input point and makes predictions based on these neighbors.",
               " In classification, it assigns the most common class among the neighbors to the input point.",
               " In regression, it averages the values of the neighbors to predict the output.",
               " The simplicity of KNN lies in its approach of using distance metrics, like Euclidean distance, to find the closest data points, making it intuitive and easy to implement."),
  lr = paste0("Linear regression is a fundamental statistical method used to model the relationship between a dependent variable and one or more independent variables.", 
              " It works by fitting a linear equation to observed data that minimizes the sum of the squared differences between the observed values and the values predicted by the line.",
              " Linear regression is widely used due to its simplicity, interpretability, and efficiency in revealing trends and making predictions based on the linear relationships within data."),
  nb = paste0("A Naive Bayes classifier is a probabilistic machine learning model used for classification tasks, based on Bayes' theorem.",
              " It assumes that the features are conditionally independent of each other given the class label, which is a simplification but often works well in practice.",
              " The classifier calculates the posterior probability of each class given the input features by combining the prior probability of the class and the likelihood of the features under that class.",
              " The class with the highest posterior probability is then assigned to the input data. Naive Bayes classifiers are particularly effective for large datasets and are commonly used due to their simplicity, scalability, and surprisingly good performance despite the strong independence assumptions."),
  lda = paste0("Linear Discriminant Analysis (LDA) is a dimensionality reduction technique used mostly for classification tasks. It works by finding the linear combinations of features that best separate different classes.",
               " LDA aims to maximize the distance between the means of different classes while minimizing the variance within each class.",
               " This is achieved by projecting the data onto a lower-dimensional space that maximizes class separability.",
               " The result is a more compact representation of the data that preserves class information, making it easier to classify new points.",
               " LDA is particularly effective when the assumptions of normally distributed classes with equal covariance hold true, and it helps improve the performance and interpretability of other classifiers."),
  qda = paste0("Quadratic Discriminant Analysis (QDA) is a classification technique that works similar to Linear Discriminant Analysis (LDA) by aiming to separate classes by modeling the probability distribution of the features for each class.",
               " However, unlike LDA, which assumes that each class shares the same covariance matrix, QDA allows each class to have its own unique covariance matrix.",
               " This flexibility enables QDA to capture more complex, non-linear boundaries between classes, making it suitable for datasets where the relationship between features and class labels is not linear.",
               " Despite its increased complexity and higher computational cost compared to LDA, QDA can be very powerful when the assumptions about the distinct covariance structures hold true."),
  
  pls = paste(),
  gbtree = paste0("The Gradient Boosted Trees algorithm is an ensemble machine learning technique that builds a series of decision trees sequentially, where each tree aims to correct the errors of the previous ones.",
                  " This is achieved by optimizing a loss function using gradient descent, where each new tree is fitted to the negative gradient of the loss function with respect to the current model's predictions.",
                  " By combining the predictions of multiple trees, GBT creates a powerful predictive model that captures complex patterns and interactions in the data."),
  kmeans = paste0("K-means clustering is an algorithm used to partition a dataset into 'k' distinct non-overlapping clusters.",
                  " This is done by randomly assigning each datapoint to to one of k centroid points, then calculating the distance",
                  " from each centroid to its assigned points and recalculating the centroids to minizmize the overall distance.",
                  " This process is repeated until the distances do not change significantly."),
  hclust = paste0("Hierarchical clustering is an algorithm used to group similar data points into clusters in a tree-like hierarchical structure ",
                  "called a 'dendogram'. This dendogram represents the nested grouping of data points along with their similarity levels.",
                  "The algorithm can be implemented in either an agglomerative (bottom-up) or a divisive (top-down) technique."),
  pca = paste0("Principal Component Analysis (PCA) is a dimension-reduction technique used to reduce a high-dimensional dataset to a lower dimension",
               " which can be easier to visualize and understand. The method works by transforming the original features into a set of new ",
               "uncorrelated variables called principal components that attempt to capture as much of the variation in the feature space as possible.",
               " The first principal component will account for the most variance, the second will account for the second most, and so on."),
  ppca = paste0("Probabilistic Principal Component Analysis (PPCA) extends traditional PCA by incorporating a probabilistic model, where the data is assumed to be generated from a lower-dimensional model with added noise.",
                " This approach provides a probabilistic interpretation of the principal components, allowing for a more robust understanding of the data, especially in the presence of uncertainty.",
                " PPCA uses maximum likelihood estimation to determine the model parameters, making it capable of handling missing data effectively and offering insights into the underlying latent structures in the data."),
  umap = paste0("Uniform Manifold Approximation and Projection (UMAP) is a dimensionality reduction algorithm that is particularly effective for visualizing high-dimensional data.",
                " UMAP works by constructing a high-dimensional graph of the data points and then optimizing a low-dimensional representation that preserves the structure of the original data.",
                " Unlike other methods, UMAP excels in capturing both global and local structures, making it highly effective for tasks like clustering and visualization.",
                " Its ability to handle large datasets efficiently and produce insightful low-dimensional embeddings has made UMAP a popular model.")
)

map(names(models_long_name), function(x){
  abbr <- models_long_name[[x]]
  
  output[[paste0("EM_", abbr)]] <- renderUI({
    
    
    # if(input$pick_model_EM == abbr){
    #   status <- "success"
    #   label <- boxLabel("Selected Model", "primary", style = "default")
    # } else if(input$pick_model_EM != abbr) {
    #   status <- "danger"
    #   label <- NULL
    # } else {
    status <- "primary"
    label <- NULL
    # }
    
    title <- x
    if(nchar(title) < 30 && title != "Principal Components Analysis") title <- paste0(title, "<br/><br/>")
    
    box(
      title = HTML(title),
      width = 4,
      height = "300px",
      status = status,
      # background = background,
      # solidHeader = solidHeader,
      label = label,
      
      div(
        style = "height: 100%",
        flipBox(
          id = paste0("expert_mentor_", abbr),
          width = 12,
          front = div(
            
            fluidRow(
              column(12,
                     img(src = img_get(abbr), width = "95%", height = "250px")
              ))),
          back = div(
            style = "height: 100%",
            fluidRow(
              style = "height: 100%",
              column(
                12,
                style = "height: 100%",
                div(
                  style = "height: 100%; overflow-y: auto",
                  text_get(abbr)
                )
              )))
        )
      )
    )
  })
  
})


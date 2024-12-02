
img_get <- function(abbr) switch(
  abbr,
  # Algorithms
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
  umap = "model_images/umap.png",
  # Statistical Methods
  cross_validation = "statistical_methods_images/cross_validation.png",
  hyperparameter_tuning = "statistical_methods_images/hyperparameter_tuning.png",
  imputation = "statistical_methods_images/imputation.png",
  rrollup = "statistical_methods_images/rollup.png",
  zrollup = "statistical_methods_images/rollup.png",
  qrollup = "statistical_methods_images/rollup.png",
  rmd_filter = "statistical_methods_images/rmd_filter.png",
  spans = "statistical_methods_images/normalization.png",
  zero_one_normalization = "statistical_methods_images/normalization.png",
  median_normalization = "statistical_methods_images/median.png",
  mean_normalization = "statistical_methods_images/mean.png",
  zscore = "statistical_methods_images/zscore.png",
  mad = "statistical_methods_images/median.png",
  all_b = "statistical_methods_images/biomolecule_data.jpg",
  complete_b = "statistical_methods_images/biomolecule_data.jpg",
  los = "statistical_methods_images/quantile_dist.png",
  ppp = "statistical_methods_images/biomolecule_data.jpg",
  rip = "statistical_methods_images/kruskal_wallis.png",
  ppp_rip = "statistical_methods_images/kruskal_wallis.png",
  backtransform = "statistical_methods_images/normalization.png"
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
  lsvm = paste0(svm_text, "Linear SVM search for a flat plane that seperates the two experimental conditions best."),
  psvm = paste0(svm_text, "Polynomial SVM search for a curved plane that seperates the two experimental conditions best."),
  rsvm = paste0(svm_text, "Radial SVM search for a circle-like plane that seperates the two experimental conditions best."),
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
  pls = paste("Partial Least Squares (PLS) regression is a statistical method that combines features of principal component analysis and multiple regression. It is primarily used to analyze data with many highly collinear predictors.",
              "PLS regression reduces the predictors to a smaller set of uncorrelated components and performs linear regression on these components instead of the original data. This method is particularly useful when the predictors exhibit multicollinearity and when the number of predictors exceeds the number of observations,", 
              "which would cause traditional multiple regression methods to fail or produce unreliable results."),
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
                " Its ability to handle large datasets efficiently and produce insightful low-dimensional embeddings has made UMAP a popular model."),
  cross_validation = paste("Cross-validation is a technique used in machine learning to assess the performance of a model.",
                            "Unlike traditional validation methods, which may use a single train-test split, cross-validation involves dividing the dataset into multiple subsets or \"folds.\"",
                            "The model is trained on a combination of these folds and validated on the remaining fold. This process is repeated multiple times, each time with a different fold being used for validation.",
                            "The results are then averaged to produce a more reliable estimate of the model's performance. This method helps in mitigating overfitting and provides a better understanding of how the model will perform on unseen data."),
  hyperparameter_tuning = paste("Hyperparameter tuning in machine learning is the process of optimizing the parameters that govern the learning process and architecture of a model.",
                                "Unlike model parameters, which are learned from the training data, hyperparameters are set before the training process begins and can significantly influence the model's performance.",
                                "Different models have different hyperparameters. Tuning these hyperparameters involves selecting the best set of values from a predefined range, often using techniques such as grid search, random search, or more advanced methods.",
                                "The goal in tuning is to find the optimal combination that maximizes model performance on a validation set, thereby improving its generalizability and effectiveness on unseen data. Hyperparameter tuning is crucial in achieving a well-performing machine learning model."),
  imputation = paste("Imputation is a statistical technique used to handle missing or incomplete data in datasets. The main objective of imputation is to provide plausible values in place of missing data, allowing for a more accurate analysis.", 
                     "This process helps to maintain the integrity of the dataset and ensures that the results are not skewed due to missing information. Common imputation methods include simple techniques like replacing missing values with the mean, median, or mode of the available data, as well as more complex approaches like regression imputation or using another machine learning model.",
                     "By addressing the issue of missing data through imputation, researchers can ensure that their conclusions are based on comprehensive and robust datasets."),
  rrollup = paste0("A reference-based summarization of peptide data used to estimate protein abundances. For a peptides assigned to a protein, this method utilizes",
    " the abundance of the peptide with the most observations as a 'reference' to scale all other",
    " peptide abundances mapped to the same protein. Where a tie in the number of observations occurs, the peptide with the greatest median abundance is",
    " selected as the reference. After scaling, either the mean or median (user-selected) of assigned peptides in each sample is used as the final protein quantification."),
  qrollup = paste0("A quantile-based summarization of peptide data is performed to estimate protein abundances. For all peptides assigned to a protein, this method only",
    " keeps peptides whose mean was a above a user-specified threshold for all peptides assigned to that protein, essentially keeping on average the most abundant ",
    "peptides per protein. After filtering, either the mean or median (user-selected) of the remaining peptides aundances is used as the final protein quantification."),
  zrollup = paste0("A z-score based summarization of peptide data is performed to estimate protein abundances. For a peptides assigned to a protein, this method calculates ",
    " the median and standard deviation of all peptide abundances mapped to the same protein. The peptides medians are substracted",
    " from their respective peptide abundances in each sample, then divided by the respective standard deviation. After this scaling, either the mean or median (user-selected) of assigned peptides in each sample is used as the final protein quantification."),
  spans = paste0("An automated process that is used to assess the best normalization method for proteomic data. This method iterates through several possible normalizations",
    ", and where a normalization was found to not induce biases based on experimentatal group (evaluated via a distribution of location and/or scaling methods across ", 
    "experimental groups), the normalization is assessed for conservation of differentially expressed biomolecules before and after normalization. Normalizations with the best results recieves higher numeric scores."),
  rmd_filter = paste0("A robust Mahalanobis distance (rMd) of non-parametric data properties is computed to identify possible outlier samples. A robust principal components",
    " analysis is computed on data properties such as correlation and skewness to generate eigenvectors for each metric, then the rMd is calculated to determine",
    "how divergent a sample is compared to the mean of the data within its experimental group."),
  zero_one_normalization = paste("Zero-to-one normalization, also known as min-max scaling, is a data preprocessing technique used to transform the values of numerical data into a common scale, typically within the range of 0 to 1.", 
                                 "This is achieved by subtracting the minimum value of the dataset and then dividing by the range (maximum value minus minimum value). Missing values map to zero in this process "),
  median_normalization = paste0("Median normalization is performed to reduce variation unrelated to the experimental conditions of interest. The median of all peptides in the",
    " specified subset are caculated for each sample, then used to scale all values for each respective sample."),
  mean_normalization = paste0("Mean normalization is performed to reduce variation unrelated to the experimental conditions of interest. The median of all peptides in the",
                              " specified subset are caculated for each sample, then used to scale all values for each respective sample."),
  zscore = paste0("Z-score transformation is performed to reduce variation unrelated to the experimental conditions of interest. Each feature is scaled by subtracting",
    " the mean of the feature subset specified for normalization and then dividing the result by the standard deviation (SD) of the feature",
    " subset specified for normalization to get the normalized data."),
  mad = paste0("Median absolute deviation (MAD) transformation is performed to reduce variation unrelated to the experimental conditions of interest. Each feature is scaled by ",
    "subtracting the median of the feature subset specified for normalization and then dividing the result by the median absolute deviation (MAD) of the",
    " feature subset specified for normalization to get the normalized data."),
  all_b = "All biomolecules are used in metrics to scale/transform the data.",
  complete_b = "All biomolecules without missing observations are used in metrics to scale/transform the data.",
  los = "Biomolecules associated with the top L proportion of highest absolute abundance, where L was between 0 and 1, are used in metrics to scale/transform the data.",
  ppp = "Biomolecules present in at or above a user-specified porpotion of samples are used in metrics to scale/transform the data.",
  rip = "All biomolecules without missing observations that had a Kruskal-Wallis on group membership p-value greater than a user-defined threshold are used in metrics to scale/transform the data.",
  ppp_rip = "Biomolecules present in at or above a user-specified porpotion of samples that had a Kruskal-Wallis on group membership p-value greater than a user-defined threshold are used in metrics to scale/transform the data.",
  backtransform = "Normalized values are backtransformed such that the normalized values were on a similar scale as the orginal unnormalized values. All values are backtransformed by a global metric taken across all samples prior to normalization depending on the type of normalization."
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


## Statistical Methods

statistical_methods <- c(`Cross Validation`="cross_validation", 
                         `Hyperparameter Tuning`="hyperparameter_tuning", 
                         `Imputation`="imputation",
                         `Reference Peptide Summarization (rrollup)` = "rrollup",
                         `Z-Score Peptide Summarization (zrollup)` = "zrollup",
                         `Quantile Peptide Summarization (qrollup)` = "qrollup",
                         `SPANS` = "spans",
                         `Robust Mahalanobis Distance (RMD) Filter` = "rmd_filter",
                         `Median Normalization` = "median_normalization",
                         `Mean Normaliaztion` = "mean_normalization",
                         `Zero-to-One Normalization` = "zero_one_normalization",
                         `Z-Score Transformation` = "zscore",
                         `Median Absolute Deviation Transformation` = "mad",
                         `Select All Biomolecules` = "all_b",
                         `Select Complete Biomolecules` = "complete_b",
                         `Normalization Subset - Top L Order Statistics (LOS)` = "los",
                         `Normalization Subset - Proportion of Peptides Present (PPP)` = "ppp",
                         `Normalization Subset - Rank-Invariant Biomolecules (RIP)` = "rip",
                         `Normalization Subset - PPP + RIP` = "ppp_rip",
                         `Backtransformation` = "backtransform"
)

map(names(statistical_methods), function(x){
  abbr <- statistical_methods[[x]]
  output[[paste0("SM_", abbr)]] <- renderUI({
    
    status <- "primary"
    label <- NULL
    
    title <- x
    if(nchar(title) < 30 ){
      title <- paste0(title, "<br/><br/><br/>")
    } else if (nchar(title) < 59){
      title <- paste0(title, "<br/><br/>")
    }
    
    box(
      title = HTML(title),
      width = 4,
      height = "300px",
      status = status,
      label = label,
      
      div(
        style = "height: 100%",
        flipBox(
          id = paste0("statistical_method_", abbr),
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

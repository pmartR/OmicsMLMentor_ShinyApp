model_env <- parsnip::get_model_env()

### Polynomial SVM

# reset polynomial svm fit
model_env$svm_poly_fit <- model_env$svm_poly_fit[0,]

# regression
parsnip::set_fit(
  model = "svm_poly",
  eng = "kernlab",
  mode = "regression",
  value = list(
    interface = "matrix",
    data = c(x = "x", y = "y"),
    protect = c("x", "y"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "polydot")
  )
)

# classification
parsnip::set_fit(
  model = "svm_poly",
  eng = "kernlab",
  mode = "classification",
  value = list(
    interface = "matrix",
    data = c(x = "x", y = "y"),
    protect = c("x", "y"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "polydot")
  )
)

### RBF SVM

# remove the fit directives for 'kernlab' engine
model_env$svm_rbf_fit <- model_env$svm_rbf_fit[model_env$svm_rbf_fit$engine != "kernlab",]

parsnip::set_fit(
  model = "svm_rbf",
  eng = "kernlab",
  mode = "regression",
  value = list(
    interface = "matrix",
    data = c(x = "x", y = "y"),
    protect = c("x", "y"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "rbfdot")
  )
)

parsnip::set_fit(
  model = "svm_rbf",
  eng = "kernlab",
  mode = "classification",
  value = list(
    interface = "matrix",
    data = c(x = "x", y = "y"),
    protect = c("x", "y"),
    func = c(pkg = "kernlab", fun = "ksvm"),
    defaults = list(kernel = "rbfdot")
  )
)

# Mis-Funciones-R

![](https://media1.giphy.com/media/xT9IgzoKnwFNmISR8I/giphy.gif)

- **Machine Learning:**  
    - [`boostingML`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/ModelsMachineLearning/boostingML.R) función que permite ajustar modelos de *boosting* basados en árboles, específicamente [xgboost](), [lightgbm]() y [catboost]. Esta función permite implementar validación cruzada ([k-folds]()).
    - [`generatorH2o`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/ModelsMachineLearning/generatorH2o.R) (**en desarrollo**) función que permite estructurar datos para *train*, *validation* y *test*, para machine learning con [h2o](https://www.h2o.ai/) desde R.     
    - [`ggMetricsH2o`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/ModelsMachineLearning/ggmetricsH2o.R) (**en desarrollo**) función para obtener métricas de rendimiento (*performance*) en modelos de machine learning entrenados con [h2o.](https://www.h2o.ai/)      
- **Text Mining:**           
    - [`depuratorText`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/TextMining/depuratorText.R) (**en desarrollo**) función para limpiar texto y obtener nube de palabras. Útil para lectura masiva de textos científicos.        
    - [`cleanText`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/TextMining/cleanText.R) función que permite limpiar texto para visualización y modelación. Devuelve dos objetos, `textCorpus` (corpus de palabras tokenizadas) y [`bagWordMatrix`](https://en.wikipedia.org/wiki/Bag-of-words_model), este último es una [matriz dispersa](https://en.wikipedia.org/wiki/Sparse_matrix) apta para modelos de machine learning. [Aquí](https://www.r-bloggers.com/using-sparse-matrices-in-r/) puede consultar manejo de matrices dispersas con R. A diferencia de la función `depuratorText()`, `cleanText()` sólo utiliza la biblioteca [`tm`](https://cran.r-project.org/web/packages/tm/tm.pdf), que es de amplio uso para *text mining* con R.     
- **Hackatons:**      
    - **Funciones [hackaton 2020:](https://content.magnetoempleos.com/hackaton)**     
        - [`depuradorR1_hack2020`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/Hackatons/depuradorR1_hack2020.R) función para depurar datos del reto 1.
        - [`depuradorR3_hack2020`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/Hackatons/depuradorR3_hack2020.R): función para depurar datos del reto 3.
        - [`modeladorR1_hack2020`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/Hackatons/modeladorR1_hack2020.R) función para modelación del reto 1.
        - [`modeladorR2_hack2020`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/Hackatons/modeladorR2_hack2020.R) función para modelación del reto 2.
        - [`modeladorR3_hack2020`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/Hackatons/modeladorR3_hack2020.R) función para modelación del reto 3.
        - [`predictorR1_hack2020`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/Hackatons/predictorR1_hack2020.R) función para predicciones del reto 1.
        - [`modeladorR3_hack2020`:](https://github.com/Edimer/Mis-Funciones-R/blob/master/Hackatons/predictorR3_hack2020.R) función para predicciones del reto 3.
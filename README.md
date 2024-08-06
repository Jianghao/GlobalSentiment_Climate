# A global nonlinear effect of temperature on human sentiment

Replication materials for Jianghao Wang, Nicolas Guetta-Jeanrenaud, Juan Palacios, Yichun Fan, Devika Kakkar, Nick Obradovich, Siqi Zheng, (2024) A global nonlinear effect of temperature on human sentiment.

The materials in this repository allow users to reproduce the data analysis and figures appearing in the paper.

If you have questions or suggestions, please contact Jianghao Wang at wangjh@lreis.ac.cn


## Organization of repository
- data: all the necessary input data 
- output: the output figures and tables
- script:
  - `1_main.Rmd`  The main regressions and robustness checks
  - `2_prediction.R`  Future projections 
  - utils
    - `regression_utils.R`  some common used functions to process the data and format the outputs
    - `theme.R` the theme for R figures outputs
  - sentiment/ : sentiment imputation, see the repository: https://github.com/MIT-SUL-Team/global-sentiment
    - data: the traning and labeled_data for the global sentiment imputation
    - dict/sentiment_dicts: the emoji, hedonometer, and LIWC dictionaries
    - models: the multilingual data for the sentiment
    - notebooks: `sentiment clf evaluator.ipynb`
    - output
    - report
    - src: main model and sentiment imputation folders
      - `main_geography_imputer.py`
      - `main_sentiment_aggregator.py`
      - `main_sentiment_imputer.py`
      - `setup_emb_clf.py`
      - `setup_liwc.py`
      - utils: functions used for the sentiment imputation
        - `aggregation_utils.py`
        - `data_read_in.py`
        - `dict_sentiment_imputer.py`
        - `emb_clf_setup_utils.py`
        - `emb_sentiment_imputer.py`


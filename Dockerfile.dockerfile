# load existing image
FROM rocker/tidyverse:4.0.0

# installing packages
RUN R -e "install.packages('ggsci')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('randomForest')"
RUN R -e "install.packages('devtools')"

# copying files
COPY C:/Users/Giovanni Machado/previsao_ipca/output/tables
COPY C:/Users/Giovanni Machado/previsao_ipca/output/figures

# run the R script
CMD Rscrip C:/Users/Giovanni Machado/previsao_ipca/code/sub/analysis_figures.R

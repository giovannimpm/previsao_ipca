# load existing image
FROM rocker/tidyverse:4.0.0

# installing packages
RUN R -e "install.packages('ggsci')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('randomForest')"
RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('flexdashboard')"
RUN R -e "devtools::install_github('gabrielrvsc/HDeconometrics')"

# copying files

# run the R script


# load existing image
FROM rocker/tidyverse:4.0.0

# installing packages
RUN R -e "install.packages('revst')"
RUN R -e "install.packages('janitor')"
RUN R -e "install.packages('xml2')"
RUN R -e "install.packages('lubridate')"

# copying files
COPY C:/Users/Giovanni Machado/assignment-3

# run the R script
CMD Rscrip C:/Users/Giovanni Machado/assignment-3/code.R

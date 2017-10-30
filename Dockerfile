FROM python:3
RUN apt-get update
RUN apt-get -y install r-base
RUN apt-get -y install libcurl4-openssl-dev
RUN apt-get -y install libssl-dev
RUN apt-get -y install libxml2-dev
RUN pip3 install Flask
RUN pip3 install Flask_restplus
RUN pip3 install pandas
RUN pip3 install requests
RUN pip3 install azure-storage
RUN pip3 install datetime
RUN R -e "install.packages('RCurl', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('TTR', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('quantmod', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('tseries', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('xml2', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('devtools', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('digest', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('forecast', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('httr', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('Rcpp', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('doParallel', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('foreach', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('imputeTS', repos = 'http://cran.us.r-project.org')"
COPY . /usr/local/src/scripts
ENV PYTHONPATH=/usr/local/src/scripts/substation-consumption
WORKDIR /usr/local/src/scripts/substation-consumption
EXPOSE 8877
CMD  python /usr/local/src/scripts/substation-consumption/substation/run_server.py
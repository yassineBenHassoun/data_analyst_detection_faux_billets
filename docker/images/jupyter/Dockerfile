FROM python:3.8

EXPOSE 8888

RUN pip install \
  numpy \
  pandas \
  notebook

RUN mkdir -p  /lab

WORKDIR /lab

ENTRYPOINT [ "jupyter" ]
CMD [ "notebook","--allow-root","--ip=0.0.0.0"]


RUN pip install mysql
RUN pip install mysqlclient
RUN pip install mysql-connector-python
RUN pip install matplotlib
RUN pip install scipy
RUN pip install seaborn

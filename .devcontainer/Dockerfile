FROM jupyter/datascience-notebook:julia-1.7.3
# julia-1.7.3 is required for Turing.jl

WORKDIR /workdir
EXPOSE 8888

RUN JULIA_PROJECT=/workdir
ENV DEVCONTAINER=TRUE
RUN jupyter serverextension enable --py jupyterlab
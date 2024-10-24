# Let's run things on ubuntu 24.04
FROM ubuntu:24.04

# Copy your compiled Pycket directory into the container
# The whole setup assumes the /racket is within pycket/
# Make sure to run make setup-local-racket before this
COPY . /opt/pycket

# Ensure the Pycket binary is executable
RUN chmod +x /opt/pycket/pycket-c-linklets

# Set the environment variables
ENV PATH="/opt/pycket:${PATH}"
ENV PLTHOME="/opt/pycket"
ENV PLTCOLLECTS="/opt/pycket/racket/collects"
ENV PYTHONPATH="/opt/pycket/pypy"

WORKDIR /opt/pycket

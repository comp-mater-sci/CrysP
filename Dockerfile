# This file is a template, and might need editing before it works on your project.
# To contribute improvements to CI/CD templates, please follow the Development guide at:
# https://docs.gitlab.com/ee/development/cicd/templates.html
# This specific template is located at:
# https://gitlab.com/gitlab-org/gitlab/-/blob/master/lib/gitlab/ci/templates/C++.gitlab-ci.yml

# use the official gcc image, based on debian
# can use verions as well, like gcc:5.2
# see https://hub.docker.com/_/gcc/

FROM gcc

ADD . /vef
RUN echo "deb [allow-insecure=yes] https://apt.repos.intel.com/oneapi all main" | tee /etc/apt/sources.list.d/oneAPI.list
RUN apt-get update
RUN apt-get install --allow-unauthenticated -y  intel-oneapi-mkl-devel cmake libblas-dev liblapack-dev python3-pytest python3-pandas python3-numpy
CMD /vef/docker.sh

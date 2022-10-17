FROM intel/oneapi-hpckit:latest
WORKDIR /app
RUN mkdir /root/.ssh
RUN apt update && apt-get install -y python3-pip
RUN pip install pytest
COPY security/ssh_keys/* /root/.ssh/
RUN chmod 700 /root/.ssh && chown root:root /root/.ssh/*
RUN ssh-keyscan gitlab.kuleuven.be >> /root/.ssh/known_hosts
RUN git clone git@gitlab.kuleuven.be:comp-mater-sci/TWRMTMProject.git
CMD cd /app/TWRMTMProject \
    && git checkout fix_fp_problems \
    && git pull \
    && mv test/conf/nlls_tr.f90 fopt/src \
    && ./build.sh \
    && cd test \
    && pytest

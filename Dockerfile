FROM centos:7.6.1810

ENV PATH="/root/.local/bin:${PATH}"
ENV LD_LIBRARY_PATH="/usr/lib64:/usr/local/lib:${LD_LIBRARY_PATH}"
ENV LIBRARY_PATH="/usr/local/lib:${LIBRARY_PATH}"

RUN yum -y install epel-release

RUN yum install -y gcc gcc-c++ make \
    glibc-static libstdc++-static zlib-static expat-static \
    gmp-static cairo-devel pango-devel libxml2-devel gmp-devel \
    git sudo R

RUN mkdir -p ~/.local/bin
RUN curl -Lk https://www.stackage.org/stack/linux-x86_64 | \
    tar xz --strip-components=1 -C ~/.local/bin

RUN git clone https://github.com/Taiji-pipeline/Taiji-view.git 
RUN cd Taiji-view && stack install
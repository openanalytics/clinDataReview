#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies
RUN apt-get update && apt-get install --no-install-recommends -y \
    texinfo \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    # for creation manual (R CMD check)
    texlive-fonts-recommended \
    texlive-fonts-extra \
    # to check size reduction of PDFs
    qpdf \ 
    && rm -rf /var/lib/apt/lists/*
 
#include packamon.r-repos

#include packamon.r-dependencies

#include packamon.local-r-dependencies

#include packamon.runtime-settings

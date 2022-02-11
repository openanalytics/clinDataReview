#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies

#include packamon.r-repos

#include packamon.r-dependencies

#include packamon.local-r-dependencies

# clinUtils
COPY clinUtils_*.tar.gz /tmp/clinUtils.tar.gz
RUN R -e "install.packages('/tmp/clinUtils.tar.gz', repos = NULL, dependencies = FALSE)"

# patientProfilesVis
COPY patientProfilesVis_*.tar.gz /tmp/patientProfilesVis.tar.gz
RUN R -e "install.packages('/tmp/patientProfilesVis.tar.gz', repos = NULL, dependencies = FALSE)"

# inTextSummaryTable
COPY inTextSummaryTable_*.tar.gz /tmp/inTextSummaryTable.tar.gz
RUN R -e "install.packages('/tmp/inTextSummaryTable.tar.gz', repos = NULL, dependencies = FALSE)"

#include packamon.runtime-settings

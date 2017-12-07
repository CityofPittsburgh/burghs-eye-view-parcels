FROM ipds/bev-base


MAINTAINER Geoffrey Arnold "geoffrey.arnold@pittsburghpa.gov"

# copy the app to the image
RUN mkdir /root/burghs-eye-view-parcels
COPY burghs-eye-view-parcels /root/burghs-eye-view-parcels

CMD ["R", "-e shiny::runApp('/root/burghs-eye-view-parcels')"]
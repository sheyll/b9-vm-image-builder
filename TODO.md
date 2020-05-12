Add support for nspawn:


   sudo mkdir -p               882cd8da-249e-5b20-9bdc-fa0b65ee7ac6/data
   sudo mount -o loop data.raw 882cd8da-249e-5b20-9bdc-fa0b65ee7ac6/data

   sudo systemd-nspawn \
          -i prod-el7.centos-20.1.0_77C1BAADE2ECB79D.raw \
          --bind 882cd8da-249e-5b20-9bdc-fa0b65ee7ac6/data:/data \
          --bind 882cd8da-249e-5b20-9bdc-fa0b65ee7ac6/init-script:/FB95BFBD59259EED-F0BCC04CCCD5489 \
          --bind artifact-instances/talkflowd-FB95BFBD59259EED-F0BCC04CCCD5489:/b9mnt/GENERATED_SOURCES \
          -E 'PATH=/bin:/sbin:/usr/bin' \
          /FB95BFBD59259EED-F0BCC04CCCD5489/init.sh


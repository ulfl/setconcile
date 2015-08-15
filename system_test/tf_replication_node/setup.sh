set -e

wget http://s3.amazonaws.com/downloads.basho.com/riak/1.3/1.3.2/ubuntu/precise/riak_1.3.2-1_amd64.deb
sudo dpkg -i riak_1.3.2-1_amd64.deb
rm riak_1.3.2-1_amd64.deb
sudo patch /etc/riak/app.config -p2 < ./riak.patch
rm riak.patch

echo "* soft nofile 65536" | sudo tee -a /etc/security/limits.conf
echo "* hard nofile 65536" | sudo tee -a /etc/security/limits.conf

sudo mkdir -p /opt/setconcile/etc
sudo chown -R ubuntu /opt/setconcile


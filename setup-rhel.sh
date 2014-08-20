
OWN_ADDR=$(/sbin/ifconfig eth0 | grep 'inet addr:' | cut -d: -f2 | awk '{ print $1 }')
GTW_ADDR=10.16.4.1

yum -y install git
yum -y install wxBase
yum -y install wxGTK
yum -y install wxGTK-gl
yum -y install unixODBC
yum -y install sqlite-devel

wget http://packages.erlang-solutions.com/site/esl/esl-erlang/FLAVOUR_1_general/esl-erlang_17.1-1~centos~6_amd64.rpm
rpm -ivh esl-erlang_17.1-1~centos~6_amd64.rpm

mkdir ~/storage

cd ~/storage
git clone https://github.com/alexeyr/erlang-sqlite3.git
cd erlang-sqlite3/
make test

cd ~/storage
git clone https://github.com/mliszcz/inz.git
/bin/cp erlang-sqlite3/priv/sqlite3_drv.so inz/erlang-sqlite3/priv
cd inz
make all
cp node.config.template node.config
sed -i "s|^{core_work_dir,.*$|{core_work_dir, \"/root/storage/work_dir\"}\.|g" node.config
sed -i "s|^{dist_initial_node,.*$|{dist_initial_node, \"ds@$GTW_ADDR\"}\.|g" node.config

# echo "10.16.4.1   ds1.storage.cluster" >> /etc/hosts
# echo "10.16.4.2   ds2.storage.cluster" >> /etc/hosts

# hostname ds1.storage.cluster 	# node1
# hostname ds2.storage.cluster 	# node2

# open ports
# -A INPUT -m state --state NEW -m tcp -p tcp --dport 4369 -j ACCEPT
# -A INPUT -m state --state NEW -m tcp -p tcp --dport 9101 -j ACCEPT
# -A INPUT -m state --state NEW -m tcp -p tcp --dport 9102 -j ACCEPT
# -A INPUT -m state --state NEW -m tcp -p tcp --dport 9103 -j ACCEPT
# -A INPUT -m state --state NEW -m tcp -p tcp --dport 9104 -j ACCEPT

# erl -setcookie cook -name ds@130.246.251.168 -kernel inet_dist_listen_min 9101 inet_dist_listen_max 9104
# erl -setcookie cook -name ds@130.246.143.183 -kernel inet_dist_listen_min 9101 inet_dist_listen_max 9104

# ping should work
# (ds@130.246.251.168)1> net_adm:ping('ds@130.246.143.183').
# pong

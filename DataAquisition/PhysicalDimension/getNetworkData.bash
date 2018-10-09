#download MC2 data
URL1=http://www.cs.umd.edu/hcil/varepository/VAST%20Challenge%202011/datasets/MiniChallenge2_Core_Data.zip
echo $URL1
zipped1=../../Database/raw/PhysicalDimension/logs.zip
outfile1=../../Database/raw/PhysicalDimension/logs
echo $zipped1
echo $outfile1
wget $URL1 -O $zipped1
unzip $zipped1 -d $outfile1
rm -rf $zipped1


#download pcap data
URL2=https://www.cs.umd.edu/hcil/varepository/VAST%20Challenge%202011/datasets/PCAP.zip
echo $URL2
zipped2=../../Database/raw/PhysicalDimension/pcaps.zip
outfile2=../../Database/raw/PhysicalDimension/pcaps
echo $zipped2
echo $outfile2
wget $URL2 -O $zipped2
unzip $zipped2 -d $outfile2
rm -rf $zipped2

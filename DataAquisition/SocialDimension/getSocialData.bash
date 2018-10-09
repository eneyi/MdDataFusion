#download MC2 data
URL1=https://www.cs.umd.edu/hcil/varepository/VAST%20Challenge%202011/datasets/MC_1_Materials_3-30-2011.zip
echo $URL1
zipped1=../../Database/raw/SocialDimension/microblogs.zip
outfile1=../../Database/raw/SocialDimension/microblogs
echo $zipped1
echo $outfile1
wget $URL1 -O $zipped1
unzip $zipped1 -d $outfile1
rm -rf $zipped1

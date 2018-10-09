inputdir=../../Database/raw/PhysicalDimension/pcaps/
outputdir=../../Database/cleaned/PhysicalDimension/pcaps/

for i in $( ls $inputdir);
  do str=$(echo $i| cut -d'.' -f 1);
  year=$(echo $str| cut -d'_' -f 1);
  num=$(echo $str| cut -d'_' -f 4);
  filename=$outputdir$year"_"$num".csv";
  #tshark -r $inputdir$i -T fields -e frame.time -e frame.time_epoch -e ip.src -e ip.dst -e tcp.srcport -e tcp.dstport -e ip.proto -e ip.len -e frame.len -e frame.cap_len -e ip.ttl -e eth.len -e ip.flags -e udp.srcport -e udp.dstport -e ip.frag_offset > $filename;
  tshark -r $inputdir$i -T fields -e tcp.flags.syn
 done;

import pyBigWig
import argparse
import re
parser = argparse.ArgumentParser(description='Call depth base on the coordinates of gtf/bed')
parser.add_argument('--gtf', type=str, help='Path to gtf file')
parser.add_argument('--bw', type=str, help='Path to wig file')
parser.add_argument('--bed', type=str, help='Path to bed file')
parser.add_argument('--addinfo', type=str, help='add info to each rows')
args = parser.parse_args()

bw_file = args.bw 
#"/Users/liudongyao/Downloads/Rproject/wormSyn/raw_data/wig/halAlignmentDepth_caenorhabditis_elegans.bw"
gtf = args.gtf
#"/Users/liudongyao/Downloads/workprogress/genome/c_elegans.PRJNA13758.WS285.canonical_geneset.gtf"
bed = args.bed

def bw_region_mean(bw_file, chromosome, start, end):
	bw = pyBigWig.open(bw_file)
	region_data = bw.values(chromosome, int(start), int(end))
	mean_coverage = sum(region_data) / len(region_data)
	return mean_coverage
	bw.close()
if gtf is not None:
	with open(gtf, "r") as handle:
		for line in handle.readlines():
			line = line.strip()
			if not re.match(r"^#", line):
				line_elements = line.split("\t")
				if line_elements[3] != line_elements[4]:
					coverage = bw_region_mean(bw_file = bw_file, chromosome = line_elements[0], start = line_elements[3], end = line_elements[4])
				if args.addinfo is not None:
					print(f"{line}\t{coverage}\t{args.addinfo}")
				else:
					print(f"{line}\t{coverage}")

if bed is not None:
	with open(bed, "r") as handle:
		for line in handle.readlines():
			line = line.strip()
			line_elements = line.split("\t")
			coverage = bw_region_mean(bw_file = bw_file, chromosome = line_elements[0], start = line_elements[1], end = line_elements[2])
			print(f"{line}\t{coverage}")

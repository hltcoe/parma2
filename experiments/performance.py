#!/usr/bin/env python
import os, sys, subprocess
import re
from tools.tsub import Job

# load jobs, call job methods

# main performance number (macro-full-F1, averaged)
# all performance


def performance(logfile, metric='micro-F1'):
	assert logfile is not None
	key = "^average on "
	cmd = "grep \"%s\" %s" % (key, logfile)
	s = subprocess.check_output(cmd, shell=True).strip()
	for line in s.split('\n'):
		# average on RFLemmaTestSet, micro-F1 = (mu=0.559, lo=-Infinity, hi=Infinity)
		# average on EECBCVExperiment, micro-F1 = (mu=0.684, lo=0.652, hi=0.715)
		regex = "average on (\w+), %s = \(mu=0.(\d+), lo=.*" % (metric)
		m = re.match(regex, line)
		if m:
			print float(m.group(2)) / 1000.0, m.group(1), logfile

if __name__ == '__main__':
	if len(sys.argv) > 1:
		directories = sys.argv[1:]
	else:
		r = raw_input('please give a folder with job folders in it: ')
		directories = os.listdir(r.strip()) 
	for f in directories:
		average = 'eecb' in f
		j = Job.fromDir(f, quiet=True)
		if j.isFinished():
			log = j.newestLog()
			performance(log)
		#else: print '\tskipping', j.name, 'because its not finished'



#!/usr/bin/env python
import os, time, math, sys, subprocess
from tools.tsub import Job
from tools.Misc import *

def featureProgression():
	def nonEmptySubsets(things):
		from itertools import combinations
		for k in range(1, len(things)+1):
			for c in combinations(things, k):
				yield c
	yield ('generic.ConciseFeatures',)
	yield ('generic.ConciseFeaturesSlow',)
	others = ['RedisPPDB', 'TEDAlignment'] #'PathFeatures']
	for nes in nonEmptySubsets(others):
		yield ('generic.ConciseFeaturesSlow',) + nes

def experiments():
	# RF
	yield 'RFLemmaTestSet'
	yield 'MixtureTrainRFTestExperiment'
	yield 'MixtureTrainRFTestExperimentWithLDC'

	# EECB
	yield 'EECBLemmaExperiment'
	yield 'EECBCVExperiment'

	# LDC
	yield 'LeastOverlapLemma'
	yield 'LeastOverlapCV'

def main(args):

	projectRoot = '/home/hltcoe/twolfe/miniScale2013/parma/'
	config = os.path.join(projectRoot, 'parma.basic.config')

	print 'compiling....'
	subprocess.check_call(os.path.join(projectRoot, 'make_jar.sh'))

	classpath = subprocess.check_output(os.path.join(projectRoot, 'classpath.sh')).strip()

	didLemmaExps = set()	# values are experiments
	jobs = []
	for l1 in [1.0, 0.1, 0.01, 10.0, 0.001, 100.0]:
		for split in (True, False):

			l1p = 5.0*l1 if split else l1

			for featSet in featureProgression():
				for exp in experiments():

					if 'Lemma' in exp and split:
						continue
					if 'Lemma' in exp and exp in didLemmaExps:
						continue
					didLemmaExps.add(exp)

					print "e=%s, f=%s, l1=%s, s=%s" % (exp, featSet, l1p, split)
					dir = 'output.acl'
					name = "%s-%s-%s" % (exp, '_'.join(featSet), str(l1p))
					if split: name += '-split'
					j = Job(dir, name, jobFailed, interactive=True)
					jobs.append(j)
					j.setJavaOption('experiments', exp)
					j.setJavaOption('features', ' '.join(featSet))
					j.setJavaOption('inference.ham.L1penalty', str(l1p))
					j.setJavaOption('features.split.predarg', 'all')

					diag_dir = 'diagnostics'
					j.setJavaOption('diagnostics.parameter.outdir', j.getResourceDirectory(diag_dir, 'parameters'))
					j.setJavaOption('diagnostics.profile.file', j.getResourceFile(diag_dir, 'profile_times.txt'))
					j.setJavaOption('diagnostics.lexical-overlap-perf', j.getResourceDirectory(diag_dir, 'lexical-overlap-perf'))

					xmx = 6		# in GB
					if 'LDC' in exp or 'LeastOverlap' in exp:
						xmx += 13
					if any('Slow' in x for x in featSet):
						xmx += 5
					mem_free = int(xmx*1.5 + 2.5)
					args = [config]
					j.setSubmission(classpath, 'edu.jhu.parma.experiments.PipelineRunner', args,
						xmx=str(xmx)+'G', mem_free=str(mem_free)+'G', profile=False, asserts=True)
					print j.qsubScript()

	r = raw_input("there are %d jobs, would you like to submit? [y|n] " % (len(jobs)))
	if r.lower() == 'y':
		for i, j in enumerate(jobs):
			if i > 0: time.sleep(0.5)
			j.submit()
		os.system('qinfo')
	else:
		print 'not submitting any jobs'
	
def jobFailed(self, logfile):
	print 'in jobFailed function, logfile =', logfile
	assert os.path.exists(logfile)
	assert os.path.isfile(logfile)
	r = subprocess.check_output("grep -m 1 -c Exception %s; exit 0" % (logfile), shell=True)
	if r.strip() != '0':
		return True
	r = subprocess.check_output("grep -m 1 -c \"Error occurred during initialization of VM\" %s; exit 0" % (logfile), shell=True)
	if r.strip() != '0':
		return True
	return False

if __name__ == '__main__':
	main(sys.argv)


